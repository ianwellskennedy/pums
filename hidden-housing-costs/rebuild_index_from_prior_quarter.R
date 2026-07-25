# Rebuild the CBSA renter affordability index for a new reference month by
# carrying forward the PUMS/ACS-derived columns from a prior quarter's output.
#
# Companion to cbsa_renter_affordability_index.R. That script is the full
# pipeline and should be used whenever acs_year changes. This one is for the
# common case where only the quarter moves: with acs_year fixed, the seven
# med_*_rate columns, median_income_renters, population and n_units are all
# unchanged, so the only new inputs are the two Zillow series.
#
# Advantages: needs no Census API call, no PUMA->CBSA crosswalk file, and only
# packages already required elsewhere in this repo (no tidycensus/zoo/
# spatstat/tigris/arcgisbinding). Outputs xlsx only, no shapefile.
#
# Run from the repo root.

suppressMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(readxl); library(openxlsx)
})

config <- list(
  reference_month = as.Date("2026-06-30"),  # Q2 2026; month-end to match Zillow

  prior_output = "hidden-housing-costs/outputs/cbsa_affordability_2026_03.xlsx",
  output_dir   = "hidden-housing-costs/outputs/",

  # Zillow updates these on the 16th of each month. Running before the 16th
  # yields the prior month; set reference_month accordingly.
  zillow_payments_url = "https://files.zillowstatic.com/research/public_csvs/mortgage_payment/Metro_mortgage_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  zillow_prices_url   = "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
)

# 1. CARRY FORWARD PUMS/ACS COLUMNS ----

prior <- read_xlsx(config$prior_output) %>%
  mutate(CBSA_CODE = as.character(CBSA_CODE))

cbsa_cost_rates <- prior %>% select(CBSA_CODE, CBSA_NAME, n_units, starts_with("med_"))
income_data     <- prior %>% select(CBSA_CODE, median_income_renters)
population_data <- prior %>% select(CBSA_CODE, population)

cat("Carried forward", nrow(cbsa_cost_rates), "CBSAs from", basename(config$prior_output), "\n")

# 2. DOWNLOAD ZILLOW DATA ----

zillow_dir      <- tempfile("zillow_"); dir.create(zillow_dir)
zillow_payments <- file.path(zillow_dir, "metro_mortgage_payment.csv")
zillow_prices   <- file.path(zillow_dir, "metro_zhvi.csv")

download_zillow_file <- function(url, dest_path) {
  tryCatch({
    download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
    cat("Downloaded:", basename(dest_path),
        sprintf("(%.1f KB)", file.size(dest_path) / 1024), "\n")
  }, error = function(e) stop("Failed to download ", url, "\n  Error: ", e$message))
}

download_zillow_file(config$zillow_payments_url, zillow_payments)
download_zillow_file(config$zillow_prices_url,   zillow_prices)

# 3. ZILLOW METRO -> CBSA CROSSWALK ----
# Zillow names metros by principal city ("Albany, NY"); CBSA names are full
# ("Albany-Schenectady-Troy, NY"). Reducing the CBSA name to first city + first
# state matches 383 of 385 metros with no ambiguity; two need overrides.
# Replaces the external zillow_metro_crosswalk.xlsx.

zillow_short_name <- function(cbsa_name) {
  city  <- str_split_i(cbsa_name, ", ", 1) %>% str_split_i("-", 1)
  state <- str_split_i(cbsa_name, ", ", 2) %>% str_split_i("-", 1)
  paste0(city, ", ", state)
}

name_overrides <- c(
  "Louisville/Jefferson County, KY-IN" = "Louisville, KY",
  "Wildwood-The Villages, FL"          = "The Villages, FL"
)

zillow_regions <- read_csv(zillow_prices, show_col_types = FALSE,
                           col_select = c(RegionID, RegionName, RegionType)) %>%
  filter(RegionType == "msa") %>%
  transmute(zillow_metro_code = as.character(RegionID), match_name = RegionName)

zillow_cbsa_xwalk <- cbsa_cost_rates %>%
  select(CBSA_CODE, CBSA_NAME) %>%
  mutate(match_name = coalesce(name_overrides[CBSA_NAME], zillow_short_name(CBSA_NAME))) %>%
  inner_join(zillow_regions, by = "match_name") %>%
  select(GEOID = CBSA_CODE, zillow_metro_code)

stopifnot(!any(duplicated(zillow_cbsa_xwalk$GEOID)),
          !any(duplicated(zillow_cbsa_xwalk$zillow_metro_code)))

unmatched <- setdiff(cbsa_cost_rates$CBSA_CODE, zillow_cbsa_xwalk$GEOID)
if (length(unmatched) > 0) {
  warning("Unmatched CBSAs: ",
          paste(cbsa_cost_rates$CBSA_NAME[cbsa_cost_rates$CBSA_CODE %in% unmatched],
                collapse = "; "))
}
cat("Crosswalk built:", nrow(zillow_cbsa_xwalk), "of", nrow(cbsa_cost_rates), "CBSAs\n")

# 4. LOAD ZILLOW TIME SERIES ----

clean_zillow_timeseries <- function(filepath, value_col_name) {
  read_csv(filepath, show_col_types = FALSE) %>%
    rename_with(~str_remove(., "^X")) %>%
    filter(RegionType == "msa") %>%
    pivot_longer(cols = matches("^[0-9]{4}-[0-9]{2}-[0-9]{2}$"),
                 names_to = "date", values_to = value_col_name) %>%
    mutate(date     = as.Date(date, format = "%Y-%m-%d"),
           RegionID = as.character(RegionID)) %>%
    filter(!is.na(date), !is.na(.data[[value_col_name]])) %>%
    select(zillow_metro_code = RegionID, date, all_of(value_col_name))
}

zillow_data <- clean_zillow_timeseries(zillow_payments, "mortgage_payment") %>%
  inner_join(clean_zillow_timeseries(zillow_prices, "home_price"),
             by = c("zillow_metro_code", "date")) %>%
  inner_join(zillow_cbsa_xwalk, by = "zillow_metro_code") %>%
  mutate(mortgage_payment_annual = mortgage_payment * 12) %>%
  select(GEOID, date, mortgage_payment_annual, home_price)

cat("Zillow records:", nrow(zillow_data),
    "| range:", as.character(min(zillow_data$date)),
    "to", as.character(max(zillow_data$date)), "\n")

# 5. AFFORDABILITY ----

calculate_affordability <- function(zillow_df, cost_rates_df, income_df,
                                    population_df, reference_date) {

  if (!reference_date %in% zillow_df$date) {
    available <- sort(unique(zillow_df$date[abs(zillow_df$date - reference_date) < 40]))
    stop("Reference date ", reference_date, " not found in Zillow data.\n",
         "Available dates near this: ", paste(available, collapse = ", "))
  }

  # Trailing-twelve-month mean ending at the reference date, equivalent to the
  # zoo::rollmean(k = 12, align = "right") used in the full pipeline.
  zillow_month <- zillow_df %>%
    filter(date <= reference_date) %>%
    group_by(GEOID) %>%
    arrange(date, .by_group = TRUE) %>%
    summarize(
      mortgage_ttm = if (n() >= 12) mean(tail(mortgage_payment_annual, 12)) else NA_real_,
      price_ttm    = if (n() >= 12) mean(tail(home_price, 12)) else NA_real_,
      last_date    = max(date),
      .groups = "drop"
    ) %>%
    filter(last_date == reference_date, !is.na(mortgage_ttm), !is.na(price_ttm)) %>%
    select(GEOID, mortgage_ttm, price_ttm)

  cat("  Zillow metros at", as.character(reference_date), ":", nrow(zillow_month), "\n")

  cost_rates_df %>%
    inner_join(zillow_month, by = c("CBSA_CODE" = "GEOID")) %>%
    inner_join(income_df,    by = "CBSA_CODE") %>%
    left_join(population_df, by = "CBSA_CODE") %>%
    mutate(
      annual_insurance = med_ins_rate   * price_ttm,
      annual_hoa       = med_hoa_rate   * price_ttm,
      annual_tax       = med_tax_rate   * price_ttm,
      annual_elec      = med_elec_rate  * price_ttm,
      annual_gas       = med_gas_rate   * price_ttm,
      annual_water     = med_water_rate * price_ttm,
      annual_fuel      = med_fuel_rate  * price_ttm,

      # HOA is deliberately excluded from the total, matching the full pipeline
      total_annual_cost = mortgage_ttm + annual_insurance + annual_tax +
        annual_elec + annual_gas + annual_water + annual_fuel,

      mortgage_burden  = mortgage_ttm     / median_income_renters,
      insurance_burden = annual_insurance / median_income_renters,
      hoa_burden       = annual_hoa       / median_income_renters,
      tax_burden       = annual_tax       / median_income_renters,
      elec_burden      = annual_elec      / median_income_renters,
      gas_burden       = annual_gas       / median_income_renters,
      wat_burden       = annual_water     / median_income_renters,
      fuel_burden      = annual_fuel      / median_income_renters,
      utility_burden   = (annual_elec + annual_gas + annual_fuel) / median_income_renters,

      renter_affordability_ratio = total_annual_cost / median_income_renters,
      reference_date = reference_date
    ) %>%
    select(
      CBSA_CODE, CBSA_NAME, reference_date, population, n_units,
      median_income_renters, price_ttm, mortgage_ttm,
      renter_affordability_ratio, mortgage_burden, insurance_burden,
      tax_burden, elec_burden, gas_burden, fuel_burden, wat_burden,
      utility_burden, hoa_burden,
      total_annual_cost, annual_insurance, annual_tax, annual_elec,
      annual_gas, annual_water, annual_fuel, annual_hoa,
      everything()
    )
}

# 6. REGRESSION CHECK AGAINST PRIOR QUARTER ----
# Recompute the prior quarter's reference date from current Zillow data and
# compare. Small differences are expected — Zillow revises its smoothed,
# seasonally adjusted series — but a large gap points to a bad metro match.

prior_ref <- as.Date(prior$reference_date[1])
cat("\n=== REGRESSION CHECK: recompute", as.character(prior_ref), "===\n")

recomputed <- calculate_affordability(zillow_data, cbsa_cost_rates, income_data,
                                      population_data, prior_ref)

cmp <- prior %>%
  select(CBSA_CODE, CBSA_NAME, old_price = price_ttm,
         old_ratio = renter_affordability_ratio) %>%
  inner_join(recomputed %>% select(CBSA_CODE, price_ttm, renter_affordability_ratio),
             by = "CBSA_CODE") %>%
  mutate(price_pct = abs(price_ttm - old_price) / old_price,
         ratio_abs = abs(renter_affordability_ratio - old_ratio))

cat("  price_ttm — median abs diff:", sprintf("%.3f%%", 100 * median(cmp$price_pct)),
    "| max:", sprintf("%.2f%%", 100 * max(cmp$price_pct)), "\n")
cat("  ratio     — median abs diff:", sprintf("%.5f", median(cmp$ratio_abs)),
    "| max:", sprintf("%.5f", max(cmp$ratio_abs)), "\n")
cat("  largest revisions:\n")
print(cmp %>% arrange(desc(ratio_abs)) %>%
        select(CBSA_NAME, old_ratio, renter_affordability_ratio, ratio_abs) %>%
        head(5) %>% as.data.frame())

# 7. CALCULATE CURRENT QUARTER ----

cat("\n=== CALCULATING", format(config$reference_month, "%Y-%m-%d"), "===\n")

affordability_current <- calculate_affordability(
  zillow_data, cbsa_cost_rates, income_data, population_data, config$reference_month
)

cat("\n=== VALIDATION SUMMARY ===\n")
affordability_current %>%
  summarize(
    n_metros             = n(),
    total_population     = sum(population, na.rm = TRUE),
    pct_missing_income   = round(mean(is.na(median_income_renters)) * 100, 1),
    median_affordability = round(median(renter_affordability_ratio, na.rm = TRUE), 3),
    mean_affordability   = round(mean(renter_affordability_ratio, na.rm = TRUE), 3),
    pop_weighted_affordability = round(
      sum(renter_affordability_ratio * population, na.rm = TRUE) /
        sum(population, na.rm = TRUE), 3),
    metros_over_50pct = sum(renter_affordability_ratio > 0.5, na.rm = TRUE),
    metros_over_60pct = sum(renter_affordability_ratio > 0.6, na.rm = TRUE)
  ) %>% as.data.frame() %>% print()

cat("\nTop 10 Most Expensive:\n")
affordability_current %>% arrange(desc(renter_affordability_ratio)) %>%
  select(CBSA_NAME, population, renter_affordability_ratio,
         median_income_renters, total_annual_cost) %>%
  head(10) %>% as.data.frame() %>% print()

cat("\nTop 10 Most Affordable:\n")
affordability_current %>% arrange(renter_affordability_ratio) %>%
  select(CBSA_NAME, population, renter_affordability_ratio,
         median_income_renters, total_annual_cost) %>%
  head(10) %>% as.data.frame() %>% print()

qoq <- prior %>%
  select(CBSA_CODE, q_prior = renter_affordability_ratio) %>%
  inner_join(affordability_current %>% select(CBSA_CODE, q_current = renter_affordability_ratio),
             by = "CBSA_CODE") %>%
  mutate(chg = q_current - q_prior)
cat("\nQoQ change — median:", sprintf("%+.5f", median(qoq$chg)),
    "| metros worsening:", sum(qoq$chg > 0), "of", nrow(qoq), "\n")

# 8. OUTPUT ----

out_path <- paste0(config$output_dir, "cbsa_affordability_",
                   format(config$reference_month, "%Y_%m"), ".xlsx")
write.xlsx(affordability_current, out_path)
cat("\nOutput saved to:", out_path, "\n")
