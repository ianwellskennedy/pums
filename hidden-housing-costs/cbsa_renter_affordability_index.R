# 0. SETUP ----

library(tidyverse)
library(readxl)
library(tidycensus)
library(sf)
library(openxlsx)
library(zoo)
library(spatstat)
library(tigris)
library(arcgisbinding)

# Configuration
config <- list(
  acs_year = 2024,
  reference_month = as.Date("2026-03-31"),  # Use month-end to match Zillow
  
  # API keys
  census_api_key = "6dd2c4143fc5f308c1120021fb663c15409f3757",
  
  # File paths
  puma_cbsa_crosswalk = "C:/Users/ianwe/Downloads/shapefiles/crossover_files/puma_2020_to_cbsa_2023.xlsx",
  zillow_payments     = "C:/Users/ianwe/Downloads/Metro_mortgage_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  zillow_prices       = "C:/Users/ianwe/Downloads/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  zillow_crosswalk    = "C:/Users/ianwe/Downloads/github/zillow/inputs/zillow_metro_crosswalk.xlsx",
  output_dir          = "hidden-housing-costs/outputs/",
  output_file_path_spatial_data = "hidden-housing-costs/outputs/metro_affordability_data_2024.shp",
  
  # Zillow download URLs (no timestamp parameter needed — these are stable)
  # Data is updated on the 16th of each month. If running before the 16th,
  # the file will reflect the prior month; update reference_month accordingly.
  zillow_payments_url = "https://files.zillowstatic.com/research/public_csvs/mortgage_payment/Metro_mortgage_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  zillow_prices_url   = "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
)

census_api_key(config$census_api_key)

# 1. HELPER FUNCTIONS ----

#' Standardize PUMA codes to 5 digits and STATE codes to 2 digits
standardize_puma_codes <- function(df) {
  df %>%
    mutate(
      PUMA = str_pad(as.character(PUMA), width = 5, pad = "0"),
      across(any_of(c("ST", "STATE")), ~str_pad(as.character(.), width = 2, pad = "0"))
    )
}

#' Recode PUMS utility variables to annual costs
recode_utility_costs <- function(df) {
  df %>%
    mutate(
      # Monthly costs → Annual (ELEP, GASP, CONP are monthly; code 1 or 2 = no charge/included)
      ELEP_annual = if_else(ELEP %in% c(1, 2), 0, as.numeric(ELEP) * 12),
      GASP_annual = if_else(GASP %in% c(1, 2, 3), 0, as.numeric(GASP) * 12),
      CONP_annual = if_else(CONP %in% c(1, 2), 0, as.numeric(CONP) * 12),
      # Already annual costs (WATP, FULP; code 1 or 2 = no charge)
      WATP_annual = if_else(WATP %in% c(1, 2), 0, as.numeric(WATP)),
      FULP_annual = if_else(FULP %in% c(1, 2), 0, as.numeric(FULP)),
      # Other costs
      INSP_annual  = as.numeric(INSP),
      TAXAMT_annual = as.numeric(TAXAMT),
      VALP_num     = as.numeric(VALP)
    )
}

#' Calculate cost rates (cost / home value)
calculate_cost_rates <- function(df) {
  df %>%
    mutate(
      ins_rate   = INSP_annual   / VALP_num,
      hoa_rate   = CONP_annual   / VALP_num,
      tax_rate   = TAXAMT_annual / VALP_num,
      elec_rate  = ELEP_annual   / VALP_num,
      gas_rate   = GASP_annual   / VALP_num,
      water_rate = WATP_annual   / VALP_num,
      fuel_rate  = FULP_annual   / VALP_num
    ) %>%
    # Remove infinite/NA rates from division by zero
    mutate(across(ends_with("_rate"), ~if_else(is.infinite(.) | is.nan(.), NA_real_, .)))
}

#' Clean Zillow time series data
clean_zillow_timeseries <- function(filepath, value_col_name) {
  read_csv(filepath, show_col_types = FALSE) %>%
    # Remove leading "X" from date columns
    rename_with(~str_remove(., "^X")) %>%
    # Keep only MSA geography
    filter(RegionType == "msa") %>%
    # Pivot all date columns (YYYY-MM-DD format)
    pivot_longer(
      cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
      names_to = "date",
      values_to = value_col_name
    ) %>%
    mutate(
      date     = as.Date(date, format = "%Y-%m-%d"),
      RegionID = as.character(RegionID)
    ) %>%
    # Remove rows with NA dates or values
    filter(!is.na(date), !is.na(.data[[value_col_name]])) %>%
    select(zillow_metro_code = RegionID, date, all_of(value_col_name))
}

#' Download latest Zillow CSV and save to local path
download_zillow_file <- function(url, dest_path) {
  tryCatch({
    download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
    cat("✓ Downloaded:", basename(dest_path), "\n")
    cat("  File size:", round(file.size(dest_path) / 1024, 1), "KB\n")
    cat("  Downloaded at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  }, error = function(e) {
    stop("Failed to download ", url, "\n  Error: ", e$message)
  })
}

# 2. LOAD REFERENCE DATA ----

# PUMA to CBSA crosswalk
puma_cbsa_xwalk <- read_xlsx(config$puma_cbsa_crosswalk) %>%
  rename_with(toupper) %>%
  standardize_puma_codes() %>%
  select(STATE, PUMA, CBSA_CODE, CBSA_NAME, ALLOC_FACTOR) %>%
  mutate(
    ALLOC_FACTOR = as.numeric(ALLOC_FACTOR),
    CBSA_CODE    = as.character(CBSA_CODE)
  )

# Zillow metro code to CBSA crosswalk
zillow_cbsa_xwalk <- read_xlsx(config$zillow_crosswalk) %>%
  mutate(
    zillow_metro_code = as.character(zillow_metro_code),
    GEOID             = as.character(GEOID)
  )

# 3. DOWNLOAD AND LOAD ZILLOW DATA ----

cat("\n=== DOWNLOADING ZILLOW DATA ===\n")

# Download fresh files from Zillow's public S3 bucket.
# Note: Zillow updates these on the 16th of each month.
download_zillow_file(
  url       = config$zillow_payments_url,
  dest_path = config$zillow_payments
)

download_zillow_file(
  url       = config$zillow_prices_url,
  dest_path = config$zillow_prices
)

cat("\n=== LOADING ZILLOW DATA ===\n")

zillow_payments <- clean_zillow_timeseries(config$zillow_payments, "mortgage_payment")
zillow_prices   <- clean_zillow_timeseries(config$zillow_prices,   "home_price")

zillow_data <- zillow_payments %>%
  inner_join(zillow_prices, by = c("zillow_metro_code", "date")) %>%
  inner_join(zillow_cbsa_xwalk, by = "zillow_metro_code") %>%
  # Annualize mortgage payment
  mutate(mortgage_payment_annual = mortgage_payment * 12) %>%
  select(GEOID, date, mortgage_payment_annual, home_price)

cat("\n✓ Zillow data loaded:", nrow(zillow_data), "records\n")
cat("  Date range:", as.character(min(zillow_data$date)), "to", as.character(max(zillow_data$date)), "\n")
cat("  Reference month (", as.character(config$reference_month), ") present?:",
    config$reference_month %in% zillow_data$date, "\n")

# 4. LOAD PUMS DATA ----

pums_vars <- c(
  'SERIALNO', 'PUMA', 'WGTP', 'BLD', 'TEN', 'VACS',
  'VALP', 'INSP', 'TAXAMT', 'ELEP', 'GASP', 'WATP', 'FULP', 'CONP'
)

pums_data <- get_pums(
  variables        = pums_vars,
  year             = config$acs_year,
  survey           = "acs1",
  state            = "all",  # All states for national coverage
  variables_filter = list(
    TEN  = 1:2,  # Owned units (with/without mortgage)
    VACS = 0     # Occupied units only
  ),
  recode    = TRUE,
  show_call = TRUE,
  key       = config$census_api_key
)

cat("✓ PUMS data loaded:", nrow(pums_data), "records\n")

# 5. LOAD INCOME AND POPULATION DATA ----

income_data <- get_acs(
  geography = "cbsa",
  variables = "B25119_003",  # Median household income - renter occupied
  year      = config$acs_year,
  survey    = "acs1",
  key       = config$census_api_key
) %>%
  select(GEOID, median_income_renters = estimate) %>%
  mutate(GEOID = as.character(GEOID))

cat("✓ Income data loaded:", nrow(income_data), "CBSAs\n")

population_data <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",  # Total population
  year      = config$acs_year,
  survey    = "acs1",
  key       = config$census_api_key
) %>%
  select(GEOID, population = estimate) %>%
  mutate(GEOID = as.character(GEOID))

cat("✓ Population data loaded:", nrow(population_data), "CBSAs\n")

# 6. PROCESS PUMS DATA ----

pums_processed <- pums_data %>%
  # Filter to single-family/condo units (exclude mobile homes, boats)
  filter(!BLD %in% c('1', '10')) %>%
  # Keep one record per housing unit
  distinct(SERIALNO, .keep_all = TRUE) %>%
  # Recode utilities and calculate rates
  recode_utility_costs() %>%
  calculate_cost_rates() %>%
  # Standardize PUMA codes
  standardize_puma_codes()

cat("✓ PUMS data processed:", nrow(pums_processed), "housing units\n")

# 7. AGGREGATE DIRECTLY TO CBSA ----

cbsa_cost_rates <- pums_processed %>%
  # Join PUMA to CBSA crosswalk (note: PUMS has "ST" not "STATE")
  inner_join(puma_cbsa_xwalk, by = c("STATE", "PUMA")) %>%
  # Apply allocation factor to weights
  mutate(weight = WGTP * ALLOC_FACTOR) %>%
  # Aggregate to CBSA level
  group_by(CBSA_CODE, CBSA_NAME) %>%
  summarize(
    n_units = sum(weight, na.rm = TRUE),
    # Weighted medians of rates (exclude zeros for insurance and HOA)
    med_ins_rate = weighted.median(
      ins_rate[ins_rate > 0],
      w  = weight[ins_rate > 0],
      na.rm = TRUE
    ),
    med_hoa_rate = weighted.median(
      hoa_rate[hoa_rate > 0],
      w  = weight[hoa_rate > 0],
      na.rm = TRUE
    ),
    med_tax_rate   = weighted.median(tax_rate,   w = weight, na.rm = TRUE),
    med_elec_rate  = weighted.median(elec_rate,  w = weight, na.rm = TRUE),
    med_gas_rate   = weighted.median(gas_rate,   w = weight, na.rm = TRUE),
    med_water_rate = weighted.median(water_rate, w = weight, na.rm = TRUE),
    med_fuel_rate  = weighted.median(fuel_rate,  w = weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Override California property tax rates to 1% (Prop 13)
  mutate(
    med_tax_rate = case_when(
      str_detect(CBSA_NAME, ", CA") ~ 0.01,
      TRUE ~ med_tax_rate
    )
  )

cat("✓ CBSA cost rates calculated:", nrow(cbsa_cost_rates), "CBSAs\n")

# Validate cost rates
cat("\n✓ Cost rate validation:\n")
cat("  Insurance rate range:",
    round(min(cbsa_cost_rates$med_ins_rate, na.rm = TRUE), 4), "to",
    round(max(cbsa_cost_rates$med_ins_rate, na.rm = TRUE), 4), "\n")
cat("  HOA rate range:",
    round(min(cbsa_cost_rates$med_hoa_rate, na.rm = TRUE), 4), "to",
    round(max(cbsa_cost_rates$med_hoa_rate, na.rm = TRUE), 4), "\n")
cat("  Tax rate range:",
    round(min(cbsa_cost_rates$med_tax_rate, na.rm = TRUE), 4), "to",
    round(max(cbsa_cost_rates$med_tax_rate, na.rm = TRUE), 4), "\n")
cat("  CBSAs with missing insurance rates:", sum(is.na(cbsa_cost_rates$med_ins_rate)), "\n")
cat("  CBSAs with missing HOA rates:",       sum(is.na(cbsa_cost_rates$med_hoa_rate)), "\n")

# 8. CALCULATE AFFORDABILITY ----

calculate_affordability <- function(zillow_df, cost_rates_df, income_df, population_df, reference_date) {
  
  # Verify reference date exists
  if (!reference_date %in% zillow_df$date) {
    available <- unique(zillow_df$date[abs(zillow_df$date - reference_date) < 40])
    stop("Reference date ", reference_date, " not found in Zillow data.\n",
         "Available dates near this: ", paste(available, collapse = ", "))
  }
  
  # Get Zillow data for reference date (with 12-month trailing average)
  zillow_month <- zillow_df %>%
    filter(date <= reference_date) %>%
    group_by(GEOID) %>%
    arrange(date) %>%
    mutate(
      mortgage_ttm = rollmean(mortgage_payment_annual, k = 12, align = "right", fill = NA),
      price_ttm    = rollmean(home_price,              k = 12, align = "right", fill = NA)
    ) %>%
    filter(date == reference_date) %>%
    ungroup() %>%
    filter(!is.na(mortgage_ttm), !is.na(price_ttm)) %>%
    select(GEOID, mortgage_ttm, price_ttm)
  
  cat("  Zillow data for", as.character(reference_date), ":", nrow(zillow_month), "metros\n")
  
  # Calculate affordability
  affordability <- cost_rates_df %>%
    inner_join(zillow_month, by = c("CBSA_CODE" = "GEOID")) %>%
    inner_join(income_df,    by = c("CBSA_CODE" = "GEOID")) %>%
    left_join(population_df, by = c("CBSA_CODE" = "GEOID")) %>%
    mutate(
      # Apply cost rates to current Zillow prices
      annual_insurance = med_ins_rate   * price_ttm,
      annual_hoa       = med_hoa_rate   * price_ttm,
      annual_tax       = med_tax_rate   * price_ttm,
      annual_elec      = med_elec_rate  * price_ttm,
      annual_gas       = med_gas_rate   * price_ttm,
      annual_water     = med_water_rate * price_ttm,
      annual_fuel      = med_fuel_rate  * price_ttm,
      
      # Total annual housing cost
      total_annual_cost = mortgage_ttm + annual_insurance + annual_tax +
        annual_elec + annual_gas + annual_water + annual_fuel,
      
      # Component affordability ratios
      mortgage_burden   = mortgage_ttm  / median_income_renters,
      insurance_burden  = annual_insurance / median_income_renters,
      hoa_burden        = annual_hoa     / median_income_renters,
      tax_burden        = annual_tax     / median_income_renters,
      elec_burden       = annual_elec     / median_income_renters,
      gas_burden        = annual_gas     / median_income_renters,
      wat_burden        = annual_water     / median_income_renters,
      fuel_burden       = annual_fuel     / median_income_renters,
      utility_burden    = (annual_elec + annual_gas + annual_fuel) / median_income_renters,
      
      # Total affordability ratio
      renter_affordability_ratio = total_annual_cost / median_income_renters,
      
      # Reference date
      reference_date = reference_date
    ) %>%
    select(
      CBSA_CODE, CBSA_NAME, reference_date, population, n_units,
      median_income_renters, price_ttm, mortgage_ttm,
      renter_affordability_ratio, mortgage_burden, insurance_burden,
      tax_burden, elec_burden, gas_burden, fuel_burden, wat_burden, utility_burden, hoa_burden,
      total_annual_cost, annual_insurance, annual_tax, annual_elec,
      annual_gas, annual_water, annual_fuel, annual_hoa,
      everything()
    )
  
  cat("  Final affordability records:", nrow(affordability), "metros\n\n")
  
  return(affordability)
}

# Calculate for current month
cat("\n=== CALCULATING AFFORDABILITY ===\n")

affordability_current <- calculate_affordability(
  zillow_data,
  cbsa_cost_rates,
  income_data,
  population_data,
  config$reference_month
)

# 9. VALIDATION CHECKS ----

cat("=== VALIDATION SUMMARY ===\n")

affordability_current %>%
  summarize(
    n_metros              = n(),
    total_population      = sum(population, na.rm = TRUE),
    pct_missing_income    = round(mean(is.na(median_income_renters)) * 100, 1),
    median_affordability  = round(median(renter_affordability_ratio, na.rm = TRUE), 3),
    mean_affordability    = round(mean(renter_affordability_ratio,   na.rm = TRUE), 3),
    # Population-weighted average
    pop_weighted_affordability = round(
      sum(renter_affordability_ratio * population, na.rm = TRUE) / sum(population, na.rm = TRUE), 3
    ),
    metros_over_50pct = sum(renter_affordability_ratio > 0.5, na.rm = TRUE),
    metros_over_60pct = sum(renter_affordability_ratio > 0.6, na.rm = TRUE)
  ) %>%
  print()

cat("\nTop 10 Most Expensive (by affordability ratio):\n")
affordability_current %>%
  arrange(desc(renter_affordability_ratio)) %>%
  select(CBSA_NAME, population, renter_affordability_ratio,
         median_income_renters, total_annual_cost) %>%
  head(10) %>%
  print()

cat("\nTop 10 Most Affordable:\n")
affordability_current %>%
  arrange(renter_affordability_ratio) %>%
  select(CBSA_NAME, population, renter_affordability_ratio,
         median_income_renters, total_annual_cost) %>%
  head(10) %>%
  print()

# 10. OUTPUT ----

# Current month snapshot
write.xlsx(
  affordability_current,
  paste0(config$output_dir, "cbsa_affordability_", format(config$reference_month, "%Y_%m"), ".xlsx")
)

cat("\n✓ Affordability index calculated for", nrow(affordability_current), "CBSAs\n")
cat("✓ Output saved to:", paste0(config$output_dir, "cbsa_affordability_",
                                 format(config$reference_month, "%Y_%m"), ".xlsx\n"))

# 11. SPATIAL JOIN AND MAPPING ----

# Pull CBSA boundaries from Census TIGER via tigris
cbsa_boundaries <- core_based_statistical_areas(
  cb         = TRUE,    # Use cartographic boundary (simplified, better for mapping)
  year       = 2023,    # Match your crosswalk vintage
  resolution = "5m"     # Options: "500k", "5m", "20m" — 5m is a good balance
) %>%
  select(GEOID, geometry) %>%
  mutate(GEOID = as.character(GEOID))

cat("✓ CBSA boundaries loaded:", nrow(cbsa_boundaries), "features\n")

# Join affordability data to spatial boundaries
affordability_spatial <- affordability_current %>%
  left_join(cbsa_boundaries, by = c("CBSA_CODE" = "GEOID")) %>%
  st_as_sf()

cat("✓ Spatial join complete:", nrow(affordability_spatial), "CBSAs with geometry\n")
cat("  Dropped (no affordability data):", nrow(cbsa_boundaries) - nrow(affordability_spatial), "\n")

# Quick sanity check — should be an sf object
cat("  CRS:", st_crs(affordability_spatial)$srid, "\n")

arc.check_product()

# Save as shapefile via ArcGIS binding
arc.write(
  data      = affordability_spatial,
  path      = config$output_file_path_spatial_data,
  overwrite = TRUE,
  validate  = TRUE
)

cat("✓ Spatial file saved\n")

# 12. MONTHLY TIME SERIES AUTOMATION ----

#' Generate monthly affordability time series
generate_monthly_timeseries <- function(
    zillow_df,
    cost_rates_df,
    income_df,
    population_df,
    start_date,
    end_date
) {
  
  # Find all available month-end dates in Zillow data within range
  available_dates <- unique(zillow_df$date)
  target_dates    <- available_dates[available_dates >= start_date & available_dates <= end_date]
  target_dates    <- sort(target_dates)
  
  cat("\nGenerating time series for", length(target_dates), "months\n")
  cat("From:", as.character(min(target_dates)), "to", as.character(max(target_dates)), "\n\n")
  
  # For each month, calculate affordability
  results <- map_dfr(target_dates, function(ref_date) {
    message("Processing: ", ref_date)
    
    tryCatch({
      calculate_affordability(
        zillow_df,
        cost_rates_df,
        income_df,
        population_df,
        ref_date
      )
    }, error = function(e) {
      message("  Error: ", e$message)
      return(NULL)
    })
  })
  
  return(results)
}

# Example: Generate time series from Jan 2020 to current
# (Uncomment to run)
affordability_timeseries <- generate_monthly_timeseries(
  zillow_data,
  cbsa_cost_rates,
  income_data,
  population_data,
  start_date = as.Date("2020-01-01"),
  end_date   = config$reference_month
)

# Output time series (long format)
write.xlsx(
  affordability_timeseries,
  paste0(config$output_dir, "cbsa_affordability_timeseries_long.xlsx")
)

# Reshape to wide format (optional - one row per CBSA)
affordability_wide <- affordability_timeseries %>%
  select(CBSA_CODE, CBSA_NAME, reference_date, renter_affordability_ratio) %>%
  pivot_wider(
    names_from   = reference_date,
    values_from  = renter_affordability_ratio,
    names_prefix = "afford_"
  )

write.xlsx(
  affordability_wide,
  paste0(config$output_dir, "cbsa_affordability_timeseries_wide.xlsx")
)
