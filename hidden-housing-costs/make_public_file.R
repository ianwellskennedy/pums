# Format a quarterly CBSA affordability output as the public-facing workbook:
# labelled headers, currency/percent formats, sorted least to most affordable,
# and a colour scale on the headline ratio.
#
# Input is the xlsx written by cbsa_renter_affordability_index.R or
# rebuild_index_from_prior_quarter.R. Run from the repo root.

suppressMessages({
  library(dplyr); library(readxl); library(openxlsx)
})

config <- list(
  input    = "hidden-housing-costs/outputs/cbsa_affordability_2026_09.xlsx",
  output   = "hidden-housing-costs/outputs/cbsa_affordability_2026_09_public.xlsx",
  acs_year = 2024  # ACS vintage behind the input's income, population and cost rates
)

# Output column -> public header, in sheet order. The * and ** markers are
# defined in the Notes section of the Sources & Methodology tab.
columns <- c(
  CBSA_NAME                  = "CBSA_NAME",
  reference_date             = "Reference date",
  population                 = "Population",
  n_units                    = "Housing units",
  median_income_renters      = "Median Household Income: Renters",
  price                  = "Typical-value Home Price (from Zillow)",
  mortgage_annual               = "Total annual mortgage payment on purchase of a typical-value home**",
  renter_affordability_ratio = "Total annual payment as a share of renters' median household income",
  mortgage_burden            = "Mortgage payment as a share of renters' median household income",
  insurance_burden           = "Insurance bill as a share of renters' median household income",
  tax_burden                 = "Tax bill as a share of renters' median household income",
  utility_burden             = "Utility bill as a share of renters' median household income",
  hoa_burden                 = "HOA fee as a share of renters' median household income",
  total_annual_cost          = "Total annual payment on purchase of a typical-value home** including utilities",
  annual_insurance           = "Median Hazard/Fire/Flood Insurance Payment* (for Homeowners with Insurance)",
  annual_tax                 = "Median Property Tax Bill",
  annual_elec                = "Median Electricity Bill",
  annual_gas                 = "Median Gas Bill",
  annual_water               = "Median Water Bill",
  annual_fuel                = "Median Fuel/Oil Bill",
  annual_hoa                 = "Median HOA Fee (for Homeowners with an HOA)"
)

# 1. LOAD AND ORDER ----

public <- read_xlsx(config$input) %>%
  mutate(reference_date = as.Date(reference_date)) %>%
  arrange(desc(renter_affordability_ratio)) %>%
  select(all_of(names(columns)))

headers <- unname(columns)

n_rows <- nrow(public)
rows   <- 2:(n_rows + 1)
col_of <- function(name) match(name, names(public))

ref_date   <- max(public$reference_date)
sheet_name <- paste0("Affordability Index (Q", (as.integer(format(ref_date, "%m")) - 1) %/% 3 + 1,
                     " ", format(ref_date, "%Y"), ")")

# 2. WRITE ----

wb <- createWorkbook()
addWorksheet(wb, sheet_name)
modifyBaseFont(wb, fontName = "Avenir Book", fontSize = 11)

writeData(wb, 1, as.data.frame(t(headers)), colNames = FALSE)
writeData(wb, 1, public, startRow = 2, colNames = FALSE)

# 3. FORMAT ----

header_style <- function(colour = "#000000", border = NULL) {
  createStyle(fontName = "Avenir Next LT Pro", textDecoration = "bold", fontColour = colour,
              halign = "center", valign = "center", wrapText = TRUE,
              border = border, borderStyle = "thin")
}
addStyle(wb, 1, header_style(), rows = 1, cols = seq_along(headers))
addStyle(wb, 1, createStyle(fontName = "Avenir Book", textDecoration = "bold", wrapText = TRUE),
         rows = 1, cols = 1:4, gridExpand = TRUE)
addStyle(wb, 1, header_style(border = c("top", "bottom")), rows = 1,
         cols = col_of("median_income_renters"))
addStyle(wb, 1, header_style("#9BBB59", c("top", "bottom", "right")), rows = 1,
         cols = col_of("price"))
addStyle(wb, 1, header_style("#8064A2", "bottom"), rows = 1,
         cols = col_of("annual_insurance"):col_of("annual_hoa"))
setRowHeights(wb, 1, rows = 1, heights = 55.8)

dollar_cols  <- col_of(c("median_income_renters", "price", "mortgage_annual",
                         "total_annual_cost", "annual_insurance", "annual_tax", "annual_elec",
                         "annual_gas", "annual_water", "annual_fuel", "annual_hoa"))
percent_cols <- col_of("renter_affordability_ratio"):col_of("hoa_burden")

addStyle(wb, 1, createStyle(numFmt = "DATE"),  rows = rows, cols = col_of("reference_date"), gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "#,##0"), rows = rows, cols = col_of(c("population", "n_units")), gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "\"$\"#,##0"), rows = rows, cols = dollar_cols, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "0.0%"), rows = rows, cols = percent_cols, gridExpand = TRUE)

# Green (most affordable) -> white (median) -> red (least affordable)
ratio <- public$renter_affordability_ratio
conditionalFormatting(wb, 1, cols = col_of("renter_affordability_ratio"), rows = rows,
                      style = c("#63BE7B", "#FCFCFF", "#F8696B"),
                      rule = c(min(ratio), median(ratio), max(ratio)), type = "colourScale")

widths <- rep(12, length(headers))
widths[col_of(c("CBSA_NAME", "renter_affordability_ratio", "total_annual_cost"))] <- c(48.8, 23.3, 18.6)
setColWidths(wb, 1, cols = seq_along(headers), widths = widths)

# 4. SOURCES & METHODOLOGY TAB ----

ref_month <- format(ref_date, "%B %Y")
acs       <- config$acs_year

methodology <- tribble(
  ~section, ~text,
  "Renter Affordability Index", "",
  "What it measures", paste0(
    "The annual cost of owning a typical-value home in each metro area, as a share of the median ",
    "income of renter households in that metro. It answers: could a renter household earning the ",
    "local median afford to buy the typical local home? Higher values mean less affordable."),
  "Reference period", paste0(
    "Home prices and mortgage payments are Zillow's values for ", ref_month, ". ",
    "Income, population and cost rates are from the ", acs, " American Community Survey (ACS) 1-year estimates."),
  "Coverage", paste0(
    n_rows, " metro areas (2023 core-based statistical areas): those with both a Zillow metro series ",
    "and ACS 1-year estimates."),
  "", "",
  "Sources", "",
  "Home price (Typical-value Home Price)", paste0(
    "Zillow Home Value Index (ZHVI), metro level, all homes (single-family and condo), middle tier ",
    "(33rd to 67th percentile), smoothed and seasonally adjusted. ",
    "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"),
  "Mortgage payment", paste0(
    "Zillow monthly mortgage payment on a typical-value home with a 20% down payment, same tier and ",
    "adjustments as the home price. Multiplied by 12 for the annual figure. ",
    "https://files.zillowstatic.com/research/public_csvs/mortgage_payment/Metro_mortgage_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"),
  "Renter income", paste0(
    "ACS ", acs, " 1-year, table B25119, median household income of renter-occupied households (B25119_003), by metro area."),
  "Population", paste0("ACS ", acs, " 1-year, table B01003, total population (B01003_001), by metro area."),
  "Ownership cost rates", paste0(
    "ACS ", acs, " 1-year Public Use Microdata Sample (PUMS), owner-occupied housing units, ",
    "excluding mobile homes and boats/RVs/vans. Assigned to metro areas with a 2020 PUMA to ",
    "2023 metro area crosswalk."),
  "", "",
  "Method", "",
  "1. Cost rates", paste0(
    "For each owner-occupied unit in PUMS, each annual cost is divided by the owner's reported home ",
    "value (VALP): fire/hazard/flood insurance (INSP), property taxes (TAXAMT), electricity (ELEP), ",
    "gas (GASP), water (WATP), other fuels (FULP) and condominium/homeowners association fees (CONP). ",
    "Monthly costs are multiplied by 12. Costs reported as included elsewhere or not charged count as zero."),
  "2. Metro medians", paste0(
    "Each rate is summarised as a weighted median per metro area, using housing-unit weights. The ",
    "insurance and HOA medians only include homes that pay those costs. California property tax ",
    "rates are set to 1% to reflect Proposition 13."),
  "3. Annual costs", paste0(
    "Each metro's median rates are applied to its typical home value to estimate annual insurance, ",
    "property tax and utility bills. The 'Median ... Bill' columns are these estimates, not medians ",
    "of bills reported in the ACS."),
  "4. Total annual payment", paste0(
    "Mortgage payment + insurance + property tax + electricity + gas + water + other fuels. ",
    "HOA fees are shown separately and are not included in the total."),
  "5. Affordability ratio", paste0(
    "Total annual payment divided by the metro's median renter household income. The component ",
    "shares (mortgage, insurance, tax, utilities, HOA) use the same denominator. 'Utility bill as a ",
    "share' covers electricity, gas and other fuels; water is shown separately."),
  "", "",
  "Notes", "",
  "* Insurance", "Fire, hazard and flood insurance as reported in the ACS (INSP).",
  "** Typical-value home", "The Zillow Home Value Index middle-tier home described above.",
  "Revisions", paste0(
    "Zillow revises its smoothed, seasonally adjusted series. Figures for earlier quarters in ",
    "previously published files are not updated, so compare quarters within the same release."),
  "ACS vintage", paste0(
    "Income, population and cost rates stay fixed at the ", acs, " ACS until the next ACS 1-year ",
    "PUMS is incorporated; quarter-to-quarter changes come from Zillow prices and mortgage payments.")
)

addWorksheet(wb, "Sources & Methodology")
writeData(wb, "Sources & Methodology", methodology, colNames = FALSE)
setColWidths(wb, "Sources & Methodology", cols = 1:2, widths = c(34, 110))
addStyle(wb, "Sources & Methodology", createStyle(valign = "top", wrapText = TRUE),
         rows = seq_len(nrow(methodology)), cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Sources & Methodology", createStyle(textDecoration = "bold", valign = "top", wrapText = TRUE),
         rows = seq_len(nrow(methodology)), cols = 1, gridExpand = TRUE)
heading_rows <- which(methodology$text == "" & methodology$section != "")
addStyle(wb, "Sources & Methodology",
         createStyle(fontName = "Avenir Next LT Pro", fontSize = 12, textDecoration = "bold"),
         rows = heading_rows, cols = 1, gridExpand = TRUE)
addStyle(wb, "Sources & Methodology",
         createStyle(fontName = "Avenir Next LT Pro", fontSize = 14, textDecoration = "bold"),
         rows = 1, cols = 1)

saveWorkbook(wb, config$output, overwrite = TRUE)
cat("Public file saved to:", config$output, "(", n_rows, "CBSAs )\n")
