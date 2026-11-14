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
  input  = "hidden-housing-costs/outputs/cbsa_affordability_2026_09.xlsx",
  output = "hidden-housing-costs/outputs/cbsa_affordability_2026_09_public.xlsx"
)

# Output column -> public header, in sheet order. The * and ** markers come
# from the July 2026 public file, which has no footnote text for them.
columns <- c(
  CBSA_NAME                  = "CBSA_NAME",
  reference_date             = "reference_date",
  population                 = "population",
  n_units                    = "n_units",
  median_income_renters      = "Median Household Income: Renters",
  price_ttm                  = "Typical-value Home Price (from Zillow)",
  mortgage_ttm               = "Total annual mortgage payment on purchase of a typical-value home**",
  renter_affordability_ratio = "Total annual payment as a share of renters' median household income",
  mortgage_burden            = "Mortgage payment as a share of renters' median household income",
  insurance_burden           = "Insurance bill as a share of renters' median household income",
  tax_burden                 = "Tax bill as a share of renters' median household income",
  elec_burden                = "Electric bill as a share of renters' median household income",
  gas_burden                 = "Gas bill as a share of renters' median household income",
  fuel_burden                = "Fuel/Oil bill as a share of renters' median household income",
  wat_burden                 = "Water bill as a share of renters' median household income",
  utility_burden             = "Utility bill as a share of renters' median household income",
  hoa_burden                 = "HOA fee as a share of renters' median household income",
  total_annual_cost          = "Total annual payment on purchase of a typical-value home** including utilities",
  annual_insurance           = "Median Hazard/Fire/Flood Insurance Payment* (for Homeowners with Insurance)",
  annual_tax                 = "Median Property Tax Bill",
  annual_elec                = "Median Electricity Bill",
  annual_gas                 = "Median Gas Bill",
  annual_water               = "Median Water Bill",
  annual_fuel                = "Median Fuel/Oil Bill",
  annual_hoa                 = "Median HOA Fee (for Homeowners with an HOA)",
  med_ins_rate               = "Prop Insurance as a % of Home Value",
  med_hoa_rate               = "HOA Bill as a % of Home Value",
  med_tax_rate               = "Property Taxes as a % of Home Value. Note for CA = 1%",
  med_elec_rate              = "Electric bill as a % of Home Value",
  med_gas_rate               = "Gas bill as a % of Home Value",
  med_water_rate             = "Water bill as a % of Home Value",
  med_fuel_rate              = "Fuel/Oil bill as a % of Home Value"
)

# 1. LOAD AND ORDER ----

public <- read_xlsx(config$input) %>%
  mutate(reference_date = as.Date(reference_date)) %>%
  arrange(desc(renter_affordability_ratio)) %>%
  select(all_of(names(columns))) %>%
  mutate(all_utilities_rate = med_elec_rate + med_gas_rate + med_water_rate + med_fuel_rate)

headers <- c(unname(columns), "All Utilities as % of Value")
stopifnot(length(headers) == ncol(public))

n_rows <- nrow(public)
rows   <- 2:(n_rows + 1)
col_of <- function(name) match(name, names(public))

# 2. WRITE ----

wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")
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
         cols = col_of("price_ttm"))
addStyle(wb, 1, header_style("#8064A2", "bottom"), rows = 1,
         cols = col_of("annual_insurance"):col_of("annual_hoa"))
setRowHeights(wb, 1, rows = 1, heights = 55.8)

dollar_cols  <- col_of(c("median_income_renters", "price_ttm", "mortgage_ttm",
                         "total_annual_cost", "annual_insurance", "annual_tax", "annual_elec",
                         "annual_gas", "annual_water", "annual_fuel", "annual_hoa"))
percent_cols <- c(col_of("renter_affordability_ratio"):col_of("hoa_burden"),
                  col_of("med_ins_rate"):col_of("med_fuel_rate"))

addStyle(wb, 1, createStyle(numFmt = "DATE"),  rows = rows, cols = col_of("reference_date"), gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "#,##0"), rows = rows, cols = col_of(c("population", "n_units")), gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "\"$\"#,##0"), rows = rows, cols = dollar_cols, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "0.0%"), rows = rows, cols = percent_cols, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "0.0%", fgFill = "#FDE9D9"), rows = rows,
         cols = col_of("all_utilities_rate"), gridExpand = TRUE)

# Green (most affordable) -> white (median) -> red (least affordable)
ratio <- public$renter_affordability_ratio
conditionalFormatting(wb, 1, cols = col_of("renter_affordability_ratio"), rows = rows,
                      style = c("#63BE7B", "#FCFCFF", "#F8696B"),
                      rule = c(min(ratio), median(ratio), max(ratio)), type = "colourScale")

widths <- c(48.8, 10.8, 11.7, 10.8, 13.2, 12.7, 10.8, 23.3, 10.8, 10.8, 10.8, 13.6, 13.7, 14.3,
            15.9, 10.8, 10.8, 18.6, 11, 11, 11, 11, 11, rep(10.8, 10))
setColWidths(wb, 1, cols = seq_along(headers), widths = widths)

saveWorkbook(wb, config$output, overwrite = TRUE)
cat("Public file saved to:", config$output, "(", n_rows, "CBSAs )\n")
