# Chart the 50 most populous metros: this quarter's affordability ratio and its
# change since the prior quarter.
#
# The prior quarter is recomputed from current Zillow data (same as the QoQ
# summary in rebuild_index_from_prior_quarter.R), so Zillow's revisions to its
# smoothed, seasonally adjusted series don't show up as change. Run from the
# repo root.

suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr); library(ggplot2); library(ragg)
})

config <- list(
  input       = "hidden-housing-costs/outputs/cbsa_affordability_2026_09.csv",
  prior_month = as.Date("2026-06-30"),
  output      = "hidden-housing-costs/outputs/maps/top50_metros_renter_affordability_3q26.png",
  acs_year    = 2024,
  n_metros    = 50,
  font        = "Avenir Next",
  byline      = "hidden-housing-costs/fonts/KaushanScript-Regular.ttf",
  zillow_payments_url = "https://files.zillowstatic.com/research/public_csvs/mortgage_payment/Metro_mortgage_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
  zillow_prices_url   = "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
)

systemfonts::register_font("Kaushan Script", config$byline)

# Same classes and colours as the maps
breaks  <- c(.40, .50, .60, .70, .80, .90)
palette <- c("#4575B4", "#91BFDB", "#E0F3F8", "#FFFFBF", "#FEE090", "#FC8D59", "#D73027")
change_colours <- c(worse = "#D73027", better = "#4575B4")

# 1. DATA ----

current <- read_csv(config$input, show_col_types = FALSE, col_types = cols(CBSA_CODE = "c"))

# Zillow metro names are first city + first state, as in the rebuild script
zillow_short_name <- function(cbsa_name) {
  city  <- str_split_i(cbsa_name, ", ", 1) %>% str_split_i("-", 1)
  state <- str_split_i(cbsa_name, ", ", 2) %>% str_split_i("-", 1)
  paste0(city, ", ", state)
}
name_overrides <- c("Louisville/Jefferson County, KY-IN" = "Louisville, KY",
                    "Wildwood-The Villages, FL"          = "The Villages, FL")

zillow_month <- function(url, value_col) {
  read_csv(url, show_col_types = FALSE) %>%
    filter(RegionType == "msa") %>%
    select(match_name = RegionName, value = all_of(format(config$prior_month))) %>%
    rename(!!value_col := value)
}
prior_zillow <- zillow_month(config$zillow_payments_url, "prior_mortgage") %>%
  inner_join(zillow_month(config$zillow_prices_url, "prior_price"), by = "match_name")

top <- current %>%
  arrange(desc(population)) %>%
  head(config$n_metros) %>%
  mutate(match_name = coalesce(name_overrides[CBSA_NAME], zillow_short_name(CBSA_NAME))) %>%
  inner_join(prior_zillow, by = "match_name") %>%
  mutate(
    prior_ratio = (prior_mortgage * 12 + prior_price * (med_ins_rate + med_tax_rate + med_elec_rate +
                     med_gas_rate + med_water_rate + med_fuel_rate)) / median_income_renters,
    change = 100 * (renter_affordability_ratio - prior_ratio),
    metro  = factor(match_name, levels = match_name[order(renter_affordability_ratio)]),
    class  = cut(renter_affordability_ratio, c(-Inf, breaks, Inf))
  )
stopifnot(nrow(top) == config$n_metros)

cat("Median change, top", config$n_metros, ":", sprintf("%+.2f pts", median(top$change)),
    "| worse:", sum(top$change > 0), "| better:", sum(top$change < 0), "\n")

# 2. CHART ----

bar_panel <- function(data, strip, label_fmt, x_labels, show_metros) {
  room <- 1.14 * max(abs(data$value))
  ggplot(data, aes(value, metro)) +
    geom_vline(xintercept = 0, linewidth = .4, colour = "black") +
    geom_col(aes(fill = fill), width = .72, colour = "black", linewidth = .25) +
    geom_text(aes(label = sprintf(label_fmt, value), hjust = if_else(value < 0, 1.15, -.15)),
              family = config$font, size = 3.1) +
    scale_fill_identity() +
    scale_x_continuous(labels = x_labels, limits = c(min(0, -room * (min(data$value) < 0)), room),
                       expand = expansion(mult = c(0, .01))) +
    labs(title = strip, x = NULL, y = NULL) +
    theme_minimal(base_family = config$font, base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 10.5, lineheight = 1.05, margin = margin(b = 6)),
      plot.title.position = "panel",
      axis.text.y        = if (show_metros) element_text(size = 9.5, colour = "black") else element_blank(),
      axis.text.x        = element_text(size = 8.5, colour = "grey35"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "grey88", linewidth = .3),
      plot.margin        = margin(0, 8, 0, 4)
    )
}

level_plot <- bar_panel(
  top %>% transmute(metro, value = 100 * renter_affordability_ratio, fill = palette[as.integer(class)]),
  "Share of renters' median household income\nneeded for the total payment, Q3 2026",
  "%.1f%%", function(x) paste0(x, "%"), show_metros = TRUE)
change_plot <- bar_panel(
  top %>% transmute(metro, value = change, fill = change_colours[if_else(change > 0, "worse", "better")]),
  "Change since Q2 2026\n(percentage points)",
  "%+.1f", function(x) if_else(x == 0, "0", sprintf("%+g", x)), show_metros = FALSE)

text <- function(label, x, y, size, face = "plain", family = config$font, just = c("left", "top")) {
  grid::grid.text(label, x, y, just = just,
                  gp = grid::gpar(fontfamily = family, fontface = face, fontsize = size, lineheight = 1.1))
}

agg_png(config$output, width = 10, height = 13, units = "in", res = 300)
grid::grid.newpage()
grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
text("How affordable is a typical home for renters in the 50 largest metros?", .02, .985, 17, "bold")
text("By: Ian Kennedy", .98, .985, 15, family = "Kaushan Script", just = c("right", "top"))
text(paste0("Total annual payment on a 'typical-value' home* as a share of renters' median household income,\n",
            "and the change since last quarter. Sorted from least to most affordable."), .02, .958, 11)
print(level_plot,  vp = grid::viewport(.01, .065, .66, .855, just = c("left", "bottom")))
print(change_plot, vp = grid::viewport(.68, .065, .30, .855, just = c("left", "bottom")))
text(paste0("*20% down payment on the typical value for a home in the 33rd to 67th percentile range. ",
            "Payment includes mortgage at current rates,\nhomeowner's insurance, property taxes, utilities, ",
            "and water. Q2 recomputed on current Zillow data.\n",
            "Source: Zillow; US Census Bureau, ", config$acs_year, " ACS PUMS via tidycensus."), .02, .052, 8.5)
invisible(dev.off())
cat("Saved", config$output, "\n")
