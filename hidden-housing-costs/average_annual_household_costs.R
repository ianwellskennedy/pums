# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "readxl", "tidycensus", "sf", "openxlsx", "arcgisbinding")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# Setting file paths and the Census API key----

puma_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/PUMAs/cb_2020_us_puma20_500k.shp"
output_file_path_for_puma_shp <- "hidden-housing-costs/outputs/hidden_housing_costs.shp"

output_file_path_for_cleaned_data <- "hidden-housing-costs/outputs/hidden_housing_costs.xlsx"

# Enter your own Census API key here. Visit this link if you do not yet have a Census API key: https://api.census.gov/data/key_signup.html
census_api_key <- "f8d6fbb724ef6f8e8004220898ac5ed24324b814"

# Reading in the empty shape files (ignore if not outputting a shape file) ----

puma_shp <- st_read(puma_shp_file_path) %>%
  rename(STATE = STATEFP20, PUMA = PUMACE20, STATE_NAME = ST_NAME20, PUMA_NAME = NAMELSAD20)

puma_geo <- puma_shp %>%
  select(STATE, PUMA, geometry)

puma_info <- puma_shp %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME) %>%
  st_drop_geometry()

# Reading in PUMS data ----

# Set the variables to pull from PUMS data; add to this vector or create your own!
pums_variables_of_interest <- c('SERIALNO', 'PUMA','RT', 'WGTP', 'ADJHSG', 'TYPEHUGQ', 'BLD', 'TEN', 'HFL', 'VALP',
                                # Costs 
                                'CONP', 'ELEP', 'FULP', 'GASP', 'WATP', 'INSP', 'TAXAMT')

# Retrieve the data
data <- get_pums(
  variables = pums_variables_of_interest,
  year = 2023, 
  survey = 'acs1', 
  state = 'RI',
  variables_filter = list(TEN = 1:2),
  puma = 'all', 
  rep_weights = 'none',
  recode = T,
  show_call = T,
  key = census_api_key
)

# Your code to clean/analyze PUMS data ----

data_cleaned <- data %>%
  mutate(
    # ELEP == 2 (No charge or electricity not used)
    ELEP_recode = if_else(ELEP == 2, 0, ELEP*12),
    # WATP == 2 (No charge)
    WATP_recode = if_else(WATP == 2, 0, WATP),
    CONP_recode = CONP,
    # GASP == 3 (No charge or gas not used)
    GASP_recode = if_else(GASP == 3, 0, GASP*12),
    # FULP == 2 (No charge or fuel other than gas or electricity not used)
    FULP_recode = if_else(FULP == 2, 0, FULP)
  ) %>%
  distinct(SERIALNO, .keep_all = T)

data_final <- data_cleaned %>%
  filter(BLD_label %in% c('One-family house detached', 'One-family house attached')) %>%
  group_by(STATE, PUMA) %>%
  summarize(
    sf_hh = sum(WGTP, na.rm = T),
    avg_val = weighted.mean(VALP, w = WGTP, na.rm = T),
    avg_ins = weighted.mean(INSP, w = WGTP, na.rm = T),
    avg_tax = weighted.mean(TAXAMT, w = WGTP, na.rm = T),
    avg_elec = weighted.mean(ELEP_recode, w = WGTP, na.rm = T),
    avg_wat = weighted.mean(WATP_recode, w = WGTP, na.rm = T),
    avg_gas = weighted.mean(GASP_recode, w = WGTP, na.rm = T),
    avg_fuel = weighted.mean(FULP_recode, w = WGTP, na.rm = T)
    ) %>%
  ungroup()

data_final <- data_final %>%
    mutate(avg_total = rowSums(select(., avg_ins, avg_tax, avg_elec, avg_wat, avg_gas, avg_fuel), na.rm = TRUE))

data_final <- data_final %>%
  left_join(puma_info, by = c('PUMA', 'STATE')) %>%
  mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything()) 

write.xlsx(data_final, output_file_path_for_cleaned_data)


rm(data_cleaned, pums_variables_of_interest, puma_info, puma_shp_file_path, output_file_path_for_cleaned_data)

# Outputting spatial data (ignore if not outputting a shape file) ----

data_final <- data_final %>%
  left_join(puma_geo, by = c('STATE', 'PUMA')) 

data_final_spatial <- st_as_sf(data_final)

arc.check_product()

arc.write(path = output_file_path_for_puma_shp, data = data_final_spatial, overwrite = T, validate = T)
