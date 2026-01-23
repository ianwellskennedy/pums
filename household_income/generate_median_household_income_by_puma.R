# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "tidycensus", "openxlsx", "readxl", "sf", "arcgisbinding", "janitor", "conflicted")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

conflicts_prefer(dplyr::filter)

# Setting file paths and the Census API Key ----

census_api_key <- "f8d6fbb724ef6f8e8004220898ac5ed24324b814" # Enter your Census API Key, obtain one here if need be: https://api.census.gov/data/key_signup.html

puma_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/PUMAs/cb_2020_us_puma20_500k.shp"

output_file_path_for_cleaned_data <- "household_income/outputs/household_income_by_puma.xlsx" # Change this to a file path where you would like to output a cleaned Excel file.

output_file_path_for_puma_shp <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/pums/household_income/shapefiles/median_household_income_by_puma.shp" # Change this to a file path for where you would like to output a cleaned shape file
output_file_path_for_puma_geojson<- "C:/Users/ianwe/Downloads/ArcGIS projects for github/pums/household_income/shapefiles/median_household_income_by_puma.geojson" 
# Reading in the empty shape files ----

puma_shp <- st_read(puma_shp_file_path) %>%
  rename(STATE = STATEFP20, PUMA = PUMACE20, STATE_NAME = ST_NAME20, PUMA_NAME = NAMELSAD20)

puma_geo <- puma_shp %>%
  select(STATE, PUMA, geometry)

puma_info <- puma_shp %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME) %>%
  st_drop_geometry()

# Specifying parameters/variables for PUMS data ----

PUMS_data_year <- 2024 # Set the year to pull for PUMS data
PUMS_survey_type <- 'acs1' # or 'acs5' for 5-year estimates
state_selection <- 'MA' # or a vector of state FIPS codes --> c('CA', 'CO'), or 'all'
puma_selection <- 'all' # Setting this to 'all overrides the argument for 'state' (i.e. all PUMAs' data will be read in regardless of the 'state_selection')
which_replicate_weights_to_load <- 'none' # or one of the following: 'housing', 'person', 'both'

pums_variables_of_interest <- c('SERIALNO', 'RT', 'PWGTP', 'AGEP', 'HINCP', 'TEN', 'BLD')

# Reading in PUMS data ----

data <- get_pums(
  key = census_api_key,
  variables = pums_variables_of_interest,
  year = PUMS_data_year,
  survey = PUMS_survey_type,
  state = state_selection,
  puma = puma_selection,
  rep_weights = which_replicate_weights_to_load,
  # Filter for housing unit occupants
  variables_filter = list(TYPEHUGQ = 1), 
  recode = T, 
  show_call = T
) 

# Your code to clean/analyze PUMS data ----

# Join the puma_info to the raw data (by PUMA AND State!)
data_cleaned <- data %>%
  left_join(puma_info, by = c('STATE', 'PUMA')) %>%
  mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything()) %>%
  distinct(SERIALNO, .keep_all = T)

data_cleaned <- data_cleaned %>%
  mutate(
    tenure = case_when(
      TEN %in% c('1', '2') ~ 'owner',
      TEN %in% c('3', '4') ~ 'renter',
      T ~ NA_character_
    )
    ) %>%
  filter(tenure %in% c('owner', 'renter'))

data_final <- data_cleaned %>%
  group_by(STATE, STATE_NAME, PUMA, PUMA_NAME, tenure) %>%
  summarize(households = sum(WGTP, na.rm = T),
            med_hh_inc = weighted.median(HINCP, w = WGTP, na.rm = T)) %>%
  ungroup()

# Outputting cleaned data ----

write.xlsx(data_final, output_file_path_for_cleaned_data)

# Outputting spatial data ----

data_final_spatial <- data_final %>%
  left_join(puma_geo, by = c('PUMA', 'STATE')) %>%
  st_as_sf()

arc.check_product()

arc.write(path = output_file_path_for_puma_shp, data = data_final_spatial, overwrite = T, validate = T)
