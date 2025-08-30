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

output_file_path_for_cleaned_data <- "true-homeownership-by-age-group/outputs/true_homeownership_by_age_group.xlsx" # Change this to a file path where you would like to output a cleaned Excel file.

output_file_path_for_puma_shp <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/pums/true-homeownership-by-age-group/shapefiles/true_homeownership_by_age_group.shp" # Change this to a file path for where you would like to output a cleaned shape file

# Reading in the empty shape files (ignore if not outputting a shape file) ----

puma_shp <- st_read(puma_shp_file_path) %>%
  rename(STATE = STATEFP20, PUMA = PUMACE20, STATE_NAME = ST_NAME20, PUMA_NAME = NAMELSAD20)

puma_geo <- puma_shp %>%
  select(STATE, PUMA, geometry)

puma_info <- puma_shp %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME) %>%
  st_drop_geometry()

# Specifying parameters/variables for PUMS data ----

PUMS_data_year <- 2023 # Set the year to pull for PUMS data
PUMS_survey_type <- 'acs1' # or 'acs5' for 5-year estimates
state_selection <- 'MA' # or a vector of state FIPS codes --> c('CA', 'CO'), or 'all'
puma_selection <- 'all' # Setting this to 'all overrides the argument for 'state' (i.e. all PUMAs' data will be read in regardless of the 'state_selection')
which_replicate_weights_to_load <- 'none' # or one of the following: 'housing', 'person', 'both'

# Set the variables to pull from PUMS data; add to this vector or create your own!
pums_variables_of_interest <- c('SERIALNO', 'RT', 'PWGTP', 'AGEP', 'HINCP', 'PINCP', 'TEN', 'BLD', 'HHT', 'HHLDRAGEP', 'SMOCP', 'OCPIP','GRNTP', 'GRPIP', 'SCH', 'SCHG', 'HUPAC', 'RELSHIPP')


# Reading in a variable list for the PUMS_data_year in question ----

PUMS_data_year_for_variable_pull <- 2023

pums_variables <- tidycensus::pums_variables

pums_variables <- pums_variables %>%
  filter(survey == PUMS_survey_type & year == PUMS_data_year_for_variable_pull) %>%
  distinct(var_code, .keep_all = TRUE) %>%
  select(var_code:val_max, recode:val_na)

# write.xlsx(pums_variables, paste0("R:/ADHOC-JBREC/Ian-K/API Template Scripts/PUMS Data/pums_variables_", PUMS_data_year_for_variable_pull, ".xlsx"))

# Reading in PUMS data ----

data <- get_pums(
  key = census_api_key,
  variables = pums_variables_of_interest,
  year = PUMS_data_year,
  survey = PUMS_survey_type,
  state = state_selection,
  puma = puma_selection,
  rep_weights = which_replicate_weights_to_load,
  variables_filter = list(TYPEHUGQ = 1, SCH = 1, AGEP = 18:200), # Filter for housing unit occupants
  recode = T, 
  show_call = T
) 

# Your code to clean/analyze PUMS data ----

# Join the puma_info to the raw data (by PUMA AND State!)
data_cleaned <- data %>%
  left_join(puma_info, by = c('STATE', 'PUMA')) %>%
  mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything())

data_cleaned <- data_cleaned %>%
  group_by(STATE, STATE_NAME, PUMA, PUMA_NAME, SERIALNO) %>%
  summarize(HHLDRAGEP = mean(HHLDRAGEP, na.rm = T),
            AGEP,
            TEN_label,
            RELSHIPP_label,
            PWGTP) %>%
  ungroup()

data_cleaned <- data_cleaned %>%
  mutate(
    owner_renter_status = case_when(
        RELSHIPP_label %in% c('Biological son or daughter', 'Adopted son or daughter', 
                              'Stepson or stepdaughter', 'Grandchild', 
                              'Son-in-law or daughter-in-law', 'Foster child') ~ 'lives_with_parents',
      TRUE ~ 'does_not_live_with_parents'
    ),
    age_group = case_when(
      AGEP >= 18 & AGEP <= 24 ~ '18_24',
      AGEP >= 25 & AGEP <= 29 ~ '25_29',
      AGEP >= 30 & AGEP <= 39 ~ '30s',
      AGEP >= 40 & AGEP <= 49 ~ '40s',
      AGEP >= 50 & AGEP <= 59 ~ '50s',
      AGEP >= 60 & AGEP <= 69 ~ '60s',
      AGEP >= 70 ~ '70_plus',
      TRUE ~ NA_character_
    )
  )

data_final_puma <- data_cleaned %>%
  group_by(STATE, STATE_NAME, PUMA, PUMA_NAME, owner_renter_status, age_group) %>%
  summarize(pop = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  group_by(STATE, STATE_NAME, PUMA, PUMA_NAME, age_group) %>%
  mutate(pop_sh = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(STATE:PUMA_NAME, age_group), values_from = pop_sh, names_from = owner_renter_status) %>%
  mutate(lives_with_parents = if_else(is.na(lives_with_parents), 0, lives_with_parents),
         does_not_live_with_parents = if_else(is.na(does_not_live_with_parents), 0, does_not_live_with_parents)) %>%
  select(PUMA_NAME, PUMA, STATE_NAME, STATE, everything())

data_final_state <- data_cleaned %>%
  group_by(STATE, STATE_NAME, owner_renter_status, age_group) %>%
  summarize(pop = sum(PWGTP, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(STATE, STATE_NAME, age_group) %>%
  mutate(pop_sh = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(STATE:STATE_NAME, age_group), values_from = pop_sh, names_from = owner_renter_status) %>%
  mutate(lives_with_parents = if_else(is.na(lives_with_parents), 0, lives_with_parents),
         does_not_live_with_parents = if_else(is.na(does_not_live_with_parents), 0, does_not_live_with_parents)) %>%
  select(STATE_NAME, STATE, everything())

data_final_national <- data_cleaned %>%
  group_by(owner_renter_status, age_group) %>%
  summarize(pop = sum(PWGTP, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(age_group) %>%
  mutate(pop_sh = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = age_group, values_from = pop_sh, names_from = owner_renter_status) %>%
  mutate(lives_with_parents = if_else(is.na(lives_with_parents), 0, lives_with_parents),
         does_not_live_with_parents = if_else(is.na(does_not_live_with_parents), 0, does_not_live_with_parents))


# Outputting cleaned data ----

dataset_list <- list('PUMA Data' = data_final_puma,
                     'State Data' = data_final_state,
                     'National Data' = data_final_national)

write.xlsx(dataset_list, output_file_path_for_cleaned_data)

# Outputting spatial data (IGNORE IF NOT OUTPUTTING A SHAPE FILE) ----

data_final_spatial <- data_final_puma %>%
  left_join(puma_geo, by = c('PUMA', 'STATE')) %>%
  st_as_sf()

arc.check_product()

arc.write(path = output_file_path_for_puma_shp, data = data_final_spatial, overwrite = T, validate = T)
