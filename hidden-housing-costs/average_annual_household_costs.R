# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "readxl", "tidycensus", "sf", "openxlsx", "arcgisbinding", "spatstat", "zoo")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# Setting file paths and the Census API key ----

# Set the year to pull ACS data in for
acs_year <- 2024

puma_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/PUMAs/cb_2020_us_puma20_500k.shp" # Set the file path to the PUMA level .shp file. This file can be downloaded here: https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_puma20_500k.zip

cbsa_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/CBSAs/cb_2024_us_cbsa_5m.shp"

puma_cbsa_crossover_file_path <- "C:/Users/ianwe/Downloads/shapefiles/crossover_files/puma_2020_to_cbsa_2023.xlsx"

zillow_metro_data_file_path <- "C:/Users/ianwe/Downloads/Metro_total_monthly_payment_downpayment_0.10_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

zillow_metro_codes_file_path <- "C:/Users/ianwe/Downloads/github/zillow/inputs/zillow_metro_crosswalk.xlsx"

output_file_path_for_puma_shp <- paste0("hidden-housing-costs/outputs/hidden_housing_costs_", acs_year, ".shp") # Set the file path to output the .shp file to.

output_file_path_for_cleaned_data <- paste0("hidden-housing-costs/outputs/hidden_housing_costs_", acs_year, ".xlsx") # Set the file path to output the tabular file to.

output_file_path_for_metro_affordablity_data <- paste0("hidden-housing-costs/outputs/metro_housing_affordability_data_", acs_year, ".xlsx") # Set the file path to output the tabular file to.

# Enter your own Census API key here. Visit this link if you do not yet have a Census API key: https://api.census.gov/data/key_signup.html
census_api_key <- "6dd2c4143fc5f308c1120021fb663c15409f3757"

# Reading in the empty shape files (ignore if not outputting a shape file) ----

puma_shp <- st_read(puma_shp_file_path) %>%
  rename(STATE = STATEFP20, PUMA = PUMACE20, STATE_NAME = ST_NAME20, PUMA_NAME = NAMELSAD20)

puma_geo <- puma_shp %>%
  select(STATE, PUMA, geometry)

puma_info <- puma_shp %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME) %>%
  st_drop_geometry()

cbsa_shp <- st_read(cbsa_shp_file_path) %>%
  rename(CBSA = CBSAFP, CBSA_NAME = NAME)

cbsa_geo <- cbsa_shp %>%
  select(CBSA, CBSA_NAME, geometry)

cbsa_info <- cbsa_geo %>%
  st_drop_geometry()
# Specifying parameters/variables for PUMS data ----

PUMS_survey_type <- 'acs1' # or 'acs5' for 5-year estimates
state_selection <- 'RI' # or a vector of state FIPS codes --> c('CA', 'CO'), or 'all'
puma_selection <- 'all' # Setting this to 'all overrides the argument for 'state' (i.e. all PUMAs' data will be read in regardless of the 'state_selection')
which_replicate_weights_to_load <- 'none' # or one of the following: 'housing', 'person', 'both'
census_api_key <- 'f8d6fbb724ef6f8e8004220898ac5ed24324b814' # Provide the Census API Key, if others are running this you will need to get a Census API key here: https://api.census.gov/data/key_signup.html

# Reading in crossover file ----

puma_to_cbsa_crossover_file <- read.xlsx(puma_cbsa_crossover_file_path)
names(puma_to_cbsa_crossover_file) <- toupper(names(puma_to_cbsa_crossover_file))

puma_to_cbsa_crossover_file <- puma_to_cbsa_crossover_file %>%
  select(-c(HOUSING_UNITS_2020)) %>%
  mutate(
    PUMA = as.character(PUMA),
    STATE = as.character(STATE),
    ALLOC_FACTOR = as.numeric(ALLOC_FACTOR),
    PUMA = case_when(
      str_length(PUMA) == 3 ~ paste0("00", PUMA),
      str_length(PUMA) == 4 ~ paste0("0", PUMA),
      T ~ PUMA
    ),
    STATE = case_when(
      str_length(STATE) == 1 ~ paste0("0", STATE),
      T ~ STATE
    )
    )

# Reading in PUMS data ----

# Set the variables to pull from PUMS data; add to this vector or create your own!
pums_variables_of_interest <- c('SERIALNO', 'PUMA','RT', 'WGTP', 'ADJHSG', 'TYPEHUGQ', 'BLD', 'TEN', 'HFL', 'VALP', 'MRGX', 'MRGP',
                                # Costs 
                                'CONP', 'ELEP', 'FULP', 'GASP', 'WATP', 'INSP', 'TAXAMT')

# Retrieve the data
data <- get_pums(
  variables = pums_variables_of_interest,
  year = acs_year, 
  survey = PUMS_survey_type, 
  state = state_selection,
  variables_filter = list(TEN = 1:2), # Filter for owned households
  puma = puma_selection, 
  rep_weights = which_replicate_weights_to_load,
  recode = T,
  show_call = T,
  key = census_api_key
)

# Read in metro-level household income data ----

# Read in the preferred variable spreadsheet (create your own within this file: R:/ADHOC-JBREC/Ian-K/API Template Scripts/ACS/Summary Tables/acs_variables_2023_acs1.xlsx)
variables <- read.xlsx("C:/Users/ianwe/Downloads/github/acs/acs-variables/acs_variables_2024_acs1.xlsx", 
                       sheet = 'Affordability')

# Select 'name' and 'amended_label' (and rename 'name' to code')
variables <- variables %>%
  select(name, amended_label) %>%
  rename(code = name)

# Create Codes, containing all of the preferred variable codes
variable_codes <- variables$code
# Create Labels, containing all of the amended labels
variable_labels <- variables$amended_label

income_data <- get_acs(
  geography = 'cbsa',
  variables = variable_codes,
  year = 2024,
  geometry = F,
  key = census_api_key,
  survey = 'acs1',
  show_call = T
)

income_data <- income_data %>%
  # Rename 'variable' to 'Code'
  rename(code = variable) %>%
  # Join the variable spreadsheet to the ACS data by 'Code'
  left_join(variables, by = 'code') %>%
  # Rename the listed 'Variable' with the 'AmendedLabel' from the variable spreadsheet
  rename(variable = amended_label) %>%
  # Drop the 'Code' column
  select(-c(code, moe))

# Pivot the ACS data to a wide format, with columns named by variable. Each geography unit will have one row with one column per variable.
income_data <- income_data %>%
  pivot_wider(names_from = 'variable', values_from = 'estimate', id_cols = 'GEOID')

# Read in FRED data to inflation-adjust utility costs ----

utility_series <- c(
  'CUUR0000SEHF01', # Consumer Price Index for All Urban Consumers: Electricity in U.S. City Average
  'CUUR0000SEHF02', # Consumer Price Index for All Urban Consumers: Utility (Piped) Gas Service in U.S. City Average 
  'CUUR0000SEHG',   # Consumer Price Index for All Urban Consumers: Water and Sewer and Trash Collection Services in U.S. City Average
  'CUUR0000SEHE'    # Consumer Price Index for All Urban Consumers: Fuel Oil and Other Fuels in U.S. City Average
)

# Set the FRED API Key, if a new user is using this you will have to obtain an API key from here: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key(key = 'cbebc48b543b6420b4aa3ff9bd7a9878')

get_fred_data <- function(variables) {
  
  # Create an empty list to store the data frames
  dataframes_list <- list()
  
  # For each variable stored in variables
  for (i in utility_series) {
    
    # Fetch the variable's time series using the fred API
    ## All data fetched will be monthly (frequency = 'm')
    utility_data <- fredr(
      series_id = i, 
      sort_order = 'asc', 
      frequency = 'm', 
      units = 'pc1',
      observation_start = as.Date('2024-11-01'), 
      observation_end = as.Date('2024-11-01')
    ) %>%
      select(date, value)
    
    
    # Store the variable's data in dataframes_list, naming the dataframe by 'col_name' (i.e. the title of the variable)
    dataframes_list[[i]] <- utility_data
  }
  
  # Return the list of data frames
  return(dataframes_list)
}

utility_data <- get_fred_data(utility_series)

utility_data <- do.call(cbind, utility_data)

utility_data <- utility_data %>%
  # Rename the first column to 'Date'
  rename(date = colnames(.)[1]) %>%
  # Select Date and columns that end with '.value' 
  select(1, ends_with('.value')) %>%
  # Rename columns by dropping '.value' from all column names that end with '.value'
  rename_with(~gsub(".value", "", .), ends_with('.value'))

utility_data <- utility_data %>%
  rename(elec_yoy = CUUR0000SEHF01,
         gas_yoy = CUUR0000SEHF02,
         wat_yoy = CUUR0000SEHG,
         fuel_yoy = CUUR0000SEHE)

# Read in zillow data ----

zillow_metro_data <- read.csv(zillow_metro_data_file_path)
zillow_metro_codes_crosswalk <- read.xlsx(zillow_metro_codes_file_path)

names(zillow_metro_data) <- str_remove(names(zillow_metro_data), "X")

zillow_metro_data <- zillow_metro_data %>%
  pivot_longer(names_to = 'date', values_to = 'paymnt', cols = `2012.01.31`:ncol(zillow_metro_data))

zillow_metro_data <- zillow_metro_data %>%
  mutate(date = as.Date(date, format = "%Y.%m.%d"),
         RegionID = as.character(RegionID)) %>%
  filter(RegionType == 'msa')

zillow_metro_data <- zillow_metro_data %>%
  rename(zillow_metro_code = RegionID, pop_rank = SizeRank) %>%
  select(zillow_metro_code, pop_rank, date, paymnt) %>%
  arrange(zillow_metro_code, date)

zillow_metro_data <- zillow_metro_data %>%
  group_by(zillow_metro_code) %>%
  mutate(ttm = rollmean(paymnt, k = 12, align = "right", fill = NA)) %>%
  ungroup() 

zillow_metro_data <- zillow_metro_data %>%
  filter(date == max(date)) %>%
  select(-date)

zillow_metro_codes_crosswalk <- read.xlsx(zillow_metro_codes_file_path) %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code))

zillow_metro_data <- zillow_metro_data %>% 
  left_join(zillow_metro_codes_crosswalk, by = c('zillow_metro_code'))

zillow_metro_data <- zillow_metro_data %>% 
  select(ends_with('metro_name'), GEOID, zillow_metro_code, everything())

# Clean PUMS data ----

data_cleaned <- data %>%
  mutate(ELEP = as.numeric(ELEP),
         WATP = as.numeric(WATP),
         CONP = as.numeric(CONP),
         GASP = as.numeric(GASP),
         FULP = as.numeric(FULP),
         VALP = as.numeric(VALP),
         INSP = as.numeric(INSP),
         TAXAMT = as.numeric(TAXAMT),
         MRGP = as.numeric(MRGP)) %>%
  mutate(
    # ELEP == 2 (No charge or electricity not used)
    ELEP_recode = if_else(ELEP == 2, 0, ELEP*12),
    # WATP == 2 (No charge)
    WATP_recode = if_else(WATP == 2, 0, WATP),
    CONP_recode = CONP,
    # GASP == 3 (No charge or gas not used)
    GASP_recode = if_else(GASP == 3, 0, GASP*12),
    # FULP == 2 (No charge or fuel other than gas or electricity not used)
    FULP_recode = if_else(FULP == 2, 0, FULP),
    ins_rate = INSP / VALP,
    prop_tax_rate = TAXAMT / VALP,
    
  ) %>%
  distinct(SERIALNO, .keep_all = T) %>%
  filter(BLD %in% c('2','3'))

data_cleaned <- data_cleaned %>%
  group_by(STATE, PUMA) %>%
  summarize(
    sf_hh = sum(WGTP, na.rm = T),
    avg_val = weighted.mean(VALP, w = WGTP, na.rm = T),
    avg_ins = weighted.mean(INSP, w = WGTP, na.rm = T),
    avg_tax = weighted.mean(TAXAMT, w = WGTP, na.rm = T),
    avg_elec = weighted.mean(ELEP_recode, w = WGTP, na.rm = T),
    avg_wat = weighted.mean(WATP_recode, w = WGTP, na.rm = T),
    avg_gas = weighted.mean(GASP_recode, w = WGTP, na.rm = T),
    avg_fuel = weighted.mean(FULP_recode, w = WGTP, na.rm = T),
    med_ins_rate = weighted.median(ins_rate, na.rm = T)*100,
    avg_ins_rate = weighted.mean(ins_rate, na.rm = T)*100,
    med_tax_rate = weighted.median(prop_tax_rate, na.rm = T)*100,
    avg_tax_rate = weighted.mean(prop_tax_rate, na.rm = T)*100
  ) %>%
  ungroup()

data_cleaned <- data_cleaned %>%
  mutate(avg_total = rowSums(select(., avg_ins, avg_tax, avg_elec, avg_wat, avg_gas, avg_fuel), na.rm = TRUE))

data_final <- data_cleaned %>%
  left_join(puma_info, by = c('PUMA', 'STATE')) %>%
  mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything()) 

# Generate metro level data ----

cbsa_data_final <- data %>%
  mutate(ELEP = as.numeric(ELEP),
         WATP = as.numeric(WATP),
         CONP = as.numeric(CONP),
         GASP = as.numeric(GASP),
         FULP = as.numeric(FULP),
         VALP = as.numeric(VALP),
         INSP = as.numeric(INSP),
         TAXAMT = as.numeric(TAXAMT),
         MRGP = as.numeric(MRGP)) %>%
  mutate(
    # ELEP == 2 (No charge or electricity not used)
    ELEP_recode = if_else(ELEP == 2, 0, ELEP*12),
    # WATP == 2 (No charge)
    WATP_recode = if_else(WATP == 2, 0, WATP),
    CONP_recode = CONP,
    # GASP == 3 (No charge or gas not used)
    GASP_recode = if_else(GASP == 3, 0, GASP*12),
    # FULP == 2 (No charge or fuel other than gas or electricity not used)
    FULP_recode = if_else(FULP == 2, 0, FULP),
    ins_rate = INSP / VALP,
    prop_tax_rate = TAXAMT / VALP
    
  ) %>%
  distinct(SERIALNO, .keep_all = T) %>%
  filter(BLD %in% c('2','3'))

cbsa_data_final <- cbsa_data_final %>%
  group_by(STATE, PUMA) %>%
  summarize(
    sf_hh = sum(WGTP, na.rm = T),
    avg_val = weighted.mean(VALP, w = WGTP, na.rm = T),
    avg_ins = weighted.mean(INSP, w = WGTP, na.rm = T),
    avg_tax = weighted.mean(TAXAMT, w = WGTP, na.rm = T),
    avg_elec = weighted.mean(ELEP_recode, w = WGTP, na.rm = T),
    avg_wat = weighted.mean(WATP_recode, w = WGTP, na.rm = T),
    avg_gas = weighted.mean(GASP_recode, w = WGTP, na.rm = T),
    avg_fuel = weighted.mean(FULP_recode, w = WGTP, na.rm = T),
    med_ins_rate = weighted.median(ins_rate, na.rm = T)*100,
    avg_ins_rate = weighted.mean(ins_rate, na.rm = T)*100,
    med_tax_rate = weighted.median(prop_tax_rate, na.rm = T)*100,
    avg_tax_rate = weighted.mean(prop_tax_rate, na.rm = T)*100
  ) %>%
  ungroup()

cbsa_data_final <- cbsa_data_final %>%
  mutate(avg_total = rowSums(select(., avg_ins, avg_tax, avg_elec, avg_wat, avg_gas, avg_fuel), na.rm = TRUE))

cbsa_data_final <- cbsa_data_final %>%
  left_join(puma_info, by = c('PUMA', 'STATE')) %>%
  mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything()) 

cbsa_data_final <- cbsa_data_final %>%
  left_join(puma_to_cbsa_crossover_file, by = c('PUMA', 'STATE')) %>%
  mutate(sf_hh_cbsa = sf_hh * ALLOC_FACTOR)

cbsa_data_final <- cbsa_data_final %>%
  group_by(CBSA_NAME, CBSA_CODE) %>%
  summarize(
    sf_hh = sum(sf_hh_cbsa, na.rm = T),
    avg_val = weighted.mean(avg_val, w = sf_hh_cbsa, na.rm = T),
    avg_ins = weighted.mean(avg_ins, w = sf_hh_cbsa, na.rm = T),
    avg_tax = weighted.mean(avg_tax, w = sf_hh_cbsa, na.rm = T),
    avg_elec = weighted.mean(avg_elec, w = sf_hh_cbsa, na.rm = T),
    avg_wat = weighted.mean(avg_wat, w = sf_hh_cbsa, na.rm = T),
    avg_gas = weighted.mean(avg_gas, w = sf_hh_cbsa, na.rm = T),
    avg_fuel = weighted.mean(avg_fuel, w = sf_hh_cbsa, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    CBSA_NAME = if_else(CBSA_NAME == '[not in any CBSA]', 'Non-metro areas', CBSA_NAME),
    CBSA_CODE = as.character(CBSA_CODE)   
  ) 

cbsa_data_final <- cbsa_data_final %>%
  select(CBSA_NAME:sf_hh, avg_elec:avg_fuel)

cbsa_data_final <- cbsa_data_final %>%
  mutate(
    avg_elec = avg_elec + avg_elec * (utility_data$elec_yoy[1] / 100),
    avg_wat = avg_wat + avg_wat * (utility_data$wat_yoy[1] / 100),
    avg_gas = avg_gas + avg_gas * (utility_data$gas_yoy[1] / 100),
    avg_fuel = avg_fuel + avg_fuel * (utility_data$fuel_yoy[1] / 100),
                                 )

cbsa_data_final <- cbsa_data_final %>%
  left_join(zillow_metro_data, by = c('CBSA_CODE' = 'GEOID')) %>%
  filter(!is.na(zillow_metro_name)) 

cbsa_data_final <- cbsa_data_final %>%
  select(CBSA_NAME:CBSA_CODE, sf_hh, avg_elec:avg_fuel, ttm)

cbsa_data_final <- cbsa_data_final %>%
  mutate(ttm = ttm*12) %>%
  rename(zillow_payment_10_down = ttm)

cbsa_data_final <- cbsa_data_final %>%
  mutate(zillow_payment_10_down_plus_utilities = zillow_payment_10_down + avg_elec + avg_wat + avg_gas + avg_fuel)

cbsa_data_final <- cbsa_data_final %>%
  left_join(income_data, by = c('CBSA_CODE' = 'GEOID'))

# Prep final data for output ----

data_final <- data_cleaned %>%
  group_by(STATE, PUMA) %>%
  summarize(
    sf_hh = sum(WGTP, na.rm = T),
    avg_val = weighted.mean(VALP, w = WGTP, na.rm = T),
    avg_ins = weighted.mean(INSP, w = WGTP, na.rm = T),
    avg_tax = weighted.mean(TAXAMT, w = WGTP, na.rm = T),
    avg_elec = weighted.mean(ELEP_recode, w = WGTP, na.rm = T),
    avg_wat = weighted.mean(WATP_recode, w = WGTP, na.rm = T),
    avg_gas = weighted.mean(GASP_recode, w = WGTP, na.rm = T),
    avg_fuel = weighted.mean(FULP_recode, w = WGTP, na.rm = T),
    med_ins_rate = weighted.median(ins_rate, na.rm = T)*100,
    avg_ins_rate = weighted.mean(ins_rate, na.rm = T)*100,
    med_tax_rate = weighted.median(prop_tax_rate, na.rm = T)*100,
    avg_tax_rate = weighted.mean(prop_tax_rate, na.rm = T)*100
    ) %>%
  ungroup()

data_final <- data_final %>%
    mutate(avg_total = rowSums(select(., avg_ins, avg_tax, avg_elec, avg_wat, avg_gas, avg_fuel), na.rm = TRUE))

data_final <- data_final %>%
  left_join(puma_info, by = c('PUMA', 'STATE')) %>%
  mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything()) 

# Output tabular data ----

write.xlsx(data_final, output_file_path_for_cleaned_data)

write.xlsx(cbsa_data_final, output_file_path_for_metro_affordablity_data) 

rm(data_cleaned, pums_variables_of_interest, puma_info, puma_shp_file_path, output_file_path_for_cleaned_data)

# Outputting spatial data (ignore if not outputting a shape file) ----

data_final_spatial <- data_final %>%
  left_join(puma_geo, by = c('STATE', 'PUMA')) %>%
  st_as_sf()

arc.check_product()

arc.write(path = output_file_path_for_puma_shp, data = data_final_spatial, overwrite = T, validate = T)
