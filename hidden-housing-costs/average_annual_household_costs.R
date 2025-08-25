# Packages ----

library(tidyverse)
library(readxl)
library(tidycensus)
library(sf)
library(openxlsx)
library(arcgisbinding)

# Setting file paths ----

PUMA_shapefile_filepath <- "C:/Users/ianwe/Downloads/shapefiles/2023/PUMAs/cb_2020_us_puma20_500k.shp"
output_filepath_for_PUMA_shapefile <- "hidden-housing-costs/outputs/hidden_housing_costs.shp"

output_file_path_for_cleaned_data <- "hidden-housing-costs/outputs/hidden_housing_costs.xlsx"

# Reading in the empty shape files (ignore if not outputting a shape file) ----

pumas_shapefile <- st_read(PUMA_shapefile_filepath) %>%
  rename(STATE = STATEFP20, PUMA = PUMACE20, STATE_NAME = ST_NAME20, PUMA_NAME = NAMELSAD20)

pumas_geometry <- pumas_shapefile %>%
  select(STATE, PUMA, geometry)

pumas_information <- pumas_shapefile %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME) %>%
  st_drop_geometry()

# Reading in PUMS data ----

# Set the variables to pull from PUMS data; add to this vector or create your own!
pums_variables_of_interest <- c('SERIALNO','RT', 'WGTP', 'ADJHSG', 'TYPEHUGQ', 'BLD', 'TEN', 'HFL', 'VALP',
                                # Costs 
                                'CONP', 'ELEP', 'FULP', 'GASP', 'WATP', 'INSP', 'TAXAMT', 
                                'MHP', 'MRGP', 'MRGT', 'MRGI', 'SMP')

# Retrieve the data
data <- get_pums(
  variables = pums_variables_of_interest,
  year = 2023, 
  survey = 'acs1', 
  state = 'RI',
  variables_filter = list(TEN = 1:2),
  puma = 'all', 
  rep_weights = 'none',
  recode = TRUE,
  show_call = TRUE,
  key = "6dd2c4143fc5f308c1120021fb663c15409f3757"
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
    FULP_recode = if_else(FULP == 2, 0, FULP),
    # MHP == -1 (GQ/vacant/not owned or being bought/not mobile home)
    MHP_recode = if_else(MHP == -1, NA, MHP),
    
    MRG_recode = MRGP
  ) %>%
  distinct(SERIALNO, .keep_all = T)

data_cleaned <- data_cleaned %>%
  mutate(ins_val = INSP / VALP,
         tax_val = TAXAMT / VALP,
         elec_val = ELEP_recode / VALP,
         wat_val = WATP_recode / VALP,
         gas_val = GASP_recode / VALP,
         fuel_val = FULP_recode / VALP,
         con_val = CONP_recode / VALP)

data_final_sf <- data_cleaned %>%
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

data_final_sf <- data_final_sf %>%
    mutate(avg_total = rowSums(select(., avg_ins, avg_tax, avg_elec, avg_wat, avg_gas, avg_fuel), na.rm = TRUE),
           tot_val = avg_total / avg_val)

data_final_sf <- data_final_sf %>%
  left_join(pumas_information, by = c('PUMA', 'STATE')) %>%
  mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
  select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything()) 

rm(data_cleaned, pums_variables_of_interest)

write.xlsx(data_final_sf, output_file_path_for_cleaned_data)

# Outputting spatial data (ignore if not outputting a shape file) ----

data_final_sf <- data_final_sf %>%
  left_join(pumas_geometry, by = c('STATE', 'PUMA')) 

PUMA_data_for_shapefile <- st_as_sf(data_final_sf)

arc.check_product()

arc.write(path = output_filepath_for_PUMA_shapefile, data = PUMA_data_for_shapefile, overwrite = T, validate = T)
