# set working directory to the folder containing this script (requires script to be run in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)
library(esri2sf)
library(sf)

############
# lat and lon coordinates for wells from CalGEM GIS data
wells_locations <- esri2sf("https://gis.conservation.ca.gov/server/rest/services/WellSTAR/Wells/MapServer/0")

wells_locations <- wells_locations %>%
  st_drop_geometry() %>%
  select(api10 = API, lat = Latitude, lon = Longitude, field_name = FieldName)

############
# process data from SQL Server downloads

# years to loop through
years <- c(2018:2021)

# data frames to hold data
q_inject <- tibble()
m_inject <- tibble()
a_wells <- tibble()

# loop to read in data
for (y in years) {
  
  # quarterly injection data  
  quarterly <- read_csv(paste0("sql_server_csv/",y,"/dbo.",y,"CaliforniaOilAndGasWellQuarterlyInjection.csv"), col_types = cols(.default = "c")) %>%
    clean_names() %>%
    rename(api = api_number) %>%
    mutate(year = y) %>%
    unique()
  
  # monthly injection data
  monthly <- read_csv(paste0("sql_server_csv/",y,"/dbo.",y,"CaliforniaOilAndGasWellMonthlyInjection.csv"), col_types = cols(.default = "c")) %>%
    clean_names() %>%
    rename(api = api_number) %>%
    mutate(year = y) %>%
    unique()
  
  # annual wells data
  wells <- read_csv(paste0("sql_server_csv/",y,"/dbo.",y,"CaliforniaOilAndGasWells.csv"), col_types = cols(.default = "c")) %>%
    clean_names() %>%
    mutate(year = y) %>%
    unique()
  
  # append data
  q_inject <- bind_rows(q_inject,quarterly)
  m_inject <- bind_rows(m_inject,monthly)
  a_wells <- bind_rows(a_wells,wells)
}


# clean up environment
rm(monthly,quarterly,wells,years,y)

# edit data types, monthly reports
m_inject <- m_inject %>%
  mutate(injection_date = as.Date(injection_date),
         steam_water_injected = as.double(steam_water_injected),
         gas_air_injected = as.double(gas_air_injected),
         days_injecting = as.double(days_injecting),
         surface_injection_pressure = as.double(surface_injection_pressure),
         casing_injection_pressure = as.double(casing_injection_pressure)) %>%
  rename(steam_water_injected_bbl = steam_water_injected)

# add human-readable labels for water source and kind codes
m_inject <- m_inject %>%
  mutate(surface_injection_pressure = as.double(surface_injection_pressure),
         water_source_text = case_when(water_source == 0 ~ "Not Applicable",
                                       water_source == 1 ~ "Oil or Gas Well",
                                       water_source == 2 ~ "Water Source Well",
                                       water_source == 3 ~ "Domestic Water System",
                                       water_source == 4 ~ "Surface Water",
                                       water_source == 5 ~ "Industrial Waste",
                                       water_source == 6 ~ "Domestic Waste",
                                       water_source == 7 ~ "Other",
                                       TRUE ~ "Error/Missing"),
         water_kind_text = case_when(water_kind == 0 ~ "Not Applicable",
                                     water_kind == 1 ~ "Saline",
                                     water_kind == 2 ~ "Fresh",
                                     water_kind == 3 ~ "Chemical Mixture",
                                     water_kind == 4 ~ "Other",
                                     TRUE ~ "Error/Missing"))

# edit data types, quarterly reports
q_inject <- q_inject %>%
  mutate(injection_report_date = as.Date(injection_report_date),
         steam_water_injected = as.double(steam_water_injected)) %>%
  rename(steam_water_injected_bbl = steam_water_injected)

q_inject <- q_inject %>%
  mutate(water_source_text = case_when(water_source_type == "01" ~ "Oil or Gas Well - In Field",
                                       water_source_type == "02" ~ "Water Source Well",
                                       water_source_type == "03" ~ "Domestic Water System",
                                       water_source_type == "04" ~ "Surface Water",
                                       water_source_type == "05" ~ "Industrial Waste",
                                       water_source_type == "06" ~ "Domestic Waste",
                                       water_source_type == "07" ~ "Other",
                                       water_source_type == "08" ~ "Oil or Gas Well - Other Field/Operator",
                                       water_source_type == "09" ~ "Well Stimulation Treatment",
                                       water_source_type == "10" ~ "Other Class II Recycled",
                                       water_source_type == "11" ~ "Class II Recycled Drilling",
                                       TRUE ~ "Error/Missing"))

############
# join injection data to wells location data
a_wells_join <- a_wells %>%
  select(api, operator_code = operatorcode, operator_name, county, year) %>%
  mutate(api10 = substr(api,1,10)) %>%
  unique() %>%
  left_join(wells_locations, by = "api10")

m_inject <- left_join(m_inject, a_wells_join, by = c("api", "year"))

q_inject <- left_join(q_inject, a_wells_join, by = c("api", "year"))

# clean up environment
rm(a_wells, a_wells_join, wells_locations)

# save Rdata
save.image("calgem2.RData")

