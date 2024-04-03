## automating data update

## library
library(tidyverse)
library(ipumsr)
library(here)
library(tigris)

# load data currently in use and current_source
if (file.exists("data/map_data.rda")) {
  
  load("data/map_data.rda")
  
} else { # haven't run script before
  
  current_source <- "non-existent"
  
}

# find the most recent ACS 5 year data release on IPUMS NHGIS
most_recent_acs5 <- get_metadata_nhgis("datasets") %>%
  filter(str_detect(name, "ACS5a")) %>%
  filter(group == max(group)) %>%
  pull(name)

# if statement to update data source
if (length(most_recent_acs5) != 1) { # error
  
  print("Check API call.")
  
} else if (current_source == most_recent_acs5){ # don't update data
  
  print("Already up to date!")
  
} else { # data needs to be updated
  
  # define a new extract for the most recent data release
  extract <- define_extract_nhgis(  
    description = "ACS Means of Transportation to Work & Median Income - Update",  
    datasets = ds_spec(most_recent_acs5,    
                       data_tables = c("B08301", "B19013"),    
                       geog_levels = "tract"))
  
  # create a temporary folder for the new extract
  temp_path <- here(paste0("extract_data/"))
  dir.create(temp_path)
  
  # submit and download extract
  extract <- submit_extract(extract) %>%
    wait_for_extract()
  
  download_extract(extract, temp_path)
  
  # checks that only one extract in temporary folder
  # if (length(list.files(here(temp_path), pattern = "\\.zip$")) != 1) {
  #   
  #   print("Extract download issue: more than one extract in folder")
  #   break
  #   
  # }
  
  # read extract into R
  nhgis_dat <- read_nhgis(paste0(temp_path, "/", list.files(here(temp_path), pattern = "\\.zip$")[1])) %>%
    janitor::clean_names() 
  
  # get nhgis variable naming convention (unique for table and year)
  # need naming convention to access variables in the dataset
  nhgis_code_tr <- get_metadata_nhgis(dataset = most_recent_acs5, data_table = "B08301")$nhgis_code %>%
    janitor::make_clean_names()
  
  nhgis_code_inc <- get_metadata_nhgis(dataset = most_recent_acs5, data_table = "B19013")$nhgis_code %>%
    janitor::make_clean_names()
  
  nhgis_dat <- nhgis_dat %>%
    mutate(geoid = paste0(statea, countya, tracta)) %>%
    # filter to only LA, NYC, and Chicago
    filter(str_detect(geoid, "^06037") | str_detect(geoid, "^36061") | str_detect(geoid, "^17031")) %>%
    select(year, state, county, geoid, name_e, matches("e\\d{3}$")) %>%
    rename_with(~ str_remove(., nhgis_code_tr), everything()) %>%
    rename(median_income = sym(paste0(nhgis_code_inc, "e001"))) %>%
    # want to represent commuters using each type of transport as a share of total
    # get percentage of people in each census tract using each form of transport
    mutate(drive_pct = ((e003*100)/e001),
           bus_pct = ((e011*100)/e001),
           rail_pct = (((e012 + e013 + e014)*100)/e001),
           ferry_pct = ((e015*100)/e001),
           taxicab_pct = ((e016*100)/e001),
           motorcycle_pct = ((e017*100)/e001),
           bicycle_pct = ((e018*100)/e001),
           walk_pct = ((e019*100)/e001),
           wfh_pct = ((e021*100)/e001)) %>%
    filter(e001 > 0) %>%
    select(-matches("e\\d{3}$")) %>%
    mutate_at(vars(ends_with("_pct")), ~round(., digits = 1))
  
  # create datasets for each city with tract geometries from relevant year
  transport_chi <- tracts(state = "17", county = "031", year = str_sub(most_recent_acs5, 6, 9)) %>%
    janitor::clean_names() %>%
    select(geoid, geometry) %>%
    right_join(nhgis_dat %>% filter(str_detect(geoid, "^17031")),
              by = "geoid") 
  
  transport_nyc <- tracts(state = "36", county = "061", year = str_sub(most_recent_acs5, 6, 9)) %>%
    janitor::clean_names() %>%
    select(geoid, geometry) %>%
    right_join(nhgis_dat %>% filter(str_detect(geoid, "^36061")),
              by = "geoid")
  
  transport_la <- tracts(state = "06", county = "037", year = str_sub(most_recent_acs5, 6, 9)) %>%
    janitor::clean_names() %>%
    select(geoid, geometry) %>%
    right_join(nhgis_dat %>% filter(str_detect(geoid, "^06037")),
              by = "geoid")
  
  # reflect updated data source
  current_source <- most_recent_acs5
  
  # save city-specific datasets to same file for app access
  save(transport_chi, transport_la, transport_nyc, current_source, file = "TransportApp/data/map_data.rda")
  
  # delete extract and remove folder
  unlink(temp_path, recursive = TRUE)
}

