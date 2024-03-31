## automating data update

## library
library(tidyverse)
library(ipumsr)
library(here)
library(tigris)

load("data/map_data.rda")

most_recent_acs5 <- get_metadata_nhgis("datasets") %>%
  filter(str_detect(name, "ACS5a")) %>%
  filter(group == max(group)) %>%
  pull(name)

## need to error if most_recent_acs5 is longer than 1

if (current_source == most_recent_acs5){
  
  print("Already up to date!")
  
} else {
  
  extract <- define_extract_nhgis(  
    description = "ACS Means of Transportation to Work - Update",  
    datasets = ds_spec(most_recent_acs5,    
                       data_tables = c("B08301"),    
                       geog_levels = "tract"))
  
  temp_path <- here(paste0("extract_data/"))
  dir.create(temp_path)
  
  extract <- submit_extract(extract) %>%
    wait_for_extract()
  
  download_extract(extract, temp_path)
  
  ## need to check if only 1
  list.files(here(temp_path), pattern = "\\.zip$")[1]
  
  nhgis_code <- get_metadata_nhgis(dataset = most_recent_acs5, data_table = "B08301")$nhgis_code %>%
    janitor::make_clean_names()
  
  nhgis_dat <- read_nhgis(paste0(temp_path, "/", list.files(here(temp_path), pattern = "\\.zip$")[1])) %>%
    janitor::clean_names() 
  
  nhgis_dat <- nhgis_dat %>%
    mutate(geoid = paste0(statea, countya, tracta)) %>%
    filter(str_detect(geoid, "^06037") | str_detect(geoid, "^36061") | str_detect(geoid, "^17031")) %>%
    ## check if name is always called the same thing
    select(year, state, county, geoid, name_e, matches("e\\d{3}$")) %>% 
    rename_with(~ str_remove(., nhgis_code), everything()) %>%
    mutate(drive_pct = ((e003*100)/e001),
           bus_pct = ((e011*100)/e001),
           rail_pct = (((e012 + e013 + e014)*100)/e001),
           ferry_pct = ((e015*100)/e001),
           taxicab_pct = ((e016*100)/e001),
           motorcycle_pct = ((e017*100)/e001),
           bicycle_pct = ((e018*100)/e001),
           walk_pct = ((e019*100)/e001),
           wfh_pct = ((e021*100)/e001)) %>%
    select(-matches("e\\d{3}$")) %>%
    mutate_at(vars(ends_with("_pct")), ~round(., digits = 1))
  
  ##
  transport_chi <- tracts(state = "17", county = "031", year = str_sub(most_recent_acs5, 6, 9)) %>%
    janitor::clean_names() %>%
    select(geoid, geometry) %>%
    left_join(nhgis_dat %>% filter(str_detect(geoid, "^17031")),
              by = "geoid") 
  
  transport_nyc <- tracts(state = "36", county = "061", year = str_sub(most_recent_acs5, 6, 9)) %>%
    janitor::clean_names() %>%
    select(geoid, geometry) %>%
    left_join(nhgis_dat %>% filter(str_detect(geoid, "^36061")),
              by = "geoid")
  
  transport_la <- tracts(state = "06", county = "037", year = str_sub(most_recent_acs5, 6, 9)) %>%
    janitor::clean_names() %>%
    select(geoid, geometry) %>%
    left_join(nhgis_dat %>% filter(str_detect(geoid, "^06037")),
              by = "geoid")
  
  current_source <- most_recent_acs5
  
  save(transport_chi, transport_la, transport_nyc, current_source, file = "data/map_data.rda")
  
  ## need to delete extract and unlink path
}