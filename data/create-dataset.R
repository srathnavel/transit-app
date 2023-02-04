## Loading and wrangling data

library(tidyverse)
library(ggplot2)
library(tigris)
library(tidycensus)
library(mapview)

### api key
census_api_key(key = "5c646ee6cabd55f9a8901d90127a44384575414b", install = TRUE)

## chicago data
chicago <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "IL",
  county = "Cook",
  geometry = TRUE
) %>%
  janitor::clean_names() %>%
  mutate(tract_id = str_sub(geoid, start= -6))

## vehicle data filtered chicago

vehicles_chi <- read_csv("data/nhgis0002_ds244_20195_tract.csv") %>%
  janitor::clean_names() %>%
  filter(county == "Cook County") %>%
  filter(state == "Illinois") %>%
  mutate(tract_id = str_sub(gisjoin, start= -6)) %>%
  subset(select = -c(regiona,
                     divisiona,
                     cousuba,
                     placea,
                     blkgrpa,
                     concita,
                     aianhha,
                     res_onlya,
                     trusta,
                     aihhtli,
                     aits,
                     anrca,
                     cbsaa,
                     csaa,
                     metdiva,
                     memi,
                     nectaa,
                     cnectaa,
                     nectadiva,
                     uaa,
                     cdcurra,
                     sldua,
                     sldla,
                     zcta5a,
                     submcda,
                     sdelma,
                     sdseca,
                     sdunia,
                     ur,
                     pci,
                     puma5a,
                     bttra,
                     btbga))

## joining chicago data 
transport_chi <- left_join(chicago, vehicles_chi, by = "tract_id") %>%
  mutate(total = alu1e001,
         drive = alu1e003,
         carpool = alu1e004,
         public_transport = alu1e010,
         bus = alu1e011,
         rail = alu1e012 + alu1e013 + alu1e014,
         ferry = alu1e015,
         taxicab = alu1e016,
         motorcycle = alu1e017,
         bicycle = alu1e018,
         walk = alu1e019,
         other = alu1e020,
         wfh = alu1e021
  ) %>%
  subset(select = -c(alu1e005, alu1e006, alu1e007, alu1e008, alu1e009)) %>%
  mutate(drive_pct = ((drive*100)/total),
         bus_pct = ((bus*100)/total),
         rail_pct = ((rail*100)/total),
         ferry_pct = ((ferry*100)/total),
         taxicab_pct = ((taxicab*100)/total),
         motorcycle_pct = ((motorcycle*100)/total),
         bicycle_pct = ((bicycle*100)/total),
         walk_pct = ((walk*100)/total),
         wfh_pct = ((wfh*100)/total))

## nyc data

ny <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  geometry = TRUE
) %>%
  janitor::clean_names() %>%
  mutate(tract_id = str_sub(geoid, start= -6))

## vehicle data filtered ny

vehicles_nyc <- read_csv("data/nhgis0002_ds244_20195_tract.csv") %>%
  janitor::clean_names() %>%
  filter(county == "New York County") %>%
  filter(state == "New York") %>%
  mutate(tract_id = str_sub(gisjoin, start= -6)) %>%
  subset(select = -c(regiona,
                     divisiona,
                     cousuba,
                     placea,
                     blkgrpa,
                     concita,
                     aianhha,
                     res_onlya,
                     trusta,
                     aihhtli,
                     aits,
                     anrca,
                     cbsaa,
                     csaa,
                     metdiva,
                     memi,
                     nectaa,
                     cnectaa,
                     nectadiva,
                     uaa,
                     cdcurra,
                     sldua,
                     sldla,
                     zcta5a,
                     submcda,
                     sdelma,
                     sdseca,
                     sdunia,
                     ur,
                     pci,
                     puma5a,
                     bttra,
                     btbga))

## joining nyc data

transport_nyc <- left_join(ny, vehicles_nyc, by = "tract_id") %>%
  mutate(total = alu1e001,
         drive = alu1e003,
         carpool = alu1e004,
         public_transport = alu1e010,
         bus = alu1e011,
         rail = alu1e012 + alu1e013 + alu1e014,
         ferry = alu1e015,
         taxicab = alu1e016,
         motorcycle = alu1e017,
         bicycle = alu1e018,
         walk = alu1e019,
         other = alu1e020,
         wfh = alu1e021
  ) %>%
  subset(select = -c(alu1e005, alu1e006, alu1e007, alu1e008, alu1e009)) %>%
  mutate(drive_pct = ((drive*100)/total),
         bus_pct = ((bus*100)/total),
         rail_pct = ((rail*100)/total),
         ferry_pct = ((ferry*100)/total),
         taxicab_pct = ((taxicab*100)/total),
         motorcycle_pct = ((motorcycle*100)/total),
         bicycle_pct = ((bicycle*100)/total),
         walk_pct = ((walk*100)/total),
         wfh_pct = ((wfh*100)/total))

## la data

la <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "CA",
  county = "Los Angeles",
  geometry = TRUE
) %>%
  janitor::clean_names() %>%
  mutate(tract_id = str_sub(geoid, start= -6))

## vehicle data filtered la

vehicles_la <- read_csv("data/nhgis0002_ds244_20195_tract.csv") %>%
  janitor::clean_names() %>%
  filter(county == "Los Angeles County") %>%
  filter(state == "California") %>%
  mutate(tract_id = str_sub(gisjoin, start= -6)) %>%
  subset(select = -c(regiona,
                     divisiona,
                     cousuba,
                     placea,
                     blkgrpa,
                     concita,
                     aianhha,
                     res_onlya,
                     trusta,
                     aihhtli,
                     aits,
                     anrca,
                     cbsaa,
                     csaa,
                     metdiva,
                     memi,
                     nectaa,
                     cnectaa,
                     nectadiva,
                     uaa,
                     cdcurra,
                     sldua,
                     sldla,
                     zcta5a,
                     submcda,
                     sdelma,
                     sdseca,
                     sdunia,
                     ur,
                     pci,
                     puma5a,
                     bttra,
                     btbga))

## joining la data

transport_la <- left_join(la, vehicles_la, by = "tract_id") %>%
  mutate(total = alu1e001,
         drive = alu1e003,
         carpool = alu1e004,
         public_transport = alu1e010,
         bus = alu1e011,
         rail = alu1e012 + alu1e013 + alu1e014,
         ferry = alu1e015,
         taxicab = alu1e016,
         motorcycle = alu1e017,
         bicycle = alu1e018,
         walk = alu1e019,
         other = alu1e020,
         wfh = alu1e021
  ) %>%
  subset(select = -c(alu1e005, alu1e006, alu1e007, alu1e008, alu1e009)) %>%
  mutate(drive_pct = ((drive*100)/total),
         bus_pct = ((bus*100)/total),
         rail_pct = ((rail*100)/total),
         ferry_pct = ((ferry*100)/total),
         taxicab_pct = ((taxicab*100)/total),
         motorcycle_pct = ((motorcycle*100)/total),
         bicycle_pct = ((bicycle*100)/total),
         walk_pct = ((walk*100)/total),
         wfh_pct = ((wfh*100)/total))

save(transport_chi, transport_la, transport_nyc, file = "transport-dataset.rda")
