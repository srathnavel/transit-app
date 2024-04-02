## creating html specifications for map popups

## library
library(tidyverse)
library(glue)
library(htmltools)

# load map data
load("TransportApp/data/map_data.rda")

# create popups
popup_chi <- glue("<strong>{transport_chi$name_e}</strong><br />
                  Median Income: ${transport_chi$median_inc}") %>% 
  lapply(HTML)

popup_la <- glue("<strong>{transport_la$name_e}</strong><br />
                 Median Income: ${transport_la$median_inc}") %>% 
  lapply(HTML)

popup_nyc <- glue("<strong>{transport_nyc$name_e}</strong><br />
                  Median Income: ${transport_nyc$median_inc}") %>% 
  lapply(HTML)

# save
save(popup_chi, popup_la, popup_nyc, file = "TransportApp/data/popup_data.rda")
