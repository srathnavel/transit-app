## Miscellaneous visualizations 

library(tidyverse)
library(ggplot2)
library(tigris)
library(tidycensus)
library(mapview)
library(leaflet)
library(leafpop)

load("data/transport-dataset.rda")

load("popup.rda")

mapview(transport_chi, 
        zcol = "estimate", 
        legend = TRUE,
        layer.name = "Median Income", 
        popup = popup_chi)

mapview(transport_la, 
        zcol = "estimate", 
        legend = TRUE,
        layer.name = "Median Income", 
        popup = popup_la)

mapview(transport_nyc, 
        zcol = "estimate", 
        legend = TRUE,
        layer.name = "Median Income", 
        popup = popup_nyc)

# pop = popupTable(transport_chi,
#                  zcol = c("name",
#                           "estimate",
#                           "tract_id"),
#                  row.numbers = TRUE)

# mapview(transport_chi, 
#         zcol = "rail_pct", 
#         legend = TRUE,
#         layer.name = "% of commuters",
#         map.types = "CartoDB.Positron",
#         popup = popupTable(transport_chi,
#                            zcol = c("name",
#                                     "estimate",
#                                     "tract_id")))

# popup_chi <- transport_chi %>%
#   subset(select = c(name,
#                     tract_id,
#                     estimate))
# 
# mapview(transport_chi, 
#         zcol = "rail_pct", 
#         legend = TRUE,
#         layer.name = "% of commuters",
#         map.types = "CartoDB.Positron",
#         popup = popupTable(popup_chi))
# 
# mapview(transport_la, 
#         zcol = "estimate", 
#         legend = TRUE,
#         layer.name = "Median Income",
#         popup = popupTable(popup_la, 
#                            className = "mapview-popup"))

mapview(transport_nyc, 
        zcol = "estimate", 
        legend = TRUE,
        layer.name = "Median Income",
        map.types = "OpenStreetMap")