## Creating popup data

library(tidyverse)
library(glue)
library(htmltools)

load("data/transport-dataset.rda")

popup_chi <- glue::glue("<strong>{transport_chi$name}</strong><br />
                      Median Income: ${transport_chi$estimate}") %>% 
  lapply(htmltools::HTML)

popup_la <- glue::glue("<strong>{transport_la$name}</strong><br />
                      Median Income: ${transport_la$estimate}") %>% 
  lapply(htmltools::HTML)

popup_nyc <- glue::glue("<strong>{transport_nyc$name}</strong><br />
                      Median Income: ${transport_nyc$estimate}") %>% 
  lapply(htmltools::HTML)

save(popup_chi, popup_la, popup_nyc, file = "popup.rda")