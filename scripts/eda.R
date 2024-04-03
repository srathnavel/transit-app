## exploratory analysis

## library
library(tidyverse)
library(ggplot2)
library(sf)
library(mapview)
library(leaflet)

# load data
load("TransportApp/data/map_data.rda")

#### mapping median income ####

transport_chi %>%
  ggplot() +
  geom_sf(aes(fill = total)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs("Total workers") +
  theme_void()

#### correlations between median income and transport type ####

cor(transport_chi$median_income, transport_chi$wfh_pct, method = "pearson", use = "complete.obs")
cor(transport_nyc$median_income, transport_nyc$wfh_pct, method = "pearson", use = "complete.obs")
cor(transport_la$median_income, transport_la$wfh_pct, method = "pearson", use = "complete.obs")

## REMEMBER TO TALK IN COUNTY TERMS

transport_nyc %>%
  ggplot() +
  geom_point(aes(x = wfh_pct, y = median_income)) +
  geom_smooth(aes(x = drive_pct, y = median_income), formula = (y ~ x))


#### exploring neighbor characteristics ####

# helper function
remove_self <- function(self, vector) {
  mapply(function(idx, n) n[n != idx], idx = self, n = vector, SIMPLIFY = FALSE)
}

# using st_intersects to identify income characteristics of neighboring tracts
chi_neighbors <- transport_chi %>%
  rowid_to_column("index") %>%
  # find neighbors
  mutate(neighbors = st_intersects(geometry, transport_chi)) %>%
  # don't allow a tract to be its own neighbor
  mutate(neighbors = remove_self(index, neighbors)) %>%
  # get median incomes for each neighbor
  mutate(median_incomes = map(neighbors, ~transport_chi$median_income[.x])) %>%
  rowwise() %>%
  # find the average median income across all neighbors
  mutate(mean_neighbor_income = mean(median_incomes, na.rm = TRUE)) %>%
  # express differences between tract median income and mean of neighbor median incomes
  mutate(abs_difference = abs(mean_neighbor_income - median_income)) %>%
  mutate(pct_difference = 100*abs_difference/median_income)

mean(chi_neighbors$pct_difference, na.rm = TRUE)

chi_neighbors %>%
  ggplot() +
  geom_sf(aes(fill = pct_difference)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0, 100)) +
  theme_void()

nyc_neighbors <- transport_nyc %>%
  rowid_to_column("index") %>%
  mutate(neighbors = st_intersects(geometry, transport_nyc)) %>%
  mutate(neighbors = remove_self(index, neighbors)) %>%
  mutate(median_incomes = map(neighbors, ~transport_nyc$median_income[.x])) %>%
  rowwise() %>%
  mutate(mean_neighbor_income = mean(median_incomes, na.rm = TRUE)) %>%
  mutate(abs_difference = abs(mean_neighbor_income - median_income)) %>%
  mutate(pct_difference = 100*abs_difference/median_income)

mean(nyc_neighbors$pct_difference, na.rm = TRUE)

nyc_neighbors %>%
  ggplot() +
  geom_sf(aes(fill = pct_difference)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0, 100)) +
  theme_void()

# la_neighbors <- transport_la %>%
#   rowid_to_column("index") %>%
#   mutate(neighbors = st_intersects(geometry, transport_la)) %>%
#   mutate(neighbors = remove_self(index, neighbors)) %>%
#   mutate(median_incomes = map(neighbors, ~transport_la$median_income[.x])) %>%
#   rowwise() %>%
#   mutate(mean_neighbor_income = mean(median_incomes, na.rm = TRUE)) %>%
#   mutate(abs_difference = abs(mean_neighbor_income - median_income)) %>%
#   mutate(pct_difference = abs_difference/median_income)
# 
# mean(la_neighbors$pct_difference, na.rm = TRUE)
# 
# la_neighbors %>%
#   ggplot() +
#   geom_sf(aes(fill = pct_difference)) +
#   scale_fill_gradient(low = "white", high = "blue", limits = c(0, 10)) +
#   theme_void()

