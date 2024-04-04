## exploratory analysis

## library
library(tidyverse)
library(ggplot2)
library(sf)

# load data
load("TransportApp/data/map_data.rda")

#### preliminary mapping totals and income ####

transport_chi %>%
  ggplot() +
  geom_sf(aes(fill = total)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Total workers in B08301", "Cook County") +
  theme_void()

transport_chi %>%
  ggplot() +
  geom_sf(aes(fill = median_income)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Median income in B19013", "Cook County") +
  theme_void()

#### correlations between median income and transport type ####

# get list of transit type variables
transit_types <- colnames(st_drop_geometry(transport_chi) %>% 
                            select(ends_with("pct")))

# for each transit type print correlation between tract median income and % transit usage
income_corr <- function(df) {
  for (type in transit_types) {
    
    corr <- cor(df$median_income, st_drop_geometry(df) %>% pull(sym(type)), 
                method = "pearson", use = "complete.obs")
    
    print(paste0(deparse(substitute(df)), # prints dataframe name for easy id in console
                 ": ", type, ", ", corr))
    
  }
}

income_corr(transport_chi)
income_corr(transport_nyc)

# plot lines of best fit for strong correlations
transport_chi %>%
  ggplot() +
  geom_smooth(aes(x = wfh_pct, y = median_income, color = "wfh"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = rail_pct, y = median_income, color = "rail"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = bus_pct, y = median_income, color = "bus"), method = "lm", se = FALSE) +
  xlab("Transit usage") +
  ylab("Median Income") +
  # census reports median incomes above 250000 as 250001
  ylim(0, 250001) +
  ggtitle("Tracts in Cook County") +
  theme_bw()

transport_nyc %>%
  ggplot() +
  geom_smooth(aes(x = wfh_pct, y = median_income, color = "wfh"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = rail_pct, y = median_income, color = "rail"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = bus_pct, y = median_income, color = "bus"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = walk_pct, y = median_income, color = "walk"), method = "lm", se = FALSE) +
  xlab("Transit usage") +
  ylab("Median Income") +
  ylim(0, 250001) +
  ggtitle("Tracts in New York County") +
  theme_bw()

#### exploring neighbor characteristics ####

# helper function
remove_self <- function(self, vector) {
  mapply(function(idx, n) n[n != idx], idx = self, n = vector, SIMPLIFY = FALSE)
}

# identify income characteristics of neighboring tracts
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
  mutate(pct_difference = 100*(mean_neighbor_income - median_income)/median_income) %>%
  mutate(abs_pct_difference = 100*abs_difference/median_income)

mean(chi_neighbors$abs_pct_difference, na.rm = TRUE)

# mapping difference between tract and neighbor median income
chi_neighbors %>%
  ggplot() +
  geom_sf(aes(fill = pct_difference)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits = c(-100, 100)) +
  theme_void()

chi_neighbors %>%
  ggplot() +
  geom_sf(aes(fill = abs_pct_difference)) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0, 100)) +
  theme_void()

for (type in transit_types) {
  
  # correlation between pct difference from neighbor income and transit type usage
  corr <- cor(chi_neighbors$pct_difference, st_drop_geometry(chi_neighbors) %>% pull(sym(type)), 
              method = "pearson", use = "complete.obs")
  
  print(paste0("Correlation with pct_difference: ", type, ", ", corr))
  
}

# plot variables with strong correlations
chi_neighbors %>%
  ggplot() +
  geom_point(aes(x = bus_pct, y = pct_difference)) +
  geom_smooth(aes(x = bus_pct, y = pct_difference), method = "lm", se = FALSE) +
  xlab("Bus usage") +
  ylab("% difference between tract median income and \nmean neighbor income") +
  ggtitle("Tracts in Cook County") +
  theme_bw()

chi_neighbors %>%
  ggplot() +
  geom_point(aes(x = wfh_pct, y = pct_difference)) +
  geom_smooth(aes(x = wfh_pct, y = pct_difference), method = "lm", se = FALSE) +
  xlab("Working from home rates") +
  ylab("% difference between tract median income and \nmean neighbor income") +
  ggtitle("Tracts in Cook County") +
  theme_bw()
