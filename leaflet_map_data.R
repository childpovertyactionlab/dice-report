library(readr)
library(tidygeocoder)
library(dplyr)
library(sf)
data <- read.csv("data/map_point_data.csv")

df <- data %>%
  mutate(full_address = paste(street_address, city, state, zip_code, sep = ", ")) 

distinct_address <- df %>%
  distinct(full_address, .keep_all = TRUE)

geodata <- geo(address = distinct_address$full_address, method = "arcgis", full_results = TRUE)

merged_data <- df %>%
  left_join(geodata, by = c("full_address" = "address"))

sf_data <- st_as_sf(merged_data, coords = c("long", "lat"), crs = 4269)

sw_data <- sf_data %>%
  filter(district == "South West") %>%
  filter(event_type == "clean")

st_write(sw_data, "data/sw_clean_and_green.gpkg", layer = "geocoded_addresses", delete_layer = TRUE)
