library(readr)
library(tidygeocoder)
library(dplyr)
library(sf)
library(googlesheets4)

impact <- read_sheet("https://docs.google.com/spreadsheets/d/1TInROva7mCNM7qvTA-6GFrkA4zIHcYNpy2eGhsgyg-8/",
                     sheet = "Sheet1")

df <- impact %>%
  mutate(full_address = paste0(street_address, city, state, zip_code, sep = ", ")) 

distinct_address <- df %>%
  distinct(full_address, .keep_all = TRUE)

geodata <- geo(address = distinct_address$full_address, method = "arcgis", full_results = TRUE)

merged_data <- df %>%
  left_join(geodata, by = c("full_address" = "address"))

sf_data <- st_as_sf(merged_data, coords = c("location.x", "location.y"), crs = 4269)

city_data <- sf_data

st_write(city_data, "data/location.gpkg", layer = "geocoded_addresses", delete_layer = TRUE)
