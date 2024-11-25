library(tidyverse)
library(cpaltemplates)
library(rio)
library(sf)
library(googlesheets4)
library(tidygeocoder)
library(stringr)

##### Geocoding google sheet data, saving as geojson #####

impact <- read_sheet("https://docs.google.com/spreadsheets/d/1TInROva7mCNM7qvTA-6GFrkA4zIHcYNpy2eGhsgyg-8/",
                     sheet = "Sheet1")

df <- impact %>%
  mutate(full_address = paste(street_address, city, state, zip_code, sep = ", ")) 

distinct_address <- df %>%
  distinct(full_address, .keep_all = TRUE)

geodata <- geo(address = distinct_address$full_address, method = "arcgis", full_results = TRUE)

merged_data <- df %>%
  left_join(geodata, by = c("full_address" = "address"))

pre_arcgis_data <- st_as_sf(merged_data, coords = c("location.x", "location.y"), crs = 4269)

st_write(pre_arcgis_data, "data/pre_arcgis_data.geojson", delete_dsn = TRUE)

##### Readin in previously saved and update data #####

impact <- st_read("data/pre_arcgis_data.geojson") %>%
    select(1:15,18,19,28) %>%
    mutate(lbs_produce = bags * 10) %>%
  mutate(event_type = case_when(
  event_type == "distribution" ~ "Produce Drive",
  event_type == "clean" ~ "Clean Up",
  TRUE ~ event_type  # Keep other values unchanged
  )) %>%
  filter(event_type %in% c("Clean Up", "Produce Drive"))

data <- st_read("data/DICE November Update Data.geojson")

divisions <- st_read("data/dpd_divisions.geojson") %>%
  st_transform(4269) %>%
  filter(DIVISION != "CENTRAL") %>%
  select(DIVISION, geometry)

##### Transform ArcGIS data for compatibility w/ current report. Transforming dates, renaming columns, adding divisions #####
data <- data %>%
  mutate(CreationDate = as.POSIXct(CreationDate / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(EditDate = as.POSIXct(EditDate / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(activity_date = as.POSIXct(activity_date / 1000, origin = "1970-01-01", tz = "UTC"))

cleandata <- data %>%
  rename(lbs_trash = for_trash_removal_how_many_poun,
         people_served = how_many_people_were_served,
         date = activity_date,
         summary = activity_description,
         comments = untitled_question_6_other,
         partner = organization_name,
         event_type = activity_type) %>%
  st_transform(st_crs(impact)) %>%
  st_join(divisions) %>%
  rename(district = DIVISION)

##### Manual Entry of Produce Data from comments 
cleandata$bags <- 0
cleandata[5,10] <- "Produce Drive"
cleandata[5,17] <- 115
cleandata[5,13] <- 300
cleandata[21,17] <- 76
cleandata[34,17] <- 97
cleandata[47,17] <- 92
cleandata[49,17] <- 60

##### Combine new data w/ old googlesheet data #####

combined_data <- bind_rows(cleandata, impact) %>%
  st_join(divisions, left = TRUE) %>%
  mutate(District = divisions$District) %>%
  select(1,7,9:17, 25:27) %>%
  mutate(lbs_trash = na_if(lbs_trash, 0)) 

##### Manual Entry of Division #####

combined_data[3,13] <- "NORTHEAST"
combined_data[15,13] <- "NORTHEAST"
combined_data[53,13] <- "NORTHEAST"
combined_data[29,13] <- "NORTH CENTRAL"
combined_data[41,13] <- "NORTH CENTRAL"
combined_data[52,13] <- "NORTH CENTRAL"
combined_data[39,13] <- "SOUTH CENTRAL"
combined_data[51,13] <- "SOUTH CENTRAL"
combined_data[20,13] <- "SOUTH CENTRAL"
combined_data[12,13] <- "SOUTH CENTRAL"
combined_data[50,13] <- "SOUTH CENTRAL"
combined_data[48,13] <- "SOUTHEAST"
combined_data[56,13] <- "SOUTHEAST"
combined_data[40,13] <- "SOUTHEAST"
combined_data[46,13] <- "NORTHWEST"
combined_data[31,13] <- "NORTHWEST"
combined_data[45,13] <- "NORTHWEST"
combined_data[38,4] <- "Produce Drive"

final_data <- combined_data %>%
  mutate(
    month_year = floor_date(date, "month"),
    formatted_month = format(month_year, "%B, %Y")
  )

final_data$date <- as.Date(final_data$date)


##### Estimate Missing Produce Data #####

# Group by DIVISION to calculate group-specific ratios
final_data <- final_data %>%
  group_by(DIVISION) %>%
  mutate(
    # Calculate the division ratio only for "Produce Drive" events
    division_ratio = ifelse(event_type == "Produce Drive", 
                            mean(bags / people_served, na.rm = TRUE), 
                            NA),
    # Update bags only for "Produce Drive" events with missing values
    bags = ifelse(event_type == "Produce Drive" & bags == 0, 
                  round(people_served * division_ratio), 
                  bags)
  ) %>%
  ungroup() %>%
  mutate(lbs_produce = bags * 10)


st_write(final_data, "data/dice_data_november_24.geojson", delete_dsn = TRUE)


