library(tidyverse)
library(cpaltemplates)
library(sf)
library(tidygeocoder)
library(stringr)

olddata <- st_read("data/dice_data_feb4.geojson")

newdata <- st_read("data/MarchUpdate.geojson")

divisions <- st_read("data/dpd_divisions.geojson") %>%
  st_transform(4269) %>%
  filter(DIVISION != "CENTRAL") %>%
  select(DIVISION, geometry)

newdata <- newdata %>%
  mutate(CreationDate = format(as.Date(CreationDate, format = "%a, %d %b %Y %H:%M:%S"), "%Y-%m-%d")) %>%
  mutate(activity_date  = format(as.Date(activity_date, format = "%a, %d %b %Y %H:%M:%S"), "%Y-%m-%d"))


newcleandata <- newdata %>%
  rename(lbs_trash = for_trash_removal_how_many_poun,
         people_served = how_many_people_were_served,
         date = activity_date,
         summary = activity_description,
         comments = untitled_question_6_other,
         partner = organization_name,
         event_type = activity_type) %>%
  st_transform(st_crs(olddata)) %>%
  st_join(divisions) %>%
  st_transform(crs = 4269)

filtercleandata <- newcleandata %>%
  as.data.frame() %>%
  filter(date > as.Date("2025-01-26")) %>%
  mutate(CreationDate = as.Date(CreationDate))

attatchment_key <- final_data_2 %>%
  select(objectid, division, event_type)
  
final_data <- olddata %>%
  mutate(DIVISION = division) %>%
  st_transform(crs = 4269) %>%
  bind_rows(filtercleandata) %>%
  mutate(
    month_year = floor_date(date, "month"),
    formatted_month = format(month_year, "%B, %Y")
  ) 

final_data$date <- as.Date(final_data$date)
final_datav <- st_make_valid(final_data)
final_datav[127,15] <- "SOUTHWEST"
final_datav[124,15] <- "SOUTHEAST"

final_data_2 <- final_datav %>%
  mutate(division = DIVISION) %>%
  select(!DIVISION) %>%
  st_make_valid()
  
## Remove olddata
file.remove("data/dice_data_feb4.geojson")

st_write(final_data_2, "data/dice_data_march3.geojson")
