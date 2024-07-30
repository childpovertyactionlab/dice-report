library(sf)
library(tidyverse)

ne_rtm_cells <- st_read("data/rtm_cells/NorthEast_Top13.shp") 
nw_rtm_cells <- st_read("data/rtm_cells/NorthWest_Top10.shp")
nc_rtm_cells <- st_read("data/rtm_cells/NorthCentral_Top8.shp")
se_rtm_cells <- st_read("data/rtm_cells/SouthEast_Top10.shp")
sw_rtm_cells <- st_read("data/rtm_cells/SouthWest_Top10.shp")
sc_rtm_cells <- st_read("data/rtm_cells/SouthCentral_Top13.shp")

nc_rtm_cells <- st_transform(nc_rtm_cells, crs = 4269) %>%
  select("highest", "rrs", "geometry") %>%
  mutate(division = "North Central")
ne_rtm_cells <- st_transform(ne_rtm_cells, crs = 4269) %>%
  select("highest", "rrs", "geometry") %>%
  mutate(division = "North East")
nw_rtm_cells <- st_transform(nw_rtm_cells, crs = 4269) %>%
  select("highest", "rrs", "geometry") %>%
  mutate(division = "North West")
sc_rtm_cells <- st_transform(sc_rtm_cells, crs = 4269) %>%
  select("highest", "rrs", "geometry") %>%
  mutate(division = "South Central")
se_rtm_cells <- st_transform(se_rtm_cells, crs = 4269) %>%
  select("highest", "rrs", "geometry") %>%
  mutate(division = "Southeast")
sw_rtm_cells <- st_transform(sw_rtm_cells, crs = 4269) %>%
  select("highest", "rrs", "geometry") %>%
  mutate(division = "Southwest")

rtm_cells <- bind_rows(nc_rtm_cells, ne_rtm_cells, nw_rtm_cells, sc_rtm_cells, se_rtm_cells, sw_rtm_cells)

st_write(rtm_cells, dsn = "data/rtm_cells/citywide_rtm.shp")

