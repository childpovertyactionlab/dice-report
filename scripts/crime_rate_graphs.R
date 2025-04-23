library(tidyverse)
library(cpaltemplates)
library(sf)
library(mapgl)
library(glue)
library(highcharter)
library(lubridate)

incidents <- st_read("C:\\Users\\erose\\CPAL Dropbox\\Data Library\\City of Dallas\\04_Public Safety\\Dallas Police\\Data\\Incidents\\Processed Data\\ORR Dallas Police Incidents Patch.geojson") 

incidents2024 <- incidents %>%
  filter(Year == 2024)

clean_incidents <- incidents %>%
  rename(nibrs_crime = NIBRS_Crime,
         division = Division) %>%
  st_drop_geometry()

violent_crime <- clean_incidents %>%
  filter(
    nibrs_crime %in% c(
      "AGG ASSAULT - NFV", 
      "MURDER & NONNEGLIGENT MANSLAUGHTER", 
      "ROBBERY-INDIVIDUAL",
      "ROBBERY-BUSINESS"
    )
  )

incidents_by_division_month <- violent_crime %>%
  group_by(division, Year, Month) %>%
  summarise(count = n()) %>%
  filter(division != "") %>%
  ungroup()

prev_3_yr_avg <- incidents_by_division_month %>%
  filter(Year %in% c("2021", "2022", "2023")) %>%
  group_by(division, Month) %>%
  summarise(mean = mean(count, na.rm = TRUE), .groups = 'drop') %>%
  mutate(mean = round(mean)) %>%
  mutate(Month = month(Month, label = TRUE, abbr = FALSE))

write.csv(prev_3_yr_avg, "data/crime/prev_3_avg_crime.csv")

monthly_avg_2024 <- incidents_by_division_month %>%
  filter(Year == "2024") %>%
  group_by(division, Month) %>%
  summarise(mean = mean(count, na.rm = TRUE), .groups = 'drop') %>%
  mutate(mean = round(mean)) %>%
  mutate(Month = month(Month, label = TRUE, abbr = FALSE)) 

write.csv(monthly_avg_2024, "data/crime/monthly_avg_2024_crime.csv")

by_type_prev_3 <- violent_crime%>%
  group_by(division, Year, nibrs_crime) %>%
  summarise(yr_crime = n(), .groups = 'drop') %>%
  group_by(nibrs_crime, division) %>%
  mutate(prev_3 = mean(yr_crime)) %>%
  distinct(division, nibrs_crime, prev_3) %>%
  mutate(across(where(is.numeric), round))

write.csv(by_type_prev_3, "data/crime/by_type_prev_3.csv")

by_type_24 <- violent_crime %>%
  filter(Year == "2024") %>%
  group_by(nibrs_crime, division) %>%
  summarise(yr_crime = n(), .groups = 'drop') 
  

write.csv(by_type_24, "data/crime/by_type_24.csv")


clean_incidents <- incidents %>%
  rename(nibrs_crime = NIBRS_Crime,
         division = Division) %>%
  st_drop_geometry()

# 2. Filter for violent crime and valid division
violent_crime <- clean_incidents %>%
  filter(
    nibrs_crime %in% c("AGG ASSAULT - NFV", "MURDER & NONNEGLIGENT MANSLAUGHTER", 
                       "ROBBERY-INDIVIDUAL", "ROBBERY-BUSINESS"),
    division != ""
  )

# 3. Monthly counts by division and crime type
incidents_by_division_month <- violent_crime %>%
  group_by(division, Year, Month, nibrs_crime) %>%
  summarise(count = n(), .groups = "drop")

# 4. Create complete combination grid
all_combos <- expand_grid(
  division = unique(violent_crime$division),
  Year = 2021:2024,
  Month = 1:12,
  nibrs_crime = unique(violent_crime$nibrs_crime)
)

# 5. Join and fill in missing combinations with 0
incidents_full <- all_combos %>%
  left_join(incidents_by_division_month, by = c("division", "Year", "Month", "nibrs_crime")) %>%
  mutate(count = replace_na(count, 0),
         Month = month(Month, label = TRUE, abbr = FALSE))

# 6. Calculate monthly average for 2021–2023
prev_3_yr_avg <- incidents_full %>%
  filter(Year %in% 2021:2023) %>%
  group_by(division, Month) %>%
  summarise(mean = round(mean(count, na.rm = TRUE)), .groups = "drop")

write.csv(prev_3_yr_avg, "data/crime/prev_3_avg_crime.csv", row.names = FALSE)

# 7. Calculate monthly average for 2024
monthly_avg_2024 <- incidents_full %>%
  filter(Year == 2024) %>%
  group_by(division, Month) %>%
  summarise(mean = round(mean(count, na.rm = TRUE)), .groups = "drop")

write.csv(monthly_avg_2024, "data/crime/monthly_avg_2024_crime.csv", row.names = FALSE)

# 8. Yearly totals by crime type and division for 2021–2023 average
by_type_prev_3 <- incidents_full %>%
  filter(Year %in% 2021:2023) %>%
  group_by(division, nibrs_crime, Year) %>%
  summarise(year_total = sum(count), .groups = "drop") %>%
  group_by(division, nibrs_crime) %>%
  summarise(prev_3 = round(mean(year_total)), .groups = "drop")

write.csv(by_type_prev_3, "data/crime/by_type_prev_3.csv", row.names = FALSE)

# 9. Yearly totals by crime type and division for 2024
by_type_24 <- incidents_full %>%
  filter(Year == 2024) %>%
  group_by(division, nibrs_crime) %>%
  summarise(yr_crime = sum(count), .groups = "drop")

write.csv(by_type_24, "data/crime/by_type_24.csv", row.names = FALSE)


##### Northeast Crime #####

prev_3_yr_avg_NE <- prev_3_yr_avg %>%
  filter(division == "Northeast")

monthly_avg_2024_NE <- monthly_avg_2024 %>%
  filter(division == "Northeast")

prev_3_yr_type_NE <- by_type_prev_3 %>%
  filter(division == "Northeast")

by_type_24_NE <- by_type_24 %>%
  filter(division == "Northeast")

highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = prev_3_yr_avg_NE$Month) %>%
  hc_yAxis(min = 0) %>% 
  hc_add_series(data = prev_3_yr_avg_NE$mean, name = "Previous 3 Year Average" , x = prev_3_yr_avg_NE$Month) %>%
  hc_add_series(data = monthly_avg_2024_NE$mean, name = "2024 Incidents", x = monthly_avg_2024_NE$Month) %>%
  hc_title(text = "Incidents of Violent Crime in Northeast Division") %>%
  hc_tooltip(pointFormat = "{series.x}:{point.y}") %>%
  hc_colors(palette_cpal_main) %>%
  hc_plotOptions(series = list(
    lineWidth = 4,  
    marker = list(
      enabled = TRUE,
      symbol = "circle", 
      radius = 6,         
      fillColor = NULL,   
      lineWidth = 2,
      lineColor = NULL    
    )
  )) |> 
  hc_legend(enabled = TRUE )

highchart() %>%
    hc_chart(type = "line") %>%
    hc_xAxis(categories = monthly_avg_2024_NE$Month) %>%
    hc_yAxis(min = 0) %>% 
    hc_add_series(data = prev_3_yr_avg_NE$mean, x = prev_3_yr_avg_NE, name = "Previous 3 Year Average") %>%
    hc_add_series(data = monthly_avg_2024_NE$mean, name = "2024 Incidents") %>%
    hc_title(text = "Incidents of Violent Crime in Northeast Division") %>%
    hc_tooltip(pointFormat = "{series.name}: {point.y}") %>%
    hc_colors(palette_cpal_main) %>%
    hc_plotOptions(series = list(
      lineWidth = 4,  
      marker = list(
        enabled = TRUE,
        symbol = "circle", 
        radius = 6,         
        fillColor = NULL,   
        lineWidth = 2,
        lineColor = NULL    
      )
    )) %>%
    hc_legend(enabled = TRUE)
  
highchart() %>%
  hc_chart(type = "column", width = 500) %>%
  hc_title(text = "Violent Crime By Type") %>%
  hc_xAxis(categories = by_type_24_NE$nibrs_crime) %>%
  hc_yAxis(title = list(text = "Incidents")) %>%
  hc_add_series(name = "2024", data = by_type_24_NE$yr_crime) %>%
  hc_add_series(name = "Previous 3 year average", data = prev_3_yr_type_NE$prev_3) %>%
  hc_plotOptions(column = list(
    grouping = TRUE,
    shadow = FALSE,
    borderWidth = 0
  ))



chart2 <- highchart() %>%
  hc_chart(type = "column", width = 400) %>%
  hc_xAxis(categories = by_type_2024_NE$nibrs_crime) %>%
  hc_add_series(data = list_parse2(by_type_24_NE), name = "Crime") %>%
  hc_title(text = "Crime by Type") %>%
  hc_tooltip(pointFormat = "{point.y:.2f} Incidents" ) %>%
  hc_colors(palette_cpal_main) %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(column = list(
    allowPointSelect = TRUE,
    cursor = 'pointer',
    dataLabels = list(
      enabled = FALSE
    )
  ))

print(chart2) 
##### Northwest Crime #####

prev_3_yr_avg_NW <- prev_3_yr_avg %>%
  filter(division == "Northwest")

monthly_avg_2024_NW <- monthly_avg_2024 %>%
  filter(division == "Northwest")

prev_3_yr_type_NW <- by_type_prev_3 %>%
  filter(division == "Northwest")

by_type_2024_NW <- by_type_24 %>%
  filter(division == "Northwest")

highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = prev_3_yr_avg_NW$Month) %>%
  hc_yAxis(min = 0) %>% 
  hc_add_series(data = monthly_avg_2024_NW$mean, name = "2024 Incidents", x = monthly_avg_2024_NW$Month) %>%
  hc_add_series(data = prev_3_yr_avg_NW$mean, name = "Previous 3 Year Average" , x = prev_3_yr_avg_NW$Month) %>%
  hc_title(text = "Incidents of Violent Crime in Northwest Division") %>%
  hc_tooltip(pointFormat = "{series.x}: {point.y}") %>%
  hc_colors(palette_cpal_main) %>%
  hc_plotOptions(series = list(
    lineWidth = 4,  
    marker = list(
      enabled = TRUE,
      symbol = "circle", 
      radius = 6,         
      fillColor = NULL,   
      lineWidth = 2,
      lineColor = NULL    
    )
  )) |> 
  hc_legend(enabled = TRUE )

##### North Central Crime #####

prev_3_yr_avg_NC <- prev_3_yr_avg %>%
  as.data.frame(prev_3_yr_avg) %>%
  filter(division == "North Central")

monthly_avg_2024_NC <- monthly_avg_2024 %>%
  as.data.frame(monthly_avg_2024) %>%
  filter(division == "North Central")

prev_3_yr_type_NC <- by_type_prev_3 %>%
  filter(division == "North Central")

by_type_2024_NC <- by_type_24 %>%
  filter(division == "North Central")

highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = prev_3_yr_avg_NC$Month) %>%
  hc_yAxis(min = 0) %>% 
  hc_add_series(
    data = list_parse(prev_3_yr_avg_NC$mean), 
    name = "Previous 3 Year Average", 
    pointStart = 0  # Start at the first index of the x-axis
  ) %>%
  hc_add_series(
    data = list_parse2(monthly_avg_2024_NC$mean), 
    name = "2024 Incidents", 
    pointStart = match(monthly_avg_2024_NC$Month[1], prev_3_yr_avg_NC$Month) - 1
  ) %>%
  hc_title(text = "Incidents of Violent Crime in North Central Division") %>%
  hc_tooltip(pointFormat = "{series.name}: {point.y}") %>%
  hc_colors(palette_cpal_main) %>%
  hc_plotOptions(series = list(
    lineWidth = 4,  
    marker = list(
      enabled = TRUE,
      symbol = "circle", 
      radius = 6,         
      fillColor = NULL,   
      lineWidth = 2,
      lineColor = NULL    
    )
  )) %>% 
  hc_legend(enabled = TRUE)

##### Southeast Crime #####

prev_3_yr_avg_SE <- prev_3_yr_avg %>%
  filter(division == "Southeast")

monthly_avg_2024_SE <- monthly_avg_2024 %>%
  filter(division == "Southeast")

prev_3_yr_type_SE <- by_type_prev_3 %>%
  filter(division == "Southeast")

by_type_2024_SE <- by_type_24 %>%
  filter(division == "Southeast")

highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = prev_3_yr_avg_SE$Month) %>%
  hc_yAxis(min = 0) %>% 
  hc_add_series(data = monthly_avg_2024_SE$mean, name = "2024 Incidents", x = monthly_avg_2024_SE$Month) %>%
  hc_add_series(data = prev_3_yr_avg_SE$mean, name = "Previous 3 Year Average" , x = prev_3_yr_avg_SE$Month) %>%
  hc_title(text = "Incidents of Violent Crime in North Central Division") %>%
  hc_tooltip(pointFormat = "{series.x}: {point.y}") %>%
  hc_colors(palette_cpal_main) %>%
  hc_plotOptions(series = list(
    lineWidth = 4,  
    marker = list(
      enabled = TRUE,
      symbol = "circle", 
      radius = 6,         
      fillColor = NULL,   
      lineWidth = 2,
      lineColor = NULL    
    )
  )) |> 
  hc_legend(enabled = TRUE )

##### Southwest Crime #####



##### South Central Crime #####

prev_3_yr_avg_SC <- prev_3_yr_avg %>%
  filter(division == "South Central")

monthly_avg_2024_SC <- monthly_avg_2024 %>%
  filter(division == "South Central")

highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = prev_3_yr_avg_SC$Month) %>%
  hc_yAxis(min = 0) %>% 
  hc_add_series(data = prev_3_yr_avg_SC$mean, name = "Previous 3 Year Average", x = prev_3_yr_avg_SC$Month) %>%
  hc_add_series(data = monthly_avg_2024_SC$mean, name = "2024 Incidents", x = monthly_avg_2024_SC$Month) %>%
  hc_title(text = "Incidents of Violent Crime in South Central Division") %>%
  hc_tooltip(pointFormat = "{series.name}: {point.y:.2f}") %>%
  hc_colors(palette_cpal_main) %>%
  hc_plotOptions(series = list(
    lineWidth = 4,  
    marker = list(
      enabled = TRUE,
      symbol = "circle", 
      radius = 6,         
      fillColor = NULL,   
      lineWidth = 2,
      lineColor = NULL    
    )
  )) |> 
  hc_legend(enabled = TRUE)



incidents_by_sector_yr <- incidents %>%
  group_by(Year, division, nibrs_crime) %>%
  summarise(count = n(), .groups = "keep") %>%
  filter(division != "") %>%
  ungroup() 


highchart() %>%
  hc_chart(type = "column", width = 400) %>%
  hc_xAxis(categories = violent_crime_NE$nibrs_crime) %>%
  hc_add_series(data = list_parse2(violent_crime_NE$count), name = "Incidents") %>%
  hc_title(text = "Incidents of Violent Crime by Type") %>%
  hc_tooltip(pointFormat = "{point.y:.2f} Incidents" ) %>%
  hc_colors(palette_cpal_main) %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(column = list(
    allowPointSelect = TRUE,
    cursor = 'pointer',
    dataLabels = list(
      enabled = FALSE
    )
  ))


