library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(sf)
library(broom)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

spdf_fortified_name <- tidy(spdf, region = "google_name")

FoodSrvcByCounty$county = as.character(FoodSrvcByCounty$County)
spdf_fortified$county = toupper(spdf_fortified$id)
spdf_fortified <- spdf_fortified %>%
  left_join(. , FoodSrvcByCounty, by = c("county" = "county"))

head(spdf_fortified)

ggplot() +
  geom_polygon(data = spdf_fortified,
               aes(
                 x = long,
                 y = lat,
                 group = group,
                 fill =  FoodServices.2007
               )) +
  theme_void() + labs(fill = 'Food Services - 2007',
                      title = "Food Services by State- 2007")
