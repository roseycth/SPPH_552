# set up ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)
library(tmap)
library(tmaptools)
library(shiny)

# read in data ------------------------------------------------------------

# NYC neighbourhood

nbhd_nyc <- 
  st_read('data/raw/shapefiles/nyc_neighbourhoods.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 2263)

# NYC subway line

subway_nyc <- 
  st_read('data/raw/shapefiles/routes_nyc_subway_nov2020/routes_nyc_subway_nov2020.shp') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 2263)

# NYC subway stop

subway_stops_nyc <- 
  st_read('data/raw/shapefiles/stops_nyc_subway_nov2020/stops_nyc_subway_nov2020.shp') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 2263)

# NYC Airbnb

airbnb_nyc <- 
  read_csv('data/raw/nyc_listings_d.csv') %>% 
  st_as_sf(coords = c('longitude','latitude'),
           crs = 4326) %>% 
  st_transform(crs = 2263)


# NYC real estate sales
real_estate_nyc <-
  st_read('data/raw/shapefiles/real_property_sales_nyc_2019/real_property_sales_nyc_2019.shp') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 2263)


# Process Data ------------------------------------------------------------

airbnb_nyc$price = as.numeric(gsub("\\$", "", airbnb_nyc$price))

nbhd_bbox <-
  nbhd_nyc %>%
  st_bbox()

# Neighborhood (Airbnb and Real Estate Price) -----------------------------

tmap_mode('view')

# NYC:real estate sales
m1 <- 
real_estate_nyc %>% 
  st_join(nbhd_nyc %>% select(neighbourhood)) %>% 
  as_tibble() %>% 
  group_by(neighbourhood) %>%
  mutate(sqft_price = price/tot_sqft) %>%
  filter(is.infinite(sqft_price) == FALSE) %>%
  summarize(mean_price = mean(sqft_price,na.rm =TRUE))%>% 
  left_join(
    nbhd_nyc,
    .,
    by='neighbourhood') %>%
  tm_shape(name = 'NYC Real Estate Price (2019) by Neighborhood')+
  tm_polygons(
    col = 'mean_price',
    style = 'fixed',
    breaks =
      c(-Inf,
        200,
        250,
        300,
        400,
        +Inf),
    palette = "GnBu",
    n = 6,
    contrast = c(0.2, 0.8),
    #alpha = 0.8,
    popup.vars = c(
      'average per square feet price($)' = 'mean_price'),
    title = 'NYC 2019 Average Real Estate Sales Price($, Per Sq Ft)')+
  tmap_options(check.and.fix = TRUE) +
  tm_view(bbox = nbhd_bbox)

tmap_save(m1, "output/webpage/housing_price_nbhd.html")

# NYC: Airbnb Average Price by Neighborhoods

m2 <- 
  airbnb_nyc %>%
  st_join(nbhd_nyc %>% select(neighbourhood)) %>% 
  as_tibble() %>%
  group_by(neighbourhood_cleansed) %>% 
  summarize(mean_price = mean(price,na.rm = TRUE)) %>% 
  left_join(
    nbhd_nyc,
    .,
    by=c('neighbourhood'='neighbourhood_cleansed')) %>%
  tm_shape(name = 'NYC Airbnb Price by Neighborhood')+
  tm_polygons(
    col = 'mean_price',
    style = 'fixed',
    breaks = 
      c(-Inf,
        100,
        120,
        140,
        200, 
        +Inf),
    n = 6, 
    palette = "GnBu",
    contrast = c(0.2, 0.8),
    popup.vars = c(
      'average price per night stay($)' = 'mean_price'),
    title = 'NYC Average Airbnb Price($, Per Night)')+
  tmap_options(check.and.fix = TRUE) +
  tm_view(bbox = nbhd_bbox)

tmap_save(m2, "output/webpage/airbnb_price_nbhd.html")

# Transportation ----------------------------------------------------------


# Sample 40% of Airbnb

set.seed(123)

airbnb_nyc_small <- 
  airbnb_nyc %>% 
  select(
    name,
    host_name,
    host_is_superhost,
    price,
    room_type,
    number_of_reviews,
    review_scores_rating,
    minimum_nights,
    availability_365,
    neighbourhood_cleansed
  ) %>% 
  sample_frac(0.4)


m3 <- 
  tm_shape(nbhd_nyc,
         name = 'NYC Neighborhoods')+
  tm_polygons(
    id = 'neighbourhood',
    popup.vars = c('Borough' ='neighbourhood_group'))+
  tmap_options(check.and.fix = TRUE)+
  
  
  tm_shape(airbnb_nyc_small,
           name = 'NYC Airbnb Listings Distribution')+
  tm_dots(
    id = 'name',
    col = 'price',
    style = 'fixed',
    breaks =
      c(-Inf,
        70,
        105,
        155,
        250,
        +Inf),
    palette = "GnBu",
    n = 6,
    contrast = c(0.2, 0.8),
    popup.vars = c(
      'Host Name' = 'host_name',
      'Super Host?' = 'host_is_superhost',
      'Price ($)' = 'price',
      'Room Type' = 'room_type',
      'Number of Reviews' = 'number_of_reviews',
      'Rating Score' = 'review_scores_rating',
      'Minimum Nights to book' = 'minimum_nights',
      'Availability in future 365 days' = 'availability_365'),
    title = 'Price of Airbnb Listings ($, Per Night)') +
  


  tm_shape(subway_nyc,
           name = 'NYC Subway Routes') +
  
  tm_lines(
    id = 'route_long',
    lwd = 2,
    col = 'color',
    popup.vars = c('Route ID' = 'route_id')) +
  
  tm_shape(subway_stops_nyc,
           name = 'NYC Subway Stops') +
  tm_markers(
    id = 'stop_name',
    popup.vars = 
      c('Subway Train Route' = 'trains',
        'Structure' ='structure')) +
  tm_view(bbox = nbhd_bbox)

tmap_save(m3, "output/webpage/airbnb_transportaion.html")
  