# set up ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(shiny)

# read in data ------------------------------------------------------------

# Vancouver neighbourhood
van_boundary <- 
  st_read('/Users/roseycth/Documents/UBC/2024W2/SPPH552/final_proj/local-area-boundary.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower())

fountain_van <- 
  st_read('/Users/roseycth/Documents/UBC/2024W2/SPPH552/final_proj/drinking-fountains.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower())

comm_center_van <- 
  st_read('/Users/roseycth/Documents/UBC/2024W2/SPPH552/final_proj/community-centres.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower())

# Mapping -----------------------------------------------------------------


tmap_mode('view')

tm_shape(van_boundary,
       name = 'Vancouver Neighborhoods')+
tm_polygons(colour='blue',alpha = 0.4)+
tm_shape(fountain_van,
           name = 'Water Fountains') +
tm_markers(
  id = 'name',
  popup.vars = 
    c('Detailed location' = 'location',
      'Neighbourhood' = 'geo_local_area',
      'Pet Friendly(Y/N)' ='pet_friendly',
      'Maintainer' ='maintainer',
      'Operation Season' = 'in_operation'))

tm_shape(van_boundary, name = 'Vancouver Neighborhoods') +
  tm_polygons(
    id = 'name',
    col='lightblue',
    alpha = 0.4,
    popup.vars = c('Name'= 'name')) +
  tm_basemap("OpenStreetMap")+
  
  tm_shape(fountain_van, name = 'Water Fountains') +
  tm_markers(
    id = 'name',
    popup.vars = 
      c('Detailed location' = 'location',
        'Neighbourhood' = 'geo_local_area',
        'Pet Friendly(Y/N)' ='pet_friendly',
        'Maintainer' ='maintainer',
        'Operation Season' = 'in_operation')) +
  
  tm_shape(comm_center_van, name = 'Community centers') +
  tm_dots(
    id = 'name',
    col = 'red',
    popup.vars = 
      c('Address' = 'address',
        'Website link' = 'urllink'))







