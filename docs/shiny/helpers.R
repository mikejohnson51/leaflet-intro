library(flexdashboard) # Shiny setup
library(shiny)         # Shiny
library(dqshiny)       # Shiny add-on

library(AOI)           # Data and geocoding
library(dataRetrieval) # USGS data access

library(sf)            # all things spatial ...
library(dplyr)         # data manipulations ...

library(leaflet)       # Interactive mapping
library(leafgl)        # WebGL interface for larger data
library(leafpop)       # nicer popups :)

library(dygraphs)      # Interactive graphs

# Needed Data

## `sf` object of USA counties
counties        = AOI::aoi_get(state = 'conus', county = "all") %>%
  mutate(location = paste(name, state_abbr, sep = ", ")) %>%
  select(geoid, name, state_name, location)

## list of USGS gages locations reporting data in the last year ...
sites           = readRDS("usgs_sites.rds")

# Core functions
basemap        = function(){
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lat = 39, lng = -95, zoom = 3)
}

zoom_to_county = function(map, counties, FIP){

  shp    = filter(counties, geoid == FIP)
  bounds = as.vector(st_bbox(shp))

  clearGroup(map, 'shp') %>%
    addPolygons(data = shp,
                color      = "#003660",
                fillColor  = "#FEBC11",
                fillOpacity = .2,
                group = "shp") %>%
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

# USGS functions
get_streamflow = function(siteID){
  readNWISdv(siteNumbers = siteID,
             parameterCd = '00060',
             startDate   = Sys.Date() - 365) %>%
    renameNWISColumns()
}

find_nldi  = function(x, y){
  findNLDI(location = c(x,y), nav = c('UM'),
           find = c("basin", "flowlines"), distance = 1000)
}
