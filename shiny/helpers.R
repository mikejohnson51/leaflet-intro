library(flexdashboard)
library(shiny)
library(dqshiny)

library(AOI)
library(dataRetrieval)

library(sf)
library(dplyr)

library(leaflet)
library(leafgl)
library(leafpop)

library(dygraphs)


# Needed Data
counties     = AOI::aoi_get(state = 'conus', county = "all")
sites        = readRDS("usgs_sites.rds")
unique_sites = sites[!duplicated(sites$name),]

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
