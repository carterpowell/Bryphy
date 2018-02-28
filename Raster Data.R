
# Libraries ---------------------------------------------------------------
require(raster)
require(rgdal)
library(maps)
library(mapdata)
library(dismo) 
library(rJava) 
library(maptools)
library(jsonlite)

# Overlaying A World Clim Raster onto our Raster --------------------------

#Download data from World Clim
currentEnv=getData("worldclim", var="bio", res=2.5)
