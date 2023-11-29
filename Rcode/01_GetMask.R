######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

rm(list = ls())
library(sf)
library(tidyverse)
library(raster)
library(geosphere)
library(sp)
library(glue)
library(rgee)

#-------- init
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ee_Initialize()
WGS84Proj<-"+proj=longlat +datum=WGS84 +no_defs"
fdata = format(Sys.Date(),"%Y_%m_%d")
source('Functions/downloadChelsa.R')

#--------- water mask data
WtrMsk = ee$ImageCollection("MODIS/006/MOD44W") %>%
  ee$ImageCollection$filterDate(paste0(2015,'-01-01'), paste0(2015,'-12-31')) %>%
  ee$ImageCollection$map(function(x) x$select("water_mask")) %>%
  ee$ImageCollection$mean() %>% ee$ImageCollection$toBands() 
WtrMsk = WtrMsk$gt(0)
WtrMsk = WtrMsk$updateMask(WtrMsk$eq(0))
WtrMsk = WtrMsk$add(1)
Map$addLayer(WtrMsk, list(min = 0, max = 1, palette= c('black', 'blue')), name = "water_mask")

#--------- function for creating crop geometry
crop_geometry = function(min_long,max_long,min_lat,max_lat){
  geometry = ee$Geometry$Rectangle(coords = c(min_long,min_lat,max_long,max_lat),proj="EPSG:4326",geodesic=FALSE)
  return(geometry)
}
cropgoem = crop_geometry(-180,180,-70,85) #crop_geometry(-180,180,-70,80)

#--------- download combined mask raster
r = ee_as_raster(WtrMsk,region=cropgoem,via="drive",container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE)
r1 = terra::rast(r)
terra::plot(r1)

#--------- process mask
chelsaDir = glue("{getwd()}/Data/")
downloadChelsa_bioclim(timeperiod = "1981-2010", var = "kg2", outdir = chelsaDir)
cu = list.files(chelsaDir,pattern="*.tif",full.names=TRUE) %>% grep("kg2",.,value=TRUE) %>% grep("1981",.,value=TRUE) %>% terra::rast()
r2 = terra::resample(r1, cu, threads=TRUE, method="near")
terra::plot(r2)
r2 = terra::ifel(r2==0,NA,r2)
terra::plot(r2)

#--------- write mask do disk
terra::writeRaster(r2, "Data/WaterMask_fullCoverage.tif", overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")
terra::tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE)

