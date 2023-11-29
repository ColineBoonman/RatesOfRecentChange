######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

rm(list = ls())
library(sf)
library(tidyverse)
library(terra)
library(geosphere)
library(sp)
library(fst)
library(glue)
library(rgee)
library(tictoc)

#-------- init
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ee_Initialize(drive = TRUE)
WGS84Proj<-"+proj=longlat +datum=WGS84 +no_defs"
fdata = format(Sys.Date(),"%Y_%m_%d")
tempo_dir = paste0("TempDir_",fdata)
dir.create(tempo_dir)
terraOptions(tempdir=tempo_dir, memfrac=0.8, todisk=FALSE)
source("Functions/extractGEE.R")
source("Functions/downloadChelsa.R")
cropgoem = crop_geometry(-180,180,-70,85) #crop_geometry(-180,180,-70,80)

#--------- water mask function
WtrMsk = ee$ImageCollection("MODIS/006/MOD44W") %>%
  ee$ImageCollection$filterDate(paste0(2015,'-01-01'), paste0(2015,'-12-31')) %>%
  ee$ImageCollection$map(function(x) x$select("water_mask")) %>%
  ee$ImageCollection$mean() %>% ee$ImageCollection$toBands() 
Map$addLayer(WtrMsk, list(min = 0, max = 1, palette= c('BBBBBB', 'blue')), name = "water_mask")
wtrMsk = function(input_img) input_img$updateMask(WtrMsk$eq(0))

#--------- kg2 clim regions for masking of species occ
kg2 = wtrMsk(ee$Image("users/selwynhoeks/bioclim_kg2_1981-2010")) # see Beck et al. in CHELSA BIO+
Map$addLayer(kg2, list(min = 0, max = 31, palette= c('red','blue', 'green')), name = "test")

#--------- cropland calculations
Global_cropland_2003 = ee$ImageCollection("users/potapovpeter/Global_cropland_2003") %>% ee$ImageCollection$mean()
Global_cropland_2007 = ee$ImageCollection("users/potapovpeter/Global_cropland_2007") %>% ee$ImageCollection$mean()
Global_cropland_2011 = ee$ImageCollection("users/potapovpeter/Global_cropland_2011") %>% ee$ImageCollection$mean()
Global_cropland_2015 = ee$ImageCollection("users/potapovpeter/Global_cropland_2015") %>% ee$ImageCollection$mean()
Global_cropland_2019 = ee$ImageCollection("users/potapovpeter/Global_cropland_2019") %>% ee$ImageCollection$mean()
Global_cropland_2003 = wtrMsk(Global_cropland_2003)
Global_cropland_2007 = wtrMsk(Global_cropland_2007)
Global_cropland_2011 = wtrMsk(Global_cropland_2011)
Global_cropland_2015 = wtrMsk(Global_cropland_2015)
Global_cropland_2019 = wtrMsk(Global_cropland_2019)
Map$addLayer(Global_cropland_2007, list(min = 0, max = 1, palette= c('BBBBBB', '009900')), name = "gain")

Croplandpost2003 = Global_cropland_2007$add(Global_cropland_2011)
Croplandpost2003 = Croplandpost2003$add(Global_cropland_2015)
Croplandpost2003 = Croplandpost2003$add(Global_cropland_2019)
Croplandpost2003Bin = Croplandpost2003$gt(0)
CroplandChange = Croplandpost2003Bin$subtract(Global_cropland_2003)
CroplandChangeMasked = CroplandChange$updateMask(CroplandChange$eq(1))
Map$addLayer(CroplandChangeMasked, list(min = 0, max = 4, palette= c('BBBBBB', '009900')), name = "gain")

map = Map$addLayer(Croplandpost2003, list(min = 0, max = 1, palette= c('red', 'green')), name = "Croplandpost2003")
map = map +Map$addLayer(Global_cropland_2003, list(min = 0, max = 1, palette= c('red', 'green')), name = "Global_cropland_2003")
map = map + Map$addLayer(CroplandChangeMasked, list(min = 0, max = 1, palette= c('BBBBBB', 'purple')), name = "gainbin")
map

# extract layer
CroplandChangeMasked = wtrMsk(CroplandChangeMasked)

# download layers
r = ee_as_raster(CroplandChangeMasked,region=cropgoem,via="drive",container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE)
rt = merge_downloaded_rasters(r)
plot(rt[[1]])
names(rt)

# process rast
chelsaDir = glue("{getwd()}/Data/")
downloadChelsa_bioclim(timeperiod = "1981-2010", var = "kg2", outdir = chelsaDir)
cu = list.files(chelsaDir,pattern="*.tif",full.names=TRUE) %>% grep("kg2",.,value=TRUE) %>% grep("1981",.,value=TRUE) %>% terra::rast()
rt2 = resample(rt, cu, threads=TRUE, method="near")
plot(rt2[[1]])
#r2 = ifel(r2==0,NA,r2)
#plot(r2)

# write rast do disk
writeRaster(rt2, "Data/EnvDataGEE/CropLandChange_data.tif", overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")
tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE)

#--------- Forest cover calculations

GFCC_2000 = ee$Image("users/selwynhoeks/GFCC_2000") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands()
GFCC_2005 = ee$Image("users/selwynhoeks/GFCC_2005") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands() 
GFCC_2010 = ee$Image("users/selwynhoeks/GFCC_2010") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands() 
GFCC_2015 = ee$Image("users/selwynhoeks/GFCC_2015") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands()
GFCC_2000 = wtrMsk(GFCC_2000)
GFCC_2005 = wtrMsk(GFCC_2005)
GFCC_2010 = wtrMsk(GFCC_2010)
GFCC_2015 = wtrMsk(GFCC_2015)
FCC_threshold = 5
Bin_TC1 = GFCC_2000$subtract(GFCC_2005)$gte(FCC_threshold)
Bin_TC2 = GFCC_2005$subtract(GFCC_2010)$gte(FCC_threshold)
Bin_TC3 = GFCC_2010$subtract(GFCC_2015)$gte(FCC_threshold)
Bin_TC = Bin_TC1$add(Bin_TC2)
Bin_TC = Bin_TC$add(Bin_TC3)
TCmax = Bin_TC$gt(0)
map = Map$addLayer(Bin_TC, name = "Bin_TC", list(min = 0, max = 3, palette= c('grey', 'green','orange','red')))
map = map + Map$addLayer(TCmax, name = "TCmax", list(min = 0, max = 1, palette= c('grey', 'red')))
map

# download layers
r = ee_as_raster(TCmax,region=cropgoem,via="drive",container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE)
rt = rast(r)
rt = ifel(rt==0,NA,rt)
plot(rt[[1]])
names(rt) = "ForestCoverLoss_GFCCloss"

# process rast
chelsaDir = glue("{getwd()}/Data/")
downloadChelsa_bioclim(timeperiod = "1981-2010", var = "kg2", outdir = chelsaDir)
cu = list.files(chelsaDir,pattern="*.tif",full.names=TRUE) %>% grep("kg2",.,value=TRUE) %>% grep("1981",.,value=TRUE) %>% terra::rast()
rt2 = resample(rt, cu, threads=TRUE, method="near")
plot(rt2[[1]])

# write rast do disk
writeRaster(rt2, "Data/EnvDataGEE/ForestCoverLoss_GFCCloss_data.tif", overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")
tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE)

#--------- DEFORESTATION HANSEN

HansenGFC= ee$Image("UMD/hansen/global_forest_change_2021_v1_9")
HansenGFCLoss = HansenGFC$select("loss")
HansenGFCLossMasked = HansenGFCLoss$updateMask(HansenGFCLoss$eq(1))
HansenGFCLossMasked = wtrMsk(HansenGFCLossMasked$updateMask(HansenGFCLossMasked$eq(1)))

# download layer
r = ee_as_raster(HansenGFCLossMasked,region=cropgoem,via="drive",container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE)
rt = rast(r)
plot(rt[[1]])
names(rt) = "global_forest_change_2021_v1_9_loss"
rt = ifel(rt==0,NA,rt)

# process rast
chelsaDir = glue("{getwd()}/Data/")
downloadChelsa_bioclim(timeperiod = "1981-2010", var = "kg2", outdir = chelsaDir)
cu = list.files(chelsaDir,pattern="*.tif",full.names=TRUE) %>% grep("kg2",.,value=TRUE) %>% grep("1981",.,value=TRUE) %>% terra::rast()
rt2 = resample(rt, cu, threads=TRUE, method="near")
plot(rt2[[1]])

# write rast do disk
writeRaster(rt2, "Data/EnvDataGEE/HansenGFCLossMasked_data.tif", overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")
tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE)

#--------- URBAN

# open layer
urban = ee$Image("projects/glad/GLCLU2020/Builtup_type")$select("b1")
urban_buildup = urban$updateMask(urban$eq(2))
urban_buildup = urban_buildup$gt(0)
urban_buildup = wtrMsk(urban_buildup$updateMask(urban_buildup$eq(1)))

# download layer
r = ee_as_raster(urban_buildup,region=cropgoem,via="drive",container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE)
rt = rast(r)
plot(rt[[1]])
names(rt) = "urban_buildup_2"

# process rast
chelsaDir = glue("{getwd()}/Data/")
downloadChelsa_bioclim(timeperiod = "1981-2010", var = "kg2", outdir = chelsaDir)
cu = list.files(chelsaDir,pattern="*.tif",full.names=TRUE) %>% grep("kg2",.,value=TRUE) %>% grep("1981",.,value=TRUE) %>% terra::rast()
rt2 = resample(rt, cu, threads=TRUE, method="near")
rt2 = ifel(rt2==0,NA,rt2)
plot(rt2[[1]])

# write rast do disk
writeRaster(rt2, "Data/EnvDataGEE/GLCLU2020_UrbanBuildup_data.tif", overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")
tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE)


#--------- Fire

chelsaDir = glue("{getwd()}/Data/")
downloadChelsa_bioclim(timeperiod = "1981-2010", var = "kg2", outdir = chelsaDir)
cu = list.files(chelsaDir,pattern="*.tif",full.names=TRUE) %>% grep("kg2",.,value=TRUE) %>% grep("1981",.,value=TRUE) %>% terra::rast()

years = 2001:2020
for(y in years){
  tmp_data = ee$ImageCollection("ESA/CCI/FireCCI/5_1") %>%
    ee$ImageCollection$map(function(x) x$select("BurnDate")) %>%
    ee$ImageCollection$filterDate(paste0(y,'-01-01'), paste0(y,'-12-31')) %>%
    ee$ImageCollection$max() %>% ee$ImageCollection$toBands() %>% ee$Image$gt(0)
  tmp_data = wtrMsk(tmp_data)
  #Map$addLayer(tmp_data, name = "tmp_data", list(min = 0, max = 3, palette= c('grey', 'green','orange','red')))
  
  if(!file.exists(paste0("Data/EnvDataGEE/FireCCI_",y,".tif"))){
    
    # different outputs can be returned depending on error message
    r = tryCatch({ ee_as_raster(tmp_data,region=cropgoem,via="drive",container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE) },
                    warning = function(w) {0},
                    error = function(e) {0})
    if(is.null(dim(r)[1])){
      r = ee_as_raster(tmp_data,region=cropgoem,via="drive",container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE) 
    }
    
    rt = rast(r)
    names(rt) = paste0("fire_",y)
    
    # process rast
    rt2 = resample(rt, cu, threads=TRUE, method="near")
    rt2 = ifel(rt2==0,NA,rt2) # CHECK!
    #plot(rt2)
    
    # write rast do disk
    writeRaster(rt2, paste0("Data/EnvDataGEE/FireCCI_",y,".tif"), overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")
  }
}

# combine layers
fire = list.files("Data/EnvDataGEE/",pattern="*.tif",full.names=TRUE) %>% grep("FireCCI",.,value=TRUE) %>% terra::rast()
writeRaster(fire, paste0("Data/EnvDataGEE/FireCCI_combined.tif"), overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")

tmpFiles(current=TRUE, orphan=FALSE, old=FALSE, remove=TRUE)
