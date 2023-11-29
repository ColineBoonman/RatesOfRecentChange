######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

library(terra)
library(fst)
library(glue)
library(sf)
library(stringr)

#-------- init
setwd("...")
Chelsa_outdir = glue("{getwd()}/Data/EnvDataChelsa/")
WGS84Proj<-"+proj=longlat +datum=WGS84 +no_defs"
fdata = format(Sys.Date(),"%Y_%m_%d")
tempo_dir = paste0("TempDir_",fdata,"_",sample(1000:9999,1))
dir.create(tempo_dir)
terraOptions(tempo_dir=tempo_dir, memfrac=0.8, todisk=FALSE, memmax=90)
source("Functions/downloadChelsa.R")

#-------- open water mask
wtr = rast("Data/WaterMask_fullCoverage.tif")

# Climate - max temperature 2000:2019 $divide(10)$subtract(273.15)
# Climate - min temperature 2000:2019 $divide(10)$subtract(273.15)
# Climate - mean precipitation 2000:2018 $divide(100)
# Climate - sd precipitation 2000:2018 $divide(100)
# Climate - mean vpd 2000:2018
# Climate - sd vpd 2000:2018

#-------- tasmax data 2000:2019
var = "tasmax"
for(i in 2000:2018){
  print(i)
  downloadChelsa_monthlyYearSpecific(year = i, var = var, outdir = Chelsa_outdir, ignore.stdout = TRUE, ignore.stderr = TRUE)
  files = list.files(Chelsa_outdir,pattern="*.tif",full.names=TRUE) %>% grep(var,.,value=TRUE) %>% grep(i,.,value=TRUE) %>% grep("monthly",.,value=TRUE)
  print("tasmax max...")
  rast_max = files %>% terra::rast() %>% max()
  crs(rast_max) = crs(wtr)
  rast_max = terra::crop(rast_max,wtr)
  rast_max = ((rast_max*wtr)/(10))-273.15
  terra::writeRaster(rast_max, paste0(Chelsa_outdir,"/yearly",var,"_",i,"_max",".tif"), overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES"))
  system(paste0("rm ",paste(files,collapse = " ")))
  gc()
}

#-------- tasmax data 2000:2019
var = "tasmin"
for(i in 2000:2018){
  print(i)
  downloadChelsa_monthlyYearSpecific(year = i, var = var, outdir = Chelsa_outdir, ignore.stdout = TRUE, ignore.stderr = TRUE)
  files = list.files(Chelsa_outdir,pattern="*.tif",full.names=TRUE) %>% grep(var,.,value=TRUE) %>% grep(i,.,value=TRUE) %>% grep("monthly",.,value=TRUE)
  print("tasmin min...")
  rast_min = files %>% terra::rast() %>% min()
  crs(rast_min) = crs(wtr)
  rast_min = terra::crop(rast_min,wtr)
  rast_min = ((rast_min*wtr)/(10))-273.15
  terra::writeRaster(rast_min, paste0(Chelsa_outdir,"/yearly",var,"_",i,"_min",".tif"), overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES"))
  system(paste0("rm ",paste(files,collapse = " ")))
  gc()
}

#-------- precipitation data 2000:2018
var = "pr"
for(i in 2000:2018){
  print(i)
  downloadChelsa_monthlyYearSpecific(year = i, var = var, outdir = Chelsa_outdir, ignore.stdout = TRUE, ignore.stderr = TRUE)
  files = list.files(Chelsa_outdir,pattern="*.tif",full.names=TRUE) %>% grep(var,.,value=TRUE) %>% grep(i,.,value=TRUE) %>% grep("monthly",.,value=TRUE)
  print("pr mean...")
  rast_mean = files %>% terra::rast() %>% terra::mean()
  crs(rast_mean) = crs(wtr)
  rast_mean = terra::crop(rast_mean,wtr)
  rast_mean = (rast_mean*wtr)/(100)
  terra::writeRaster(rast_mean, paste0(Chelsa_outdir,"/yearly",var,"_",i,"_mean",".tif"), overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES"))
  print("pr sd...")
  rast_sd = files %>% terra::rast() %>% terra::stdev(pop=FALSE)
  crs(rast_sd) = crs(wtr)
  rast_sd = terra::crop(rast_sd,wtr)
  rast_sd = (rast_sd*wtr)/(100)
  terra::writeRaster(rast_sd, paste0(Chelsa_outdir,"/yearly",var,"_",i,"_sd",".tif"), overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES"))
  system(paste0("rm ",paste(files,collapse = " ")))
  gc()
}

#-------- vpd data 2000:2018
var = "vpd"
for(i in 2000:2018){
  print(i)
  downloadChelsa_monthlyYearSpecific(year = i, var = var, outdir = Chelsa_outdir, ignore.stdout = TRUE, ignore.stderr = TRUE)
  files = list.files(Chelsa_outdir,pattern="*.tif",full.names=TRUE) %>% grep(var,.,value=TRUE) %>% grep(i,.,value=TRUE) %>% grep("monthly",.,value=TRUE)
  print("vpd mean...")
  rast_mean = files %>% terra::rast() %>% terra::mean()
  crs(rast_mean) = crs(wtr)
  rast_mean = terra::crop(rast_mean,wtr)
  rast_mean = (rast_mean*wtr)
  terra::writeRaster(rast_mean, paste0(Chelsa_outdir,"/yearly",var,"_",i,"_mean",".tif"), overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES"))
  print("vpd sd...")
  rast_sd = files %>% terra::rast() %>% terra::stdev(pop=FALSE)
  crs(rast_sd) = crs(wtr)
  rast_sd = terra::crop(rast_sd,wtr)
  rast_sd = (rast_sd*wtr)
  terra::writeRaster(rast_sd, paste0(Chelsa_outdir,"/yearly",var,"_",i,"_sd",".tif"), overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES"))
  system(paste0("rm ",paste(files,collapse = " ")))
  gc()
}


