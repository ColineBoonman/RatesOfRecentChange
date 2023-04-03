##########
## Code is intelectual property from Selwyn Hoeks - https://github.com/SHoeks



library(tidyverse)
library(raster)
library(rgee)
library(geosphere)
library(sp)
library(sf)

#-------- init
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
ee_Initialize(drive = TRUE)
WGS84Proj<-"+proj=longlat +datum=WGS84 +no_defs"
shp = sf::read_sf("Global.polygon.shp") # open polygon data
scale = 1000
index = 1:10
st_area(shp[index,])
fdata = format(Sys.Date(),"%Y_%m_%d")
tempo_dir = paste0("TempDir_",fdata)
dir.create(tempo_dir)

#--------- water mask function

WtrMsk = ee$ImageCollection("MODIS/006/MOD44W") %>%
  ee$ImageCollection$filterDate(paste0(2015,'-01-01'), paste0(2015,'-12-31')) %>%
  ee$ImageCollection$map(function(x) x$select("water_mask")) %>%
  ee$ImageCollection$mean() %>% ee$ImageCollection$toBands() 
Map$addLayer(WtrMsk, list(min = 0, max = 1, palette= c('BBBBBB', 'blue')), name = "water_mask")
wtrMsk = function(input_img) input_img$updateMask(WtrMsk$eq(0))

#--------- mblm slope function

calc_mblm_slope_pval = function(df, row_grep_id, plot=FALSE){
  df$mblm_slope = df$mblm_slope_pval = NA
  print(grep(row_grep_id,names(df),value=TRUE))
  for(i in 1:nrow(df)){
    y = as.numeric(df[i,grep(row_grep_id,names(df))])
    x = 1:length(y)
    #print(i)
    if(sum(y,na.rm = TRUE)>0 | sum(y,na.rm = TRUE)<0){
      lm=mblm::mblm(y~x, repeated = TRUE)
      df$mblm_slope[i] = as.numeric(coef(lm)[2])
      df$mblm_slope_pval[i] = summary(lm)$coefficients[2,4]
    }
    if(plot) {
      plot(x,y)
      abline(lm)
    }
  }
  return(df)
}

#--------- extract sp data function

extract_sp_data_sum = function(env_data,shp,n_batch,scale_set=1000){
  n_sp=nrow(shp)
  idx_start = seq(1,n_sp,by=n_batch)
  idx_stop = c(seq(n_batch,n_sp,by=n_batch),n_sp)
  extract_df = data.frame()
  n_loop = length(idx_start)
  for(i in 1:n_loop){
    print(paste(i,"/",n_loop))
    idx = idx_start[i]:idx_stop[i]
    tmp_df = ee_extract(x = env_data, y = shp[idx,], fun = ee$Reducer$sum(), scale = scale_set)
    extract_df = rbind(extract_df,tmp_df)
  }
  return(extract_df)
}

extract_sp_data_mean = function(env_data,shp,n_batch,scale_set=1000){
  n_sp=nrow(shp)
  idx_start = seq(1,n_sp,by=n_batch)
  idx_stop = c(seq(n_batch,n_sp,by=n_batch),n_sp)
  extract_df = data.frame()
  n_loop = length(idx_start)
  for(i in 1:n_loop){
    print(paste(i,"/",n_loop))
    idx = idx_start[i]:idx_stop[i]
    tmp_df = ee_extract(x = env_data, y = shp[idx,], fun = ee$Reducer$mean(), scale = scale_set)
    extract_df = rbind(extract_df,tmp_df)
  }
  return(extract_df)
}

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

map = Map$addLayer(Croplandpost2003, list(min = 0, max = 1, palette= c('red', 'green')), name = "Croplandpost2003")
map = map +Map$addLayer(Global_cropland_2003, list(min = 0, max = 1, palette= c('red', 'green')), name = "Global_cropland_2003")
map = map + Map$addLayer(CroplandChangeMasked, list(min = 0, max = 1, palette= c('BBBBBB', 'purple')), name = "gainbin")
test_sf = sf_as_ee(shp[3,])
test_sf$geometry()$area()$getInfo()
map = map + Map$addLayer(test_sf, list(color = "000000"), "test_sf")
map

# combine bands
cl = wtrMsk(CroplandChange$pixelArea())
cl = cl$addBands(wtrMsk(CroplandChangeMasked$pixelArea()$updateMask(CroplandChangeMasked$eq(1))))
cl = cl$addBands(wtrMsk(Global_cropland_2003$pixelArea()$updateMask(Global_cropland_2003$eq(1))))
cl = cl$addBands(wtrMsk(Global_cropland_2007$pixelArea()$updateMask(Global_cropland_2007$eq(1))))
cl = cl$addBands(wtrMsk(Global_cropland_2011$pixelArea()$updateMask(Global_cropland_2011$eq(1))))
cl = cl$addBands(wtrMsk(Global_cropland_2015$pixelArea()$updateMask(Global_cropland_2015$eq(1))))
cl = cl$addBands(wtrMsk(Global_cropland_2019$pixelArea()$updateMask(Global_cropland_2019$eq(1))))
cl = cl$rename(c("polygon_m2","cropland_change_m2_2003_2019","cropland_2003_m2","cropland_2007_m2",
                 "cropland_2011_m2","cropland_2015_m2","cropland_2019_m2"))

# extract sp data
t=Sys.time()
cl_change = extract_sp_data_sum(env_data=cl,shp=shp,n_batch=500,scale_set=1000)
tp = Sys.time() - t

save.image(file=paste0(tempo_dir,"/img01.RData"))

#--------- Forest cover calculations

GFCC_2000 = ee$Image("users/selwynhoeks/GFCC_2000") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands()
GFCC_2005 = ee$Image("users/selwynhoeks/GFCC_2005") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands() 
GFCC_2010 = ee$Image("users/selwynhoeks/GFCC_2010") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands() 
GFCC_2015 = ee$Image("users/selwynhoeks/GFCC_2015") %>% ee$ImageCollection$mean() %>% ee$ImageCollection$toBands()
GFCC_2000 = wtrMsk(GFCC_2000)
GFCC_2005 = wtrMsk(GFCC_2005)
GFCC_2010 = wtrMsk(GFCC_2010)
GFCC_2015 = wtrMsk(GFCC_2015)
Map$addLayer(GFCC_2000, list(min = 0, max = 100, palette= c('BBBBBB', '009900')), name = "gain")

# pixelArea = GFCC_2000$pixelArea()
# pixMax = ee_extract(x = pixelArea, y = shp[index,], fun = ee$Reducer$sum(), scale = scale)
# st_area(shp[index,])

FCC_threshold = 5
Bin_TC1 = GFCC_2000$subtract(GFCC_2005)$gte(FCC_threshold)
Bin_TC2 = GFCC_2005$subtract(GFCC_2010)$gte(FCC_threshold)
Bin_TC3 = GFCC_2010$subtract(GFCC_2015)$gte(FCC_threshold)
# Map$addLayer(Bin_TC1, name = "Bin_TC1", list(min = 0, max = 1, palette= c('red', 'green')))
# Map$addLayer(Bin_TC2, name = "Bin_TC2", list(min = 0, max = 1, palette= c('red', 'green')))
# Map$addLayer(Bin_TC1, name = "Bin_TC3", list(min = 0, max = 1, palette= c('red', 'green')))

Bin_TC = Bin_TC1$add(Bin_TC2)
Bin_TC = Bin_TC$add(Bin_TC3)
TCmax = Bin_TC$gt(0)
TCcell = Bin_TC$gt(-1)
map = Map$addLayer(Bin_TC, name = "Bin_TC", list(min = 0, max = 3, palette= c('grey', 'green','orange','red')))
map = map + Map$addLayer(TCmax, name = "TCmax", list(min = 0, max = 1, palette= c('grey', 'red')))
test_sf = sf_as_ee(shp[10,])
test_sf$geometry()$area()$getInfo()
map = map + Map$addLayer(test_sf, list(color = "000000"), "test_sf")
map

fc = wtrMsk(TCmax$pixelArea())
fc = fc$addBands(wtrMsk(TCmax$pixelArea()$updateMask(TCmax$eq(1))))
fc = fc$addBands(wtrMsk(Bin_TC1$pixelArea()$updateMask(Bin_TC1$eq(1))))
fc = fc$addBands(wtrMsk(Bin_TC2$pixelArea()$updateMask(Bin_TC2$eq(1))))
fc = fc$addBands(wtrMsk(Bin_TC3$pixelArea()$updateMask(Bin_TC3$eq(1))))
fc = fc$rename(c("polygon_m2","tc_max_change_2000_2015_m2","tc_2000_2005_m2","tc_2005_2010_m2","tc_2010_2015_m2"))

# extract sp data
#m2_cells_tc = ee_extract(x = fc, y = shp[index,],scale = scale, fun = ee$Reducer$sum())
#m2_cells_tc
t=Sys.time()
m2_cells_tc = extract_sp_data_sum(env_data=fc,shp=shp,n_batch=500,scale_set=1000)
tp = Sys.time() - t

# actual area fc
fc2 = GFCC_2000$divide(100)$multiply(GFCC_2000$pixelArea())
fc2 = fc2$addBands(GFCC_2005$divide(100)$multiply(GFCC_2005$pixelArea()))
fc2 = fc2$addBands(GFCC_2010$divide(100)$multiply(GFCC_2010$pixelArea()))
fc2 = fc2$addBands(GFCC_2015$divide(100)$multiply(GFCC_2015$pixelArea()))
fc2 = fc2$rename(paste0("tc_m2_",seq(2000,2015,5)))
                 
#tc_fraction = ee_extract(x = fc2, y = shp[index,],scale = scale, fun = ee$Reducer$sum())
t=Sys.time()
tc_fraction = extract_sp_data_sum(env_data=fc2,shp=shp,n_batch=500,scale_set=1000)
tc_fraction$polygon_m2 = m2_cells_tc$polygon_m2
tp = Sys.time() - t

save.image(file=paste0(tempo_dir,"/img02.RData"))

#--------- DEFORESTATION HANSEN

HansenGFC= ee$Image("UMD/hansen/global_forest_change_2021_v1_9")
HansenGFCLoss = HansenGFC$select("loss")
HansenGFCLossMasked = HansenGFCLoss$updateMask(HansenGFCLoss$eq(1))
HansenGFC2000 = HansenGFC$select("treecover2000")
HansenGFC2000Bin50 = HansenGFC2000$gt(50)
HansenGFC2020Bin50 = HansenGFC2000Bin50$subtract(HansenGFCLoss)
HansenGFC2000Bin50 = HansenGFC2000Bin50$updateMask(HansenGFC2000Bin50$eq(1))
HansenGFC2020Bin50 = HansenGFC2020Bin50$updateMask(HansenGFC2020Bin50$eq(1))
map = Map$addLayer(HansenGFC2000Bin50, name = "HansenGFC2000Bin50", list(min = 1, max = 1, palette= c('green')))
map = map + Map$addLayer(HansenGFCLossMasked, name = "HansenGFCLossMasked", list(min = 1, max = 1, palette= c('red')))
map = map + Map$addLayer(HansenGFC2020Bin50, name = "HansenGFC2020Bin50", list(min = 0, max = 1, palette= c('red','purple')))
#map = map + Map$addLayer(HansenGFC2000, name = "HansenGFC2000", list(min = 0, max = 100, palette= c('001000','009900')))
map

HansenGFCLoss = wtrMsk(HansenGFCLoss)
HansenGFCLossMasked = wtrMsk(HansenGFCLossMasked)
HansenGFC2000Bin50 = wtrMsk(HansenGFC2000Bin50)
HansenGFC2020Bin50 = wtrMsk(HansenGFC2020Bin50)

hGFC = wtrMsk(HansenGFCLoss$pixelArea())
hGFC = hGFC$addBands(wtrMsk(HansenGFCLossMasked$pixelArea()$updateMask(HansenGFCLossMasked$eq(1))))
hGFC = hGFC$addBands(wtrMsk(HansenGFC2000Bin50$pixelArea()$updateMask(HansenGFC2000Bin50$eq(1))))
hGFC = hGFC$addBands(wtrMsk(HansenGFC2020Bin50$pixelArea()$updateMask(HansenGFC2020Bin50$eq(1))))
hGFC = hGFC$rename(c("polygon_m2","HansenGFCLoss_m2","Hansen_forest_2000Bin50_m2","Hansen_forest_2020Bin50_m2"))

#m2_hGFC = ee_extract(x = hGFC, y = shp[index,], scale = scale, fun = ee$Reducer$sum())
t=Sys.time()
m2_hGFC = extract_sp_data_sum(env_data=hGFC,shp=shp,n_batch=500,scale_set=1000)
tp = Sys.time() - t

save.image(file=paste0(tempo_dir,"/img03.RData"))

#--------- URBAN

# check 1 classification!
urban = ee$Image("projects/glad/GLCLU2020/Builtup_type")$select("b1")
urban_buildup = urban$updateMask(urban$eq(2))
urban_buildup = urban_buildup$gt(0)
urban2020 = urban$gt(0); urban2020 = urban2020$updateMask(urban2020$eq(1))
urban2000 = urban$updateMask(urban$eq(1))
urban = wtrMsk(urban)
urban_buildup = wtrMsk(urban_buildup)
urban2020 = wtrMsk(urban2020)
urban2000 = wtrMsk(urban2000)

m=Map$addLayer(urban, name = "urban", list(min = 0, max = 2, palette= c('green','purple','grey')))
m=m+Map$addLayer(urban2000, name = "urban2000", list(min = 0, max = 1, palette= c("white",'yellow')))
m=m+Map$addLayer(urban2020, name = "urban2020", list(min = 1, max = 1, palette= c('orange')))
m=m+Map$addLayer(urban_buildup, name = "urban_buildup", list(min = 1, max = 1, palette= c('red')))
m               

urb_build = wtrMsk(urban$pixelArea())
urb_build = urb_build$addBands(wtrMsk(urban2000$pixelArea()$updateMask(urban2000$eq(1))))
urb_build = urb_build$addBands(wtrMsk(urban2020$pixelArea()$updateMask(urban2020$eq(1))))
urb_build = urb_build$addBands(wtrMsk(urban_buildup$pixelArea()$updateMask(urban_buildup$eq(1))))
urb_build = urb_build$rename(c("polygon_m2","urban2000_m2","urban2020_m2","urban_buildup_m2"))

#m2_urb_build = ee_extract(x = urb_build, y = shp[index,], scale = scale, fun = ee$Reducer$sum())
t=Sys.time()
m2_urb_build = extract_sp_data_sum(env_data=urb_build,shp=shp,n_batch=500,scale_set=1000)
tp = Sys.time() - t

save.image(file=paste0(tempo_dir,"/img04.RData"))

#--------- Fire

years = 2001:2020
for(y in years){
  tmp_data = ee$ImageCollection("ESA/CCI/FireCCI/5_1") %>%
    ee$ImageCollection$map(function(x) x$select("BurnDate")) %>%
    ee$ImageCollection$filterDate(paste0(y,'-01-01'), paste0(y,'-12-31')) %>%
    ee$ImageCollection$max() %>% ee$ImageCollection$toBands() %>% ee$Image$gt(0)
  tmp_data = wtrMsk(tmp_data)
  assign(paste0("fire_",y), tmp_data)
}

Map$addLayer(fire_2001, name = "fire_2001", list(min = 0, max = 1, palette= c('grey','red')))
Map$addLayer(fire_2020, name = "fire_2020", list(min = 0, max = 1, palette= c('grey','red')))
total_area = fire_2001$unmask(0);
total_area = wtrMsk(total_area)
Map$addLayer(total_area, name = "total_area", list(min = 0, max = 1, palette= c('grey','red')))

fire_comp = wtrMsk(total_area$pixelArea())
for(i in 1:length(years)) {
  tmp_img = get(paste0("fire_",years[i]))
  tmp_img = wtrMsk(tmp_img$pixelArea()$updateMask(tmp_img$eq(1)))
  fire_comp = fire_comp$addBands(tmp_img)
}
fire_comp = fire_comp$rename(c("polygon_m2",paste0("fire_m2_",years)))
fire_comp$bandNames()$getInfo()

#m2_fire_test = ee_extract(x = fire_comp, y = shp[1:10,], scale = scale, fun = ee$Reducer$sum())
t=Sys.time()
m2_fire = extract_sp_data_sum(env_data=fire_comp,shp=shp,n_batch=500,scale_set=1000)
tp = Sys.time() - t

m2_fire = calc_mblm_slope_pval(m2_fire,"fire_")

save.image(file=paste0(tempo_dir,"/img05.RData"))

#--------- Climate - mean temperature

tas_comp = wtrMsk(ee$Image("users/selwynhoeks/tas_2000")$divide(10)$subtract(273.15)) # open and convert data
Map$addLayer(tas_comp, name = "tmp_data", list(min = 0, max = 40, palette= c('green','red')))
for(i in 2001:2019) {
  tmp = ee$Image(paste0("users/selwynhoeks/tas_",i))$divide(10)$subtract(273.15) # open and convert data
  tmp = wtrMsk(tmp)
  tas_comp = tas_comp$addBands(tmp)
}
tas_comp = tas_comp$rename(paste0("tas_",2000:2019))
tas_comp$bandNames()$getInfo()
m=Map$addLayer(tas_comp$select("tas_2000"), name = "tas_2000", list(min = 0, max = 40, palette= c('green','red')))
m=m+Map$addLayer(tas_comp$select("tas_2019"), name = "tas_2019", list(min = 0, max = 40, palette= c('green','red')))
m

#tas_mean = ee_extract(x = tas_comp, y = shp[index,], scale = scale, fun = ee$Reducer$mean())
t=Sys.time()
tas_mean = extract_sp_data_mean(env_data=tas_comp,shp=shp,n_batch=100,scale_set=1000)
tp = Sys.time() - t

tas_mean = calc_mblm_slope_pval(tas_mean,"tas_")
#tas_mean[which(is.na(tas_mean$mblm_slope)),]

save.image(file=paste0(tempo_dir,"/img06.RData"))

#--------- Climate - max temperature

tasmax_comp = wtrMsk(ee$Image("users/selwynhoeks/tasmax_2000")$divide(10)$subtract(273.15)) # open and convert data
m=Map$addLayer(tasmax_comp, name = "tmp_data", list(min = 0, max = 40, palette= c('green','red')))
for(i in 2001:2019) {
  tmp = ee$Image(paste0("users/selwynhoeks/tasmax_",i))$divide(10)$subtract(273.15) # open and convert data
  tmp = wtrMsk(tmp)
  tasmax_comp = tasmax_comp$addBands(tmp)
}
tasmax_comp = tasmax_comp$rename(paste0("tasmax_",2000:2019))
tasmax_comp$bandNames()$getInfo()
m=Map$addLayer(tasmax_comp$select("tasmax_2000"), name = "tasmax_2000", list(min = 0, max = 40, palette= c('green','red')))
m=m+Map$addLayer(tasmax_comp$select("tasmax_2019"), name = "tasmax_2019", list(min = 0, max = 40, palette= c('green','red')))
m

#tasmax_comp_mean = ee_extract(x = tasmax_comp, y = shp[index,], scale = scale, fun = ee$Reducer$mean())
t=Sys.time()
tasmax_comp_mean = extract_sp_data_mean(env_data=tasmax_comp,shp=shp,n_batch=200,scale_set=1000)
tp = Sys.time() - t

tasmax_comp_mean = calc_mblm_slope_pval(tasmax_comp_mean,"tasmax_")

save.image(file=paste0(tempo_dir,"/img07.RData"))

#--------- Climate - min temperature

tasmin_comp = wtrMsk(ee$Image("users/selwynhoeks/tasmin_2000")$divide(10)$subtract(273.15)) # open and convert data
Map$addLayer(tasmin_comp, name = "tmp_data", list(min = 0, max = 40, palette= c('green','red')))
for(i in 2001:2019) {
  tmp = ee$Image(paste0("users/selwynhoeks/tasmin_",i))$divide(10)$subtract(273.15) # open and convert data
  tmp = wtrMsk(tmp)
  tasmin_comp = tasmin_comp$addBands(tmp)
}
tasmin_comp = tasmin_comp$rename(paste0("tasmin_",2000:2019))
tasmin_comp$bandNames()$getInfo()
m=Map$addLayer(tasmin_comp$select("tasmin_2000"), name = "tasmin_2000", list(min = 0, max = 40, palette= c('green','red')))
m=m+Map$addLayer(tasmin_comp$select("tasmin_2019"), name = "tasmin_2019", list(min = 0, max = 40, palette= c('green','red')))
m

#tasmin_comp_mean = ee_extract(x = tasmin_comp, y = shp[index,], scale = scale, fun = ee$Reducer$mean())
t=Sys.time()
tasmin_comp_mean = extract_sp_data_mean(env_data=tasmin_comp,shp=shp,n_batch=250,scale_set=1000)
tp = Sys.time() - t

tasmin_comp_mean = calc_mblm_slope_pval(tasmin_comp_mean,"tasmin_")
tasmin_comp_mean[which(is.na(tasmin_comp_mean$mblm_slope)),]

save.image(file=paste0(tempo_dir,"/img08.RData"))

#--------- Climate - mean precipitation

pr_mean = wtrMsk(ee$Image("users/selwynhoeks/pr_mean_yearly_2000"))$divide(100)
Map$addLayer(pr_mean, name = "pr_mean", list(min = 0, max = 200, palette= c('green','red')))
for(i in 2001:2018) {
  tmp = ee$Image(paste0("users/selwynhoeks/pr_mean_yearly_",i))$divide(100)
  tmp = wtrMsk(tmp)
  pr_mean = pr_mean$addBands(tmp)
}
pr_mean = pr_mean$rename(paste0("pr_mean_",2000:2018))
pr_mean$bandNames()$getInfo()
m=Map$addLayer(pr_mean$select("pr_mean_2005"), name = "pr_mean_2005", list(min = 0, max = 200, palette= c('green','red')))
m=m+Map$addLayer(pr_mean$select("pr_mean_2015"), name = "pr_mean_2015", list(min = 0, max = 200, palette= c('green','red')))
m

#pr_mean_extract = ee_extract(x = pr_mean, y = shp[index,], scale = scale, fun = ee$Reducer$mean())
t=Sys.time()
pr_mean_extract = extract_sp_data_mean(env_data=pr_mean,shp=shp,n_batch=250,scale_set=1000)
tp = Sys.time() - t

pr_mean_extract = calc_mblm_slope_pval(pr_mean_extract,"pr_mean_")
pr_mean_extract[which(is.na(pr_mean_extract$mblm_slope)),]

save.image(file=paste0(tempo_dir,"/img09.RData"))

#--------- Climate - sd precipitation

pr_sd = wtrMsk(ee$Image("users/selwynhoeks/pr_sd_yearly_2000"))$divide(100)
Map$addLayer(pr_sd, name = "pr_sd", list(min = 0, max = 20, palette= c('green','red')))
for(i in 2001:2018) {
  tmp = ee$Image(paste0("users/selwynhoeks/pr_sd_yearly_",i))$divide(100)
  tmp = wtrMsk(tmp)
  pr_sd = pr_sd$addBands(tmp)
}
pr_sd = pr_sd$rename(paste0("pr_sd_",2000:2018))
pr_sd$bandNames()$getInfo()
m=Map$addLayer(pr_sd$select("pr_sd_2005"), name = "pr_sd_2005", list(min = 0, max = 20, palette= c('green','red')))
m=m+Map$addLayer(pr_sd$select("pr_sd_2015"), name = "pr_sd_2015", list(min = 0, max = 20, palette= c('green','red')))
m

#pr_sd_extract = ee_extract(x = pr_sd, y = shp[index,], scale = scale, fun = ee$Reducer$mean())
t=Sys.time()
pr_sd_extract = extract_sp_data_mean(env_data=pr_sd,shp=shp,n_batch=250,scale_set=1000)
tp = Sys.time() - t

pr_sd_extract = calc_mblm_slope_pval(pr_sd_extract,"pr_sd_")

save.image(file=paste0(tempo_dir,"/img10.RData"))

#--------- Climate - mean vpd

vpd_mean = wtrMsk(ee$Image("users/selwynhoeks/vpd_mean_yearly_2000_v2"))
Map$addLayer(vpd_mean, name = "vpd_mean", list(min = 0, max = 2500, palette= c('green','red')))
for(i in 2001:2018) {
  tmp = ee$Image(paste0("users/selwynhoeks/vpd_mean_yearly_",i,"_v2"))
  tmp = wtrMsk(tmp)
  vpd_mean = vpd_mean$addBands(tmp)
}
vpd_mean = vpd_mean$rename(paste0("vpd_mean_",2000:2018))
vpd_mean$bandNames()$getInfo()
m=Map$addLayer(vpd_mean$select("vpd_mean_2005"), name = "vpd_mean_2005", list(min = 0, max = 2500, palette= c('green','red')))
m=m+Map$addLayer(vpd_mean$select("vpd_mean_2015"), name = "vpd_mean_2015", list(min = 0, max = 2500, palette= c('green','red')))
m

#vpd_mean_extract = ee_extract(x = vpd_mean, y = shp[index,], scale = scale, fun = ee$Reducer$mean())
t=Sys.time()
vpd_mean_extract = extract_sp_data_mean(env_data=vpd_mean,shp=shp,n_batch=250,scale_set=1000)
tp = Sys.time() - t

vpd_mean_extract = calc_mblm_slope_pval(vpd_mean_extract,"vpd_mean_")

save.image(file=paste0(tempo_dir,"/img11.RData"))

#--------- Climate - sd vpd

vpd_sd = wtrMsk(ee$Image("users/selwynhoeks/vpd_sd_yearly_2000"))
Map$addLayer(vpd_sd, name = "vpd_sd", list(min = 0, max = 500, palette= c('green','red')))
for(i in 2001:2018) {
  tmp = ee$Image(paste0("users/selwynhoeks/vpd_sd_yearly_",i))
  tmp = wtrMsk(tmp)
  vpd_sd = vpd_sd$addBands(tmp)
}
vpd_sd = vpd_sd$rename(paste0("vpd_sd_",2000:2018))
vpd_sd$bandNames()$getInfo()
m=Map$addLayer(vpd_sd$select("vpd_sd_2005"), name = "vpd_sd_2005", list(min = 0, max = 500, palette= c('green','red')))
m=m+Map$addLayer(vpd_sd$select("vpd_sd_2015"), name = "vpd_sd_2015", list(min = 0, max = 500, palette= c('green','red')))
m

#vpd_sd_extract = ee_extract(x = vpd_sd, y = shp[index,], scale = scale, fun = ee$Reducer$mean())
t=Sys.time()
vpd_sd_extract = extract_sp_data_mean(env_data=vpd_sd,shp=shp,n_batch=250,scale_set=1000)
tp = Sys.time() - t

vpd_sd_extract = calc_mblm_slope_pval(vpd_sd_extract,"vpd_sd_")

save.image(file=paste0(tempo_dir,"/img12.RData"))

#--------- Save final output as RData

out = list(vpd_sd = vpd_sd_extract,
           vpd_mean = vpd_mean_extract,
           prec_sd = pr_sd_extract,
           prec_mean = pr_mean_extract,
           tmin = tasmin_comp_mean,
           tmax = tasmax_comp_mean,
           tmean = tas_mean,
           fire = m2_fire,
           urban = m2_urb_build,
           hGFC = m2_hGFC,
           tree_fraction = tc_fraction,
           tc_change_area = m2_cells_tc,
           cropland = cl_change)

str(out,2)

save(out, file = paste0("TreeChangeEnvData_",fdata,".RData"))

save.image(file=paste0(tempo_dir,"/img13.RData"))



