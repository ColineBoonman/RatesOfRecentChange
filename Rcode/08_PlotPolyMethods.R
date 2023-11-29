######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

# init packages
rm(list=ls())
library("terra")
library("sf")
library("data.table")
library("fst")
library("glue")
library("tictoc")
library("stringr")
library("exactextractr")
options(datatable.print.class = TRUE)
gc()

# set species
# These are three species that have NA for all rates: Coffea_kihansiensis, Frangula_inconspicua, Ravenea_moorei
# These are species that a reviewer wanted to see in detail:  Vachellia_nilotica en Erythrina_velutina
# Quercus_frainetto is used for paper main text
species = "Coffea_kihansiensis"

# set dirs
setwd("...")
chelsaDir = "Data/EnvDataChelsa/"
GEEDir = "Data/EnvDataGEE/"
PlotDir = glue("Results/PlotDir_{species}_{format(Sys.Date(),'%Y_%m_%d')}")
dir.create(PlotDir,recursive=TRUE,showWarnings=FALSE)

# functions
source("Functions/extractGEE.R")
source("Functions/climzone.R")

# plot functions
addTrans = function(color,trans) {
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color = rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans = rep(trans,length(color))
  num2hex = function(x) {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb = rbind(col2rgb(color),trans)
  res = paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

# debug plot
plotd = function(x){
  png(glue("{format(Sys.time(),'%Y_%m_%d_%H%M')}.png"))
  plot(x)
  dev.off()
}

# setup terra options and create tmp dir 
tmpDir = glue("tmp_terra_{sample(1000:9999,1)}")
dir.create(tmpDir,showWarnings = FALSE,recursive = TRUE)
terraOptions(tempo_dir=tmpDir, memfrac=0.8, todisk=TRUE, memmax=4)

# open water mask and kg2
land = rast("Data/WaterMask_fullCoverage.tif")
kg2 = rast("Data/bioclim_kg2_1981-2010.tif")

# open env data layers
geeFiles = list.files(GEEDir, pattern="*.tif", full.names=TRUE)
geeFiles = grep(paste0("_",2001:2020,collapse="|"),geeFiles,invert=TRUE,value=TRUE)
geeRast = rast(geeFiles)
names(geeRast)[1] = "CropLandChange"
climFiles = list.files(chelsaDir, pattern="*.tif", full.names=TRUE)
var_select = "yearlyvpd"
vars_stat = "mean"
climFiles = grep(var_select,climFiles,value=TRUE)
climFiles = grep(vars_stat,climFiles,value=TRUE)
print(climFiles)
climRast = rast(climFiles)
names(climRast) = str_split(str_split(climFiles,".tif",simplify=TRUE)[,1],"yearly",simplify=TRUE)[,2]
print(names(climRast))

# load and select species data
file_poly = glue("{PlotDir}/{species}_poly.shp")
if(file.exists(file_poly)){
  spPoly = st_read(file_poly)
}else{
  spPoly = st_read("Data/SpeciesPolygons/Global.polygon.shp")
  spPoly = spPoly[which(spPoly$id==species),]
  st_write(spPoly,file_poly)
}
file_points = glue("{PlotDir}/{species}_points.shp")
if(file.exists(file_points)){
  spPoints = st_read(file_points)
}else{
  spPoints = st_read("Data/SpeciesPoints/Global.datapoints.shp")
  spPoints = spPoints[which(spPoints$species==species),]
  st_write(spPoints,file_points)
}

# crop rast layers to plot
extent_plot = as.numeric(as.vector(ext(spPoly)))
extent_plot = extentWithPaddingCustom_floor(spPoly,0.05) #ext(c(5,35,35,48)) #ext(c(-12,40,32,51))
landc = crop(land,extent_plot)
landc = ifel(is.na(landc),0,1)
kg2c = crop(kg2,extent_plot)

# extract kg2 occ
bufcov = 0.25
occthreshold = 0.06
spPointsbuf = buffer(vect(spPoints), width = 1000) %>% st_as_sf()
ext_vals = exact_extract(kg2c,spPointsbuf,progress=TRUE,include_area=FALSE)
ext_vals = lapply(ext_vals, function(x) x = x[x$coverage_fraction>bufcov,])
ext_vals = rbindlist(ext_vals,idcol=TRUE); gc()
names(ext_vals) = c("point_index","kg2_value","coverage_fraction")
occKg2s = names(which((table(ext_vals$kg2_value)/nrow(ext_vals))>occthreshold))
occKg2s = as.numeric(occKg2s)
print(occKg2s)
allSpOcc = read_fst("Data/SpeciesOccExpBinaryClimRegions_bufcov25p_occthreshold5p.fst")
allSpOcc = allSpOcc[allSpOcc$species==species,]
allSpOccSpecies = allSpOcc[,which(allSpOcc==1,arr.ind=TRUE)[,2]]
print(allSpOccSpecies)

# convert species polygon to raster
spRast = rasterize(spPoly,landc)
spRast = spRast*landc
spRast = ifel(spRast==0,NA,spRast)

# process kg2c further
kg2c = ifel(kg2c %in% occKg2s,kg2c,NA)
kg2c = kg2c*landc
kg2c = ifel(kg2c==0,NA,kg2c)
kg2c_bin = ifel(is.na(kg2c),0,1)
kg2c_bin = kg2c_bin*spRast
kg2c = kg2c*spRast

# process cropland
file_cl2019 = "Data/EnvDataGEE/GlobalCropland_2019.tif"
if(file.exists(file_cl2019)){
  cl2019 = rast(file_cl2019)
}else{
  cropgoem = crop_geometry(-180,180,-70,85) 
  cl2019 = ee$ImageCollection("users/potapovpeter/Global_cropland_2019") %>% ee$ImageCollection$mean()
  cl2019 = ee_as_raster(cl2019,region=cropgoem,via="drive", container="rgee_backup",scale=1000,maxPixels=1e+09,skipEmptyTiles=TRUE)
  cl2019 = merge_downloaded_rasters(cl2019)
  cl2019_2 = resample(cl2019,climRast[[1]],method="near")
  writeRaster(cl2019_2, file_cl2019, overwrite=TRUE, gdal=c("COMPRESS=ZIP", "TFW=YES"), datatype="INT1U")
  cl2019 = cl2019_2
}
if(ext(cl2019)!=ext(spRast)) cl2019 = resample(cl2019,spRast,method="near")
cl2019 = crop(cl2019,extent_plot)
cl2019 = cl2019*spRast
cl2019_cr = cl2019*kg2c_bin
cl2019_cr = ifel(cl2019_cr==0,NA,1)
cl = crop(geeRast$CropLandChange,extent_plot)
cl = cl*spRast
cl = cl*kg2c_bin
cl = ifel(cl==0,NA,1)

# process climRast
crc_name = glue("{PlotDir}/{species}_climContRastCropped_{var_select}_{var_select}.tif")
if(file.exists(var_select)){
  crc = rast(crc_name)
}else{
  crc = crop(climRast,extent_plot)
  writeRaster(crc,crc_name)
}
crc2 = crc*kg2c_bin
crc2 = ifel(crc2==0,NA,crc2)

# eoo polygons
kg2c_bin_poly=as.polygons(kg2c_bin)
kg2c_bin_poly = st_as_sf(kg2c_bin_poly)
kg2c_bin_poly = kg2c_bin_poly[kg2c_bin_poly$`bioclim_kg2_1981-2010`==1,]

# extract vpd values
cell_size = cellSize(crc2,unit="km")
vpd_vals = exact_extract(c(crc2,cell_size),kg2c_bin_poly)[[1]]
vpd_vals$area_covered = vpd_vals$area*vpd_vals$coverage_fraction
head(vpd_vals)
vpd_eoo = sum(vpd_vals$area_covered)
vpd_yearly_mean = apply(vpd_vals,2,weighted.mean,w=vpd_vals$area_covered,na.rm=TRUE)
vpd_yearly_mean = vpd_yearly_mean[grep("vpd",names(vpd_yearly_mean))]
vpd_years_x = str_split(names(vpd_yearly_mean),"_",simplify=TRUE)[,2] %>% as.numeric()

# extract cl (cropland)
cl_ext = ifel(is.na(cl),0,1)
cl_vals = exact_extract(c(cl_ext,cell_size),kg2c_bin_poly)[[1]]
cl_vals$area_covered = cl_vals$area*cl_vals$coverage_fraction
head(cl_vals)
cl_eoo = sum(cl_vals$area_covered)
cl_change = sum(cl_vals$area_covered[cl_vals$CropLandChange==1])
cl_change_per_year_prec = ((cl_change/cl_eoo)*100)/(length(2003:2019)-1)

# plot 1
pdf(glue("{PlotDir}/{species}_plot_1.pdf"),height=4,width=6)
plot(landc,background="white",colNA="red",axes=FALSE,box=FALSE,
     col=c("white","grey70"),type="classes",levels=c("Water","Land"),
     plg = list(x="topleft", bg="white", cex=1.2),
     pax=list(las=1), mar = c(3.1, 3.1, 2.1, 2.1), main=" ",legend=FALSE)
plot(spRast, col="#005b96", legend=FALSE,add=TRUE,axes=FALSE)
plot(kg2c, col=c("#ffbaba","#ff7b7b","#ff5252","#ff0000","#a70000"), legend=FALSE,add=TRUE,axes=FALSE,
     type="classes",levels=c(9, 10, 12, 13, 19))
plot(spPoly,col=addTrans("#7570b3",0), lwd=1.5,add=TRUE,axes=FALSE)
plot(spPoints[,1],col="black", pch=19, cex=0.8, add=TRUE,axes=FALSE)
plot(spPoints[,1],col="white", pch=19, cex=0.3, add=TRUE,axes=FALSE)
dev.off()

# plot 2
pdf(glue("{PlotDir}/{species}_plot_2.pdf"),height=4,width=6)
plot(landc,background="white",colNA="red",axes=FALSE,box=FALSE,
     col=c("white","white"),type="classes",levels=c("Water","Land"),
     plg = list(x="topleft", bg="white", cex=1.2),
     pax=list(las=1), mar = c(3.1, 3.1, 2.1, 2.1), main=" ",legend=FALSE)
plot(kg2c_bin_poly,axes=FALSE,col="grey90",main="",add=TRUE,lwd=0.5)
dev.off()

# plot 3a
pdf(glue("{PlotDir}/{species}_plot_3a.pdf"),height=4,width=6)
plot(landc,background="white",colNA="red",axes=FALSE,box=FALSE,
     col=c("white","white"),type="classes",levels=c("Water","Land"),
     plg = list(x="topleft", bg="white", cex=1.2),
     pax=list(las=1), mar = c(3.1, 3.1, 2.1, 2.1), main=" ",legend=FALSE)
plot(kg2c_bin_poly,axes=FALSE,col="grey90",main="",add=TRUE,lwd=0.5)
plot(cl2019_cr, col="#005b96", legend=FALSE,add=TRUE,axes=FALSE)
dev.off()

# plot 3b
pdf(glue("{PlotDir}/{species}_plot_3b.pdf"),height=4,width=6)
plot(landc,background="white",colNA="red",axes=FALSE,box=FALSE,
     col=c("white","white"),type="classes",levels=c("Water","Land"),
     plg = list(x="topleft", bg="white", cex=1.2),
     pax=list(las=1), mar = c(3.1, 3.1, 2.1, 2.1), main=" ",legend=FALSE)
plot(kg2c_bin_poly,axes=FALSE,col="grey90",main="",add=TRUE,lwd=0.5)
plot(cl2019_cr, col="#005b96", legend=FALSE,add=TRUE,axes=FALSE)
cl2 = rast(ncol= ncol(cl)/3,nrow= nrow(cl)/3,ext=ext(cl))
cl2 = resample(cl,cl2,"max")
plot(cl2, col="#ff7b7b", legend=FALSE,add=TRUE,axes=FALSE)
dev.off()

# plot 4a
pdf(glue("{PlotDir}/{species}_plot_4a.pdf"),height=4,width=6)
plot(landc,background="white",colNA="red",axes=FALSE,box=FALSE,
     col=c("white","white"),type="classes",levels=c("Water","Land"),
     plg = list(x="topleft", bg="white", cex=1.2),
     pax=list(las=1), mar = c(3.1, 3.1, 2.1, 2.1), main=" ",legend=FALSE)
plot(crc2[[dim(crc2)[3]]],legend=FALSE,add=TRUE,axes=FALSE)
plot(kg2c_bin_poly,axes=FALSE,col=rgb(0,0,0,0),main="",add=TRUE,lwd=0.5)
dev.off()

# plot 4b
pdf(glue("{PlotDir}/{species}_plot_4b.pdf"),height=6,width=6)
par(mfrow=c(1,1),mar=c(5.1, 6.1, 4.1, 2.1), mgp=c(3, 1, 0))
plot(y=vpd_yearly_mean, x=vpd_years_x,
     pch=19,cex=2,cex.lab=2,cex.axis=2,col="black",
     ylab="VPD [Pa]",xlab="Years (2000 - 2018)")
library(mblm)
y=as.numeric(vpd_yearly_mean)
x=seq(from=2000,to=2018,by=1)
lm=mblm::mblm(vpd_yearly_mean~vpd_years_x, repeated = TRUE)
abline(lm,col="#d95f02",lwd=8)
dev.off()

# numbers
print(lm)
print(cl_eoo)
print(cl_change)
print(((cl_change/cl_eoo)*100))
print(cl_change_per_year_prec)
stat_lines = c()
stat_lines = c(stat_lines,"mblm intercept")
stat_lines = c(stat_lines,as.numeric(coef(lm)[1][1]))
stat_lines = c(stat_lines,"mblm slope")
stat_lines = c(stat_lines,as.numeric(coef(lm)[2][1]))
stat_lines = c(stat_lines,"eoo km2")
stat_lines = c(stat_lines,cl_eoo)
stat_lines = c(stat_lines,"cl_change km2")
stat_lines = c(stat_lines,cl_change)
stat_lines = c(stat_lines,"cl relative precss")
stat_lines = c(stat_lines,((cl_change/cl_eoo)*100))
stat_lines = c(stat_lines,"cl change per year prec")
stat_lines = c(stat_lines,cl_change_per_year_prec)
writeLines(stat_lines,glue("{PlotDir}/{species}_stats.txt"))

# zip pdfs
fulltargetDir = file.path(getwd(),PlotDir)
zipname = glue("{species}_{format(Sys.Date(),'%Y_%m_%d')}")
fileext = c("*.pdf","*.dbf","*.prj","*.shp","*.shx","*.txt")
system(glue("cd {fulltargetDir} && mkdir {zipname}"))
for(i in fileext) system(glue("cd {fulltargetDir} && cp {i} {zipname}"))
system(glue("cd {fulltargetDir} && zip -r {zipname}.zip {zipname}"))
system(glue("cd {fulltargetDir} && du -shc {zipname}.zip"))
system(glue("cd {fulltargetDir} && rm -rf {zipname}"))