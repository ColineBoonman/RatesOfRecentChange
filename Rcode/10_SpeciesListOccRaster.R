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
library("fasterize")
library("future")
plan(multicore)
library(raster)
options(datatable.print.class = TRUE)
rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)
gc()

# set wd
setwd("...")
source("Functions/spRichRasters.R")

# setup terra options and create tmp dir 
tmpDir = glue("tmp_sp_occ_{sample(1000:9999,1)}")
dir.create(tmpDir,showWarnings = FALSE,recursive = TRUE)
terraOptions(tempo_dir=tmpDir, memfrac=0.8, todisk=TRUE, memmax=90)

# sp list (list of species names to create occ raster for)
selectList = 7
listFiles = list.files("Data/SpeciesOccLists/",pattern="*.txt")
listFiles = listFiles[selectList]
print(listFiles)
outName = glue("SpListOcc_{str_split(listFiles,'[.]')[[1]][2]}")
spList = read.table(file.path("Data/SpeciesOccLists/",listFiles),sep=",",header=TRUE)
spList = spList$Species
for(i in 1:length(spList)) spList[i] = str_replace(spList[i]," ",'_')

# result dir 
resDir = glue("Results/speciesCellOccIdx_{outName}/")
resRast = glue("Results/{outName}.tif")
dir.create(resDir,showWarnings = FALSE,recursive = TRUE)

# load data: water mask, kg2 region map, species polygons (95 mcp)
land = rast("Data/WaterMask_fullCoverage.tif")
kg2 = rast("Data/bioclim_kg2_1981-2010.tif")
kg2 = kg2*land
sp = st_read("Data/SpeciesPolygons/Global.polygon.shp")

# load species occ clim regions kg2
bufcov = 0.25
occthreshold = 0.05 
spocc = read_fst(glue("Data/SpeciesOccExpBinaryClimRegions_bufcov{bufcov*100}p_occthreshold{occthreshold*100}p.fst"), as.data.table=TRUE)
spocc = spocc[match(sp$id,spocc$species),]

# add chunk data to sp polygons
sp$spec_check = spocc$species
sp$chunk = spocc$occ_chunk
sp$occ = spocc$occ
colselect = grep("kg2_",names(spocc),value=TRUE)
sp[,colselect] = spocc[,.SD,.SDcols=colselect]
sp = sp[order(sp$chunk),]
rm(spocc)

# template raster
out_rast = rast(nrows=nrow(land), ncols=ncol(land), nlyrs=1, crs=crs(land), extent=ext(land), vals=0)
out_raster = raster(out_rast)

# calculate polygon sums per chunk (species with identical clim region occ)
sp = sp[which(sp$id%in%spList),] # only for species in list
nrow(sp)
length(spList)
unique_chunks = unique(sp$chunk)
chunk_size = ceiling(length(unique_chunks)/8)
unique_chunks = split(unique_chunks, ceiling(seq_along(unique_chunks)/chunk_size))

# run on 8 cores
files = list.files(resDir,pattern="*.fst",full.names=TRUE)
if(length(files)!=length(unlist(unique_chunks))){
    if(length(unique_chunks)>0) f1 = future(get_occ_cell_indices_loop(sp,unique_chunks[[1]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>1) f2 = future(get_occ_cell_indices_loop(sp,unique_chunks[[2]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>2) f3 = future(get_occ_cell_indices_loop(sp,unique_chunks[[3]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>3) f4 = future(get_occ_cell_indices_loop(sp,unique_chunks[[4]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>4) f5 = future(get_occ_cell_indices_loop(sp,unique_chunks[[5]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>5) f6 = future(get_occ_cell_indices_loop(sp,unique_chunks[[6]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>6) f7 = future(get_occ_cell_indices_loop(sp,unique_chunks[[7]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>7) f8 = future(get_occ_cell_indices_loop(sp,unique_chunks[[8]],resDir,out_raster,kg2,colselect)) %plan% multicore
    if(length(unique_chunks)>0) value(f1)
    if(length(unique_chunks)>1) value(f2)
    if(length(unique_chunks)>2) value(f3)
    if(length(unique_chunks)>3) value(f4)
    if(length(unique_chunks)>4) value(f5)
    if(length(unique_chunks)>5) value(f6)
    if(length(unique_chunks)>6) value(f7)
    if(length(unique_chunks)>7) value(f8)
}

# check files
for(cc in 1:length(unique_chunks)) get_occ_cell_indices_loop(sp,unique_chunks[[cc]],resDir,out_raster,kg2,colselect)

# merge files, insert into rast
cat("inserting values into rast","\n")
out_rast = rep(out_rast,length(unique_chunks))
names(out_rast) = paste0("c",1:length(unique_chunks))
files = list.files(resDir,pattern="*.fst",full.names=TRUE)
for(i in 1:length(unique_chunks)){
    cat(i,"/",length(unique_chunks),"\n")
    i_files = files[grep(paste0(paste0("chunk",unique_chunks[[i]],".fst"),collapse="|"),files)]
    cellocc = lapply(i_files,read_fst,as.data.table=TRUE)
    cellocc = rbindlist(cellocc)[, sum(val), idx]
    out_rast[[i]][cellocc$idx] = cellocc$V1 # replace with val
    rm(cellocc)
    gc()
}
out_rast_sum = sum(out_rast)
out_rast_sum = out_rast_sum*land
plotm(out_rast_sum,str_replace(resRast,".tif",".png"))
terra::writeRaster(out_rast_sum,resRast)
