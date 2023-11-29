######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}else{
  START = args[1]
  STOP = args[2]
  cat("START:",START,"STOP:",STOP,"\n")
}


# init packages
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

START_pad = str_pad(START,4,"left","0")
STOP_pad = str_pad(STOP,4,"left","0")
out_fname = glue("Results/05_spDataExtract_tmp_chunk{START_pad}to{STOP_pad}.fst")
cat("START_pad:",START_pad,"STOP_pad:",STOP_pad,"\n")
cat("out_fname:",out_fname,"\n")

# set dirs
setwd("...")
chelsaDir = "Data/EnvDataChelsa/"
GEEDir = "Data/EnvDataGEE/"
source('Functions/climzone.R')
source('Functions/downloadChelsa.R')

# setup terra options and create tmp dir 
tmpDir = glue("tmp_terra_{sample(1000:9999,1)}")
dir.create(tmpDir,showWarnings = FALSE,recursive = TRUE)
terraOptions(tempo_dir=tmpDir, memfrac=0.8, todisk=FALSE, memmax=70)

# open water mask and kg2
land = rast("Data/WaterMask_fullCoverage.tif")
kg2 = rast("Data/bioclim_kg2_1981-2010.tif")

# open env data layers
geeFiles = list.files(GEEDir, pattern="*.tif", full.names=TRUE)
geeFiles = grep(paste0("_",2001:2020,collapse="|"),geeFiles,invert=TRUE,value=TRUE)
geeRast = rast(geeFiles)
names(geeRast)[1] = "CropLandChange"

# compute grid cell area
cell_size = cellSize(kg2, unit="km")
names(cell_size) = "cell_size_km2"

# create xy index rasters
lon = init(land, "x")
lat = init(land, "y")

# attach land and kg2 layers
ext_layer = c(lon,lat,kg2,land)
names(ext_layer) = c("lon","lat","kg2","land")

# load species data
print("loading species data")
bufcov = 0.25; occthreshold = 0.05
sp = st_read("Data/SpeciesPolygons/Global.polygon.shp")
cols_add = c("eoo_km2", "CropLandChange_km2", "fire_2001_km2", "fire_2002_km2", "fire_2003_km2", "fire_2004_km2", "fire_2005_km2", "fire_2006_km2", 
    "fire_2007_km2", "fire_2008_km2", "fire_2009_km2", "fire_2010_km2", "fire_2011_km2", "fire_2012_km2", "fire_2013_km2", "fire_2014_km2", 
    "fire_2015_km2", "fire_2016_km2", "fire_2017_km2", "fire_2018_km2", "fire_2019_km2", "fire_2020_km2", "ForestCoverLoss_GFCCloss_km2", 
    "urban_buildup_2_km2", "global_forest_change_2021_v1_9_loss_km2", "count")
if(file.exists(out_fname)){
    spOcc = read_fst(out_fname, as.data.table=TRUE)
}else{
    spOcc = read_fst(glue("Data/SpeciesOccExpBinaryClimRegions_bufcov{bufcov*100}p_occthreshold{occthreshold*100}p.fst"), as.data.table=TRUE)
    spOcc[,cols_add] = NA_real_
}

# extract function
extract_fn = function(value, coverage_fraction, kg2_occ, gee_rast, cell_size){
    dt = as.data.table(value) 
    dt[,coverage_fraction:= coverage_fraction]
    dt = dt[coverage_fraction>0.01 & value$land==1,]
    dt = dt[kg2 %in% kg2_occ,]
    cells = cellFromXY(gee_rast, dt[,c("lon","lat")])
    gee_dt = as.data.table(gee_rast[cells])
    dt = cbind(dt,gee_dt)
    dt[,cell_size_km2:=cell_size[cells][,"cell_size_km2"]]
    colselect = colnames(dt)[grep("CropLandChange|fire_|ForestCoverLoss_GFCCloss|urban_buildup_2|global_forest_change_2021_v1_9_loss|land",colnames(dt))]
    dt[, (colselect) := lapply(.SD, function(d) d*get('cell_size_km2')*get('coverage_fraction')), .SDcols = colselect]
    dt[,count:=1]
    dt = dt[, lapply(.SD, sum, na.rm=TRUE)]
    dt[,c("lon","lat","kg2","cell_size_km2","coverage_fraction"):=NULL]
    names(dt)[grep("land",names(dt))] = "eoo"
    names(dt)[grep("count",names(dt),invert=TRUE)] = paste0(names(dt)[grep("count",names(dt),invert=TRUE)],"_km2")
    return(dt)
}

# extract values
unique_chunks = unique(spOcc[,occ_chunk])[START:STOP]
# chunk = 14
# sp_j=1
for(chunk in unique_chunks) {

    cat("chunk",chunk,"/",max(unique_chunks),"\n")
    cat("nrow chunk:",nrow(spOcc[occ_chunk==chunk,]),"\n")
    kg2_occ = colSums(spOcc[occ_chunk==chunk,.SD,.SDcol=glue("kg2_{1:31}")])>0
    kg2_occ = as.numeric(str_split(names(kg2_occ)[which(kg2_occ)],"_",simplify=TRUE)[,2])
    sp_chunk = match(spOcc[occ_chunk==chunk,species],sp$id)
    sp_chunk = sp_chunk[complete.cases(sp_chunk)]
    sp_chunk = sp[sp_chunk,]
    cat("nrow polys:",nrow(sp_chunk),"\n")
    sp_chunk_idx = match(sp_chunk$id,spOcc[,species])

    # process if chunk is empty
    if(sum(colSums(spOcc[sp_chunk_idx,.SD,.SDcols=cols_add],na.rm=TRUE))==0 & nrow(sp_chunk)>0){
        
        tic()
        subchunks = getChunkIndices(n=nrow(sp_chunk),by=50)
        for(j in subchunks$chunk_index) {
            sp_j = subchunks$strt[j]:subchunks$stop[j]
            cat(j,"/",nrow(subchunks),"(",subchunks$strt[j],subchunks$stop[j],length(sp_j),")","\n")
            sp_chunk_extract = sp_chunk[sp_j,]
            ext_vals = exact_extract(ext_layer,sp_chunk_extract,progress=FALSE,include_area=FALSE,max_cells_in_memory=1e+11,
                                     fun=extract_fn,kg2_occ=kg2_occ,gee_rast=geeRast,cell_size=cell_size)
            sp_idx = match(sp_chunk_extract$id,spOcc[,species])
            spOcc[sp_idx, (cols_add) := ext_vals[,.SD,.SDcols = cols_add]]
            rm(ext_vals)
            gc()
        }
        write_fst(spOcc,out_fname)
        toc()

    }else{
        cat("chunk already processed or empty, skipping","\n")
    }

}

# write out
write_fst(spOcc,out_fname)



