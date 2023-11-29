######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
#   START = 1
#   STOP = 2
#   VAR = 1
}else{
    START = as.numeric(args[1])
    STOP = as.numeric(args[2])
    VAR = as.numeric(args[3])
    cat("START:",START,"STOP:",STOP,"\n")
}

var_select = c("yearlypr","yearlypr","yearlytasmax","yearlytasmin","yearlyvpd","yearlyvpd")[VAR]
vars_stat  = c("_mean","_sd","_max","_min","_mean","_sd")[VAR]
var_full = paste0(var_select,vars_stat)
print(var_full)

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
out_fname = glue("Results/06_ExtractSpData_{var_full}_tmp_chunk{START_pad}to{STOP_pad}.fst")
cat("START_pad:",START_pad,"STOP_pad:",STOP_pad,"\n")
cat("out_fname:",out_fname,"\n")

# set dirs
setwd("...")
chelsaDir = "Data/EnvDataChelsa/"
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
climFiles = list.files(chelsaDir, pattern="*.tif", full.names=TRUE)
climFiles = grep(var_select,climFiles,value=TRUE)
climFiles = grep(vars_stat,climFiles,value=TRUE)
print(climFiles)
climRast = rast(climFiles)
names(climRast) = str_split(str_split(climFiles,".tif",simplify=TRUE)[,1],"yearly",simplify=TRUE)[,2]
print(names(climRast))

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
cols_add = c("eoo_km2",names(climRast), "count")
if(file.exists(out_fname)){
    spOcc = read_fst(out_fname, as.data.table=TRUE)
}else{
    spOcc = read_fst(glue("Data/SpeciesOccExpBinaryClimRegions_bufcov{bufcov*100}p_occthreshold{occthreshold*100}p.fst"), as.data.table=TRUE)
    spOcc[,cols_add] = NA_real_
}

# extract function
extract_fn_clim_av = function(value, coverage_fraction, kg2_occ, clim_rast, cell_size){
    dt = as.data.table(value) 
    dt[,coverage_fraction:= coverage_fraction]
    dt = dt[coverage_fraction>0.01 & value$land==1,]
    dt = dt[kg2 %in% kg2_occ,]
    cells = cellFromXY(clim_rast, dt[,c("lon","lat")])
    clim_dt = as.data.table(clim_rast[cells])
    dt = cbind(dt,clim_dt)
    dt[,cell_size_km2:=cell_size[cells][,"cell_size_km2"]]
    dt[,weight:=cell_size_km2*coverage_fraction]
    dt[,count:=1]
    colselect = names(clim_rast)
    dt_weightmean = dt[,lapply(.SD,weighted.mean,w=weight,na.rm=TRUE),.SDcols=colselect]
    dt_weightmean[,count:=sum(dt[,count],na.rm=TRUE)]
    dt_weightmean[,eoo_km2:=sum(dt[,weight],na.rm=TRUE)]
    return(dt_weightmean)
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
                                     fun=extract_fn_clim_av,kg2_occ=kg2_occ,clim_rast=climRast,cell_size=cell_size)
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



