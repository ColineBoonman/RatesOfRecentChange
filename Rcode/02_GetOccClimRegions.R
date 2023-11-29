######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

# init packages
library("terra")
library("sf")
library("data.table")
library("fst")
library("glue")
library("tictoc")
library("exactextractr")
library("reshape2")
options(datatable.print.class = TRUE)
gc()

# set vars
bufcov = 0.25
occthreshold = 0.05

# set dirs
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/osx/Documents/SoftEng/Projects/Project_TreeChangeEnvData")
st=format(Sys.time(), "%Y_%m_%d")
chelsaDir = glue("{getwd()}/Data/")
source('Functions/climzone.R')
source('Functions/downloadChelsa.R')

# setup terra options and create tmp dir 
tmpDir = glue("tmp_terra_{sample(1000:9999,1)}")
dir.create(tmpDir,showWarnings = FALSE,recursive = TRUE)
terraOptions(tempdir=tmpDir, memfrac=0.8, todisk=FALSE)

# load kg data
downloadChelsa_bioclim(timeperiod = "1981-2010", var = "kg2", outdir = chelsaDir)
cur = list.files(chelsaDir,pattern="*.tif",full.names=TRUE) %>% grep("kg2",.,value=TRUE) %>% grep("1981",.,value=TRUE) %>% rast()
print(names(cur))
print(dim(cur))
cur_max = global(cur,max)
cur_min = global(cur,min)
n_classes = length(cur_min$min:cur_max$max)

# load species data
print("loading species data")
sp = vect("Data/SpeciesPolygons/Global.polygon.shp")
pts = vect("Data/SpeciesPoints/Global.datapoints.shp")

# buffer points, extract clim regions, process in chunks, save as rds
chunks = getChunkIndices(nrow(pts),1e6)
for(i_chuck in 1:nrow(chunks)){
  chunk_rds = glue("Data/SpeciesBufPointExtract_chunk_{i_chuck}.rds")
  if(!file.exists(chunk_rds)){
    cat("processing chunk:",i_chuck,"/",nrow(chunks),"\n")
    pts_chunk = buffer(pts[chunks$strt[i_chuck]:chunks$stop[i_chuck],], width = 1000)
    pts_chunk = st_as_sf(pts_chunk)
    ext_vals = exact_extract(cur,pts_chunk,progress=TRUE,include_area=FALSE); gc()
    ext_vals = lapply(ext_vals, function(x) x = x[x$coverage_fraction>bufcov,])
    ext_vals = rbindlist(ext_vals,idcol=TRUE); gc()
    names(ext_vals) = c("point_index","kg2_value","coverage_fraction")
    saveRDS(ext_vals, chunk_rds)
  }else{
    cat("chunk done, skipping:",i_chuck,"/",nrow(chunks),"\n")
  }
  gc()
}
rm(pts_chunk)
rm(ext_vals)
gc()

# merge chunks
all_chunk_rds = glue("Data/SpeciesBufPointExtract_chunk_{1:nrow(chunks)}.rds")
ext_vals = lapply(all_chunk_rds,readRDS)
ext_vals = rbindlist(ext_vals,idcol=TRUE)
names(ext_vals) = c("chunk_index","point_index","kg2_value","coverage_fraction")
setkey(ext_vals,chunk_index)
gc()

# add additional data (correct point_index)
ext_vals[,chunk_starts:=chunks$strt[chunk_index]]
ext_vals[,chunk_starts_min1:=chunk_starts-1]
ext_vals[,point_index_new:=point_index+chunk_starts_min1]
ext_vals[,species:=pts$species[point_index_new]]
gc()

# check ext_val
i = sample(1:nrow(pts),1)
pts$species[i]
ext_vals[point_index_new==i,]
pts_i = buffer(pts[i,], width = 1000)
pts_i = st_as_sf(pts_i)
exact_extract(cur,pts_i,progress=TRUE,include_area=FALSE)
gc()

# get grid cell frequency occ regions per species
ext_vals_freq = dcast(data=ext_vals,species ~ kg2_value,fun.aggregate = length,value.var = "kg2_value")
ext_vals_freq = as.data.table(ext_vals_freq)
names(ext_vals_freq)[2:ncol(ext_vals_freq)] = paste0("kg2_",names(ext_vals_freq)[2:ncol(ext_vals_freq)])
ext_vals_freq_fst_name = glue("Data/SpeciesOccFreqClimRegions_bufcov{bufcov*100}p.fst")
write_fst(ext_vals_freq,ext_vals_freq_fst_name)

# check freq
i=203
sp_id = sp$ID[i]
ext_vals_i = ext_vals[species==sp_id,]
kg2_val = table(ext_vals_i$kg2_value)

# calc relative freq occ grid cells
ext_vals_freq[, sum := rowSums(.SD), .SDcols = grep("kg2",names(ext_vals_freq))]
ext_vals_freq_rel = ext_vals_freq[, .SD/sum, .SDcols = names(ext_vals_freq) %like% "kg2"]
ext_vals_freq_rel$species = ext_vals_freq$species
ext_vals_freq_fst_name = glue("Data/SpeciesOccRelFreqClimRegions_bufcov{bufcov*100}p.fst")
write_fst(ext_vals_freq_rel,ext_vals_freq_fst_name)

# check freq
i=203
ext_vals_freq_rel[species==sp$ID[i],]

# apply relative freq threshold occthreshold
ext_vals_freq_rel_thr = ext_vals_freq_rel[, .SD>occthreshold, .SDcols = names(ext_vals_freq_rel) %like% "kg2"]
ext_vals_freq_rel_thr = as.data.table(ext_vals_freq_rel_thr)
ext_vals_freq_rel_thr$species = ext_vals_freq_rel$species

# check freq threshold
i=203
ext_vals_freq_rel_thr[species==sp$ID[i],]

# create binary occ 
ext_vals_binary = ext_vals_freq_rel_thr
to_replace = names(which(sapply(ext_vals_binary, is.logical)))
for (var in to_replace) ext_vals_binary[, (var):= as.numeric(get(var))]

# expend columns (add missing kg2 columns never occ)
cols_need = glue("kg2_{cur_min$min:cur_max$max}")
cols_exist = grep("kg2_",names(ext_vals_binary),value=TRUE)
cols_insert = cols_need[!cols_need %in% cols_exist]
ext_vals_binary[,cols_insert] = 0
ext_vals_binary = ext_vals_binary[,.SD,.SDcols=c("species",as.vector(cols_need))]

# create occ string
ext_vals_binary[, occ:=do.call(paste0,.SD), .SDcols=names(ext_vals_binary) %like% "kg2"]

# check binary
i=203
ext_vals_binary[species==sp$ID[i],]

# group species with similar occ 
occ_unique = unique(ext_vals_binary$occ)
ext_vals_binary[,occ_chunk:=match(occ,occ_unique)]
max(ext_vals_binary$occ_chunk)

# write binary occ fst
ext_vals_bin_fst_name = glue("Data/SpeciesOccExpBinaryClimRegions_bufcov{bufcov*100}p_occthreshold{occthreshold*100}p.fst")
write_fst(ext_vals_binary,ext_vals_bin_fst_name)