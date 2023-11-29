######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

library("fst")
library("data.table")
library("stringr")
library("glue")
library("mblm")
library("future")
plan(multicore)
setwd("...")
source("Functions/mblm.R")

# set years to use
years = 2000:2020
out_fst_name = glue("Results/DataCombined{years[1]}{years[length(years)]}_2023_09_04.fst")

# get tmp files
fstFiles = data.frame(fstFiles=list.files("Results/",full.names=TRUE,pattern="*.fst"),var=NA)
fstFiles = fstFiles[grep("DataCombined",fstFiles$fstFiles,invert=TRUE),]
fstFiles$var = str_split(fstFiles$fstFiles,"_tmp_chunk",simplify=TRUE)[,1]
fstFiles$var = str_split(fstFiles$var,"06_",simplify=TRUE)[,2]
fstFiles$var = ifelse(fstFiles$var=="","ExtractSpData_areaLossBinary",fstFiles$var)

# read and format data
fstData = lapply(fstFiles$fstFiles,read_fst,as.data.table=TRUE)
fstData = lapply(fstData,function(x) x[complete.cases(x[,eoo_km2]),])
fstData2 = list()
vars = unique(fstFiles$var)
for(i in 1:length(vars)) fstData2[[i]] = rbindlist(fstData[which(fstFiles$var==vars[i])],idcol=TRUE)
names(fstData2) = vars
for(i in 1:length(vars)) fstData2[[i]] = unique(fstData2[[i]])

# check data 1
lapply(fstData2,nrow)
lapply(fstData2,function(x) unique(x[,.id]))

# check data 2
all(fstData2[[3]][,1:10]==fstData2[[2]][,1:10])

# merge into one data.table
for(i in 1:length(fstData2)) fstData2[[i]] = fstData2[[i]][order(fstData2[[i]]$species),]
suffixes = str_split(names(fstData2),"SpData_",simplify=TRUE)[,2]
fstData3 = fstData2[[1]]
for(i in 2:length(fstData2)) fstData3 = merge(fstData3, fstData2[[i]], by="species", suffixes = c("", glue("_{suffixes[i]}")))
colkeep = data.frame(nms=names(fstData3),keep=FALSE)
colkeep$keep[grep("species|eoo|fire|CropLandChange|ForestCoverLoss|urban_buildup|global_forest_change|count",colkeep$nms)] = TRUE
colkeep$keep[match(paste0("kg2_",1:31),colkeep$nms)] = TRUE
colkeep$keep[match_no_na(paste0("pr_",years,"_mean"),colkeep$nms)] = TRUE
colkeep$keep[match_no_na(paste0("pr_",years,"_sd"),colkeep$nms)] = TRUE
colkeep$keep[match_no_na(paste0("tasmax_",years,"_max"),colkeep$nms)] = TRUE
colkeep$keep[match_no_na(paste0("tasmin_",years,"_min"),colkeep$nms)] = TRUE
colkeep$keep[match_no_na(paste0("vpd_",years,"_mean"),colkeep$nms)] = TRUE
colkeep$keep[match_no_na(paste0("vpd_",years,"_sd"),colkeep$nms)] = TRUE
fstData4 = fstData3[,.SD,.SDcols=colkeep$nms[colkeep$keep]]
firecols = grep("fire",names(fstData4),value=TRUE)
firecols = firecols[grep(paste0(years,collapse="|"),firecols)]

# check NAs in data
names(fstData4)[unique(which(is.na(fstData4),arr.ind=TRUE)[,2])]

# calculate mblm slopes
unique_chunks = 1:nrow(fstData4)
chunk_size = ceiling(length(unique_chunks)/4)
unique_chunks = split(unique_chunks, ceiling(seq_along(unique_chunks)/chunk_size))

# fire
cols = paste0("fire_",years,"_km2")
cols = names(fstData4)[match_no_na(cols,names(fstData4))]
f1 = future(mblm_slopes_pval(fstData4[unique_chunks[[1]],],cols)) %plan% multicore
f2 = future(mblm_slopes_pval(fstData4[unique_chunks[[2]],],cols)) %plan% multicore
f3 = future(mblm_slopes_pval(fstData4[unique_chunks[[3]],],cols)) %plan% multicore
f4 = future(mblm_slopes_pval(fstData4[unique_chunks[[4]],],cols)) %plan% multicore
out1 = value(f1)
out2 = value(f2)
out3 = value(f3)
out4 = value(f4)
out = rbind(out1,out2,out3,out4)
fstData4$fire_km2_mblm_slope=out$slopes[match(out$species,fstData4$species)]
fstData4$fire_km2_mblm_slope_pval=out$pvals[match(out$species,fstData4$species)]
fstData4$fire_km2_mblm_slope[is.na(fstData4$fire_km2_mblm_slope)] = 0
fstData4$fire_km2_mblm_slope_pval[is.na(fstData4$fire_km2_mblm_slope_pval)] = 0
fstData4[,(cols):=NULL]

# pr_mean
cols = paste0("pr_",years,"_mean")
cols = names(fstData4)[match_no_na(cols,names(fstData4))]
f1 = future(mblm_slopes_pval(fstData4[unique_chunks[[1]],],cols)) %plan% multicore
f2 = future(mblm_slopes_pval(fstData4[unique_chunks[[2]],],cols)) %plan% multicore
f3 = future(mblm_slopes_pval(fstData4[unique_chunks[[3]],],cols)) %plan% multicore
f4 = future(mblm_slopes_pval(fstData4[unique_chunks[[4]],],cols)) %plan% multicore
out1 = value(f1)
out2 = value(f2)
out3 = value(f3)
out4 = value(f4)
out = rbind(out1,out2,out3,out4)
which(is.na(out),arr.ind=TRUE)
fstData4$pr_mean_mblm_slope=out$slopes[match(out$species,fstData4$species)]
fstData4$pr_mean_mblm_slope_pval=out$pvals[match(out$species,fstData4$species)]
fstData4$pr_mean_mblm_slope[is.na(fstData4$pr_mean_mblm_slope)] = 0
fstData4$pr_mean_mblm_slope_pval[is.na(fstData4$pr_mean_mblm_slope_pval)] = 0
fstData4[,(cols):=NULL]

# pr_mean
cols = paste0("pr_",years,"_sd")
cols = names(fstData4)[match_no_na(cols,names(fstData4))]
f1 = future(mblm_slopes_pval(fstData4[unique_chunks[[1]],],cols)) %plan% multicore
f2 = future(mblm_slopes_pval(fstData4[unique_chunks[[2]],],cols)) %plan% multicore
f3 = future(mblm_slopes_pval(fstData4[unique_chunks[[3]],],cols)) %plan% multicore
f4 = future(mblm_slopes_pval(fstData4[unique_chunks[[4]],],cols)) %plan% multicore
out1 = value(f1)
out2 = value(f2)
out3 = value(f3)
out4 = value(f4)
out = rbind(out1,out2,out3,out4)
which(is.na(out),arr.ind=TRUE)
fstData4$pr_sd_mblm_slope=out$slopes[match(out$species,fstData4$species)]
fstData4$pr_sd_mblm_slope_pval=out$pvals[match(out$species,fstData4$species)]
fstData4$pr_sd_mblm_slope[is.na(fstData4$pr_sd_mblm_slope)] = 0
fstData4$pr_sd_mblm_slope_pval[is.na(fstData4$pr_mean_sd_slope_pval)] = 0
fstData4[,(cols):=NULL]

# tasmax_max
cols = paste0("tasmax_",years,"_max")
cols = names(fstData4)[match_no_na(cols,names(fstData4))]
f1 = future(mblm_slopes_pval(fstData4[unique_chunks[[1]],],cols)) %plan% multicore
f2 = future(mblm_slopes_pval(fstData4[unique_chunks[[2]],],cols)) %plan% multicore
f3 = future(mblm_slopes_pval(fstData4[unique_chunks[[3]],],cols)) %plan% multicore
f4 = future(mblm_slopes_pval(fstData4[unique_chunks[[4]],],cols)) %plan% multicore
out1 = value(f1)
out2 = value(f2)
out3 = value(f3)
out4 = value(f4)
out = rbind(out1,out2,out3,out4)
which(is.na(out),arr.ind=TRUE)
fstData4$tasmax_max_mblm_slope=out$slopes[match(out$species,fstData4$species)]
fstData4$tasmax_max_mblm_slope_pval=out$pvals[match(out$species,fstData4$species)]
fstData4$tasmax_max_mblm_slope[is.na(fstData4$tasmax_max_mblm_slope)] = 0
fstData4$tasmax_max_mblm_slope_pval[is.na(fstData4$tasmax_max_sd_slope_pval)] = 0
fstData4[,(cols):=NULL]

# tasmin_min
cols = paste0("tasmin_",years,"_min")
cols = names(fstData4)[match_no_na(cols,names(fstData4))]
f1 = future(mblm_slopes_pval(fstData4[unique_chunks[[1]],],cols)) %plan% multicore
f2 = future(mblm_slopes_pval(fstData4[unique_chunks[[2]],],cols)) %plan% multicore
f3 = future(mblm_slopes_pval(fstData4[unique_chunks[[3]],],cols)) %plan% multicore
f4 = future(mblm_slopes_pval(fstData4[unique_chunks[[4]],],cols)) %plan% multicore
out1 = value(f1)
out2 = value(f2)
out3 = value(f3)
out4 = value(f4)
out = rbind(out1,out2,out3,out4)
which(is.na(out),arr.ind=TRUE)
fstData4$tasmin_min_mblm_slope=out$slopes[match(out$species,fstData4$species)]
fstData4$tasmin_min_mblm_slope_pval=out$pvals[match(out$species,fstData4$species)]
fstData4$tasmin_min_mblm_slope[is.na(fstData4$tasmin_min_mblm_slope)] = 0
fstData4$tasmin_min_mblm_slope_pval[is.na(fstData4$tasmin_min_sd_slope_pval)] = 0
fstData4[,(cols):=NULL]

# vpd_mean
cols = paste0("vpd_",years,"_mean")
cols = names(fstData4)[match_no_na(cols,names(fstData4))]
f1 = future(mblm_slopes_pval(fstData4[unique_chunks[[1]],],cols)) %plan% multicore
f2 = future(mblm_slopes_pval(fstData4[unique_chunks[[2]],],cols)) %plan% multicore
f3 = future(mblm_slopes_pval(fstData4[unique_chunks[[3]],],cols)) %plan% multicore
f4 = future(mblm_slopes_pval(fstData4[unique_chunks[[4]],],cols)) %plan% multicore
out1 = value(f1)
out2 = value(f2)
out3 = value(f3)
out4 = value(f4)
out = rbind(out1,out2,out3,out4)
which(is.na(out),arr.ind=TRUE)
fstData4$vpd_mean_mblm_slope=out$slopes[match(out$species,fstData4$species)]
fstData4$vpd_mean_mblm_slope_pval=out$pvals[match(out$species,fstData4$species)]
fstData4$vpd_mean_mblm_slope[is.na(fstData4$vpd_mean_mblm_slope)] = 0
fstData4$vpd_mean_mblm_slope_pval[is.na(fstData4$vpd_mean_sd_slope_pval)] = 0
fstData4[,(cols):=NULL]

# vpd_sd
cols = paste0("vpd_",years,"_sd")
cols = names(fstData4)[match_no_na(cols,names(fstData4))]
f1 = future(mblm_slopes_pval(fstData4[unique_chunks[[1]],],cols)) %plan% multicore
f2 = future(mblm_slopes_pval(fstData4[unique_chunks[[2]],],cols)) %plan% multicore
f3 = future(mblm_slopes_pval(fstData4[unique_chunks[[3]],],cols)) %plan% multicore
f4 = future(mblm_slopes_pval(fstData4[unique_chunks[[4]],],cols)) %plan% multicore
out1 = value(f1)
out2 = value(f2)
out3 = value(f3)
out4 = value(f4)
out = rbind(out1,out2,out3,out4)
which(is.na(out),arr.ind=TRUE)
fstData4$vpd_sd_mblm_slope=out$slopes[match(out$species,fstData4$species)]
fstData4$vpd_sd_mblm_slope_pval=out$pvals[match(out$species,fstData4$species)]
fstData4$vpd_sd_mblm_slope[is.na(fstData4$vpd_sd_mblm_slope)] = 0
fstData4$vpd_sd_mblm_slope_pval[is.na(fstData4$vpd_sd_sd_slope_pval)] = 0
fstData4[,(cols):=NULL]

# plots for checking
fstData4
histm(fstData4[,vpd_sd_mblm_slope],xlim=c(-5,15),20)
histm(fstData4[,pr_mean_mblm_slope],xlim=c(-50,50),100)
eoo = grep("eoo",names(fstData4))
fstData4[,(names(fstData4)[eoo[2:length(eoo)]]):=NULL]
count = grep("count",names(fstData4))
fstData4[,(names(fstData4)[count[2:length(count)]]):=NULL]
write_fst(fstData4,out_fst_name)
