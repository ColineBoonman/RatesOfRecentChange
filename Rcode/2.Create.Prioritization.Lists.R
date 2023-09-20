rm(list = ls())
InputDir = "TREECHANGE_InputData/"

# create prioritization list for species with low species ranges or lacking data
df = read.table(file="AOO.txt",sep=",",dec=".",header=T)
df2 = read.table(file="NrOcc.txt",sep=",",dec=".",header=T)
df = merge(df,df2, by=c("Species"),all=T)
iucn = read.table(paste0(InputDir,"IUCNstatus/StatusList.txt"), sep=",", header=T)
iucn = iucn[grep(" ",iucn$Species),]
iucn$Species=stringr::str_replace(iucn$Species," ","_")
cc = merge(df,iucn, by=c("Species"),all.x=T)
colnames(cc)[1]="species"

# create prioritization list for species with < 5 occurrences and AOO<10km2
dd = cc[which(cc$n<5 | cc$AOO<10 ),]
write.table(dd[,c("species","IUCNstatus")],file="Prioritization.SmallRange.txt",sep=",",dec=".",row.names = F)


# create prioritization list for species with >= 5 occurrences and AOO>10km2
x=read.table(file="RatesOfRecentChange.txt",sep=",",dec=".",header=T)
nrow(x) # 32,090 species 

# determine thresholds per slope of when something is 'big change'
# threshold is set to >10th quantile
table(x$IUCNstatus)

idx=c(grep("slope_vpd",colnames(x)),
      grep("slope_prec",colnames(x)),
      grep("slope_tmin",colnames(x)),
      grep("slope_tmax",colnames(x)),
      grep("slope_fire",colnames(x)),
      grep("rate_urban",colnames(x)),
      grep("rate_deforestation",colnames(x)),
      grep("rate_treecover_change_area",colnames(x)),
      grep("rate_cropland",colnames(x)))
remove=c(grep("0010",colnames(x)),grep("1020",colnames(x)))
idx = idx[-which(idx %in% remove)]

for(threat in idx){
  sp.list=data.frame(species=NA,IUCNstatus=NA)
  if(min(x[,threat]) < 0 ) {
    threshold.min = quantile(x[,threat],.05)
    sp.list = rbind(sp.list,x[which(x[,threat]<threshold.min),c("species","IUCNstatus")])
  }
  { threshold.max = quantile(x[,threat],.95)
    sp.list = rbind(sp.list,x[which(x[,threat]>threshold.max),c("species","IUCNstatus")])
  }
  sp.list = sp.list[which(!is.na(sp.list$species)),]
  write.table(sp.list,file=paste0("Prioritization95.",colnames(x)[threat],".txt"),sep=",",dec=".",row.names = F)
}

#######
## create overall threat list
all.prioritization.lists = list.files("Prioritization95.",full.names = T)
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("ClimateChange",all.prioritization.lists)]
sp.list=data.frame(species=NA,IUCNstatus=NA)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(nrow(x))
  sp.list=rbind(sp.list,x)
}
sp.list = sp.list[!duplicated(sp.list),]
sp.list = sp.list[which(!is.na(sp.list$species)),]
write.table(sp.list,file="Prioritization95.Combined.txt",sep=",",dec=".",row.names = F)
nrow(sp.list) #17,393

#######
## create Climate Change threat list
all.prioritization.lists = list.files("Prioritization95.slope",full.names = T)
if(length(grep("fire",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("fire",all.prioritization.lists)]
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("ClimateChange",all.prioritization.lists)]
sp.list=data.frame(species=NA,IUCNstatus=NA)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(nrow(x))
  sp.list=rbind(sp.list,x)
}
sp.list = sp.list[!duplicated(sp.list),]
sp.list = sp.list[which(!is.na(sp.list$species)),]
write.table(sp.list,file="Prioritization95.ClimateChange.txt",sep=",",dec=".",row.names = F)
nrow(sp.list) #11,645

############
### overlap?
all.prioritization.lists = list.files("Prioritization95.",full.names = T)
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0){
  all.prioritization.lists = all.prioritization.lists[-grep("slope_p",all.prioritization.lists)]
  all.prioritization.lists = all.prioritization.lists[-grep("slope_v",all.prioritization.lists)]
  all.prioritization.lists = all.prioritization.lists[-grep("slope_t",all.prioritization.lists)]
}
sp.list=data.frame(species=NA,IUCNstatus=NA)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(nrow(x))
  sp.list=rbind(sp.list,x)
}
sp.list = sp.list[which(!is.na(sp.list$species)),]
nrow(sp.list)# total 21,275
nrow(sp.list[!duplicated(sp.list),]) #17393 species not duplicated - 17393
dupl = sp.list[duplicated(sp.list),]
nrow(dupl[!duplicated(dupl),]) #3570 species occur twice
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #281 species occur trice
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #28 species occur four times
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #3 species occur five times 
dupl
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #0 species occur six times
dupl
