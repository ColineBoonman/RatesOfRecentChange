rm(list = ls())

####################
## Get Numbers

x=read.table(file="RatesOfRecentChange.txt",sep=",",dec=".",header=T)
nrow(x) # 34,827 species 
# create prioritization list for species with low species ranges or lacking data in 1b.Create.Dataset.R script.

# determine thresholds per slope of when something is 'big change'
# threshold is set to >10th quantile
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(is.na(x$IUCNstatus))]="Not Evaluated"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(x$IUCNstatus)

idx=c(grep("slope_vpd",colnames(x)),
      grep("slope_prec",colnames(x)),
      grep("slope_tmin",colnames(x)),
      grep("slope_tmax",colnames(x)),
      grep("slope_fire",colnames(x)),
      grep("rate_urban",colnames(x)),
      grep("rate_hGFC",colnames(x)),
      grep("rate_tc_change_area",colnames(x)),
      grep("rate_cropland",colnames(x)))
remove=c(grep("0010",colnames(x)),grep("1020",colnames(x)))
idx = idx[-which(idx %in% remove)]

for(threat in idx){
  sp.list=data.frame(Species=NA,IUCNstatus=NA)
  if(min(x[,threat]) < 0 ) {
    threshold.min = quantile(x[,threat],.05)
    sp.list = rbind(sp.list,x[which(x[,threat]<threshold.min),c("Species","IUCNstatus")])
  }
  { threshold.max = quantile(x[,threat],.95)
    sp.list = rbind(sp.list,x[which(x[,threat]>threshold.max),c("Species","IUCNstatus")])
  }
  sp.list = sp.list[which(!is.na(sp.list$Species)),]
  write.table(sp.list,file=paste0("Prioritization95.",colnames(x)[threat],".txt"),sep=",",dec=".",row.names = F)
}

#######
## create overall threat list
all.prioritization.lists = list.files("Prioritization95.",full.names = T)
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("ClimateChange",all.prioritization.lists)]
sp.list=data.frame(Species=NA,IUCNstatus=NA)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(nrow(x))
  sp.list=rbind(sp.list,x)
}
sp.list = sp.list[!duplicated(sp.list),]
sp.list = sp.list[which(!is.na(sp.list$Species)),]
write.table(sp.list,file="Prioritization95.Combined.txt",sep=",",dec=".",row.names = F)
nrow(sp.list)




#######
## create Climate Change threat list
all.prioritization.lists = list.files("Prioritization95.slope",full.names = T)
if(length(grep("fire",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("fire",all.prioritization.lists)]
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("ClimateChange",all.prioritization.lists)]
sp.list=data.frame(Species=NA,IUCNstatus=NA)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(nrow(x))
  sp.list=rbind(sp.list,x)
}
sp.list = sp.list[!duplicated(sp.list),]
sp.list = sp.list[which(!is.na(sp.list$Species)),]
write.table(sp.list,file="Prioritization95.ClimateChange.txt",sep=",",dec=".",row.names = F)
nrow(sp.list)




############
### overlap?

all.prioritization.lists = list.files("Prioritization95.",full.names = T)
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("ClimateChange",all.prioritization.lists)]
sp.list=data.frame(Species=NA,IUCNstatus=NA)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(nrow(x))
  sp.list=rbind(sp.list,x)
}
sp.list = sp.list[which(!is.na(sp.list$Species)),]
nrow(sp.list)# total 26411
nrow(sp.list[!duplicated(sp.list),]) #18175 species not duplicated
dupl = sp.list[duplicated(sp.list),]
nrow(dupl[!duplicated(dupl),]) #6180 species occur twice
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #1711 species occur trice
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #288 species occur four times
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #49 species occur five times
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #6 species occur six times
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #1 species occur seven times



############
### overlap in species? which threats?
all.prioritization.lists = list.files("Prioritization95.",full.names = T)
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("ClimateChange",all.prioritization.lists)]
sp.list=data.frame(Species=NA,IUCNstatus=NA,threat=NA)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  x$threat = gsub(".*Prioritization95.","",all.prioritization.lists[i])
  sp.list=rbind(sp.list,x)
}
sp.list = sp.list[which(!is.na(sp.list$threat)),]
sp.list.original = sp.list
nrow(sp.list[!duplicated(sp.list$Species),]) #18175 species not duplicated
unique(sp.list$threat) # all 11

dupl = sp.list[duplicated(sp.list$Species),]
nrow(dupl[!duplicated(dupl$Species),]) #6180 species occur twice

dupl = dupl[duplicated(dupl$Species),]
nrow(dupl[!duplicated(dupl$Species),]) #1711 species occur trice

dupl = dupl[duplicated(dupl$Species),]
nrow(dupl[!duplicated(dupl$Species),]) #288 species occur four times

dupl = dupl[duplicated(dupl$Species),]
nrow(dupl[!duplicated(dupl$Species),]) #49 species occur five times

dupl = dupl[duplicated(dupl$Species),]
nrow(dupl[!duplicated(dupl$Species),]) #6 species occur 6 times

dupl = dupl[duplicated(dupl$Species),]
nrow(dupl[!duplicated(dupl$Species),]) #1 species occur 7 times

sp.list.original[which(sp.list.original$Species==dupl$Species[1]),]

x[which(x$Species==dupl$Species[1]),]

18175 - 6180 - 1711 - 288 - 49 - 6 - 1
6180 - 1711 - 288 - 49 - 6 - 1
1711 - 288 - 49 - 6 - 1
288 - 49 - 6 - 1
49 - 6 - 1
6 - 1
