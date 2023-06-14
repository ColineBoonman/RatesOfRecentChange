rm(list = ls())

####################
## Get Numbers

# # ---------- prepare data
data=read.table(file="RatesOfRecentChange.txt",sep=",",dec=".",header=T)
data$IUCNstatus=as.character(data$IUCNstatus)
data$IUCNstatus[which(data$IUCNstatus=="CR")]="Threatened"
data$IUCNstatus[which(data$IUCNstatus=="DD")]="DD"
data$IUCNstatus[which(data$IUCNstatus=="EN")]="Threatened"
data$IUCNstatus[which(data$IUCNstatus=="EW")]="Threatened"
data$IUCNstatus[which(data$IUCNstatus=="Edata")]="Threatened"
data$IUCNstatus[which(data$IUCNstatus=="LC")]="Not Threatened"
data$IUCNstatus[which(data$IUCNstatus=="LR/cd")]="Threatened"
data$IUCNstatus[which(data$IUCNstatus=="LR/lc")]="Not Threatened"
data$IUCNstatus[which(data$IUCNstatus=="LR/nt")]="Not Threatened"
data$IUCNstatus[which(data$IUCNstatus=="NT")]="Not Threatened"
data$IUCNstatus[which(data$IUCNstatus=="VU")]="Vulnerable"
data$IUCNstatus[which(is.na(data$IUCNstatus))]="NA"
data$IUCNstatus=as.factor(data$IUCNstatus)

Prioritization.combined=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
Prioritization.combined$IUCNstatus=as.character(Prioritization.combined$IUCNstatus)
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="CR")]="Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="DD")]="DD"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="EN")]="Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="EW")]="Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="EPrioritization.combined")]="Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="LC")]="Not Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="LR/cd")]="Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="LR/lc")]="Not Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="LR/nt")]="Not Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="NT")]="Not Threatened"
Prioritization.combined$IUCNstatus[which(Prioritization.combined$IUCNstatus=="VU")]="Vulnerable"
Prioritization.combined$IUCNstatus[which(is.na(Prioritization.combined$IUCNstatus))]="NA"
Prioritization.combined$IUCNstatus=as.factor(Prioritization.combined$IUCNstatus)






#######
# get values for Abstract and Introduction section

# x% of the assessed species were exposed to major and increasing threats
nrow(Prioritization.combined)*100/nrow(data)

# Of these priority candidates, only x% was listed as threatened  
table(Prioritization.combined$IUCNstatus)["Threatened"]*100/nrow(Prioritization.combined)
# and included x% of all Data Deficient species. 
table(Prioritization.combined$IUCNstatus)["DD"]*100/nrow(data[which(data$IUCNstatus=="DD"),])

# tree species (x% of all known tree species in the world)
GlobalTreeSearch = read.table("global_tree_search_trees_1_7.csv",header = T,sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
nrow(GlobalTreeSearch)
nrow(data)*100/nrow(GlobalTreeSearch)

# percentage of assessed species by GTA
(142+17510+4099+24255+7700)*100/nrow(GlobalTreeSearch)




#######
# get values for Result section # ----- paragraph 1
median(data[,"rate_tc_change_area"])
quantile(data[,"rate_tc_change_area"], .95)
max(data[,"rate_tc_change_area"]) * 16
max(data[,"rate_tc_change_area"])
View(data[which(data$rate_tc_change_area>6),c("Species","IUCNstatus","rate_tc_change_area")])

median(data[,"rate_hGFC"])
quantile(data[,"rate_hGFC"], .95)
max(data[,"rate_hGFC"]) * 21
max(data[,"rate_hGFC"])
View(data[which(data$rate_hGFC>4.5),c("Species","IUCNstatus","rate_hGFC")])

max(data[,"rate_cropland"])
median(data[,"rate_cropland"]) * 20
View(data[which(data$rate_cropland>2),c("Species","IUCNstatus","rate_cropland")])
# table(data[which(data$rate_cropland>1.5),"IUCNstatus"])
# length(data[which(data$rate_cropland>1.5),"IUCNstatus"])

max(data[,"rate_urban"])
median(data[,"rate_urban"]) * 21
View(data[which(data$rate_urban>1.2),c("Species","IUCNstatus","rate_urban")])
# table(data[which(data$rate_urban>1),"IUCNstatus"])
# length(data[which(data$rate_urban>1),"IUCNstatus"])

max(data[,"slope_fire"])
min(data[,"slope_fire"])
median(data[,"slope_fire"])
View(data[which(data$slope_fire>1.8),c("Species","IUCNstatus","slope_fire")])
#table(data[which(data$slope_fire>1),"IUCNstatus"])
View(data[which(data$slope_fire< -1.6),c("Species","IUCNstatus","slope_fire")])
#table(data[which(data$slope_fire< -1),"IUCNstatus"])

nrow(data[which(data$slope_tmin>0),])*100/nrow(data)
max(data[,"slope_tmin"])
(data[which(data$slope_tmin>0.3),c("Species","IUCNstatus","slope_tmin")])
# min(data[,"slope_tmin"])
# (data[which(data$slope_tmin< -0.2),c("Species","IUCNstatus","slope_tmin")])
# nrow(data[which(data$slope_tmin<0),])*100/nrow(data)
# median(data[,"slope_tmin"])

nrow(data[which(data$slope_tmax>0),])*100/nrow(data)
max(data[,"slope_tmax"])
(data[which(data$slope_tmax>0.2),c("Species","IUCNstatus","slope_tmax")])
# min(data[,"slope_tmax"])
# (data[which(data$slope_tmax< -0.15),c("Species","IUCNstatus","slope_tmax")])
# nrow(data[which(data$slope_tmax<0),])*100/nrow(data)
# median(data[,"slope_tmax"])

nrow(data[which(data$slope_vpd>0),])*100/nrow(data)
max(data[,"slope_vpd"])
(data[which(data$slope_vpd>15),c("Species","IUCNstatus","slope_vpd")])
# min(data[,"slope_vpd"])
# (data[which(data$slope_vpd< -6),c("Species","IUCNstatus","slope_vpd")])
# nrow(data[which(data$slope_vpd<0),])*100/nrow(data)
# median(data[,"slope_vpd"])

nrow(data[which(data$slope_vpd_sd>0),])*100/nrow(data)
max(data[,"slope_vpd_sd"])
(data[which(data$slope_vpd_sd>10),c("Species","IUCNstatus","slope_vpd_sd")])
# min(data[,"slope_vpd_sd"])
# (data[which(data$slope_vpd_sd< -5),c("Species","IUCNstatus","slope_vpd_sd")])
# nrow(data[which(data$slope_vpd_sd<0),])*100/nrow(data)
# median(data[,"slope_vpd_sd"])

nrow(data[which(data$slope_prec_sd>0),])*100/nrow(data)
max(data[,"slope_prec_sd"])
(data[which(data$slope_prec_sd>1.3),c("Species","IUCNstatus","slope_prec_sd")])
# min(data[,"slope_prec_sd"])
# (data[which(data$slope_prec_sd< -0.6),c("Species","IUCNstatus","slope_prec_sd")])
# nrow(data[which(data$slope_prec_sd<0),])*100/nrow(data)
# median(data[,"slope_prec_sd"])

nrow(data[which(data$slope_prec>0),])*100/nrow(data)
nrow(data[which(data$slope_prec<0),])*100/nrow(data)
max(data[,"slope_prec"])
(data[which(data$slope_prec>35),c("Species","IUCNstatus","slope_prec")])
min(data[,"slope_prec"])
(data[which(data$slope_prec< -35),c("Species","IUCNstatus","slope_prec")])



#######
# get values for Result section # ----- paragraph 2
c=rbind(data[which(data$rate_tc_change_area>(50/16)),],
        data[which(data$rate_hGFC>(50/21)),])
c=c[,c("Species","IUCNstatus")]
c=c[!duplicated(c$Species),]
length(unique(c$Species))
(table(c[,"IUCNstatus"])["Threatened"]+table(c[,"IUCNstatus"])["Vulnerable"])*100/nrow(c)
table(c[,"IUCNstatus"])["Not Threatened"]*100/nrow(c)
(table(c[,"IUCNstatus"])["DD"]+table(c[,"IUCNstatus"])["Not Evaluated"])*100/nrow(c)

c=read.table("Prioritization95.ClimateChange.txt",sep=",",dec=".",header=T)
nrow(c)
table(c[,"IUCNstatus"])["Threatened"]*100/nrow(c)
table(c[,"IUCNstatus"])["DD"]*100/nrow(c)
table(c[,"IUCNstatus"])["DD"] * 100 / table(data[,"IUCNstatus"])["DD"]

nrow(data[which(data$EOO_km2>20000 & data$IUCNstatus=="Not Threatened"),])*100/nrow(data[which(data$EOO_km2>20000),]) # species large EOO (> 20,000 km2) were Not Threatened (60.2%)
nrow(data[which(data$EOO_km2<5000 & data$IUCNstatus=="Not Threatened"),])*100/nrow(data[which(data$EOO_km2<5000),])#17.64841
nrow(data[which(data$EOO_km2<5000 & data$IUCNstatus=="Threatened"),])*100/nrow(data[which(data$EOO_km2<5000),])#25.55581
nrow(data[which(data$EOO_km2<5000 & data$IUCNstatus=="DD"),])*100/nrow(data[which(data$EOO_km2<5000),])#2.452441
nrow(data[which(data$EOO_km2<5000 & data$IUCNstatus=="Not Evaluated"),])*100/nrow(data[which(data$EOO_km2<5000),])#38.3681
nrow(data[which(data$EOO_km2<5000 & data$IUCNstatus=="Vulnerable"),])*100/nrow(data[which(data$EOO_km2<5000),])#15.97525



#######
# get values for Result section # ----- paragraph 3
idx=c(grep("slope_vpd",colnames(data)),
      grep("slope_prec",colnames(data)),
      grep("slope_tmin",colnames(data)),
      grep("slope_tmax",colnames(data)),
      grep("slope_fire",colnames(data)),
      grep("rate_urban",colnames(data)),
      grep("rate_hGFC",colnames(data)),
      grep("rate_tc_change_area",colnames(data)),
      grep("rate_cropland",colnames(data)))
remove=c(grep("0010",colnames(data)),grep("1020",colnames(data)))
idx = idx[-which(idx %in% remove)]
for(threat in idx){
  if(min(data[,threat]) < 0 ) {
    threshold.min = quantile(data[,threat],.05)
    print(paste0(colnames(data)[threat]," - Minimum threshold (0.05) = ",threshold.min))
    print(table(data[which(data[,threat]<threshold.min),"IUCNstatus"]))
    print(nrow(data[which(data[,threat]<threshold.min),]))
  }
  { threshold.max = quantile(data[,threat],.95)
    print(paste0(colnames(data)[threat]," - Maximum threshold (0.95) = ",threshold.max))
    print(table(data[which(data[,threat]>threshold.max),"IUCNstatus"]))
    print(nrow(data[which(data[,threat]>threshold.max),]))
  }
}

splist=c()
for(threat in idx){
  if(min(data[,threat]) < 0) splist=c(splist,data[which(data[,threat]<0),"Species"])
  splist=c(splist,data[which(data[,threat]>0),"Species"])
}
print(length(unique(splist)))

c=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
nrow(c[which(c$IUCNstatus=="DD"),])
nrow(c[which(c$IUCNstatus=="DD"),]) * 100 / nrow(data[which(data$IUCNstatus=="DD"),]) 
nrow(c[which(c$IUCNstatus=="Not Evaluated"),])
nrow(c[which(c$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(data[which(data$IUCNstatus=="Not Evaluated"),]) 
nrow(c[which(c$IUCNstatus=="Vulnerable"),])
nrow(c[which(c$IUCNstatus=="Vulnerable"),]) * 100 / nrow(data[which(data$IUCNstatus=="Vulnerable"),]) 
nrow(c[which(c$IUCNstatus=="Not Threatened"),])
nrow(c[which(c$IUCNstatus=="Not Threatened"),]) * 100 / nrow(data[which(data$IUCNstatus=="Not Threatened"),]) 
nrow(c[which(c$IUCNstatus=="Threatened"),])
nrow(c[which(c$IUCNstatus=="Threatened"),]) * 100 / nrow(data[which(data$IUCNstatus=="Threatened"),]) 

all.prioritization.lists = list.files("Prioritization95.",full.names = T)
if(length(grep("Combined",all.prioritization.lists))>0) all.prioritization.lists = all.prioritization.lists[-grep("Combined",all.prioritization.lists)]
if(length(grep("ClimateChange",all.prioritization.lists))>0){
  all.prioritization.lists = all.prioritization.lists[-grep("slope_p",all.prioritization.lists)]
  all.prioritization.lists = all.prioritization.lists[-grep("slope_v",all.prioritization.lists)]
  all.prioritization.lists = all.prioritization.lists[-grep("slope_t",all.prioritization.lists)]
}
sp.list=c()
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(nrow(x))
  sp.list=c(sp.list,x$Species)
}
sp.list=data.frame(Species=sp.list)
sp.list$nr=1
View(aggregate(nr~Species,data=sp.list,FUN="sum"))

dupl = sp.list[duplicated(sp.list),]
c=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
nrow(dupl) * 100 / nrow(c)

dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #288 species occur trice
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #30 species occur four times
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #2 species occur five times
dupl

poly = sf::read_sf("Global.polygon.shp")
poly1 = poly[which(poly$id=="Zanthoxylum_mezoneurispinosum"),]
poly2 = poly[which(poly$id=="Apodytes_geldenhuysii"),]
r = raster::stack("ThreatOverlayBinarized.tif")
s=r
s[s<6]=NA
S = raster::crop(s, raster::extent(-20,150,-50,25)) 
raster::plot(S,col="red")
raster::plot(poly1,add=T,col="blue")
raster::plot(poly2,add=T)
S = raster::crop(S, raster::extent(18,20,-35,-32)) 
R = raster::crop(r, raster::extent(18,20,-35,-32)) 
raster::plot(R)
raster::plot(poly1,add=T,col="blue")
raster::plot(poly2,add=T)

all.prioritization.lists = list.files("Prioritization95.",full.names = T)
tc=read.table(file=all.prioritization.lists[grep("tc_change",all.prioritization.lists)],sep=",",dec=".",header=T)
cc=read.table(file=all.prioritization.lists[grep("ClimateChange",all.prioritization.lists)],sep=",",dec=".",header=T)
df=read.table(file=all.prioritization.lists[grep("hGFC",all.prioritization.lists)],sep=",",dec=".",header=T)
cl=read.table(file=all.prioritization.lists[grep("crop",all.prioritization.lists)],sep=",",dec=".",header=T)
bu=read.table(file=all.prioritization.lists[grep("fire",all.prioritization.lists)],sep=",",dec=".",header=T)
length(which(tc$Species %in% cc$Species))
length(which(tc$Species %in% df$Species))
length(which(tc$Species %in% cl$Species))
length(which(tc$Species %in% bu$Species))

length(which(bu$Species %in% cc$Species))
length(which(bu$Species %in% cl$Species))
length(which(bu$Species %in% df$Species))



#######
# get values for Discussion section 
length(data[which(data$rate_tc_change_area>(50/16)),"IUCNstatus"])
length(data[which(data$rate_hGFC>(50/21)),"IUCNstatus"])

y=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
# x% of all candidate tree species for prioritization were already considered to be threatened 
nrow(y[which(y$IUCNstatus=="Threatened"),]) * 100 / nrow(y) 
# and the prioritization candidates include x% of all Data Deficient species 
nrow(y[which(y$IUCNstatus=="DD"),]) * 100 / nrow(data[which(data$IUCNstatus=="DD"),]) 
# and x% of all Not Evaluated species
nrow(y[which(y$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(data[which(data$IUCNstatus=="Not Evaluated"),]) 

# More alarming may be that only x% 
table(data[which(data$rate_tc_change_area>(50/16)),"IUCNstatus"])["Threatened"]*100/length(data[which(data$rate_tc_change_area>(50/16)),"IUCNstatus"])
# and x% of the 2148 and 578 tree species with more than 50% of their extent degraded due to tree cover decline and deforestation, respectively, are listed as Threatened on the IUCN Red List. 
table(data[which(data$rate_hGFC>(50/21)),"IUCNstatus"])["Threatened"]*100/length(data[which(data$rate_hGFC>(50/21)),"IUCNstatus"])

checktop10 = data[,c("Species","slope_tmin","slope_tmax",
                     "slope_vpd","slope_prec",
                     "slope_vpd_sd","slope_prec_sd")]
top10 = c()
top10 = c(top10,head(checktop10[order(checktop10$slope_tmin, decreasing = FALSE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_tmax, decreasing = FALSE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd, decreasing = FALSE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec, decreasing = FALSE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd_sd, decreasing = FALSE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec_sd, decreasing = FALSE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_tmin, decreasing = TRUE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_tmax, decreasing = TRUE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd, decreasing = TRUE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec, decreasing = TRUE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd_sd, decreasing = TRUE),"Species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec_sd, decreasing = TRUE),"Species"]))
dupl = duplicated(top10)
top10 = data.frame(Species = top10, Dupl = dupl)
# # check which species are duplicated, and thus are among 
# # the 10 most extreme changes for at least 2 of the climate change variables
# # all Columbian species assessed in 2020, same region
# year of IUCN assessment and EOO size 
data[which(data$Species=="Bauhinia_grandifolia"),] # LC 2020 218658
data[which(data$Species=="Guatteria_argentea"),] # LC 2017 847
data[which(data$Species=="Ardisia_cabrerae"),] # LC 2020 249
data[which(data$Species=="Ardisia_mcphersonii"),] # LC 2020 38
data[which(data$Species=="Zygia_transamazonica"),] # NE xx 33772
data[which(data$Species=="Annona_pachyantha"),] # 2018 657 VU due to logging
data[which(data$Species=="Clermontia_clermontioides"),] # name Haha, hawaii, EN due to climate change 2022
data[which(data$Species=="Coussarea_machadoana"),] # 2020 VU due to urban development and logging

data[which(data$Species=="Chamaedorea_christinae"),]
all.prioritization.lists = list.files("Prioritization.Small",full.names = T)
tc=read.table(file=all.prioritization.lists[1],sep=",",dec=".",header=T)
"Chamaedorea_christinae" %in% tc$Species

y=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
# the 16,949 species that were identified for prioritization include only x% of all Threatened species 
nrow(y[which(y$IUCNstatus=="Threatened"),]) * 100 / nrow(data[which(data$IUCNstatus=="Threatened"),]) 
# and x% of all Vulnerable species
nrow(y[which(y$IUCNstatus=="Vulnerable"),]) * 100 / nrow(data[which(data$IUCNstatus=="Vulnerable"),]) 

















####################################




nrow(x) # 32,090 species 
nrow(x[which(x$Assess_year>=2020),])*100/nrow(x) # 17% was assessed in or after 2020
nrow(x[which(x$Assess_year>=2010),])*100/nrow(x) # 63% was assessed in or after 2010
nrow(x[which(x$Assess_year>=2000),])*100/nrow(x) # 65% was assessed by the IUCN in or after the year 2000
nrow(x[which(x$Assess_year==-Inf),])*100/nrow(x) # 31% has never been assessed by the IUCN




NoDataSpp = read.table(file=paste0(OutputDir,"Prioritization.SmallRange.DataLacking.txt"),sep=",",dec=".",header=T)
length(unique(NoDataSpp$Species))
OutlierSpp = read.table(file=paste0(OutputDir,"Prioritization.RRCoutliers.txt"),sep=",",dec=".",header=T)
length(unique(OutlierSpp$ID))





###


# without climate change
splist=c()
colnames(x[,idx])
for(threat in idx[7:11]){
  if(min(x[,threat]) < 0) splist=c(splist,x[which(x[,threat]<0),"Species"])
  splist=c(splist,x[which(x[,threat]>0),"Species"])
}
print(length(unique(splist)))
print(length(unique(splist))*100/nrow(x))

# without climate change and fire
splist=c()
colnames(x[,idx])
for(threat in idx[8:11]){
  if(min(x[,threat]) < 0) splist=c(splist,x[which(x[,threat]<0),"Species"])
  splist=c(splist,x[which(x[,threat]>0),"Species"])
}
print(length(unique(splist)))
print(length(unique(splist))*100/nrow(x))


###

table(x[,"IUCNstatus"])
table(x[which(x$slope_tmax>0.09|x$slope_tmax< 0),"IUCNstatus"])
# for Tmax: out of 726 DD species 104 cross threshold 0.95 = 14.3%
table(x[which(x$slope_tmax>0.12|x$slope_tmax< -0.02),"IUCNstatus"])
# for Tmax: out of 726 DD species 48 cross threshold 0.99 = 6.8%

table(x[,"IUCNstatus"])
table(x[which(x$slope_vpd>7.12|x$slope_vpd< 0),"IUCNstatus"])
# for slope_vpd: out of 726 DD species 120 cross threshold 0.95 = 16.5%
table(x[which(x$slope_vpd>9.28|x$slope_vpd< -1.88),"IUCNstatus"])
# for slope_vpd: out of 726 DD species 47 cross threshold 0.99 = 6.5%

table(x[,"IUCNstatus"])
table(x[which(x$slope_vpd_sd>2.92|x$slope_vpd_sd< 0),"IUCNstatus"])
# for slope_vpd_sd: out of 692 DD species 126 cross threshold 0.95 = 17.4%
table(x[which(x$slope_vpd_sd>4.79|x$slope_vpd_sd< -1.21),"IUCNstatus"])
# for slope_vpd_sd: out of 692 DD species 52 cross threshold 0.99 = 7.2%



# out of all DD species x% was prioritized due to climate change
y=read.table(paste0(OutputDir,"Prioritization95.ClimateChange.txt"),sep=",",dec=".",header=T)
nrow(y[which(y$IUCNstatus=="DD"),]) * 100 / nrow(x[which(x$IUCNstatus=="DD"),]) 
nrow(y[which(y$IUCNstatus=="DD"),])
nrow(y[which(y$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(x[which(x$IUCNstatus=="NA"),]) 

y=read.table(paste0(OutputDir,"Prioritization95.Combined.txt"),sep=",",dec=".",header=T)
nrow(y[which(y$IUCNstatus=="DD"),]) * 100 / nrow(x[which(x$IUCNstatus=="DD"),]) 
nrow(y[which(y$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(x[which(x$IUCNstatus=="NA"),]) 
nrow(y[which(y$IUCNstatus=="Vulnerable"),]) * 100 / nrow(x[which(x$IUCNstatus=="Vulnerable"),]) 
nrow(y[which(y$IUCNstatus=="Not Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Not Threatened"),]) 
nrow(y[which(y$IUCNstatus=="Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Threatened"),]) 








### top 10 of species in climate list







######
# Length species lists
OutputDir = ifelse(Sys.info()[["nodename"]] == "GIS09", "E:/Coline_au710911/", "O:/Nat_Ecoinformatics/C_Write/_User/ColineBoonman_au710911/TREECHANGE_OutputData/")
all.prioritization.lists = list.files(OutputDir,"Prioritization99.",full.names = T)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(all.prioritization.lists[i])
  print(nrow(x))
}
all.prioritization.lists = list.files(OutputDir,"Prioritization95.",full.names = T)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(all.prioritization.lists[i])
  print(nrow(x))
}


cc=read.table(paste0(OutputDir,"Prioritization95.Combined.txt"),sep=",",dec=".",header=T)
nrow(cc)
cc$IUCNstatus=as.character(cc$IUCNstatus)
cc$IUCNstatus[which(cc$IUCNstatus=="CR")]="Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="DD")]="DD"
cc$IUCNstatus[which(cc$IUCNstatus=="EN")]="Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="EW")]="Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="Ecc")]="Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="LC")]="Not Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="LR/cd")]="Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="LR/lc")]="Not Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="LR/nt")]="Not Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="NT")]="Not Threatened"
cc$IUCNstatus[which(cc$IUCNstatus=="VU")]="Vulnerable"
cc$IUCNstatus[which(is.na(cc$IUCNstatus))]="NA"
cc$IUCNstatus=as.factor(cc$IUCNstatus)
table(cc$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)

x=read.table(paste0(OutputDir,"Prioritization99.Combined.txt"),sep=",",dec=".",header=T)
nrow(x)
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(x$IUCNstatus=="CR")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="DD")]="DD"
x$IUCNstatus[which(x$IUCNstatus=="EN")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EW")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EX")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LC")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/cd")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/lc")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/nt")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="NT")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="VU")]="Vulnerable"
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(x$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)

x=read.table(file=paste0(OutputDir,"Complete.Reality.txt"),sep=",",dec=".",header=T)
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(x$IUCNstatus=="CR")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="DD")]="DD"
x$IUCNstatus[which(x$IUCNstatus=="EN")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EW")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EX")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LC")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/cd")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/lc")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/nt")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="NT")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="VU")]="Vulnerable"
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(x$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)

x=read.table(paste0(OutputDir,"Prioritization99.ClimateChange.txt"),sep=",",dec=".",header=T)
nrow(x)
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(x$IUCNstatus=="CR")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="DD")]="DD"
x$IUCNstatus[which(x$IUCNstatus=="EN")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EW")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EX")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LC")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/cd")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/lc")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/nt")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="NT")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="VU")]="Vulnerable"
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)

x=read.table(paste0(OutputDir,"Prioritization95.ClimateChange.txt"),sep=",",dec=".",header=T)
nrow(x)
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(x$IUCNstatus=="CR")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="DD")]="DD"
x$IUCNstatus[which(x$IUCNstatus=="EN")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EW")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EX")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LC")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/cd")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/lc")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/nt")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="NT")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="VU")]="Vulnerable"
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)



all.prioritization.lists = list.files(path = OutputDir, full.names = T, recursive = F)[grep("Prioritization95.",list.files(path = OutputDir, full.names = F, recursive = F))]
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(all.prioritization.lists[i])
  print(nrow(x))
}
(1742*4+3484*2+2087+2607+1829+2468+3484)/11 # 2401 species per list






############# KS distribution test

data1 <- rpois(n=20, lambda=10)
data2 <- rpois(n=20, lambda=5)
data3 = rnorm(10000)
data4 = rnorm(50)
ks.test(data1, data2)
ks.test(data3, data4)
par(mfrow=c(1,2)); hist(data3);hist(data4)

ks.test(x$slope_vpd, x$slope_vpd[which(x$IUCNstatus=="Threatened")])
ks.test(x$slope_vpd, x$slope_vpd[which(x$IUCNstatus=="Not Threatened")])
ks.test(x$slope_vpd, x$slope_vpd[which(x$IUCNstatus=="Vulnerable")])
ks.test(x$slope_vpd, x$slope_vpd[which(x$IUCNstatus=="DD")])
ks.test(x$slope_vpd, x$slope_vpd[which(x$IUCNstatus=="NA")])
par(mfrow=c(1,2)); hist(x$slope_vpd);hist(x$slope_vpd[which(x$IUCNstatus=="Threatened")])

ks.test(x$slope_fire, x$slope_fire[which(x$IUCNstatus=="Threatened")])
ks.test(x$slope_fire, x$slope_fire[which(x$IUCNstatus=="Not Threatened")])
ks.test(x$slope_fire, x$slope_fire[which(x$IUCNstatus=="Vulnerable")])
ks.test(x$slope_fire, x$slope_fire[which(x$IUCNstatus=="DD")])
ks.test(x$slope_fire, x$slope_fire[which(x$IUCNstatus=="NA")])
par(mfrow=c(1,2)); hist(x$slope_fire);hist(x$slope_fire[which(x$IUCNstatus=="Threatened")])

ks.test(x$rate_crop, x$rate_crop[which(x$IUCNstatus=="Threatened")])
ks.test(x$rate_crop, x$rate_crop[which(x$IUCNstatus=="Not Threatened")])
ks.test(x$rate_crop, x$rate_crop[which(x$IUCNstatus=="Vulnerable")])
ks.test(x$rate_crop, x$rate_crop[which(x$IUCNstatus=="DD")])
ks.test(x$rate_crop, x$rate_crop[which(x$IUCNstatus=="NA")])
par(mfrow=c(1,2)); hist(x$rate_crop);hist(x$rate_crop[which(x$IUCNstatus=="Threatened")])
ks.test(x$rate_crop[which(x$IUCNstatus=="Threatened")], x$rate_crop[which(x$IUCNstatus=="Not Threatened")])
ks.test(x$rate_crop[which(x$IUCNstatus=="Threatened")], x$rate_crop[which(x$IUCNstatus=="Vulnerable")])
ks.test(x$rate_crop[which(x$IUCNstatus=="Threatened")], x$rate_crop[which(x$IUCNstatus=="DD")])
ks.test(x$rate_crop[which(x$IUCNstatus=="Threatened")], x$rate_crop[which(x$IUCNstatus=="NA")])
par(mfrow=c(1,2)); hist(x$rate_crop[which(x$IUCNstatus=="Not Threatened")]);hist(x$rate_crop[which(x$IUCNstatus=="Threatened")])








#################################
# numbers which is more urgent or occurring? climate change or human land use change?
y  =read.table(paste0(OutputDir,"Prioritization95.Combined.txt"),sep=",",dec=".",header=T)
cc =read.table(paste0(OutputDir,"Prioritization95.ClimateChange.txt"),sep=",",dec=".",header=T)
cro=read.table(paste0(OutputDir,"Prioritization95.rate_cropland.txt"),sep=",",dec=".",header=T)
def=read.table(paste0(OutputDir,"Prioritization95.rate_hGFC.txt"),sep=",",dec=".",header=T)
tcc=read.table(paste0(OutputDir,"Prioritization95.rate_tc_change_area.txt"),sep=",",dec=".",header=T)
urb=read.table(paste0(OutputDir,"Prioritization95.rate_urban.txt"),sep=",",dec=".",header=T)
fir=read.table(paste0(OutputDir,"Prioritization95.slope_fire.txt"),sep=",",dec=".",header=T)

hluc = rbind(cro,def,tcc,urb) # human land use change
hluc = hluc[!duplicated(hluc),]
nrow(hluc)
nrow(cc)

x=read.table(file=paste0(OutputDir,"RatesOfRecentChange.txt"),sep=",",dec=".",header=T)
x$EOO = x$EOOmedian / 1000000 #from m2 to km2
# create conservation status groups
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(x$IUCNstatus=="CR")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="DD")]="DD"
x$IUCNstatus[which(x$IUCNstatus=="EN")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EW")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="EX")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LC")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/cd")]="Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/lc")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="LR/nt")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="NT")]="Not Threatened"
x$IUCNstatus[which(x$IUCNstatus=="VU")]="Vulnerable"
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)

nrow(hluc[which(hluc$IUCNstatus=="DD"),]) * 100 / nrow(x[which(x$IUCNstatus=="DD"),]) 
nrow(hluc[which(hluc$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(x[which(x$IUCNstatus=="NA"),]) 
nrow(hluc[which(hluc$IUCNstatus=="Vulnerable"),]) * 100 / nrow(x[which(x$IUCNstatus=="Vulnerable"),]) 
nrow(hluc[which(hluc$IUCNstatus=="Not Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Not Threatened"),]) 
nrow(hluc[which(hluc$IUCNstatus=="Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Threatened"),]) 

nrow(cc[which(cc$IUCNstatus=="DD"),]) * 100 / nrow(x[which(x$IUCNstatus=="DD"),]) 
nrow(cc[which(cc$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(x[which(x$IUCNstatus=="NA"),]) 
nrow(cc[which(cc$IUCNstatus=="Vulnerable"),]) * 100 / nrow(x[which(x$IUCNstatus=="Vulnerable"),]) 
nrow(cc[which(cc$IUCNstatus=="Not Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Not Threatened"),]) 
nrow(cc[which(cc$IUCNstatus=="Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Threatened"),]) 

