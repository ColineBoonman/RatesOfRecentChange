rm(list = ls())
# define directions paths


####################
## Get Numbers

# # ---------- prepare data
data=read.table(file="RatesOfRecentChange.txt",sep=",",dec=".",header=T)
Prioritization.combined=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)

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
41835*100/nrow(GlobalTreeSearch) # original nr of species

# percentage of assessed species by GTA
(142+17510+4099+24255+7700)*100/nrow(GlobalTreeSearch)




#######
# get values for Result section # ----- paragraph 1
median(data[,"rate_treecover_change_area"])
quantile(data[,"rate_treecover_change_area"], .95)
max(data[,"rate_treecover_change_area"]) * 15
max(data[,"rate_treecover_change_area"])
View(data[which(data$rate_treecover_change_area>6.66),c("species","IUCNstatus","rate_treecover_change_area")])

median(data[,"rate_deforestation"])
quantile(data[,"rate_deforestation"], .95)
max(data[,"rate_deforestation"]) * 20
max(data[,"rate_deforestation"])
View(data[which(data$rate_deforestation>4.99),c("species","IUCNstatus","rate_deforestation")])

max(data[,"rate_cropland"])
median(data[,"rate_cropland"]) * 16
View(data[which(data$rate_cropland>2),c("species","IUCNstatus","rate_cropland")])
# table(data[which(data$rate_cropland>1.5),"IUCNstatus"])
# length(data[which(data$rate_cropland>1.5),"IUCNstatus"])

max(data[,"rate_urban"])
median(data[,"rate_urban"]) * 20
View(data[which(data$rate_urban>1.2),c("species","IUCNstatus","rate_urban")])
# table(data[which(data$rate_urban>1),"IUCNstatus"])
# length(data[which(data$rate_urban>1),"IUCNstatus"])

max(data[,"slope_fire"])
min(data[,"slope_fire"])
median(data[,"slope_fire"])
View(data[which(data$slope_fire>1.7),c("species","IUCNstatus","slope_fire")])
#table(data[which(data$slope_fire>1),"IUCNstatus"])
View(data[which(data$slope_fire< -1.6),c("species","IUCNstatus","slope_fire")])
#table(data[which(data$slope_fire< -1),"IUCNstatus"])

nrow(data[which(data$slope_tmin>0),])*100/nrow(data)
max(data[,"slope_tmin"])
(data[which(data$slope_tmin>0.3),c("species","IUCNstatus","slope_tmin")])
# min(data[,"slope_tmin"])
# (data[which(data$slope_tmin< -0.2),c("species","IUCNstatus","slope_tmin")])
# nrow(data[which(data$slope_tmin<0),])*100/nrow(data)
# median(data[,"slope_tmin"])

nrow(data[which(data$slope_tmax>0),])*100/nrow(data)
max(data[,"slope_tmax"])
(data[which(data$slope_tmax>0.2),c("species","IUCNstatus","slope_tmax")])
# min(data[,"slope_tmax"])
# (data[which(data$slope_tmax< -0.15),c("species","IUCNstatus","slope_tmax")])
# nrow(data[which(data$slope_tmax<0),])*100/nrow(data)
# median(data[,"slope_tmax"])

nrow(data[which(data$slope_vpd>0),])*100/nrow(data)
max(data[,"slope_vpd"])
(data[which(data$slope_vpd>15),c("species","IUCNstatus","slope_vpd")])
# min(data[,"slope_vpd"])
# (data[which(data$slope_vpd< -6),c("species","IUCNstatus","slope_vpd")])
# nrow(data[which(data$slope_vpd<0),])*100/nrow(data)
# median(data[,"slope_vpd"])

nrow(data[which(data$slope_vpd_sd>0),])*100/nrow(data)
max(data[,"slope_vpd_sd"])
(data[which(data$slope_vpd_sd>10),c("species","IUCNstatus","slope_vpd_sd")])
# min(data[,"slope_vpd_sd"])
# (data[which(data$slope_vpd_sd< -5),c("species","IUCNstatus","slope_vpd_sd")])
# nrow(data[which(data$slope_vpd_sd<0),])*100/nrow(data)
# median(data[,"slope_vpd_sd"])

nrow(data[which(data$slope_prec_sd>0),])*100/nrow(data)
max(data[,"slope_prec_sd"])
(data[which(data$slope_prec_sd>1.3),c("species","IUCNstatus","slope_prec_sd")])
# min(data[,"slope_prec_sd"])
# (data[which(data$slope_prec_sd< -0.6),c("species","IUCNstatus","slope_prec_sd")])
# nrow(data[which(data$slope_prec_sd<0),])*100/nrow(data)
# median(data[,"slope_prec_sd"])

nrow(data[which(data$slope_prec>0),])*100/nrow(data)
nrow(data[which(data$slope_prec<0),])*100/nrow(data)
max(data[,"slope_prec"])
(data[which(data$slope_prec>35),c("species","IUCNstatus","slope_prec")])
min(data[,"slope_prec"])
(data[which(data$slope_prec< -35),c("species","IUCNstatus","slope_prec")])

all.prioritization.lists = list.files("Prioritization95.",full.names = T)
tc=read.table(file=all.prioritization.lists[grep("treecover_change",all.prioritization.lists)],sep=",",dec=".",header=T)
cc=read.table(file=all.prioritization.lists[grep("ClimateChange",all.prioritization.lists)],sep=",",dec=".",header=T)
df=read.table(file=all.prioritization.lists[grep("deforestation",all.prioritization.lists)],sep=",",dec=".",header=T)
cl=read.table(file=all.prioritization.lists[grep("crop",all.prioritization.lists)],sep=",",dec=".",header=T)
bu=read.table(file=all.prioritization.lists[grep("fire",all.prioritization.lists)],sep=",",dec=".",header=T)
length(which(tc$species %in% cc$species))
length(which(tc$species %in% df$species))
length(which(tc$species %in% cl$species))
length(which(tc$species %in% bu$species))

length(which(bu$species %in% cc$species))
length(which(bu$species %in% cl$species))
length(which(bu$species %in% df$species))

#######
# get values for Result section # ----- paragraph 2
c=rbind(data[which(data$rate_treecover_change_area>(50/15)),],
        data[which(data$rate_deforestation>(50/20)),])
c=c[,c("species","IUCNstatus")]
c=c[!duplicated(c$species),]
length(unique(c$species))
(table(c[,"IUCNstatus"])["Threatened"]+table(c[,"IUCNstatus"])["Vulnerable"])*100/nrow(c)
table(c[,"IUCNstatus"])["NotThreatened"]*100/nrow(c)
(table(c[,"IUCNstatus"])["DD"]+table(c[,"IUCNstatus"])["NotEvaluated"])*100/nrow(c)

c=read.table("Prioritization95.ClimateChange.txt",sep=",",dec=".",header=T)
nrow(c)
table(c[,"IUCNstatus"])["Threatened"]*100/nrow(c)
table(c[,"IUCNstatus"])["DD"]*100/nrow(c)
table(c[,"IUCNstatus"])["DD"] * 100 / table(data[,"IUCNstatus"])["DD"]

nrow(data[which(data$eoo_km2>20000 & data$IUCNstatus=="NotThreatened"),])*100/nrow(data[which(data$eoo_km2>20000),]) # species large EOO (> 20,000 km2) were Not Threatened (60.2%)
nrow(data[which(data$eoo_km2<5000 & data$IUCNstatus=="NotThreatened"),])*100/nrow(data[which(data$eoo_km2<5000),])#17.64841
nrow(data[which(data$eoo_km2<5000 & data$IUCNstatus=="Threatened"),])*100/nrow(data[which(data$eoo_km2<5000),])#25.55581
nrow(data[which(data$eoo_km2<5000 & data$IUCNstatus=="DD"),])*100/nrow(data[which(data$eoo_km2<5000),])#2.452441
nrow(data[which(data$eoo_km2<5000 & data$IUCNstatus=="NotEvaluated"),])*100/nrow(data[which(data$eoo_km2<5000),])#38.3681
nrow(data[which(data$eoo_km2<5000 & data$IUCNstatus=="Vulnerable"),])*100/nrow(data[which(data$eoo_km2<5000),])#15.97525



#######
# get values for Result section # ----- paragraph 3
idx=c(grep("slope_vpd",colnames(data)),
      grep("slope_prec",colnames(data)),
      grep("slope_tmin",colnames(data)),
      grep("slope_tmax",colnames(data)),
      grep("slope_fire",colnames(data)),
      grep("rate_urban",colnames(data)),
      grep("rate_deforestation",colnames(data)),
      grep("rate_treecover_change_area",colnames(data)),
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

c=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
nrow(c[which(c$IUCNstatus=="DD"),])
nrow(c[which(c$IUCNstatus=="DD"),]) * 100 / nrow(data[which(data$IUCNstatus=="DD"),]) 
nrow(c[which(c$IUCNstatus=="NotEvaluated"),])
nrow(c[which(c$IUCNstatus=="NotEvaluated"),]) * 100 / nrow(data[which(data$IUCNstatus=="NotEvaluated"),]) 
nrow(c[which(c$IUCNstatus=="Vulnerable"),])
nrow(c[which(c$IUCNstatus=="Vulnerable"),]) * 100 / nrow(data[which(data$IUCNstatus=="Vulnerable"),]) 
nrow(c[which(c$IUCNstatus=="NotThreatened"),])
nrow(c[which(c$IUCNstatus=="NotThreatened"),]) * 100 / nrow(data[which(data$IUCNstatus=="NotThreatened"),]) 
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
  sp.list=c(sp.list,x$species)
}
sp.list=data.frame(species=sp.list)
sp.list$nr=1
View(aggregate(nr~species,data=sp.list,FUN="sum"))

dupl = sp.list[duplicated(sp.list),]
c=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
nrow(dupl) * 100 / nrow(c)

dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #281 species occur trice
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #28 species occur four times
dupl = dupl[duplicated(dupl),]
nrow(dupl[!duplicated(dupl),]) #3 species occur five times
dupl





#######
# get values for Discussion section 
length(data[which(data$rate_treecover_change_area>(50/15)),"IUCNstatus"])
length(data[which(data$rate_deforestation>(50/20)),"IUCNstatus"])

# More alarming may be that only x% 
table(data[which(data$rate_treecover_change_area>(50/15)),"IUCNstatus"])["Threatened"]*100/length(data[which(data$rate_treecover_change_area>(50/15)),"IUCNstatus"])
# and x% of the 2148 and 578 tree species with more than 50% of their extent degraded due to tree cover decline and deforestation, respectively, are listed as Threatened on the IUCN Red List. 
table(data[which(data$rate_deforestation>(50/20)),"IUCNstatus"])["Threatened"]*100/length(data[which(data$rate_deforestation>(50/20)),"IUCNstatus"])
#or
table(data[which(data$rate_treecover_change_area>(50/15)),"IUCNstatus"])
table(data[which(data$rate_deforestation>(50/20)),"IUCNstatus"])
(370+123)*100/(2293+549)

y=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
# x% of all candidate tree species for prioritization were already considered to be threatened 
nrow(y[which(y$IUCNstatus=="Threatened"),]) * 100 / nrow(y) 
# and the prioritization candidates include x% of all Data Deficient species 
nrow(y[which(y$IUCNstatus=="DD"),]) * 100 / nrow(data[which(data$IUCNstatus=="DD"),]) 
# and x% of all Not Evaluated species
nrow(y[which(y$IUCNstatus=="NotEvaluated"),]) * 100 / nrow(data[which(data$IUCNstatus=="NotEvaluated"),]) 


checktop10 = data[,c("species","slope_tmin","slope_tmax",
                     "slope_vpd","slope_prec",
                     "slope_vpd_sd","slope_prec_sd")]
top10 = c()
top10 = c(top10,head(checktop10[order(checktop10$slope_tmin, decreasing = FALSE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_tmax, decreasing = FALSE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd, decreasing = FALSE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec, decreasing = FALSE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd_sd, decreasing = FALSE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec_sd, decreasing = FALSE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_tmin, decreasing = TRUE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_tmax, decreasing = TRUE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd, decreasing = TRUE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec, decreasing = TRUE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_vpd_sd, decreasing = TRUE),"species"]))
top10 = c(top10,head(checktop10[order(checktop10$slope_prec_sd, decreasing = TRUE),"species"]))
dupl = duplicated(top10)
top10 = data.frame(species = top10, Dupl = dupl)
# # check which species are duplicated, and thus are among 
# # the 10 most extreme changes for at least 2 of the climate change variables
# # all Columbian species assessed in 2020, same region
# year of IUCN assessment and EOO size 
data[which(data$species=="Bauhinia_grandifolia"),] # LC 2020 218658
data[which(data$species=="Guatteria_argentea"),] # LC 2017 847
data[which(data$species=="Ardisia_cabrerae"),] # LC 2020 249
data[which(data$species=="Ardisia_mcphersonii"),] # LC 2020 38
data[which(data$species=="Zygia_transamazonica"),] # NE xx 33772
data[which(data$species=="Annona_pachyantha"),] # 2018 657 VU due to logging
data[which(data$species=="Clermontia_clermontioides"),] # name Haha, hawaii, EN due to climate change 2022
data[which(data$species=="Coussarea_machadoana"),] # 2020 VU due to urban development and logging

data[which(data$species=="Chamaedorea_christinae"),]
all.prioritization.lists = list.files("Prioritization.Small",full.names = T)
tc=read.table(file=all.prioritization.lists[1],sep=",",dec=".",header=T)
"Chamaedorea_christinae" %in% tc$species

y=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
# the 16,949 species that were identified for prioritization include only x% of all Threatened species 
nrow(y[which(y$IUCNstatus=="Threatened"),]) * 100 / nrow(data[which(data$IUCNstatus=="Threatened"),]) 
# and x% of all Vulnerable species
nrow(y[which(y$IUCNstatus=="Vulnerable"),]) * 100 / nrow(data[which(data$IUCNstatus=="Vulnerable"),]) 





####################################
# Methods
x=read.table(file="RatesOfRecentChange.txt",sep=",",dec=".",header=T)
nrow(x) # 32,090 species 
nrow(x[which(x$Assess_year>=2020),])*100/nrow(x) # 17% was assessed in or after 2020
nrow(x[which(x$Assess_year>=2010),])*100/nrow(x) # 63% was assessed in or after 2010
nrow(x[which(x$Assess_year>=2000),])*100/nrow(x) # 65% was assessed by the IUCN in or after the year 2000
nrow(x[which(x$Assess_year==-Inf | x$Assess_year<2000),])*100/nrow(x) # 34.6% has never been assessed by the IUCN
#


