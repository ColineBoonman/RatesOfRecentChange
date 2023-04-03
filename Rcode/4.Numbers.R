rm(list = ls())

####################
## Get Numbers

x=read.table(file="RatesOfRecentChange.txt",sep=",",dec=".",header=T)
x$EOO = x$EOO / 1000000 #from m2 to km2

nrow(x) # 33,861 species 


#######
# get values for result section
max(x[,"rate_tc_change_area"])
median(x[,"rate_tc_change_area"])
View(x[which(x$rate_tc_change_area>6),c("ID","IUCNstatus","rate_tc_change_area")])
table(x[which(x$rate_tc_change_area>(50/16)),"IUCNstatus"])
length(x[which(x$rate_tc_change_area>(50/16)),"IUCNstatus"])

max(x[,"rate_cropland"])
median(x[,"rate_cropland"])
View(x[which(x$rate_cropland>2),c("ID","IUCNstatus","rate_cropland")])
table(x[which(x$rate_cropland>1.5),"IUCNstatus"])
length(x[which(x$rate_cropland>1.5),"IUCNstatus"])

max(x[,"rate_urban"])
median(x[,"rate_urban"])
View(x[which(x$rate_urban>1.2),c("ID","IUCNstatus","rate_urban")])
table(x[which(x$rate_urban>1),"IUCNstatus"])
length(x[which(x$rate_urban>1),"IUCNstatus"])

max(x[,"rate_hGFC"])
median(x[,"rate_hGFC"])
View(x[which(x$rate_hGFC>4.5),c("ID","IUCNstatus","rate_hGFC")])
table(x[which(x$rate_hGFC>(50/21)),"IUCNstatus"])
length(x[which(x$rate_hGFC>(50/21)),"IUCNstatus"])

max(x[,"slope_fire"])
min(x[,"slope_fire"])
median(x[,"slope_fire"])
(x[which(x$slope_fire>1.8),c("ID","IUCNstatus","slope_fire")])
table(x[which(x$slope_fire>1),"IUCNstatus"])
(x[which(x$slope_fire< -1.6),c("ID","IUCNstatus","slope_fire")])
table(x[which(x$slope_fire< -1),"IUCNstatus"])

max(x[,"slope_tmin"])
(x[which(x$slope_tmin>0.2),c("ID","IUCNstatus","slope_tmin")])
min(x[,"slope_tmin"])
(x[which(x$slope_tmin< -0.2),c("ID","IUCNstatus","slope_tmin")])
nrow(x[which(x$slope_tmin>0),])*100/nrow(x)
nrow(x[which(x$slope_tmin<0),])*100/nrow(x)
median(x[,"slope_tmin"])

max(x[,"slope_tmax"])
(x[which(x$slope_tmax>0.2),c("ID","IUCNstatus","slope_tmax")])
min(x[,"slope_tmax"])
(x[which(x$slope_tmax< -0.15),c("ID","IUCNstatus","slope_tmax")])
nrow(x[which(x$slope_tmax>0),])*100/nrow(x)
nrow(x[which(x$slope_tmax<0),])*100/nrow(x)
median(x[,"slope_tmax"])

max(x[,"slope_prec"])
max(x[,"slope_prec"])*20
(x[which(x$slope_prec>30),c("ID","IUCNstatus","slope_prec")])
min(x[,"slope_prec"])
(x[which(x$slope_prec< -40),c("ID","IUCNstatus","slope_prec")])
nrow(x[which(x$slope_prec>0),])*100/nrow(x)
nrow(x[which(x$slope_prec<0),])*100/nrow(x)
median(x[,"slope_prec"])

max(x[,"slope_prec_sd"])
(x[which(x$slope_prec_sd>1.3),c("ID","IUCNstatus","slope_prec_sd")])
min(x[,"slope_prec_sd"])
(x[which(x$slope_prec_sd< -0.6),c("ID","IUCNstatus","slope_prec_sd")])
nrow(x[which(x$slope_prec_sd>0),])*100/nrow(x)
nrow(x[which(x$slope_prec_sd<0),])*100/nrow(x)
median(x[,"slope_prec_sd"])

max(x[,"slope_vpd"])
(x[which(x$slope_vpd>15),c("ID","IUCNstatus","slope_vpd")])
min(x[,"slope_vpd"])
(x[which(x$slope_vpd< -6),c("ID","IUCNstatus","slope_vpd")])
nrow(x[which(x$slope_vpd>0),])*100/nrow(x)
nrow(x[which(x$slope_vpd<0),])*100/nrow(x)
median(x[,"slope_vpd"])

max(x[,"slope_vpd_sd"])
(x[which(x$slope_vpd_sd>11),c("ID","IUCNstatus","slope_vpd_sd")])
min(x[,"slope_vpd_sd"])
(x[which(x$slope_vpd_sd< -5),c("ID","IUCNstatus","slope_vpd_sd")])
nrow(x[which(x$slope_vpd_sd>0),])*100/nrow(x)
nrow(x[which(x$slope_vpd_sd<0),])*100/nrow(x)
median(x[,"slope_vpd_sd"])

###
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
  if(min(x[,threat]) < 0 ) {
    threshold.min = quantile(x[,threat],.01)
    print(paste0(colnames(x)[threat]," - Minimum threshold (0.01) = ",threshold.min))
    print(table(x[which(x[,threat]<threshold.min),"IUCNstatus"]))
    print(nrow(x[which(x[,threat]<threshold.min),]))
  }
  { threshold.max = quantile(x[,threat],.99)
    print(paste0(colnames(x)[threat]," - Maximum threshold (0.99) = ",threshold.max))
    print(table(x[which(x[,threat]>threshold.max),"IUCNstatus"]))
    print(nrow(x[which(x[,threat]>threshold.max),]))
  }
}

for(threat in idx){
  if(min(x[,threat]) < 0 ) {
    threshold.min = quantile(x[,threat],.05)
    print(paste0(colnames(x)[threat]," - Minimum threshold (0.05) = ",threshold.min))
    print(table(x[which(x[,threat]<threshold.min),"IUCNstatus"]))
    print(nrow(x[which(x[,threat]<threshold.min),]))
  }
  { threshold.max = quantile(x[,threat],.95)
    print(paste0(colnames(x)[threat]," - Maximum threshold (0.95) = ",threshold.max))
    print(table(x[which(x[,threat]>threshold.max),"IUCNstatus"]))
    print(nrow(x[which(x[,threat]>threshold.max),]))
  }
}

splist=c()
for(threat in idx){
  if(min(x[,threat]) < 0) splist=c(splist,x[which(x[,threat]<0),"Species"])
  splist=c(splist,x[which(x[,threat]>0),"Species"])
}
print(length(unique(splist)))
print(length(unique(splist))*100/nrow(x))

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
table(x[which(x$rate_tc_change_area>(50/16)),"IUCNstatus"])
# for tree cover decline: out of 3067 threatened species 372 have >50%EOOloss = 12.1%
# for tree cover decline: out of 16945 not threatened species 528 have >50%EOOloss = 3.1%

table(x[,"IUCNstatus"])
table(x[which(x$rate_hGFC>(50/21)),"IUCNstatus"])
# for deforestation: out of 3067 threatened species 166 have >50%EOOloss = 5.4%
# for deforestation: out of 16945 not threatened species 161 have >50%EOOloss = 1.0%

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
# both y files below can be found in the priorization list folder on Github
y=read.table("Prioritization95.ClimateChange.txt",sep=",",dec=".",header=T)
nrow(y[which(y$IUCNstatus=="DD"),]) * 100 / nrow(x[which(x$IUCNstatus=="DD"),]) 
nrow(y[which(y$IUCNstatus=="DD"),])
nrow(y[which(y$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(x[which(x$IUCNstatus=="NA"),]) 

y=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
nrow(y[which(y$IUCNstatus=="DD"),]) * 100 / nrow(x[which(x$IUCNstatus=="DD"),]) 
nrow(y[which(y$IUCNstatus=="Not Evaluated"),]) * 100 / nrow(x[which(x$IUCNstatus=="NA"),]) 
nrow(y[which(y$IUCNstatus=="Vulnerable"),]) * 100 / nrow(x[which(x$IUCNstatus=="Vulnerable"),]) 
nrow(y[which(y$IUCNstatus=="Not Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Not Threatened"),]) 
nrow(y[which(y$IUCNstatus=="Threatened"),]) * 100 / nrow(x[which(x$IUCNstatus=="Threatened"),]) 




######
nrow(x[which(x$EOO>20000 & x$IUCNstatus=="Not Threatened"),])*100/nrow(x[which(x$EOO>20000),])
# species large EOO (> 20,000 km2) were Not Threatened (56.7%)
nrow(x[which(x$EOO<5000 & x$IUCNstatus=="Not Threatened"),])*100/nrow(x[which(x$EOO<5000),])#15.3
nrow(x[which(x$EOO<5000 & x$IUCNstatus=="Threatened"),])*100/nrow(x[which(x$EOO<5000),])#27.1
nrow(x[which(x$EOO<5000 & x$IUCNstatus=="DD"),])*100/nrow(x[which(x$EOO<5000),])#3.5
nrow(x[which(x$EOO<5000 & x$IUCNstatus=="NA"),])*100/nrow(x[which(x$EOO<5000),])#39.9
nrow(x[which(x$EOO<5000 & x$IUCNstatus=="Vulnerable"),])*100/nrow(x[which(x$EOO<5000),])#14.1
# species small EOO (< 5,000 km2) were Threatened (8.8%).
nrow(x[which(x$EOO<5000),])#4257






### top 10 of species in climate list

checktop10 = x[,c("Species","slope_tmin","slope_tmax",
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
# Coussapoa batavorum LC
# Pentagonia tapacula LC
# Annona pachyantha VU due to logging
# Ardisia cabrerae LC
# Chamaedorea christinae - not found - already in paper
# Ardisia mcphersonii LC
# Clermontia clermontioides - names Haha, hawaii, EN due to climate change
# Nothocestrum breviflorum - CR in 1998! hawaii, no climate change





######
# Length species lists
all.prioritization.lists = list.files("Prioritization99.",full.names = T)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(all.prioritization.lists[i])
  print(nrow(x))
}
all.prioritization.lists = list.files("Prioritization95.",full.names = T)
for(i in 1:length(all.prioritization.lists)){
  x=read.table(file=all.prioritization.lists[i],sep=",",dec=".",header=T)
  print(all.prioritization.lists[i])
  print(nrow(x))
}


cc=read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
nrow(cc)
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(cc$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)

x=read.table(file="RatesOfRecentChange.txt",sep=",",dec=".",header=T)
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(x$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)

x=read.table("Prioritization95.ClimateChange.txt",sep=",",dec=".",header=T)
nrow(x)
x$IUCNstatus=as.character(x$IUCNstatus)
x$IUCNstatus[which(is.na(x$IUCNstatus))]="NA"
x$IUCNstatus=as.factor(x$IUCNstatus)
table(x$IUCNstatus)*100/nrow(x)


