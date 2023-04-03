rm(list = ls())
# define directions paths
load("TreeChangeEnvData_2022_10_19.RData") #09_28


####################
## merge Rate of Change data to iucn data
names(out)
vpd_sd = out[[1]]
vpd_mean = out[[2]]
prec_sd = out[[3]]
prec_mean = out[[4]]
tmin = out[[5]]
tmax = out[[6]]
tmean = out[[7]]
fire = out[[8]]
urban = out[[9]]
hGFC = out[[10]]
tree_fraction = out[[11]]
tc_change_area = out[[12]]
cropland = out[[13]]

poly_fire = fire$polygon_m2 # still in m2, but since in % ok
poly_crop = cropland$polygon_m2
poly_urb = urban$polygon_m2
poly_hgfc = hGFC$polygon_m2
poly_tc = tc_change_area$polygon_m2 
median=apply(cbind(poly_fire,poly_crop,poly_urb,poly_hgfc,poly_tc), 1, median, na.rm=T)

#
vpd_sd$slope_vpd_sd = vpd_sd$mblm_slope
vpd_sd$slope_vpd_sd = ifelse(vpd_sd$mblm_slope_pval < 0.05, vpd_sd$slope_vpd_sd, 0)
#
vpd_mean$slope_vpd = vpd_mean$mblm_slope
vpd_mean$slope_vpd = ifelse(vpd_mean$mblm_slope_pval < 0.05, vpd_mean$slope_vpd, 0)
#
prec_sd$slope_prec_sd = prec_sd$mblm_slope
prec_sd$slope_prec_sd = ifelse(prec_sd$mblm_slope_pval < 0.05, prec_sd$slope_prec_sd, 0)
#
prec_mean$slope_prec = prec_mean$mblm_slope
prec_mean$slope_prec = ifelse(prec_mean$mblm_slope_pval < 0.05, prec_mean$slope_prec, 0)
#
tmin$slope_tmin = tmin$mblm_slope
tmin$slope_tmin = ifelse(tmin$mblm_slope_pval < 0.05, tmin$slope_tmin, 0)
#
tmax$slope_tmax = tmax$mblm_slope
tmax$slope_tmax = ifelse(tmax$mblm_slope_pval < 0.05, tmax$slope_tmax, 0)
#
tmean$slope_tmean = tmean$mblm_slope
tmean$slope_tmean = ifelse(tmean$mblm_slope_pval < 0.05, tmean$slope_tmean, 0)
#
fire$slope_fire = fire$mblm_slope# in m2
fire$slope_fire = ifelse(fire$mblm_slope_pval < 0.05, fire$slope_fire, 0)
fire$slope_fire = fire$slope_fire * 100 / median # still in m2, but since in % ok
#
urban$rate_urban = urban$urban_buildup_m2 *100 / median / 21
hGFC$rate_hGFC = hGFC$HansenGFCLoss_m2 *100 / median / 21
# tree_fraction
tc_change_area$rate_tc_change_area = tc_change_area$tc_max_change_2000_2015_m2 *100 / median / 16
cropland$rate_cropland = cropland$cropland_change_m2_2003_2019 *100 / median / 20

urban$perc.21y_urban = urban$urban_buildup_m2 *100 / median
hGFC$perc.21y_hGFC = hGFC$HansenGFCLoss_m2 *100 / median
tc_change_area$perc.16y_tc_change_area = tc_change_area$tc_max_change_2000_2015_m2 *100 / median 
cropland$perc.20y_cropland = cropland$cropland_change_m2_2003_2019 *100 / median

fire[which(is.na(fire$mblm_slope)),] # species with slope of NA is because all values are 0 - set slope to 0
fire$slope_fire = ifelse(is.na(fire$slope_fire),0, fire$slope_fire)
fire$EOOmedian = median

cc = Reduce(function(df1, df2) merge(df1, df2, by = "ID", all.x = TRUE),
            list(vpd_sd,
                 vpd_mean,
                 prec_sd,
                 prec_mean,
                 tmin,
                 tmax,
                 tmean,
                 fire,
                 urban,
                 hGFC,
                 tc_change_area,
                 cropland))






# load rest of the data
iucn = read.table("IUCNstatus/StatusList.txt", sep=",", header=T)
sp.list = read.csv ('SpeciesList/Species_wenyong_pep.csv')
sp.list = sp.list[which(sp.list$delete=="FALSE"),"species"]

# taxonomic harmonization
# - done in: 
# O:\Nat_Ecoinformatics\C_Write\_User\ColineBoonman_au710911\TREECHANGE_InputData\IUCNstatus
# 1.IUCNRedListTREECHANGE.NameHarmonization



# combine files
iucn = iucn[grep(" ",iucn$Species),]
iucn$ID=stringr::str_replace(iucn$Species," ","_")
cc = merge(cc,iucn, by=c("ID"),all=T)
cc$Assess_year = as.numeric(as.character(cc$Assess_year))
cc$IUCNstatus.recat=NA
cc[which(cc$IUCNstatus == "LR/lc"),"IUCNstatus.recat"] = "PNT"
cc[which(cc$IUCNstatus == "LR/nt"),"IUCNstatus.recat"] = "PNT"
cc[which(cc$IUCNstatus == "LR/cd"),"IUCNstatus.recat"] = "LT"
cc[which(cc$IUCNstatus == "CR"),"IUCNstatus.recat"] = "LT"
cc[which(cc$IUCNstatus == "EN"),"IUCNstatus.recat"] = "LT"
cc[which(cc$IUCNstatus == "VU"),"IUCNstatus.recat"] = "PT"
cc[which(cc$IUCNstatus == "NT"),"IUCNstatus.recat"] = "PNT"
cc[which(cc$IUCNstatus == "LC"),"IUCNstatus.recat"] = "PNT"
cc[which(cc$IUCNstatus == "DD"),"IUCNstatus.recat"] = "DD"
cc[which(cc$IUCNstatus == "EW"),"IUCNstatus.recat"] = "LT"
cc[which(cc$IUCNstatus == "EX"),"IUCNstatus.recat"] = "LT"
cc[which(is.na(cc$IUCNstatus)),"IUCNstatus.recat"] = "NotEvaluated"
cc$IUCNstatus.recat=as.factor(cc$IUCNstatus.recat)



#
# test outliers and flag them
Q1 = quantile(cc$slope_fire,0.01,na.rm=T)
Q3 = quantile(cc$slope_fire,0.99,na.rm=T)
IQR = Q3-Q1
cc$ID[which(cc$slope_fire < (Q1-(3*IQR)) | cc$slope_fire > (Q3+(3*IQR)) )]

Q1 = quantile(cc$rate_tc_change_area,0.01,na.rm=T)
Q3 = quantile(cc$rate_tc_change_area,0.99,na.rm=T)
IQR = Q3-Q1
cc$ID[which(cc$rate_tc_change_area < (Q1-(3*IQR)) | cc$rate_tc_change_area > (Q3+(3*IQR)) )]

Q1 = quantile(cc$rate_cropland,0.01,na.rm=T)
Q3 = quantile(cc$rate_cropland,0.99,na.rm=T)
IQR = Q3-Q1
cc$ID[which(cc$rate_cropland < (Q1-(3*IQR)) | cc$rate_cropland > (Q3+(3*IQR)) )]

Q1 = quantile(cc$rate_urban,0.01,na.rm=T)
Q3 = quantile(cc$rate_urban,0.99,na.rm=T)
IQR = Q3-Q1
cc$ID[which(cc$rate_urban < (Q1-(3*IQR)) | cc$rate_urban > (Q3+(3*IQR)) )]

Q1 = quantile(cc$rate_hGFC,0.01,na.rm=T)
Q3 = quantile(cc$rate_hGFC,0.99,na.rm=T)
IQR = Q3-Q1
cc$ID[which(cc$rate_hGFC < (Q1-(3*IQR)) | cc$rate_hGFC > (Q3+(3*IQR)) )]

# 5 unique names as outliers - set slopes to 0 for all
cc$outlier = NA
cc$outlier[which( cc$ID == "Entada_abyssinica"| #
                  cc$ID == "Clusia_viscida"|    #  
                  cc$ID == "Eugenia_liberiana"|
                  cc$ID == "Ocotea_acutifolia"|#
                  cc$ID == "Erythroxylum_cambodianum")] = "yes"










############
## create slopes of time window 2000 - 2010 and 2010 - 2020
library(mblm)
calc_mblm_slope_pval = function(df, row_grep_id, plot=FALSE){
  df$mblm_slope0010 = df$mblm_slope_pval0010 = df$mblm_slope1020 = df$mblm_slope_pval1020 = NA
  print(grep(row_grep_id,names(df),value=TRUE))
  for(i in 1:nrow(df)){
    all = grep(row_grep_id,names(df),value=TRUE)
    allnr = grep(row_grep_id,names(df))
    window0010 = c(allnr[grep(c("200"),all)],allnr[grep(c("2010"),all)])
    window1020 = c(allnr[grep(c("201"),all)],allnr[grep(c("2020"),all)])
    y1 = as.numeric(df[i,window0010])
    x1 = 1:length(y1)
    y2 = as.numeric(df[i,window1020])
    x2 = 1:length(y2)
    #print(i)
    if(sum(y1,na.rm = TRUE)>0 | sum(y1,na.rm = TRUE)<0){
      lm=mblm::mblm(y1~x1, repeated = TRUE)
      df$mblm_slope0010[i] = as.numeric(coef(lm)[2])
      df$mblm_slope_pval0010[i] = summary(lm)$coefficients[2,4]
    }
    if(sum(y2,na.rm = TRUE)>0 | sum(y2,na.rm = TRUE)<0){
      lm=mblm::mblm(y2~x2, repeated = TRUE)
      df$mblm_slope1020[i] = as.numeric(coef(lm)[2])
      df$mblm_slope_pval1020[i] = summary(lm)$coefficients[2,4]
    }
  }
  return(df)
}

new = calc_mblm_slope_pval(vpd_sd,"vpd_sd_")
new$slope_vpd_sd0010 = new$mblm_slope0010
new$slope_vpd_sd0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_vpd_sd0010, 0)
new$slope_vpd_sd1020 = new$mblm_slope1020
new$slope_vpd_sd1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_vpd_sd1020, 0)
vpd_sd=new

new = calc_mblm_slope_pval(vpd_mean,"vpd_mean_")
new$slope_vpd0010 = new$mblm_slope0010
new$slope_vpd0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_vpd0010, 0)
new$slope_vpd1020 = new$mblm_slope1020
new$slope_vpd1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_vpd1020, 0)
vpd_mean=new

new = calc_mblm_slope_pval(prec_sd,"pr_sd_")
new$slope_prec_sd0010 = new$mblm_slope0010
new$slope_prec_sd0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_prec_sd0010, 0)
new$slope_prec_sd1020 = new$mblm_slope1020
new$slope_prec_sd1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_prec_sd1020, 0)
prec_sd=new

new = calc_mblm_slope_pval(prec_mean,"pr_mean_")
new$slope_prec0010 = new$mblm_slope0010
new$slope_prec0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_prec0010, 0)
new$slope_prec1020 = new$mblm_slope1020
new$slope_prec1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_prec1020, 0)
prec_mean=new

#
new = calc_mblm_slope_pval(tmin,"tasmin_")
new$slope_tmin0010 = new$mblm_slope0010
new$slope_tmin0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_tmin0010, 0)
new$slope_tmin1020 = new$mblm_slope1020
new$slope_tmin1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_tmin1020, 0)
tmin=new

new = calc_mblm_slope_pval(tmax,"tasmax_")
new$slope_tmax0010 = new$mblm_slope0010
new$slope_tmax0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_tmax0010, 0)
new$slope_tmax1020 = new$mblm_slope1020
new$slope_tmax1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_tmax1020, 0)
tmax=new

new = calc_mblm_slope_pval(tmean,"tas_")
new$slope_tmean0010 = new$mblm_slope0010
new$slope_tmean0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_tmean0010, 0)
new$slope_tmean1020 = new$mblm_slope1020
new$slope_tmean1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_tmean1020, 0)
tmean=new
#
new = calc_mblm_slope_pval(fire,"fire_")
new$slope_fire0010 = new$mblm_slope0010
new$slope_fire0010 = ifelse(new$mblm_slope_pval0010 < 0.05, new$slope_fire0010, 0)
new$slope_fire1020 = new$mblm_slope1020
new$slope_fire1020 = ifelse(new$mblm_slope_pval1020 < 0.05, new$slope_fire1020, 0)
new$slope_fire0010 = new$slope_fire0010 * 100 / new$polygon_m2
new$slope_fire1020 = new$slope_fire1020 * 100 / new$polygon_m2 # still in m2, but since in % ok
fire=new
# not possible for the following variables, because they are rates not slopes
# urban
# hGFC
# cropland
# tc_change_area

# combine the new slopes/rates
new = Reduce(function(df1, df2) merge(df1, df2, by = "ID", all.x = TRUE),
             list(vpd_sd,
                  vpd_mean,
                  prec_sd,
                  prec_mean,
                  tmin,
                  tmax,
                  tmean,
                  fire))

# then merge with cc by species
idx = c(grep(c("ID"),names(new),value=T),grep(c("0010"),names(new),value=T),grep(c("1020"),names(new),value=T))
idxnr = c(grep(c("ID"),names(new),value=F),grep(c("0010"),names(new),value=F),grep(c("1020"),names(new),value=F))
idx = idxnr[-grep("mblm",idx)]
cc = merge(cc,new[,idx], by=c("ID"),all.x = T)













####################
## Getting data numbers for paper
check = cc

# ## not based on number of observations but based on gridcell area covered.
# # number of species with n<3 species (i.e. no polygon)
# nrow(check[which(check$n<3),])
# check = check[which(check$n>=3),]

# # number of species covering <10 grid cells (following rules to define Critically Endangered (AOO<10km2))
# x=read.table("Complete.txt",sep=",",dec=".",header=T)
# too.small.range.spp = x$ID[which(x$AOO<10)]
# too.small.range.spp = too.small.range.spp[!duplicated(too.small.range.spp)]
# length(too.small.range.spp)
# save.list = check[which(check$ID %in% too.small.range.spp),c("Species","IUCNstatus")]
# write.table(save.list,file="Prioritization.SmallRange.DataLacking.txt",sep=",",dec=".",row.names = F)
# '%!in%' <- function(x,y)!('%in%'(x,y))
# nrow(check)
# check=check[which(check$ID %!in% too.small.range.spp),]
# nrow(check)


# number of species with outliers
nrow(check[which(check$outlier=="yes"),])
check[which(check$outlier=="yes"),"ID"]

save.list = check[which(check$outlier=="yes"),c("ID","IUCNstatus")]
write.table(save.list,file="Prioritization.RRCoutliers.txt",sep=",",dec=".",row.names = F)

check = check[which(is.na(check$outlier)),]





####################
## Save the data

write.table(check,file="Complete.Reality.txt",sep=",",dec=".",row.names = F)

idx=which(    names(check)=="slope_vpd_sd"|
              names(check)=="slope_vpd"|
              names(check)=="slope_prec_sd"|
              names(check)=="slope_prec"|
              names(check)=="slope_tmin"|
              names(check)=="slope_tmax"|
              names(check)=="slope_tmean"|
              names(check)=="slope_fire"|
              names(check)=="rate_urban"|
              names(check)=="rate_hGFC"|
              names(check)=="rate_tc_change_area"|
              names(check)=="rate_cropland"|
              names(check)=="IUCNstatus.recat"|
              names(check)=="IUCNstatus"|
              names(check)=="Assess_year"|
              names(check)=="ID"|
              names(check)=="EOOmedian")
idx=c(idx,grep("0010",names(check)),grep("1020",names(check)))
check.toshare = check[,idx]
check.toshare = check.toshare[,-grep("tmean",names(check.toshare))]
check.toshare = check.toshare[,-grep("slope_fire0010",names(check.toshare))]
check.toshare = check.toshare[,-grep("slope_fire1020",names(check.toshare))]
colnames(check.toshare)[grep("EOO",names(check.toshare))]="EOO"

check.toshare$IUCNstatus=as.character(check.toshare$IUCNstatus)
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="CR")]="Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="DD")]="DD"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="EN")]="Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="EW")]="Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="EX")]="Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="LC")]="Not Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="LR/cd")]="Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="LR/lc")]="Not Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="LR/nt")]="Not Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="NT")]="Not Threatened"
check.toshare$IUCNstatus[which(check.toshare$IUCNstatus=="VU")]="Vulnerable"
check.toshare$IUCNstatus[which(is.na(check.toshare$IUCNstatus))]=NA
check.toshare$IUCNstatus=as.factor(check.toshare$IUCNstatus)

check.toshare = check.toshare[,-grep("Assess_year",names(check.toshare))]
check.toshare = check.toshare[,-grep("IUCNstatus.recat",names(check.toshare))]

write.table(check.toshare,file="RatesOfRecentChange.txt",sep=",",dec=".",row.names = F)
c = read.table("RatesOfRecentChange.txt", sep=",", header=T)







