rm(list = ls())
# define directions paths
load("TreeChangeEnvData_2022_10_19.RData") #09_28


####################
## Question 1 - Do polygons compare?
names(out)
fire = out[[8]]
urban = out[[9]]
hGFC = out[[10]]
tree_fraction = out[[11]]
tc_change_area = out[[12]]
cropland = out[[13]]
#
fire = fire$polygon_m2 # still in m2, but since in % ok
crop = cropland$polygon_m2
urb = urban$polygon_m2
hgfc = hGFC$polygon_m2
tc = tc_change_area$polygon_m2 

## polygon size from GEE compared to themselves
par(mfrow=c(1,5))
plot(fire/1000000,crop/1000000,ylab="cropland poly",xlab="cropland poly");abline(1,1,col="red")
plot(crop/1000000,urb/1000000,ylab="cropland poly",xlab="cropland poly");abline(1,1,col="red")
plot(urb/1000000,hgfc/1000000,ylab="cropland poly",xlab="cropland poly");abline(1,1,col="red")
plot(hgfc/1000000,tc/1000000,ylab="cropland poly",xlab="cropland poly");abline(1,1,col="red")
plot(tc/1000000,fire/1000000,ylab="cropland poly",xlab="cropland poly");abline(1,1,col="red")
check=as.data.frame(fire/1000000)
colnames(check)[1]="fire"
check$tc=tc/1000000
check$crop=crop/1000000
check$urb=urb/1000000
check$hgfc=hgfc/1000000
check$ID=cropland$ID
# which species are outliers? outside e+01 or e-01
check$test1= check$fire/check$crop #Eucalyptus_molyneuxii Entada_abyssinica Quararibea_turbinata
check$test2= check$fire/check$urb #Eucalyptus_molyneuxii Entada_abyssinica Clusia_viscida Prunus_avium
check$test3= check$fire/check$hgfc #Eucalyptus_molyneuxii Gymnosporia_arbutifolia Blumea_balsamifera Nyctanthes_arbor-tristis
check$test4= check$fire/check$tc #Eucalyptus_molyneuxii Phyllocladus_aspleniifolius
check$test5= check$crop/check$urb #Prunus_avium Eucalyptus_molyneuxii
check$test6= check$crop/check$hgfc #Entada_abyssinica Gymnosporia_arbutifolia Blumea_balsamifera Nyctanthes_arbor-tristis
check$test7= check$crop/check$tc #Entada_abyssinica Eucalyptus_molyneuxii
check$test8= check$urb/check$hgfc #Prunus_avium Entada_abyssinica Gymnosporia_arbutifolia Blumea_balsamifera Clusia_viscida Nyctanthes_arbor-tristis
check$test9= check$urb/check$tc #Prunus_avium Entada_abyssinica Clusia_viscida
check$test0= check$hgfc/check$tc #Gymnosporia_arbutifolia Blumea_balsamifera Nyctanthes_arbor-tristis Acronychia_pubescens
View(check)
## Answer QUestion 1: not really, sometimes there is a outliying value 

# Check use of median polygon size
check$median=apply(check[,c("fire","crop","urb","hgfc","tc")], 1, median, na.rm=T)
# check is sometimes polygon size is not correcly calculated and using median makes outliers less
par(mfrow=c(1,3))
# outliers defined as > 10 or < 0.1 the rate of all other species 
# 
fire = out[[8]]
fire$slope_fire = fire$mblm_slope# in m2
fire$slope_fire = ifelse(fire$mblm_slope_pval < 0.05, fire$slope_fire, 0)
fire$slope_fire = fire$slope_fire * 100 / fire$polygon_m2 # still in m2, but since in % ok
fire$slope_new  = fire$slope_fire/1000000 * 100 / check$median
plot(fire$slope_fire,fire$slope_new);abline(1,1)
hist(fire$slope_fire);hist(fire$slope_new)
check$fireN = fire$slope_new
# two outliers: Entada_abyssinica Ocotea_acutifolia
# new outliers: Entada_abyssinica Pavetta_chapmanii Ocotea_acutifolia
# removed with new - new is much better

urban = out[[9]]
urban$rate_urban = urban$urban_buildup_m2 *100 / urban$polygon_m2 / 21
urban$perc.21y_urban = urban$urban_buildup_m2 *100 / urban$polygon_m2
urban$rate_new = urban$urban_buildup_m2/1000000 *100 / check$median / 21
plot(urban$rate_urban,urban$rate_new);abline(1,1)
hist(urban$rate_urban);hist(urban$rate_new)
check$urbN = urban$rate_new
# two outliers: Prunus_avium Ocotea_acutifolia
# 3 outliers with new: Entada_abyssinica Ocotea_acutifolia Clusia_viscida
# Ocotea_acutifolia is not an outlier?!
# new is worse -  but all other species are the same value

hGFC = out[[10]]
hGFC$rate_hGFC = hGFC$HansenGFCLoss_m2 *100 / hGFC$polygon_m2 / 21
hGFC$perc.21y_hGFC = hGFC$HansenGFCLoss_m2 *100 / hGFC$polygon_m2
hGFC$rate_new = hGFC$HansenGFCLoss_m2/1000000 *100 / check$median / 21
plot(hGFC$rate_hGFC,hGFC$rate_new);abline(1,1)
hist(hGFC$rate_hGFC);hist(hGFC$rate_new)
check$hgfcN = hGFC$rate_new
# 6 outliers: Entada_abyssinica Blumea_balsamifera 
# Gymnosporia_arbutifolia Clusia_viscida Ocotea_acutifolia Nyctanthes_arbor-tristis
# new 3 outliers: Entada_abyssinica Clusia_viscida Ocotea_acutifolia
# overall the same

tc_change_area = out[[12]]
tc_change_area$rate_tc_change_area = tc_change_area$tc_max_change_2000_2015_m2 *100 / tc_change_area$polygon_m2 / 16
tc_change_area$perc.16y_tc_change_area = tc_change_area$tc_max_change_2000_2015_m2 *100 / tc_change_area$polygon_m2 
tc_change_area$rate_new = tc_change_area$tc_max_change_2000_2015_m2/1000000 *100 / check$median / 16
plot(tc_change_area$rate_tc_change_area,tc_change_area$rate_new);abline(1,1)
hist(tc_change_area$rate_tc_change_area);hist(tc_change_area$rate_new)
check$tcN = tc_change_area$rate_new
# 3 outliers: Entada_abyssinica Clusia_viscida Ocotea_acutifolia
# 100% same

cropland = out[[13]]
cropland$rate_cropland = cropland$cropland_change_m2_2003_2019 *100 / cropland$polygon_m2 / 20
cropland$perc.20y_cropland = cropland$cropland_change_m2_2003_2019 *100 / cropland$polygon_m2
cropland$rate_new = cropland$cropland_change_m2_2003_2019/1000000 *100 / check$median / 20
plot(cropland$rate_cropland,cropland$rate_new);abline(1,1)
hist(cropland$rate_cropland);hist(cropland$rate_new)
check$cropN = cropland$rate_new
# 3 outliers: Quararibea_turbinata Ocotea_acutifolia Clusia_viscida
# new outliers" Entada_abyssinica Ocotea_acutifolia Clusia_viscida
# new is worse

# which species are outliers? outside e+01 or e-01
check$medtest1=fire$polygon_m2/1000000/check$median # Eucalyptus_molyneuxii
check$medtest2=urban$polygon_m2/1000000/check$median # Prunus_avium Entada_abyssinica Clusia_viscida
check$medtest3=hGFC$polygon_m2/1000000/check$median # Gymnosporia_arbutifolia Blumea_balsamifera Nyctanthes_arbor-tristis
check$medtest4=tc_change_area$polygon_m2/1000000/check$median #
check$medtest5=cropland$polygon_m2/1000000/check$median # Quararibea_turbinata Entada_abyssinica Eucalyptus_molyneuxii
View(check)
# do this more professionally when we really calculate the slopes below.











####################
## Question 2 - DO WE USE ALL OR ONLY SIGN SLOPES?

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
fire$slope_fire = fire$slope_fire * 100 / fire$polygon_m2 # still in m2, but since in % ok
#
urban$rate_urban = urban$urban_buildup_m2 *100 / urban$polygon_m2 / 21
hGFC$rate_hGFC = hGFC$HansenGFCLoss_m2 *100 / hGFC$polygon_m2 / 21
# tree_fraction
tc_change_area$rate_tc_change_area = tc_change_area$tc_max_change_2000_2015_m2 *100 / tc_change_area$polygon_m2 / 16
cropland$rate_cropland = cropland$cropland_change_m2_2003_2019 *100 / cropland$polygon_m2 / 20

urban$perc.21y_urban = urban$urban_buildup_m2 *100 / urban$polygon_m2
hGFC$perc.21y_hGFC = hGFC$HansenGFCLoss_m2 *100 / hGFC$polygon_m2
tc_change_area$perc.16y_tc_change_area = tc_change_area$tc_max_change_2000_2015_m2 *100 / tc_change_area$polygon_m2 
cropland$perc.20y_cropland = cropland$cropland_change_m2_2003_2019 *100 / cropland$polygon_m2

par(mfrow=c(4,3),mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0))
hist(vpd_mean$mblm_slope)
hist(vpd_mean$mblm_slope[which(vpd_mean$mblm_slope_pval<0.05)])
hist(vpd_mean$mblm_slope[which(vpd_mean$mblm_slope_pval>0.05)])
hist(vpd_sd$mblm_slope)
hist(vpd_sd$mblm_slope[which(vpd_sd$mblm_slope_pval<0.05)])
hist(vpd_sd$mblm_slope[which(vpd_sd$mblm_slope_pval>0.05)])
hist(prec_mean$mblm_slope)
hist(prec_mean$mblm_slope[which(prec_mean$mblm_slope_pval<0.05)])
hist(prec_mean$mblm_slope[which(prec_mean$mblm_slope_pval>0.05)])
hist(prec_sd$mblm_slope)
hist(prec_sd$mblm_slope[which(prec_sd$mblm_slope_pval<0.05)])
hist(prec_sd$mblm_slope[which(prec_sd$mblm_slope_pval>0.05)])
hist(tmean$mblm_slope)
hist(tmean$mblm_slope[which(tmean$mblm_slope_pval<0.05)])
hist(tmean$mblm_slope[which(tmean$mblm_slope_pval>0.05)])
hist(tmin$mblm_slope)
hist(tmin$mblm_slope[which(tmin$mblm_slope_pval<0.05)])
hist(tmin$mblm_slope[which(tmin$mblm_slope_pval>0.05)])
hist(tmax$mblm_slope)
hist(tmax$mblm_slope[which(tmax$mblm_slope_pval<0.05)])
hist(tmax$mblm_slope[which(tmax$mblm_slope_pval>0.05)])
# we will use all sign slopes only, since the distribution and ranges of slopes remains the same for all slopes and sign slopes -  no bias but TRUE trends
# par(mfrow=c(1,1))
# sp="Chrysolepis_chrysophylla"
# test = tmean[which(tmean$ID==sp),]
# test=gather(test,slope,value,tas_2000,tas_2001,      
#             tas_2002,tas_2003,tas_2004,       
#             tas_2005,tas_2006,tas_2007,       
#             tas_2008,tas_2009,tas_2010,       
#             tas_2011,tas_2012,tas_2013,       
#             tas_2014,tas_2015,tas_2016,       
#             tas_2017,tas_2018,tas_2019 , factor_key=TRUE)
# test$year = as.numeric(substring(test$slope,5))
# test=gather(test,slope,value,fire_m2_2001,fire_m2_2002,   
#             fire_m2_2003,fire_m2_2004,fire_m2_2005,   
#             fire_m2_2006,fire_m2_2007,fire_m2_2008,   
#             fire_m2_2009,fire_m2_2010,fire_m2_2011,   
#             fire_m2_2012,fire_m2_2013,fire_m2_2014,   
#             fire_m2_2015,fire_m2_2016,fire_m2_2017,   
#             fire_m2_2018,fire_m2_2019,fire_m2_2020, factor_key=TRUE)
# test$year = as.numeric(substring(test$slope,9))
# plot(test$year,test$value)








