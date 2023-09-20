rm(list = ls())
library(rworldmap)
library(cleangeo)
library(raster)
library(tidyr)
library(ggplot2)

#################################
## plotting species with AOO <10km2
OutputDir = getwd()
FigureDir = "Figures/")


sPDF <- getMap();sPDF <- clgeo_Clean(sPDF)  ## Needed to fix up some non-closed polygons 
cont <-sapply(levels(sPDF$continent),
              FUN = function(i) {
                poly <- gUnionCascaded(subset(sPDF, continent==i))## Merge polygons within a continent
                poly <- spChFIDs(poly, i)## Give each polygon a unique ID
                SpatialPolygonsDataFrame(poly,data.frame(continent=i, row.names=i))## Make SPDF from SpatialPolygons object
              },USE.NAMES=TRUE)
cont <- Reduce(spRbind, cont);cont<-cont[!cont$continent=="Antarctica",]## Bind the 6 continent-level SPDFs into a single SPDF

water100 = raster::raster("MOD44W/data/land5.res100.tif")

sp.list = read.table("Prioritization.SmallRange.txt",sep=",",dec=".",header=T)
sp.list=sp.list$species

Points = "Global.datapoints.shp"
p = sf::read_sf(Points)
point = sf::as_Spatial(p)
point$loc = paste0(round(point@coords[,1],0)," ",round(point@coords[,2],0))
POINT = point



# Global 'small' species richness map - by point
point = POINT
point = point[point$species %in% sp.list,]
point$overlap = 1
agg.point  = aggregate(overlap~species+loc,data=point,FUN="sum")
agg.point$NrSp = 1
agg.point  = aggregate(NrSp~loc,data=agg.point,FUN="sum")
agg.point = tidyr::separate(data = agg.point, col = loc, into = c("long", "lat"), sep = "\\ ")
point = sf::st_as_sf(agg.point, coords = c("long","lat"), remove = F)
pointraster = raster::rasterize(point, water100, field="NrSp",fun ="sum")
raster::plot(pointraster)
raster::writeRaster(pointraster,paste0(OutputDir,"SMall.GlobalSpeciesRichnessByPoint100km.tif"), format= "GTiff",overwrite=T)
#
global = raster::raster(paste0(OutputDir,"SMall.GlobalSpeciesRichnessByPoint100km.tif"))
YlBr = colorRampPalette(c("#fed98e", "#fe9929", "#d95f0e", "#993404","#000000"))(100)
pdf(file=paste0(FigureDir,"SMall.SpeciesRichnessMap.bypoint.pdf"), width = 9, height = 6)
par(mfrow=c(1,1),mar=c(1, 1, 1, 1), mgp=c(0, 0, 0))
plot(log10(global),  legend = T,
     colNA="white", col=YlBr,axes=FALSE, box=FALSE,
     main="species richness - points")
plot(cont, add=TRUE)
dev.off()






### --- Create global occurrence density raster by point
point = POINT
point$loc.sp = paste0(point$loc," ",point$species)
p = point[!duplicated(point$loc.sp),]
xy = p$loc[p$species %in% sp.list]
xy = xy[!duplicated(xy)]
p.keep = p[p$loc %in% xy,]

# merge with rates of recent change data
data=read.table(file=paste0(OutputDir,"RatesOfRecentChange.txt"),sep=",",dec=".",header=T)
df=merge(p.keep,data,by=c("species"),all.x=T)
DF=df@data

MEAN = function(x) {mean(x,na.rm=T)}
DF = aggregate(.~loc,data=DF[,c(2,4,7:29)],FUN=MEAN)

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMedian <- function(data) sapply(data, median, na.rm = TRUE)
for(i in 1:length(sp.list)){
  print(paste0(i,"  from  ",length(sp.list)))
  sp=sp.list[i]
  locations = p$loc[p$species==sp]
  new = DF[which(DF$loc %in% locations),]
  new = colMedian(new[,2:length(new)])
  if(i==1){
    NEW = data.frame(matrix(ncol=length(DF),nrow=0, dimnames=list(NULL, colnames(DF))))
    NEW=NEW[,-1]
    NEW=rbind(NEW,new)
    colnames(NEW)=colnames(DF)[-1]
  }
  if(i>1){
    NEW = rbind(NEW,new)
  }
}
NEW = NEW[,c(1:12)]



NEW$species=sp.list
# Global 'small' species richness map - by point
point = point[point$species %in% sp.list,]
point = merge(NEW,point,by=c("species"),all.x=T)

P = point
P = P[,3:14]
P = P[!duplicated(P),]
agg.point  = aggregate(.~loc,data=P,FUN="mean")
agg.point = tidyr::separate(data = agg.point, col = loc, into = c("long", "lat"), sep = "\\ ")
point = sf::st_as_sf(agg.point, coords = c("long","lat"), remove = F)

raster_vpdsd = raster::rasterize(point, water100, field="slope_vpd_sd",fun ="sum")
raster_vpd = raster::rasterize(point, water100, field="slope_vpd",fun ="sum")
raster_precsd = raster::rasterize(point, water100, field="slope_prec_sd",fun ="sum")
raster_prec = raster::rasterize(point, water100, field="slope_prec",fun ="sum")
raster_tmin = raster::rasterize(point, water100, field="slope_tmin",fun ="sum")
raster_tmax = raster::rasterize(point, water100, field="slope_tmax",fun ="sum")
raster_fire = raster::rasterize(point, water100, field="slope_fire",fun ="sum")
raster_urban = raster::rasterize(point, water100, field="rate_urban",fun ="sum")
raster_defor = raster::rasterize(point, water100, field="rate_deforestation",fun ="sum")
raster_tcc = raster::rasterize(point, water100, field="rate_treecover_change_area",fun ="sum")
raster_cropland = raster::rasterize(point, water100, field="rate_cropland",fun ="sum")
raster::plot(raster_vpdsd)
raster::writeRaster(raster_vpdsd,paste0(OutputDir,"SMall.RRC.raster_vpdsd.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_vpd,paste0(OutputDir,"SMall.RRC.raster_vpd.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_precsd,paste0(OutputDir,"SMall.RRC.raster_precsd.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_prec,paste0(OutputDir,"SMall.RRC.raster_prec.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_tmin,paste0(OutputDir,"SMall.RRC.raster_tmin.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_tmax,paste0(OutputDir,"SMall.RRC.raster_tmax.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_fire,paste0(OutputDir,"SMall.RRC.raster_fire.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_urban,paste0(OutputDir,"SMall.RRC.raster_urban.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_defor,paste0(OutputDir,"SMall.RRC.raster_defor.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_tcc,paste0(OutputDir,"SMall.RRC.raster_tcc.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_cropland,paste0(OutputDir,"SMall.RRC.raster_cropland.ByPoint100km.tif"), format= "GTiff",overwrite=T)
#
YlBr = colorRampPalette(c("darkblue","blue","green","#fed98e", "#fe9929", "#d95f0e", "#993404","purple","violet","#000000"))(100)
pdf(file=paste0(FigureDir,"SMall.RRC.bypoint.pdf"), width = 9, height = 6)
par(mfrow=c(4,3),mar=c(1, 1, 1, 1), mgp=c(0, 0, 0))
plot(raster_vpd,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="VPD");plot(cont, add=TRUE)
plot(raster_vpdsd,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="VPD sd");plot(cont, add=TRUE)
plot(raster_prec,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Precipitation");plot(cont, add=TRUE)
plot(raster_precsd,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Precipitation sd");plot(cont, add=TRUE)
plot(raster_tmin,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Minimum Temp");plot(cont, add=TRUE)
plot(raster_tmax,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Maximum Temp");plot(cont, add=TRUE)
plot(raster_fire,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Fire");plot(cont, add=TRUE)
plot(raster_urban,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Build-up expansion");plot(cont, add=TRUE)
plot(raster_defor,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Deforestation");plot(cont, add=TRUE)
plot(raster_tcc,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Tree cover decline");plot(cont, add=TRUE)
plot(raster_cropland,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Cropland expansion");plot(cont, add=TRUE)
dev.off()






###################################################################################
###################################################################################
### ALL SPECIES
###################################################################################
###################################################################################

### --- Create global occurrence density raster by point
point = POINT
point$loc.sp = paste0(point$loc," ",point$species)
p = point[!duplicated(point$loc.sp),]

#all
sp.list=read.table(paste0(OutputDir,"PolygonSpeciesList.txt"),header = T)
sp.list=sp.list$x
xy = p$loc[p$species %in% sp.list]
xy = xy[!duplicated(xy)]
p.keep = p[p$loc %in% xy,]


# remove all species that were excluded from the original analysis
sp.x = read.table(paste0(OutputDir,"Prioritization.SmallRange.txt"),sep=",",dec=".",header=T)
sp.x2 = read.table(paste0(OutputDir,"Prioritization.LinePolygon.txt"),sep=",",dec=".",header=T)
sp.x3 = read.table(paste0(OutputDir,"Prioritization.RRCoutliers.txt"),sep=",",dec=".",header=T)
sp.x=rbind(sp.x,sp.x2,sp.x3)
sp.x=sp.x$species
'%!in%' <- function(x,y)!('%in%'(x,y))
sp.list = sp.list[sp.list %!in% sp.x]

# merge with rates of recent change data
data=read.table(file=paste0(OutputDir,"RatesOfRecentChange.txt"),sep=",",dec=".",header=T)
df=merge(p.keep,data,by=c("species"),all.x=T)
DF=df@data

MEAN = function(x) {mean(x,na.rm=T)}
DF = aggregate(.~loc,data=DF[,c(2,4,7:29)],FUN=MEAN)
DF = DF [,1:13]
agg.point = tidyr::separate(data = DF, col = loc, into = c("long", "lat"), sep = "\\ ")
point = sf::st_as_sf(agg.point, coords = c("long","lat"), remove = F)

raster_vpdsd = raster::rasterize(point, water100, field="slope_vpd_sd",fun ="sum")
raster_vpd = raster::rasterize(point, water100, field="slope_vpd",fun ="sum")
raster_precsd = raster::rasterize(point, water100, field="slope_prec_sd",fun ="sum")
raster_prec = raster::rasterize(point, water100, field="slope_prec",fun ="sum")
raster_tmin = raster::rasterize(point, water100, field="slope_tmin",fun ="sum")
raster_tmax = raster::rasterize(point, water100, field="slope_tmax",fun ="sum")
raster_fire = raster::rasterize(point, water100, field="slope_fire",fun ="sum")
raster_urban = raster::rasterize(point, water100, field="rate_urban",fun ="sum")
raster_defor = raster::rasterize(point, water100, field="rate_deforestation",fun ="sum")
raster_tcc = raster::rasterize(point, water100, field="rate_treecover_change_area",fun ="sum")
raster_cropland = raster::rasterize(point, water100, field="rate_cropland",fun ="sum")
raster::plot(raster_vpdsd)
raster::writeRaster(raster_vpdsd,paste0(OutputDir,"SMall.ALLSP.RRC.raster_vpdsd.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_vpd,paste0(OutputDir,"SMall.ALLSP.RRC.raster_vpd.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_precsd,paste0(OutputDir,"SMall.ALLSP.RRC.raster_precsd.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_prec,paste0(OutputDir,"SMall.ALLSP.RRC.raster_prec.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_tmin,paste0(OutputDir,"SMall.ALLSP.RRC.raster_tmin.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_tmax,paste0(OutputDir,"SMall.ALLSP.RRC.raster_tmax.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_fire,paste0(OutputDir,"SMall.ALLSP.RRC.raster_fire.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_urban,paste0(OutputDir,"SMall.ALLSP.RRC.raster_urban.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_defor,paste0(OutputDir,"SMall.ALLSP.RRC.raster_defor.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_tcc,paste0(OutputDir,"SMall.ALLSP.RRC.raster_tcc.ByPoint100km.tif"), format= "GTiff",overwrite=T)
raster::writeRaster(raster_cropland,paste0(OutputDir,"SMall.ALLSP.RRC.raster_cropland.ByPoint100km.tif"), format= "GTiff",overwrite=T)
#
YlBr = colorRampPalette(c("darkblue","blue","green","#fed98e", "#fe9929", "#d95f0e", "#993404","purple","violet","#000000"))(100)
pdf(file=paste0(FigureDir,"SMall.ALLSP.RRC.bypoint.pdf"), width = 9, height = 6)
par(mfrow=c(4,3),mar=c(1, 1, 1, 1), mgp=c(0, 0, 0))
plot(raster_vpd,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="VPD");plot(cont, add=TRUE)
plot(raster_vpdsd,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="VPD sd");plot(cont, add=TRUE)
plot(raster_prec,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Precipitation");plot(cont, add=TRUE)
plot(raster_precsd,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Precipitation sd");plot(cont, add=TRUE)
plot(raster_tmin,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Minimum Temp");plot(cont, add=TRUE)
plot(raster_tmax,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Maximum Temp");plot(cont, add=TRUE)
plot(raster_fire,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Fire");plot(cont, add=TRUE)
plot(raster_urban,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Build-up expansion");plot(cont, add=TRUE)
plot(raster_defor,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Deforestation");plot(cont, add=TRUE)
plot(raster_tcc,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Tree cover decline");plot(cont, add=TRUE)
plot(raster_cropland,  legend = T,colNA="white", col=YlBr,axes=FALSE, box=T,
     main="Cropland expansion");plot(cont, add=TRUE)
dev.off()



###################################################################################
###################################################################################
### COMBINE THE TWO
###################################################################################
###################################################################################

rare.vpdsd=raster::raster(paste0(OutputDir,"SMall.RRC.raster_vpdsd.ByPoint100km.tif"))
rare.vpd=raster::raster(paste0(OutputDir,"SMall.RRC.raster_vpd.ByPoint100km.tif"))
rare.precsd=raster::raster(paste0(OutputDir,"SMall.RRC.raster_precsd.ByPoint100km.tif"))
rare.prec=raster::raster(paste0(OutputDir,"SMall.RRC.raster_prec.ByPoint100km.tif"))
rare.tmin=raster::raster(paste0(OutputDir,"SMall.RRC.raster_tmin.ByPoint100km.tif"))
rare.tmax=raster::raster(paste0(OutputDir,"SMall.RRC.raster_tmax.ByPoint100km.tif"))
rare.fire=raster::raster(paste0(OutputDir,"SMall.RRC.raster_fire.ByPoint100km.tif"))
rare.urban=raster::raster(paste0(OutputDir,"SMall.RRC.raster_urban.ByPoint100km.tif"))
rare.defor=raster::raster(paste0(OutputDir,"SMall.RRC.raster_defor.ByPoint100km.tif"))
rare.tcc=raster::raster(paste0(OutputDir,"SMall.RRC.raster_tcc.ByPoint100km.tif"))
rare.cropland=raster::raster(paste0(OutputDir,"SMall.RRC.raster_cropland.ByPoint100km.tif"))
all.vpdsd=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_vpdsd.ByPoint100km.tif"))
all.vpd=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_vpd.ByPoint100km.tif"))
all.precsd=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_precsd.ByPoint100km.tif"))
all.prec=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_prec.ByPoint100km.tif"))
all.tmin=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_tmin.ByPoint100km.tif"))
all.tmax=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_tmax.ByPoint100km.tif"))
all.fire=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_fire.ByPoint100km.tif"))
all.urban=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_urban.ByPoint100km.tif"))
all.defor=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_defor.ByPoint100km.tif"))
all.tcc=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_tcc.ByPoint100km.tif"))
all.cropland=raster::raster(paste0(OutputDir,"SMall.ALLSP.RRC.raster_cropland.ByPoint100km.tif"))

all.vpdsd[!is.na(rare.vpdsd)] = NA
all.vpd[!is.na(rare.vpd)] = NA
all.precsd[!is.na(rare.precsd)] = NA
all.prec[!is.na(rare.prec)] = NA
all.tmin[!is.na(rare.tmin)] = NA
all.tmax[!is.na(rare.tmax)] = NA
all.fire[!is.na(rare.fire)] = NA
all.urban[!is.na(rare.urban)] = NA
all.defor[!is.na(rare.defor)] = NA
all.tcc[!is.na(rare.tcc)] = NA
all.cropland[!is.na(rare.cropland)] = NA

pdf(file=paste0(FigureDir,"Boxplot.RRCdistribution.pdf"), width = 10, height = 7)
par(mfrow=c(3,4))
r.x = terra::as.data.frame(rare.vpdsd);r.x$Threat="VPD SD";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.vpdsd);a.x$Threat="VPD SD";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.vpd);r.x$Threat="VPD";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.vpd);a.x$Threat="VPD";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.precsd);r.x$Threat="Precipitation SD";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.precsd);a.x$Threat="Precipitation SD";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.prec);r.x$Threat="Precipitation";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.prec);a.x$Threat="Precipitation";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.tmin);r.x$Threat="Minimum Temp";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.tmin);a.x$Threat="Minimum Temp";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.tmax);r.x$Threat="Maximum Temp";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.tmax);a.x$Threat="Maximum Temp";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

####

r.x = terra::as.data.frame(rare.fire);r.x$Threat="Fire";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.fire);a.x$Threat="Fire";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.urban);r.x$Threat="Built-up exp";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.urban);a.x$Threat="Built-up exp";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.cropland);r.x$Threat="Cropland exp";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.cropland);a.x$Threat="Cropland exp";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.tcc);r.x$Threat="Tree Cover decline";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.tcc);a.x$Threat="Tree Cover decline";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

r.x = terra::as.data.frame(rare.defor);r.x$Threat="Deforestation";r.x=na.omit(r.x);colnames(r.x)[1]="RRC";r.x$What="Rare"
a.x = terra::as.data.frame(all.defor);a.x$Threat="Deforestation";a.x=na.omit(a.x);colnames(a.x)[1]="RRC";a.x$What="No Rare"
x = rbind(r.x,a.x)
boxplot(RRC~What,data=x, main=unique(x$Threat), ylab="RRC",xlab="") 
t.test(r.x$RRC,a.x$RRC,paired=F)

dev.off()

