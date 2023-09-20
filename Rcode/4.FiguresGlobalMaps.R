rm(list = ls())
library(rworldmap)
library(cleangeo)
library(raster)
library(tidyr)

#################################
## plotting species with AOO <10km2
FigureDir = "Figures/"

sPDF <- getMap();sPDF <- clgeo_Clean(sPDF)  ## Needed to fix up some non-closed polygons 
cont <-sapply(levels(sPDF$continent),
              FUN = function(i) {
                poly <- gUnionCascaded(subset(sPDF, continent==i))## Merge polygons within a continent
                poly <- spChFIDs(poly, i)## Give each polygon a unique ID
                SpatialPolygonsDataFrame(poly,data.frame(continent=i, row.names=i))## Make SPDF from SpatialPolygons object
              },USE.NAMES=TRUE)
cont <- Reduce(spRbind, cont);cont<-cont[!cont$continent=="Antarctica",]## Bind the 6 continent-level SPDFs into a single SPDF

# Global species richness map - by polygon
global = raster::raster("GlobalSpeciesRichnessByPoly.tif")
YlBr = colorRampPalette(c("#fed98e", "#fe9929", "#d95f0e", "#993404","#000000"))(100)
pdf(file=paste0(FigureDir,"SpeciesRichnessMap.bypolygon.pdf"), width = 9, height = 6)
par(mfrow=c(1,1),mar=c(1, 1, 1, 1), mgp=c(0, 0, 0))
plot(log10(global), legend = T,
     colNA="white", col=YlBr,axes=FALSE, box=FALSE,
     main="species richness - polygons")
plot(cont, add=TRUE)
dev.off()


#
urban =   stack("SpListOcc_rate_urban.tif")
tcd =     stack("SpListOcc_rate_treecover_change_area.tif")
defor =   stack("SpListOcc_rate_deforestation.tif")
crop =    stack("SpListOcc_rate_cropland.tif")
fire =    stack("SpListOcc_slope_fire.tif")
cc =      stack("SpListOcc_ClimateChange.tif")
greens =    colorRampPalette(c("gray50","salmon1","springgreen","springgreen3","#189e4e","#006d2c","grey25","black"))(100)
reds =      colorRampPalette(c("gray50","lightskyblue1","salmon1","salmon3","red","darkred","grey25","black"))(100)
blues =     colorRampPalette(c("gray50","tan1",       "lightskyblue1","steelblue1","blue","darkblue","black"))(100)
pinks =     colorRampPalette(c("gray50","khaki1",       "pink2","violet","deeppink","darkviolet","grey25","black"))(100)
yellows =   colorRampPalette(c("gray50","pink2",       "yellow","orange","tan3","tan4","grey25","black"))(100)
darkgreens =colorRampPalette(c("gray50","salmon1",       "aquamarine1","aquamarine3","aquamarine4","darkgreen","grey25","black"))(100)
pdf(file=paste0(FigureDir,"GlobalPrioritizationAllseperate95.pdf"), width = 18, height = 12)
par(mfrow=c(2,3),mar=c(1, 1, 1, 1), mgp=c(0, 0, 0))
plot(urban, legend = T, main="Urban",colNA="transparent", axes=FALSE, box=FALSE, col=blues, zlim=c(1,NA))
plot(cont,add=T)
plot(cc, legend = T, main="Climate", colNA="transparent", axes=FALSE, box=FALSE, col=reds, zlim=c(1,NA))
plot(cont,add=T)
plot(tcd, legend = T, main="Tree Cover", colNA="transparent", axes=FALSE, box=FALSE, col=darkgreens, zlim=c(1,NA))
plot(cont,add=T)
plot(fire, legend = T, main="Burned Area", colNA="transparent", axes=FALSE, box=FALSE, col=pinks, zlim=c(1,NA))
plot(cont,add=T)
plot(defor, legend = T, main="Deforestation", colNA="transparent", axes=FALSE, box=FALSE, col=greens, zlim=c(1,NA))
plot(cont,add=T)
plot(crop, legend = T, main="Cropland", colNA="transparent", axes=FALSE, box=FALSE, col=yellows, zlim=c(1,NA))
plot(cont,add=T)
dev.off()


#
all =   raster("ThreatOverlayBinarized.tif")
allfactor = raster::as.factor(all)
pdf(file=paste0(FigureDir,"GlobalPrioritizationAllcombined95.1to5.pdf"), width = 18, height = 12)
par(mfrow=c(1,1),mar=c(5, 1, 1, 1), mgp=c(0, 0, 0))
plot(allfactor, legend = F, colNA="transparent", axes=FALSE, box=FALSE, 
     col=c("white","#D5C6D4","#B99EE4","#FF7294","#4169E1","#37BC94","#FFFF00")
     #c("white","coral3","lightcyan3","papayawhip","royalblue","forestgreen", "yellow"),
     )
plot(cont,add=T)

par(xpd = TRUE)
legend(
  "bottom",
  legend = c("0", "1", "2", "3", "4","5","6"),
  fill = c("white","#D5C6D4","#B99EE4","#FF7294","#4169E1","#37BC94","#FFFF00"),
          #c("white","coral3","lightcyan3","papayawhip","royalblue","forestgreen", "yellow"),
  horiz = TRUE,
  inset = -0.175
)
dev.off()





new =colorRampPalette(c("lightgrey","gray50","lightblue","#7570b3","black"))(100)#,"#1b9e77", "#7570b3"
new.zoom =colorRampPalette(c("lightgrey","khaki","yellow","darkseagreen2","lightblue","#7570b3","blue3","black"))(100)#,"#1b9e77", "#7570b3"
#
priority.relative =  raster("GlobalSpeciesNrSpByPoly_PriorityRelative.tif")
log10priority =  log10(raster("GlobalSpeciesRichnessByPoly.tif"))
log10priority.relative = log10(priority.relative)
sqrtpriority.relative = sqrt(priority.relative)
#
pdf(file=paste0(FigureDir,"GlobalMapThreatenedSpecies.pdf"), width = 24, height = 12)
par(mfrow=c(2,3),mar=c(1, 1, 1, 1), mgp=c(0, 0, 0))
plot(log10priority, legend = T, main="Absolute", colNA="transparent",axes=F, box=F, col=new)
plot(cont,add=T)
#
plot(priority.relative, legend = T, main="Relative", colNA="transparent",axes=F, box=F, col=new, zlim=c(0.0001,1))
plot(cont,add=T)
plot(priority.relative, legend = T, main="Relative SA", colNA="transparent", axes=F, box=F, col=new.zoom, zlim=c(0.0001,1), 
     xlim = c(-90,-60), ylim = c(-12, 17)) # -90,-50   -15,13
plot(cont,add=T)
plot(priority.relative, legend = T, main="Relative Mad",colNA="transparent", axes=F, box=F, col=new.zoom, zlim=c(0.0001,1), 
     xlim = c(42,53), ylim = c(-27, -11))
plot(cont,add=T)
plot(priority.relative, legend = T, main="Relative aust",colNA="transparent", axes=F, box=F, col=new.zoom, zlim=c(0.0001,1), 
     xlim = c(92,155), ylim = c(-23, 19))
plot(cont,add=T)
plot(priority.relative, legend = T, main="Relative california",colNA="transparent", axes=F, box=F, col=new.zoom, zlim=c(0.0001,1), 
     xlim = c(-130,-108), ylim = c(21, 45))
plot(cont,add=T)
dev.off()

