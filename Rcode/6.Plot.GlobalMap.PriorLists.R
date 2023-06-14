
#################################
## plotting species with AOO <10km2
excl = raster::raster("exclude.tif")
poly = sf::read_sf("Global.polygon.shp")
poly$idx=1:nrow(poly)
water = raster::raster(paste0(InputDir,"MOD44W/data/land5.tif"))

library(rworldmap)
library(cleangeo)
library(raster)
library(tidyr)
library(raster)

sPDF <- getMap();sPDF <- clgeo_Clean(sPDF)  ## Needed to fix up some non-closed polygons 
cont <-sapply(levels(sPDF$continent),
              FUN = function(i) {
                poly <- gUnionCascaded(subset(sPDF, continent==i))## Merge polygons within a continent
                poly <- spChFIDs(poly, i)## Give each polygon a unique ID
                SpatialPolygonsDataFrame(poly,data.frame(continent=i, row.names=i))## Make SPDF from SpatialPolygons object
              },USE.NAMES=TRUE)
cont <- Reduce(spRbind, cont);cont<-cont[!cont$continent=="Antarctica",]## Bind the 6 continent-level SPDFs into a single SPDF


addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}


# GlobalPrioritizationComplete95.tif (and all other prioritization maps) - created as follows:
y = read.table("Prioritization95.Combined.txt",sep=",",dec=".",header=T)
poly = sf::read_sf("Global.polygon.shp")
poly$sp = NA
Species = gsub(" ","_",y$Species)
poly$sp = ifelse(poly$ID %in% Species, "Yes","No")
sf::st_write(poly[which(poly$sp=="Yes"),], dsn = "Global.polygon.PrioritizationComplete95.shp", driver = "ESRI Shapefile",append=F,overwrite=T)
#
poly = "Global.polygon.PrioritizationComplete95.shp"
tif = "GlobalPrioritizationComplete95.WaterNotRemoved.tif"
water.extent = "-180 -90 180 84" # <xmin> <ymin> <xmax> <ymax> # raster::raster("land.tif") is based on MOD44W 
cmd = paste0(gdal_rasterize, ' -burn 1 -add -te ',water.extent,' -tr 0.01 0.01 -tap -co  COMPRESS=LZW "',poly,'" "',tif,'"')
system(cmd)
calc= "A+B" # all values of 255 are set to NA, rest (all 1) to 0
cmd = paste0(gdal_calc,# change values inside cells
             ' -A ','"','GlobalPrioritizationComplete95.WaterNotRemoved.tif" ',
             ' -B ','"','land.tif" ', # a map defining water and land surfance created with MOD44
             '--outfile=','"','GlobalPrioritizationComplete95.tif" ',
             '--calc=','"',calc,'" ')
system(cmd)
# the code above was then applied to each threat layer

###########
### Overlap climate and complete by different colors???
#
priority =stack("GlobalPrioritizationComplete95.tif")
urban =   stack("GlobalPrioritizationUrban95.tif")
tcd =     stack("GlobalPrioritizationTCD95.tif")
defor =   stack("GlobalPrioritizationDefor95.tif")
crop =    stack("GlobalPrioritizationCrop95.tif")
fire =    stack("GlobalPrioritizationFire95.tif")
cc =      stack("GlobalPrioritizationClimateChange95.tif")

greens =    colorRampPalette(c("gray50","salmon1","springgreen","springgreen3","#189e4e","#006d2c","grey25","black"))(100)
reds =      colorRampPalette(c("gray50","lightskyblue1","salmon1","salmon3","red","darkred","grey25","black"))(100)
blues =     colorRampPalette(c("gray50","tan1",       "lightskyblue1","steelblue1","blue","darkblue","black"))(100)
pinks =     colorRampPalette(c("gray50","khaki1",       "pink2","violet","deeppink","darkviolet","grey25","black"))(100)
yellows =   colorRampPalette(c("gray50","pink2",       "yellow","orange","tan3","tan4","grey25","black"))(100)
darkgreens =colorRampPalette(c("gray50","salmon1",       "aquamarine1","aquamarine3","aquamarine4","darkgreen","grey25","black"))(100)


pdf(file="GlobalPrioritizationAllseperate95.pdf", width = 18, height = 12)
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


########################################################################
########################################################################
# these files are at a lower resolution - instead of 0.01 degrees it was set to 1 degrees
urban =   stack("lowPrioritizationUrban95.tif")
tcd =     stack("lowPrioritizationTCD95.tif")
defor =   stack("lowPrioritizationDefor95.tif")
crop =    stack("lowPrioritizationCrop95.tif")
fire =    stack("lowPrioritizationFire95.tif")
cc =      stack("lowPrioritizationClimateChange95.tif")

alls = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02")

urban[urban>0]=1
tcd[tcd>0]=1
defor[defor>0]=1
crop[crop>0]=1
fire[fire>0]=1
cc[cc>0]=1

all = urban + tcd + defor + crop + fire + cc
allfactor = asFactor(all)

pdf(file=paste0(FigureDir,"GlobalPrioritizationAllcombined95.1to5.pdf"), width = 18, height = 12)
par(mfrow=c(1,1),mar=c(5, 1, 1, 1), mgp=c(0, 0, 0))
plot(allfactor, legend = F, colNA="transparent", axes=FALSE, box=FALSE, col=c(  
  "white","coral3",
  "lightcyan3","papayawhip",
  "royalblue",
  "forestgreen",
  "yellow"
))
plot(cont,add=T)

par(xpd = TRUE)
legend(
  "bottom",
  legend = c("0", "1", "2", "3", "4","5","6"),
  fill = c("white","coral3","lightcyan3","papayawhip",
           "royalblue",
           "forestgreen", "yellow"
  ),
  horiz = TRUE,
  inset = -0.175
)
dev.off()

