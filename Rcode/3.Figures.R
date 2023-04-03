rm(list = ls())
#libraries
library(tidyr)
library(forcats)
library(ggridges)
library(ggplot2)
library(cowplot)
library(readr)
library(dplyr)
library(easyGgplot2)
library(splines)
library(ggVennDiagram)
library(magick)
library(pdftools)
library(stringr)
# functions
source("PlottingFunctions.R")

# get data
cc = read.table("RatesOfRecentChange.txt", sep=",", header=T)
cc$EOO = cc$EOO / 1000000 #from m2 to km2

####################
## Select the data
idx=which(  names(cc)=="slope_vpd_sd"|
              names(cc)=="slope_vpd"|
              names(cc)=="slope_prec_sd"|
              names(cc)=="slope_prec"|
              names(cc)=="slope_tmin"|
              names(cc)=="slope_tmax"|
              names(cc)=="slope_tmean"|
              names(cc)=="slope_fire"|
              names(cc)=="rate_urban"|
              names(cc)=="rate_hGFC"|
              names(cc)=="rate_tc_change_area"|
              names(cc)=="rate_cropland"|
              names(cc)=="IUCNstatus.recat"|
              names(cc)=="IUCNstatus"|
              names(cc)=="Assess_year"|
              names(cc)=="ID"|
              names(cc)=="EOO")
idx=c(idx,grep("0010",names(cc)),grep("1020",names(cc)))
NEW = cc[idx]
NEW$IUCNstatus = droplevels(NEW$IUCNstatus)

## how much are deforestation and tree cover change correlated?
# cor(NEW$rate_hGFC,NEW$rate_tc_change_area)

NEW_all = NEW
NEW_2000 = NEW_all[NEW_all$Assess_year>=2000,]
NEW_2010 = NEW_all[NEW_all$Assess_year>=2000 & NEW_all$Assess_year<=2010,]
NEW_2020 = NEW_all[NEW_all$Assess_year>=2010,]

# all together
make.plots.bar.and.smoothline(window="full",data=NEW_all,eoo="all",FigureDir) 
make.plots.bar.and.smoothline(window="full",data=NEW_all,eoo="small",FigureDir) 
make.plots.bar.and.smoothline(window="full",data=NEW_all,eoo="medium",FigureDir) 
make.plots.bar.and.smoothline(window="full",data=NEW_all,eoo="large",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW_all,eoo="all",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW_all,eoo="small",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW_all,eoo="medium",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW_all,eoo="large",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW_all,eoo="all",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW_all,eoo="small",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW_all,eoo="medium",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW_all,eoo="large",FigureDir) 
make.complete(FigureDir,dir="RecentChangeImages/bar.and.smoothline")

make.plots.ordered(window="full",data=NEW_all,eoo="all",FigureDir) 
make.plots.ordered(window="full",data=NEW_all,eoo="small",FigureDir) 
make.plots.ordered(window="full",data=NEW_all,eoo="medium",FigureDir) 
make.plots.ordered(window="full",data=NEW_all,eoo="large",FigureDir) 
make.plots.ordered(window="0010",data=NEW_2010,eoo="all",FigureDir) 
make.plots.ordered(window="0010",data=NEW_2010,eoo="small",FigureDir) 
make.plots.ordered(window="0010",data=NEW_2010,eoo="medium",FigureDir) 
make.plots.ordered(window="0010",data=NEW_2010,eoo="large",FigureDir) 
make.plots.ordered(window="1020",data=NEW_2020,eoo="all",FigureDir) 
make.plots.ordered(window="1020",data=NEW_2020,eoo="small",FigureDir) 
make.plots.ordered(window="1020",data=NEW_2020,eoo="medium",FigureDir) 
make.plots.ordered(window="1020",data=NEW_2020,eoo="large",FigureDir) 










cc = read.table("RatesOfRecentChange.txt", sep=",", header=T)
idx=which(    names(cc)=="slope_vpd_sd"|
                names(cc)=="slope_vpd"|
                names(cc)=="slope_prec_sd"|
                names(cc)=="slope_prec"|
                names(cc)=="slope_tmin"|
                names(cc)=="slope_tmax"|
                names(cc)=="slope_tmean"|
                names(cc)=="slope_fire"|
                names(cc)=="rate_urban"|
                names(cc)=="rate_hGFC"|
                names(cc)=="rate_tc_change_area"|
                names(cc)=="rate_cropland"|
                names(cc)=="ID")
cc = cc[idx]
names(cc)[2:12]
names(cc)[2:12] = c("VPD seasonality","VPD","Precipitation seasonality","Precipitation",
                    "Minimum temperature","Maximum temperature",
                    "Burned area","Build-up expansion","Deforestation",
                    "Tree cover decline","Cropland expansion")


####################
## Correlation plot
library(corrplot)
library(RColorBrewer)
M <-cor(cc[,2:12])
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(cc[,2:12])# matrix of the p-value of the correlation
col<- colorRampPalette(c("#d95f02", "white", "#7570b3"))(20)

pdf("Threat.correlation.colorsonly.pdf",width=8.4,height=5.6)
corrplot(M, type="upper", order="hclust",method="col", #number
         col=col, tl.col="black", tl.srt=40,
         p.mat = p.mat, sig.level = 0.01, insig = "blank") #diag=FALSE
dev.off()

pdf("Threat.correlation.numbers.pdf",width=8.4,height=5.6)
corrplot(M, type="upper", order="hclust",method="col", #number
         addCoef.col = "black",
         col=col, tl.col="black", tl.srt=40,
         p.mat = p.mat, sig.level = 0.01,diag=FALSE) #
dev.off()

library("PerformanceAnalytics")
chart.Correlation(cc[,2:12], histogram=TRUE, pch=19)


####################
## Venn diagram
sp.list = list()
sp.names=list()
for(threat in 2:12){
  sp = c()
  if(min(cc[,threat]) < 0 ) {
    threshold.min = quantile(cc[,threat],.01)
    sp = c(sp,cc$ID[which(cc[,threat]<threshold.min)])
  }
  { threshold.max = quantile(cc[,threat],.99)
    sp = c(sp,cc$ID[which(cc[,threat]>threshold.max)])
  }
  sp.list[[threat-1]] = which(cc$ID %in% sp)
  sp.names[[threat-1]] = sp
}
names(sp.list) = names(cc)[2:12]
names(sp.names) =names(cc)[2:12]

sp.list[[12]] = c(unlist(sp.list[c("VPD seasonality")]),unlist(sp.list[c("VPD")]),
                  unlist(sp.list[c("Maximum temperature")]), unlist(sp.list[c("Minimum temperature")]),
                  unlist(sp.list[c("Precipitation seasonality")]),unlist(sp.list[c("Precipitation")]))
names(sp.list)[12] = c("Climate Change")

p=ggVennDiagram(sp.list[c("Precipitation seasonality","Precipitation","VPD seasonality","VPD","Maximum temperature","Minimum temperature")],
                label = "count", label_alpha = 0,set_size = 5) + 
  scale_fill_gradient(low = "white", high = "darkgrey") +
  scale_color_manual(values = c("#66a61e","#1b9e77","#7570b3","#e7298a","#e6ab02","#d95f02"))+
  theme(legend.position = "none")+ scale_x_continuous(expand = expansion(mult = .2))
ggsave(p, file="Venn.climate.pdf",width=8.4,height=8)

p=ggVennDiagram(sp.list[c("Burned area","Build-up expansion","Deforestation","Tree cover decline","Cropland expansion","Climate Change")],
                label = "count", label_alpha = 0,set_size = 5) + 
  scale_fill_gradient(low = "white", high = "darkgrey") +
  scale_color_manual(values = c("#e7298a","#7570b3","#1b9e77","#66a61e","#e6ab02","#d95f02"))+
  theme(legend.position = "none")+ scale_x_continuous(expand = expansion(mult = .2))
ggsave(p, file="Venn.all.pdf",width=8.4,height=8)

