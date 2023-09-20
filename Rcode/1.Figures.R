rm(list = ls())
# define directions paths
FigureDir = "Figures/")

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
              names(cc)=="rate_deforestation"|
              names(cc)=="rate_treecover_change_area"|
              names(cc)=="rate_cropland"|
              names(cc)=="IUCNstatus"|
              names(cc)=="Assess_year"|
              names(cc)=="species"|
              names(cc)=="eoo_km2")
idx=c(idx,grep("0010",names(cc)),grep("1020",names(cc)))
NEW = cc[idx]
NEW$IUCNstatus=as.factor(NEW$IUCNstatus)


## how much are deforestation and tree cover change correlated?
# cor(NEW$rate_deforestation,NEW$rate_treecover_change_area)

# all together
make.plots.bar.and.smoothline(window="full",data=NEW,eoo="all",FigureDir) 
make.plots.bar.and.smoothline(window="full",data=NEW,eoo="small",FigureDir) 
make.plots.bar.and.smoothline(window="full",data=NEW,eoo="medium",FigureDir) 
make.plots.bar.and.smoothline(window="full",data=NEW,eoo="large",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW,eoo="all",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW,eoo="small",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW,eoo="medium",FigureDir) 
make.plots.bar.and.smoothline(window="0010",data=NEW,eoo="large",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW,eoo="all",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW,eoo="small",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW,eoo="medium",FigureDir) 
make.plots.bar.and.smoothline(window="1020",data=NEW,eoo="large",FigureDir) 
make.complete(FigureDir,dir="RecentChangeImages/bar.and.smoothline")




####   -   Other figures
cc = read.table("RatesOfRecentChange.txt", sep=",", header=T)
idx=which(    names(cc)=="slope_vpd_sd"|
                names(cc)=="slope_vpd"|
                names(cc)=="slope_prec_sd"|
                names(cc)=="slope_prec"|
                names(cc)=="slope_tmin"|
                names(cc)=="slope_tmax"|
                names(cc)=="slope_fire"|
                names(cc)=="rate_urban"|
                names(cc)=="rate_deforestation"|
                names(cc)=="rate_treecover_change_area"|
                names(cc)=="rate_cropland"|
                names(cc)=="species")
cc = cc[idx]
names(cc)[2:12]
names(cc)[2:12] = c("Burned area","Deforestation","Cropland expansion","Built-up expansion",
                    "Tree cover decline",
                    "VPD seasonality","VPD","Precipitation seasonality","Precipitation",
              "Minimum temperature","Maximum temperature"
              )


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
pdf(paste0(FigureDir,"Threat.correlation.numbers.pdf"),width=8.4,height=5.6)
corrplot(M, type="upper", order="hclust",method="col", #number
         addCoef.col = "black",
         col=col, tl.col="black", tl.srt=40,
         p.mat = p.mat, sig.level = 0.01,diag=FALSE) #
dev.off()


####################
## Venn diagram
sp.list = list()
sp.names=list()
for(threat in 2:12){
  sp = c()
  if(min(cc[,threat]) < 0 ) {
    threshold.min = quantile(cc[,threat],.05)
    sp = c(sp,cc$species[which(cc[,threat]<threshold.min)])
  }
  { threshold.max = quantile(cc[,threat],.95)
    sp = c(sp,cc$species[which(cc[,threat]>threshold.max)])
  }
  sp.list[[threat-1]] = which(cc$species %in% sp)
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
ggsave(p, file=paste0(FigureDir,"Venn.climate95.pdf"),width=8.4,height=8)

p=ggVennDiagram(sp.list[c("Burned area","Built-up expansion","Deforestation","Tree cover decline","Cropland expansion","Climate Change")],
                label = "count", label_alpha = 0,set_size = 5) + 
  scale_fill_gradient(low = "white", high = "darkgrey") +
  scale_color_manual(values = c("#e7298a","#7570b3","#1b9e77","#66a61e","#e6ab02","#d95f02"))+
  theme(legend.position = "none")+ scale_x_continuous(expand = expansion(mult = .2))
ggsave(p, file=paste0(FigureDir,"Venn.all95.pdf"),width=8.4,height=8)

