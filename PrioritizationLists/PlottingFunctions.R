make.plots.bar.and.smoothline = function(window,data,eoo,FigureDir){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  if(window=="full"){idx=c(grep("slope_vpd",colnames(data)),
                           grep("slope_prec",colnames(data)),
                           grep("slope_tmin",colnames(data)),
                           grep("slope_tmax",colnames(data)),
                           grep("slope_fire",colnames(data)),
                           grep("rate_urban",colnames(data)),
                           grep("rate_deforestation",colnames(data)),
                           grep("rate_treecover_change_area",colnames(data)),
                           grep("rate_cropland",colnames(data)))
  remove=c(grep("0010",colnames(data)),grep("1020",colnames(data)))
  if(length(remove)==0)idx = idx
  if(length(remove)>0)idx = idx[-which(idx %in% remove)]}
  if(window=="0010"){idx=c(grep("0010",colnames(data)))}
  if(window=="1020"){idx=c(grep("1020",colnames(data)))}
  
  df=subset(data,select=c(IUCNstatus,eoo_km2))
  
  for(i in 1:length(idx)){
    print(i)
    d = cbind(df,data[,idx[i]])
    colnames(d)[3]="rate"
    if(eoo=="all") d=d
    if(eoo=="small") d=d[d$eoo_km2< 5000,]
    if(eoo=="medium") d=d[d$eoo_km2>5000 & d$eoo_km2<= 20000,]
    if(eoo=="large") d=d[d$eoo_km2>= 20000,]
    
    MAX = round(nrow(d[which(d$rate>0),])*100/nrow(d),1)
    if(min(d$rate,na.rm=T)<0) MIN = round(nrow(d[which(d$rate<0),])*100/nrow(d),1)
    
    extreme.up1 = quantile(d[,"rate"],.99,na.rm=T)
    extreme.low1 = quantile(d[,'rate'],.01,na.rm=T)
    extreme.up5 = quantile(d[,"rate"],.95,na.rm=T)
    extreme.low5 = quantile(d[,'rate'],.05,na.rm=T)
    
    summary(d$rate)
    if(max(d$rate,na.rm=T)>42){
      d$rate = round(d$rate/4)*4
      my_binwidth=4
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=42 & max(d$rate,na.rm=T)>16){
      d$rate = round(d$rate/2)*2
      my_binwidth=2
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=16  & max(d$rate,na.rm=T)>7){
      d$rate = round(d$rate)
      my_binwidth=1
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=7 & max(d$rate,na.rm=T)>2.7){
      d$rate = round(d$rate/0.5)*0.5
      my_binwidth=0.5
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=2.7 & max(d$rate,na.rm=T)>1.7 & min(d$rate,na.rm=T)<0){
      d$rate = round(d$rate/0.25)*0.25
      my_binwidth=0.25
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=2.7 & max(d$rate,na.rm=T)>1.7 & min(d$rate,na.rm=T)==0){
      d$rate = round(d$rate/0.1)*0.1
      my_binwidth=0.1
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=1.7 & max(d$rate,na.rm=T)>0.6){
      d$rate = round(d$rate/0.1)*0.1
      my_binwidth=0.1
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=0.6 & max(d$rate,na.rm=T)>0.35){
      d$rate = round(d$rate/0.05)*0.05
      my_binwidth=0.05
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    if(max(d$rate,na.rm=T)<=0.35 ){
      d$rate = round(d$rate/0.02)*0.02
      my_binwidth=0.02
      my_bins=5
      j=my_binwidth/4/2
      jj=j+j
      jjj=jj+j
    } 
    
    n1 = d[which(d$IUCNstatus=="NotThreatened"),]
    n2 = d[which(d$IUCNstatus=="Threatened"),]
    n3 = d[which(d$IUCNstatus=="Vulnerable"),]
    n4 = d[which(d$IUCNstatus=="DD"),]
    n5 = d[which(d$IUCNstatus=="NotEvaluated"),]
    
    z.all=as.data.frame(table(d$rate))
    colnames(z.all)=c("rate","n")
    z.all$rate=as.numeric(as.character(z.all$rate))
    maxy = 4000 + plyr::round_any(max(z.all$n),100, f = ceiling)
    
    z=as.data.frame(table(d$IUCNstatus,d$rate))
    colnames(z)=c("IUCNstatus","rate","n")
    z$rate=as.numeric(as.character(z$rate))
    z1 = z[which(z$IUCNstatus=="NotThreatened"),]
    z2 = z[which(z$IUCNstatus=="Threatened"),]
    z3 = z[which(z$IUCNstatus=="Vulnerable"),]
    z4 = z[which(z$IUCNstatus=="DD"),]
    z5 = z[which(z$IUCNstatus=="NotEvaluated"),]
    
    if(my_binwidth<=1 & my_binwidth!=0.25) my_binwidth2 = 1
    if(my_binwidth>1) my_binwidth2 = my_binwidth
    if(my_binwidth==0.25) my_binwidth2 = 1.3
    
    p = 
      ggplot(d, aes(x=rate)) +
      geom_histogram(bins=my_bins, binwidth = my_binwidth,position="identity", fill="lightgrey",alpha=1)+
      geom_vline(xintercept = 0, col="darkgrey", linetype=1, lwd=1)+
      
      geom_density(data=n1,aes(x=rate, y = ..density.. * (nrow(n1) * my_binwidth)),adjust=(log10(nrow(n1)) * my_binwidth2),stat="density",position = "identity",alpha=0.1,lwd=2, colour="#1b9e77",trim=T)+
      geom_density(data=n2,aes(x=rate, y = ..density.. * (nrow(n2) * my_binwidth)),adjust=(log10(nrow(n2)) * my_binwidth2),stat="density",position = "identity",alpha=0.1,lwd=2, colour="#d95f02",trim=T)+
      geom_density(data=n3,aes(x=rate, y = ..density.. * (nrow(n3) * my_binwidth)),adjust=(log10(nrow(n3)) * my_binwidth2),stat="density",position = "identity",alpha=0.1,lwd=2, colour="#7570b3",trim=T)+
      geom_density(data=n4,aes(x=rate, y = ..density.. * (nrow(n4) * my_binwidth)),adjust=(log10(nrow(n4)) * my_binwidth2),stat="density",position = "identity",alpha=0.1,lwd=2, colour="#737373",trim=T)+
      #geom_density(data=n5,aes(x=rate, y = ..density.. * (nrow(n5) * my_binwidth)),adjust=(log10(nrow(n5)) * my_binwidth2),stat="density",position = "identity",alpha=0.1,lwd=2, colour="black",trim=T)+
      
      #if points of 1 species over each other- plot them next to each other (rate + a bit)
      geom_point(data = z1[which(z1$n==1),], aes(x = rate+0,  y = n+0.1), pch=20,cex=4, colour="#1b9e77")+
      geom_point(data = z2[which(z2$n==1),], aes(x = rate+j,  y = n+0.1), pch=20,cex=4, colour="#d95f02")+
      geom_point(data = z3[which(z3$n==1),], aes(x = rate+jj, y = n+0.1), pch=20,cex=4, colour="#7570b3")+
      geom_point(data = z4[which(z4$n==1),], aes(x = rate+jjj,y = n+0.1), pch=20,cex=4, colour="#737373")+
      scale_y_continuous(trans = "log10", limits = c(1,maxy),expand = c(0, 0))+
      theme(axis.text=element_text(size=21,colour = "black"),
            axis.title=element_text(size=24,colour = "black"),
            axis.line=element_line(colour = "black"),
            axis.ticks=element_line(size=2,colour = "black"),
            panel.background=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())

    p=p + geom_segment(aes(x = extreme.up5, y = 1, xend = extreme.up5, yend = 1.5),col="black", linetype=2, lwd=1)
    if(min(d$rate,na.rm=T)<0) p=p + geom_segment(aes(x = extreme.low5, y = 1, xend = extreme.low5, yend = 1.5),col="black", linetype=2, lwd=1)
    if(str_detect(colnames(data)[idx[i]],"slope_tmin")) p = p+ labs(title="",x=bquote('Minimum Temp (�C'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"slope_tmax")) p = p+ labs(title="",x=bquote('Maximum Temp (�C'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"slope_fire")) p = p+ labs(title="",x=bquote('Burned area (%EOO'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"rate_urban")) p = p+ labs(title="",x=bquote('Built-up exp (%EOO'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"rate_cropland")) p = p+ labs(title="",x=bquote('Cropland exp (%EOO'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"rate_deforestation")) p = p+ labs(title="",x=bquote('Deforestation (%EOO'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"rate_treecover_change_area")) p = p+ labs(title="",x=bquote('Tree cover decl (%EOO'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"slope_vpd") & !str_detect(colnames(data)[idx[i]],"slope_vpd_sd")) p = p+ labs(title="",x=bquote('VPD (Pa'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"slope_vpd_sd")) p = p+ labs(title="",x=bquote('VPD Seas (Pa'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"slope_prec") & !str_detect(colnames(data)[idx[i]],"slope_prec_sd")) p = p+ labs(title="",x=bquote('Precipitation (mm'~year^ -1~')'), y = "Number of Species")
    if(str_detect(colnames(data)[idx[i]],"slope_prec_sd")) p = p+ labs(title="",x=bquote('Precipitation Seas (mm'~year^ -1~')'), y = "Number of Species")

    Sys.sleep(1)
    ggsave(p, file=paste0(FigureDir,"RecentChangeImages/bar.and.smoothline/Recent.Change.",colnames(data)[idx[i]],"__EOO",eoo,"__Window",window,".pdf"),
           width = 6, height = 6)
  }
  
  legend = 
    ggplot(d, aes(x=rate)) +
      annotate(geom="text", x=1.2, y=5, label="All",color="black",cex=15, hjust = "left")+
      annotate(geom="text", x=1.2, y=4, label="Not Threatened",color="black",cex=15, hjust = "left")+
      annotate(geom="text", x=1.2, y=3, label="Threatened",    color="black",cex=15, hjust = "left")+
      annotate(geom="text", x=1.2, y=2, label="Vulnerable",    color="black",cex=15, hjust = "left")+
      annotate(geom="text", x=1.2, y=1, label="Data Deficient",color="black",cex=15, hjust = "left")+
    #annotate(geom="text", x=1.2, y=1, label="Not Evaluated",color="black",cex=15, hjust = "left")+
    scale_y_continuous(limits = c(0,6))+
      scale_x_continuous(limits = c(0.4,5.2))+
      geom_point(x=1, y=5,pch=15,cex=15,col="lightgrey")+
      geom_point(x=1, y=4,pch=15,cex=15,col="#1b9e77")+
      geom_point(x=1, y=3,pch=15,cex=15,col="#d95f02")+
      geom_point(x=1, y=2,pch=15,cex=15,col="#7570b3")+
      geom_point(x=1, y=1,pch=15,cex=15,col="#737373")+
      #geom_point(x=1, y=1,pch=15,cex=15,col="black")+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
  ggsave(legend, file=paste0(FigureDir,"RecentChangeImages/bar.and.smoothline/Recent.Change.legend.pdf"),
         width = 6, height = 4)
}



make.complete = function(FigureDir,dir="RecentChangeImages/bar.and.smoothline"){
  all=list.files(paste0(FigureDir,dir),full.names=T)
  legend=image_read_pdf(paste0(FigureDir,dir,"/Recent.Change.legend.pdf"), density = 720)
  
  # make main figure
  new = all[grep("EOOall",all)]
  new = new[grep("Windowfull",new)]
  eoo = "EOOall"
  for(i in 1:length(new)){
    fig=image_read_pdf(new[i], density = 720)
    FigName = print(sub("_","",sub("__","",sub(eoo,"",sub(".*Recent.Change.","",substr(new[i], 133, nchar(new[i])-16))))))
    assign(FigName,fig)
  }
  img.all <-c(cropland,treecoverchange_area,urban,deforestation,
              tmin,tmax,fire,legend,
              vpd,vpd_sd,prec,prec_sd)
  IMG=image_montage(img.all,tile = '4x3',geometry = '+2+2')
  image_write(IMG, path = paste0(FigureDir,"Main.png"), format = "png")

  # make EOO differences SI human (1 figure)
  new = all[grep("Windowfull",all)]
  new = new[grep('(rate|fire)',new)]
  new.a = new[grep("EOOall",new)]
  new.s = new[grep("EOOsmall",new)]
  new.m = new[grep("EOOmedium",new)]
  new.l = new[grep("EOOlarge",new)]
  for(i in 1:length(new.a)){
    eoo = "EOOall"
    n=new.a
    fig=image_read_pdf(n[i], density = 720)
    FigName = print(sub("_","",sub("__","",sub(eoo,"",sub(".*Recent.Change.","",substr(n[i], 133, nchar(n[i])-16))))))
    assign(paste0("all",FigName),fig)
  }
  for(i in 1:length(new.s)){
    eoo = "EOOsmall"
    n=new.s
    fig=image_read_pdf(n[i], density = 720)
    FigName = print(sub("_","",sub("__","",sub(eoo,"",sub(".*Recent.Change.","",substr(n[i], 133, nchar(n[i])-16))))))
    assign(paste0("small",FigName),fig)
  }
  for(i in 1:length(new.m)){
    eoo = "EOOmedium"
    n=new.m
    fig=image_read_pdf(n[i], density = 720)
    FigName = print(sub("_","",sub("__","",sub(eoo,"",sub(".*Recent.Change.","",substr(n[i], 133, nchar(n[i])-16))))))
    assign(paste0("medium",FigName),fig)
  }
  for(i in 1:length(new.l)){
    eoo = "EOOlarge"
    n=new.l
    fig=image_read_pdf(n[i], density = 720)
    FigName = print(sub("_","",sub("__","",sub(eoo,"",sub(".*Recent.Change.","",substr(n[i], 133, nchar(n[i])-16))))))
    assign(paste0("large",FigName),fig)
  }
  img.all <-c(allcropland,alltreecoverchange_area,allurban,alldeforestation,allfire,
              largecropland,largetreecoverchange_area,largeurban,largedeforestation,largefire,
              mediumcropland,mediumtreecoverchange_area,mediumurban,mediumdeforestation,mediumfire,
              smallcropland,smalltreecoverchange_area,smallurban,smalldeforestation,smallfire,
              legend)
  IMG=image_montage(img.all,tile = '5x5',geometry = '+2+2')
  image_write(IMG, path = paste0(FigureDir,"SI.EOO.human.png"), format = "png")
  
  
  
    # make EOO window differences for each climate threat (6 figures)
  for(i in 1:6){  
    new = all[grep("Windowfull",all)]
    if(i==1) new = new[grep(c("max"),new)]
    if(i==2) new = new[grep(c("min"),new)]
    if(i==3){new = new[grep(c("vpd"),new)]; new = new[-grep(c("sd"),new)]}
    if(i==4){new = new[grep(c("vpd"),new)]; new = new[grep(c("sd"),new)]}
    if(i==5){new = new[grep(c("prec"),new)]; new = new[-grep(c("sd"),new)]}
    if(i==6){new = new[grep(c("prec"),new)]; new = new[grep(c("sd"),new)]}
    
    new0010 = all[grep("Window0010",all)]
    if(i==1) new0010 = new0010[grep(c("max"),new0010)]
    if(i==2) new0010 = new0010[grep(c("min"),new0010)]
    if(i==3){new0010 = new0010[grep(c("vpd"),new0010)]; new0010 = new0010[-grep(c("sd"),new0010)]}
    if(i==4){new0010 = new0010[grep(c("vpd"),new0010)]; new0010 = new0010[grep(c("sd"),new0010)]}
    if(i==5){new0010 = new0010[grep(c("prec"),new0010)]; new0010 = new0010[-grep(c("sd"),new0010)]}
    if(i==6){new0010 = new0010[grep(c("prec"),new0010)]; new0010 = new0010[grep(c("sd"),new0010)]}
    
    new1020 = all[grep("Window1020",all)]
    if(i==1) new1020 = new1020[grep(c("max"),new1020)]
    if(i==2) new1020 = new1020[grep(c("min"),new1020)]
    if(i==3){new1020 = new1020[grep(c("vpd"),new1020)]; new1020 = new1020[-grep(c("sd"),new1020)]}
    if(i==4){new1020 = new1020[grep(c("vpd"),new1020)]; new1020 = new1020[grep(c("sd"),new1020)]}
    if(i==5){new1020 = new1020[grep(c("prec"),new1020)]; new1020 = new1020[-grep(c("sd"),new1020)]}
    if(i==6){new1020 = new1020[grep(c("prec"),new1020)]; new1020 = new1020[grep(c("sd"),new1020)]}
    
    fig.full.a = image_read_pdf(new[grep("EOOall",new)], density = 720)
    fig.full.s = image_read_pdf(new[grep("EOOsmall",new)], density = 720)
    fig.full.m = image_read_pdf(new[grep("EOOmedium",new)], density = 720)
    fig.full.l = image_read_pdf(new[grep("EOOlarge",new)], density = 720)
    fig.0010.a = image_read_pdf(new0010[grep("EOOall",new0010)], density = 720)
    fig.0010.s = image_read_pdf(new0010[grep("EOOsmall",new0010)], density = 720)
    fig.0010.m = image_read_pdf(new0010[grep("EOOmedium",new0010)], density = 720)
    fig.0010.l = image_read_pdf(new0010[grep("EOOlarge",new0010)], density = 720)
    fig.1020.a = image_read_pdf(new1020[grep("EOOall",new1020)], density = 720)
    fig.1020.s = image_read_pdf(new1020[grep("EOOsmall",new1020)], density = 720)
    fig.1020.m = image_read_pdf(new1020[grep("EOOmedium",new1020)], density = 720)
    fig.1020.l = image_read_pdf(new1020[grep("EOOlarge",new1020)], density = 720)

    img.all <-c(fig.full.a,fig.0010.a,fig.1020.a,
                fig.full.l,fig.0010.l,fig.1020.l,
                fig.full.m,fig.0010.m,fig.1020.m,
                fig.full.s,fig.0010.s,fig.1020.s,
                legend)  
    IMG=image_montage(img.all,tile = '3x5',geometry = '+2+2')
    
    if(i==1)image_write(IMG, path = paste0(FigureDir,"SI.EOO.window.climate.Tmax.png"), format = "png")
    if(i==2)image_write(IMG, path = paste0(FigureDir,"SI.EOO.window.climate.Tmin.png"), format = "png")
    if(i==3)image_write(IMG, path = paste0(FigureDir,"SI.EOO.window.climate.vpd.png"), format = "png")
    if(i==4)image_write(IMG, path = paste0(FigureDir,"SI.EOO.window.climate.vpdsd.png"), format = "png")
    if(i==5)image_write(IMG, path = paste0(FigureDir,"SI.EOO.window.climate.prec.png"), format = "png")
    if(i==6)image_write(IMG, path = paste0(FigureDir,"SI.EOO.window.climate.precsd.png"), format = "png")
  }  
}