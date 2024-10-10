##packages
library(ggplot2)
library(cowplot)
library(tidyverse)

##read data
dat <- read_csv('Carya_summary1.csv')

dat2 <- dat%>%
  select(c(1,5:9))%>%
  filter(period == 'Current' | period == 'LGM' | period == 'SSP126' | period == 'SSP585')

#dat2[dat2$period == '585', ]$period <- '2090'

dat2$`Area (km2)` <- dat2$`Area (km2)`/1000000
######################################################
colnames(dat2)
range(dat2$`Area (km2)`)
Area<-ggplot(dat2,aes(x=factor(period,levels=c("LGM","Current","SSP126","SSP585")),y=`Area (km2)`))+
geom_violin(trim=F,adjust=3,fill="#e5e5e5",colour="#e5e5e5")+
geom_boxplot(color="#000000",fill="#bebebe",outlier.shape=NA,size=0.25,width=0.15)+ 
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),size=0.25,width=0.15)+
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),size=0.25,width=0.15)+
#geom_point(size=0.5)+
# scale_y_continuous(limits=c(-5,75), breaks=c(10,30,50))+
  scale_y_continuous(limits=c(-0.1,0.4), breaks=c(0,0.2,0.4))+
ylab(expression(Area~(10^{6}~km^{2})))+
xlab("")+
theme(text = element_text(family = "serif"),panel.grid.major = element_line(colour="NA"),
      panel.background = element_rect(fill = "transparent",colour=NA),
	  plot.background = element_rect(fill = "transparent",colour=NA),
	  panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))+	  
theme(axis.ticks.x=element_line(linetype=1,size=0.5,lineend = 11),
      axis.ticks.y=element_line(linetype=1,size=0.5,lineend = 11))+ 
theme(axis.title.x = element_text(size = 1),
      axis.title.y = element_text(size = 12),
	  axis.text.x = element_text(size = 8,color="black",vjust = 0.5,hjust = 0.5),
	  axis.text.y = element_text(size = 10,color="black",vjust = 0.5,hjust = 0.5,angle=90))
Area

###############
colnames(dat2)
range(dat2$`dem_mean (m)`)
Dem<-ggplot(dat2,aes(x=factor(period,levels=c("LGM","Current","SSP126","SSP585")),y=`dem_mean (m)`))+
geom_violin(trim=F,adjust=3,fill="#e5e5e5",colour="#e5e5e5")+
geom_boxplot(color="#000000",fill="#bebebe",outlier.shape=NA,size=0.25,width=0.15)+ 
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),size=0.25,width=0.15)+
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),size=0.25,width=0.15)+
#geom_point(size=0.5)+
# scale_y_continuous( breaks=c(2000,3000,4000))+
  scale_y_continuous(limits=c(100,1900), breaks=c(200,850,1900))+
ylab("Elevation (m)")+
xlab("")+
theme(text = element_text(family = "serif"),panel.grid.major = element_line(colour="NA"),
      panel.background = element_rect(fill = "transparent",colour=NA),
	  plot.background = element_rect(fill = "transparent",colour=NA))+	  
theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
	  axis.ticks.x=element_line(linetype=1,size=0.5,lineend = 11),
      axis.ticks.y=element_line(linetype=1,size=0.5,lineend = 11))+ 
theme(axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
	  axis.text.x = element_text(size = 8,color="black",vjust = 0.5),
	  axis.text.y = element_text(size = 10,color="black",vjust = 0.5,hjust = 0.5,angle=90))
Dem

###############
colnames(dat2)
range(dat2$Mean_lat)
Lat<-ggplot(dat2,aes(x=factor(period,levels=c("LGM","Current","SSP126","SSP585")),y=Mean_lat))+
geom_violin(trim=F,adjust=3,fill="#e5e5e5",colour="#e5e5e5")+
geom_boxplot(color="#000000",fill="#bebebe",outlier.shape=NA,size=0.25,width=0.15)+ 
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),size=0.25,width=0.15)+
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),size=0.25,width=0.15)+
#geom_point(size=0.5)+
# scale_y_continuous(breaks=c(27,30,33))+
  scale_y_continuous(limits=c(20,31), breaks=c(21,26,30))+
ylab("Latitude (°N)")+
xlab("")+
theme(text = element_text(family = "serif"),panel.grid.major = element_line(colour="NA"),
      panel.background = element_rect(fill = "transparent",colour=NA),
	  plot.background = element_rect(fill = "transparent",colour=NA))+	  
theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
	  axis.ticks.x=element_line(linetype=1,size=0.5,lineend = 11),
      axis.ticks.y=element_line(linetype=1,size=0.5,lineend = 11))+ 
theme(axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
	  axis.text.x = element_text(size = 8,color="black",vjust = 0.5),
	  axis.text.y = element_text(size = 10,color="black",vjust = 0.5,hjust = 0.5,angle=90))
Lat

###############
colnames(dat2)
range(dat2$Mean_long)
Long<-ggplot(dat2,aes(x=factor(period,levels=c("LGM","Current","SSP126","SSP585")),y=Mean_long))+
geom_violin(trim=F,adjust=3,fill="#e5e5e5",colour="#e5e5e5")+
geom_boxplot(color="#000000",fill="#bebebe",outlier.shape=NA,size=0.25,width=0.15)+ 
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),size=0.25,width=0.15)+
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),size=0.25,width=0.15)+
#geom_point(size=0.5)+
scale_y_continuous(limits = c(100, 117), breaks=c(100,108,116))+
ylab("Longitude (°E)")+
xlab("")+
theme(text = element_text(family = "serif"),panel.grid.major = element_line(colour="NA"),
      panel.background = element_rect(fill = "transparent",colour=NA),
	  plot.background = element_rect(fill = "transparent",colour=NA))+	  
theme(panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
	  axis.ticks.x=element_line(linetype=1,size=0.5,lineend = 11),
      axis.ticks.y=element_line(linetype=1,size=0.5,lineend = 11))+ 
theme(axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
	  axis.text.x = element_text(size = 8,color="black",vjust = 0.5),
	  axis.text.y = element_text(size = 10,color="black",angle=90,vjust = 0.5,hjust=0.5))
Long

#tu <- plot_grid(Area,Dem,Lat,Long,nrow=1)

tu<-ggdraw() +
draw_plot(Area, x = 0, y = 0.04, width = 0.25, height = 0.96) +
draw_plot(Dem, x = 0.25, y = 0, width = 0.25, height = 1) +
draw_plot(Lat, x = 0.5, y = 0, width = 0.25, height = 1) +
draw_plot(Long, x = 0.75, y = 0, width = 0.25, height = 1) 
tu
##save plot

ggsave("stacked_models.pdf", plot = tu, 
       device = cairo_pdf,
       width =25, height = 8, units = "cm",dpi = 300)


