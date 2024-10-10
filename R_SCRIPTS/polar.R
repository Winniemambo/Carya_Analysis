#load packages
library(ggplot2)
library(cowplot)
library(tidyverse)

##read data
dat <- read_csv('Carya_summary_pair.csv')
dat2<-dat[,c(1,2,4,5,6)]
dat_LC <- subset(dat2,Period_group=="LGM-Current")
dat_CF1 <- subset(dat2,Period_group=="Current-SSP126")
dat_CF2 <- subset(dat2,Period_group=="Current-SSP585")
#####################################################
# Create a vector of colors for each species
arrow_colors <- c("#00FF00", "red", "#FF00FF", "#8000FF", "#00FFFF")

LC<-ggplot()+
 geom_segment(aes(x = seq(0,1.75*pi,0.25*pi),
                  y = rep(0,8), 
				  xend = seq(0,1.75*pi,0.25*pi), 
				  yend = rep(300000,8)),
				  size = 0.25,
				  colour="#969696")+	
 geom_segment(aes(x = rep(0,4),
                  y = seq(0,300000,100000), 
				  xend = rep(2*pi,4), 
				  yend = seq(0,300000,100000)),
				  size = 0.25,
				  colour="#969696")+				  
 geom_segment(aes(x = dat_LC$Shift_radian,
                  y = rep(0,length(dat_LC$Shift_radian)), 
				  xend = dat_LC$Shift_radian, 
				  yend = dat_LC$Shift_distance),
				  size = 0.5,
				  colour= arrow_colors,  # Use the vector of colors
                  arrow = arrow(length = unit(0.1,"cm")))+
 geom_segment(aes(x = 5.79,
                  y = 0,
				  xend = 5.79,
				  yend = mean(dat_LC$Shift_distance)),
				  size = 1,
				  colour= "#242424",
                  arrow = arrow(length = unit(0.2,"cm")))+
  scale_x_continuous(breaks=seq(0, 1.5*pi, 0.5*pi),
                     labels= c("North", "East", "South", "West"))+
  scale_y_continuous(breaks=NULL)+
 xlab("")+
 ylab("")+
 geom_text(aes(x=c(pi,pi, pi),y=c(100000, 200000, 300000),
               label = c("100 km", "200 km", "300 km")),hjust=0,vjust=-0.5,
           family = "serif",size = 2.5, color="black")+
 theme(text = element_text(family = "serif"),
      panel.background = element_rect(fill = "transparent",colour=NA),
	  plot.background = element_rect(fill = "transparent",colour=NA),
	  axis.ticks.y=element_blank(),
	  axis.text.x = element_text(size = 10,vjust=-0.5,color="black"))+	
 coord_polar()    

LC
#####################################################
##################################################################################
CF1<-ggplot()+
  geom_segment(aes(x = seq(0,1.75*pi,0.25*pi),
                   y = rep(0,8), 
                   xend = seq(0,1.75*pi,0.25*pi), 
                   yend = rep(405000,8)),
               size = 0.25,
               colour="#969696")+	
  geom_segment(aes(x = rep(0,4),
                   y = seq(0,405000,135000), 
                   xend = rep(2*pi,4), 
                   yend = seq(0,405000,135000)),
               size = 0.25,
               colour="#969696")+				  
  geom_segment(aes(x = dat_CF1$Shift_radian,
                   y = rep(0,length(dat_CF1$Shift_radian)), 
                   xend = dat_CF1$Shift_radian, 
                   yend = dat_CF1$Shift_distance),
               size = 0.5,
               colour= arrow_colors,  # Use the vector of colors
               arrow = arrow(length = unit(0.1,"cm")))+
  geom_segment(aes(x = 6.04,
                   y = 0,
                   xend = 6.04,
                   yend = mean(dat_CF1$Shift_distance)),
               size = 1,
               colour= "#242424",
               arrow = arrow(length = unit(0.2,"cm")))+
  scale_x_continuous(breaks=seq(0, 1.5*pi, 0.5*pi),
                     labels= c("North", "East", "South", "West"))+
  scale_y_continuous(breaks=NULL)+
  xlab("")+
  ylab("")+
  geom_text(aes(x=c(pi,pi, pi),y=c(135000, 270000, 405000),
                label = c("135 km", "270 km", "405 km")),hjust=0,vjust=-0.5,
            family = "serif",size = 2.5, color="black")+
  theme(text = element_text(family = "serif"),
        panel.background = element_rect(fill = "transparent",colour=NA),
        plot.background = element_rect(fill = "transparent",colour=NA),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 10,vjust=-0.5,color="black"))+	
  coord_polar()    

CF1

#####################################################
##################################################################################
CF2<-ggplot()+
  geom_segment(aes(x = seq(0,1.75*pi,0.25*pi),
                   y = rep(0,8), 
                   xend = seq(0,1.75*pi,0.25*pi), 
                   yend = rep(510000,8)),
               size = 0.25,
               colour="#969696")+	
  geom_segment(aes(x = rep(0,4),
                   y = seq(0,510000,170000), 
                   xend = rep(2*pi,4), 
                   yend = seq(0,510000,170000)),
               size = 0.25,
               colour="#969696")+				  
  geom_segment(aes(x = dat_CF2$Shift_radian,
                   y = rep(0,length(dat_CF2$Shift_radian)), 
                   xend = dat_CF2$Shift_radian, 
                   yend = dat_CF2$Shift_distance),
               size = 0.5,
               colour= arrow_colors,  # Use the vector of colors
               arrow = arrow(length = unit(0.1,"cm")))+
  geom_segment(aes(x = 5.59,
                   y = 0,
                   xend = 5.85,
                   yend = mean(dat_CF2$Shift_distance)),
               size = 1,
               colour= "#242424",
               arrow = arrow(length = unit(0.2,"cm")))+
  scale_x_continuous(breaks=seq(0, 1.5*pi, 0.5*pi),
                     labels= c("North", "East", "South", "West"))+
  scale_y_continuous(breaks=NULL)+
  xlab("")+
  ylab("")+
  geom_text(aes(x=c(pi,pi, pi),y=c(170000, 340000, 510000),
                label = c("170 km", "340 km", "510 km")),hjust=0,vjust=-0.5,
            family = "serif",size = 2.5, color="black")+
  theme(text = element_text(family = "serif"),
        panel.background = element_rect(fill = "transparent",colour=NA),
        plot.background = element_rect(fill = "transparent",colour=NA),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 10,vjust=-0.5,color="black"))+	
  coord_polar()    

CF2


##results
tu <- plot_grid(LC,CF1,CF2,nrow=1)
##export results
ggsave("polar_all/polar_LC.pdf", plot = LC, 
       device = cairo_pdf,
       width =8, height = 8, dpi = 300, units = "cm")  
ggsave("polar_all/polar_CF1.pdf", plot = CF1, 
       device = cairo_pdf,
       width =8, height = 8, dpi = 300, units = "cm")
ggsave("polar_all/polar_CF2.pdf", plot = CF2, 
       device = cairo_pdf,
       width =8, height = 8, dpi = 300, units = "cm") 
	   	   
	   
