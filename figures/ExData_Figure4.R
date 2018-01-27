#### Making figures for the Platy manuscript #####
##### Beginning code copied from site.comparisons.R #####

#set wd
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy")

rm(list=ls())

dev.off()

library(scales); library(ggplot2);library(dichromat);library(maptools);library(scales);library(RColorBrewer);library(rgdal);library(ggplot2)
theme_set(theme_bw())

#####  Load data ####
load("data/temperature/KI_SB_temp_1hr.RData")

#############################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#         Figures           #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#############################

#just the temperature
startday <- as.POSIXct("2014-09-01")
endday <- as.POSIXct("2016-05-01")

`Site 3` <- "#8dd3c7"
`Site 5` <- "#ffffb3"
`Site 8` <- "#bebada"
`Site 15` <-  "#fb8072"
site15_col <-  "#fb8072"
`Site 25` <- "#80b1d3"
`Site 27` <- "#fdb462"
`Site 30` <-  "#b3de69"
`Site 32` <- "#fccde5"
`Site 34` <- "#d9d9d9"
`Site 35` <- "#bc80bd"
`Site 40` <- "#ccebc5"
#ffed6f
  
# Subset based on redeployment date
site30_1hr$temperature_1hr[c(45793:45817)] <- NaN
# site30_1hr[c(45793:45817),]

## daily trend for each site on one graph for El Nino
p <- ggplot(site15_1hr, aes(x=xi2,temperature_1hr)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=18),
        text = element_text(size=18),
        axis.text.x = element_text(size=10),
        legend.position = c(0.75,0.25)) +
  scale_x_datetime(limits=c(startday,endday),
                   expand=c(0,0),date_breaks = "1 month", date_labels = "%b-%Y") +
  labs(x="") + 
  scale_y_continuous(limits=c(25.5,31),breaks = c(25,26,27,28,29,30,31),
                     expand=c(0,0),name="Temperature (°C)") +
  guides(color = guide_legend(override.aes = list(size = 7))) +
  geom_line(col=site15_col,aes(color=`Site 15`),show.legend = TRUE) + 
  geom_line(data=site3_1hr,aes(color=`Site 3`)) + labs(color="") +
  geom_line(data=site5_1hr,aes(color=`Site 5`)) + 
  geom_line(data=site8_1hr,aes(color=`Site 8`)) +
  geom_line(data=site25_1hr,aes(color=`Site 25`)) +
  geom_line(data=site27_1hr,aes(color=`Site 27`)) +
  geom_line(data=site30_1hr,aes(color=`Site 30`)) +
  geom_line(data=site32_1hr,aes(color=`Site 32`)) +
  geom_line(data=site34_1hr,aes(color=`Site 34`)) +
  geom_line(data=site35_1hr,aes(color=`Site 35`)) +
  geom_line(data=site40_1hr,aes(color=`Site 40`)) +
  geom_abline(slope=0,intercept=28.1,linetype="dashed")+
  geom_abline(slope=0,intercept=29.1,color="maroon")+
  scale_color_manual(values = c(`Site 15`,`Site 3`,`Site 5`,
                                `Site 8`,`Site 25`,`Site 27`, 
                                `Site 30`,`Site 32`,`Site 34`,
                                `Site 35`,`Site 40`),
                     labels = c('Site 15','Site 3','Site 5',
                                'Site 8','Site 25','Site 27', 
                                'Site 30','Site 32','Site 34',
                                'Site 35','Site 40'))

#jpeg(file="figures/Extended_Data/ExData_Figure4.jpeg", height=8, width=15, units = "in",res = 300)
#p
#dev.off()

#tiff(file="figures/Extended_Data/ExData_Figure4.tiff", height=8, width=15, units = "in",res = 300)
#p
#dev.off()

# png(file="figures/Extended_Data/ExData_Figure4.png", height=8, width=15, units = "in",res = 300)
# p
# dev.off()

# 
# startday2 <- as.POSIXct("2016-06-30")
# endday2 <- as.POSIXct("2016-12-30")
# 
# ggplot(site15_1hr, aes(x=xi2,temperature_1hr)) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.text = element_text(size=10),
#         legend.position = "top") +
#   scale_x_datetime(limits=c(startday2,endday2),expand=c(0,0),date_breaks = "1 week", date_labels = "%b-%d") +
#   labs(x="") +
#   scale_y_continuous(limits=c(24.5,31),breaks = c(25,26,27,28,29,30,31),expand=c(0,0),name="Temperature (°C)") + geom_line(data=site30_1hr,col=site30_col)
# 


############create a map to be an inset ######################

#set wd
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy/figures")

### site data
sites<-read.csv('ki_map_files/ki_sites_platy.csv')

#take out sites that did not have a seabird at them
sites<-sites[!sites$site=="14",]
sites<-sites[!sites$site=="37",]
sites<-sites[!sites$site=="38",]

## set palette 

# `Site 3` <- "#8dd3c7"
# `Site 5` <- "#ffffb3"
# `Site 8` <- "#bebada"
# `Site 15` <-  "#fb8072"
# `Site 25` <- "#80b1d3"
# `Site 27` <- "#fdb462"
# `Site 30` <-  "#b3de69"
# `Site 32` <- "#fccde5"
# `Site 34` <- "#d9d9d9"
# `Site 35` <- "#bc80bd"
# `Site 40` <- "#ccebc5"

psites<-c("3","5","8","15","25", "27", "30","32", "34", "35", "40")
site.cols<-as.data.frame(psites)
site.cols$col<-c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5")
sites$col<-site.cols$col[match(sites$site, site.cols$psites)]


### Sites sampled for platy paper
#pdf(file="KI_map_sites_temp_platyms.pdf")
#png(file="KI_map_sites_temp_platyms.png",width = 7, height = 7,units="in",res=300)
#tiff(file="KI_map_sites_temp_platyms.tiff",width = 7, height = 7,units="in",res=300)
jpeg(file="KI_map_sites_temp_platyms.jpeg",width = 7, height = 7,units="in",res=300)
source("ki_map_files/KI_base_B&W.R")
points(sites$lon, sites$lat, bg=alpha(sites$col,0.9), pch=21, cex=3.0)
with(sites, text(lon, lat, label=site, cex=0.7))
text(-157.3, 1.9, "Bay\n of\n Wrecks", cex=1)
text(-157.52, 1.82, "Vaskess\n Bay", cex=1)

dev.off()



############## now going to inset it to the figure ##########################

#set wd
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy/figures/Extended_Data")

require(grImport)
library(png)
library(grid)
#require(EBImage)
require(gridExtra)
#require(ReadImage)
require(magick)
library(here)

plot<-image_read("ExData_Figure4.png")

map<-image_read("KI_map_sites_temp_platyms.png")

final_plot<-image_mosaic(image_scale(c(plot, map),"500"), operator = compose_types("OverCompositeOp"))





#jpeg(file="ExData_Figure4_2.jpeg", height=8, width=15, units = "in",res = 300)
final_plot
#png("figures/Extended_Data/ExData_Figure4_2.png",height=8, width=15, units = "in",res = 300)
#p
#print(p)
#print(m, vp = vp1)
dev.off()







# 
# #map<-readPNG("figures/ki_map_files/KI_map_sites_temp_platyms.png")
# #m<- rasterGrob(map, interpolate = TRUE)
# #m = ggplotGrob(qplot(1, 1))
# m<-grid.picture("figures/KI_map_sites_temp_platyms.jpeg")
# #p<-p+annotation_custom(grob = m, xmin=0, xmax=10, ymin=25.8, ymax=29)
# 
# vp1 <- viewport(width = 10,
#                 height = 10,
#                 x = 1,
#                 y = 7)
# 




# 
# g = qplot(0.1 , 0.1)
# vp1 <- viewport(width = 0.3, 
#                 height = 0.3, 
#                 x = 0.4, 
#                 y = 0.7)
# vp2 <- viewport(width = 0.3, 
#                 height = 0.3, 
#                 x = 0.8, 
#                 y = 0.3)
# #png("text.png")
# print(p)
# print(g, vp = vp1)
# print(g, vp = vp2)
# #dev.off()


# map<-readPNG("figures/ki_map_files/KI_map_sites_temp_platyms.png")
# m<- rasterGrob(map, interpolate = TRUE)
# p<-p+annotation_custom(m, xmin=0, xmax=0.5, ymin=25.8, ymax=29)
# 
# jpeg(file="figures/Extended Data/ExData_Figure4_2.jpeg", height=8, width=15, units = "in",res = 300)
# p
# dev.off()



# p = qplot(1:10, 1:10)
# g = ggplotGrob(qplot(1, 1))
# p + annotation_custom(grob = g, xmin = 3, xmax = 6, ymin = 6, ymax = 10)
# 
# 
# 
# img <- readPNG(system.file("img", "Rlogo.png", package="png"))
# g <- rasterGrob(img, interpolate=TRUE)
# 
# qplot(1:10, 1:10, geom="blank") +
#   annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
#   geom_point()



