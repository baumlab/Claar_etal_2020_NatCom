#### Making figures for the Platy manuscript #####
##### Beginning code copied from site.comparisons.R #####

rm(list=ls())

dev.off()

library(scales); library(ggplot2);library(dichromat);library(maptools);library(scales);library(RColorBrewer);library(rgdal);library(ggplot2); library(plyr);library(dplyr);library(reshape2)
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

D_cols <- c("Very High"="#8c510a", "High"="#d8b365", "Medium"="#c7eae5", "Low"="#5ab4ac", "Very Low"="#01665e")


`Site 3` <- D_cols[["High"]]
`Site 5` <- D_cols[["Low"]]
`Site 8` <- D_cols[["Medium"]]
`Site 15` <-  D_cols[["Very Low"]]
site15_col <-  D_cols[["Very Low"]]
`Site 25` <- D_cols[["High"]]
`Site 27` <- D_cols[["Very High"]]
`Site 30` <-  D_cols[["Very High"]]
`Site 32` <- D_cols[["Very High"]]
`Site 34` <- D_cols[["Medium"]]
`Site 35` <- D_cols[["Medium"]]
`Site 40` <- D_cols[["Very High"]]
`Site 19` <- D_cols[["Very Low"]]

# Subset based on redeployment date
site30_1hr$temperature_1hr[c(45793:45817)] <- NaN
# site30_1hr[c(45793:45817),]


all_temp <- full_join(site3_1hr,site5_1hr,by="xi2")
all_temp <- full_join(all_temp,site8_1hr,by="xi2")
all_temp <- full_join(all_temp,site15_1hr,by="xi2")
all_temp <- full_join(all_temp,site19_1hr,by="xi2")
all_temp <- full_join(all_temp,site25_1hr,by="xi2")
all_temp <- full_join(all_temp,site27_1hr,by="xi2")
all_temp <- full_join(all_temp,site30_1hr,by="xi2")
all_temp <- full_join(all_temp,site32_1hr,by="xi2")
all_temp <- full_join(all_temp,site34_1hr,by="xi2")
all_temp <- full_join(all_temp,site35_1hr,by="xi2")
all_temp <- full_join(all_temp,site40_1hr,by="xi2")
colnames(all_temp) <- c("xi2","Site 3","Site 5","Site 8","Site 15","Site 19","Site 25","Site 27","Site 30","Site 32","Site 34","Site 35","Site 40")


all_temp_long <- melt(all_temp, id="xi2") 

## daily trend for each site on one graph for El Nino
p <- ggplot(all_temp_long, aes(x=xi2)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=18),
        text = element_text(size=18),
        axis.text.x = element_text(size=10),
        legend.position = c(0.503,0.33),
        legend.margin = margin(0,3,0,0)) +
  scale_x_datetime(limits=c(startday,endday),
                   expand=c(0,0),date_breaks = "1 month", date_labels = "%b-%Y") +
  labs(x="") + 
  scale_y_continuous(limits=c(25.5,31),breaks = c(25,26,27,28,29,30,31),
                     expand=c(0,0),name="Temperature (°C)") +
  guides(color = guide_legend(override.aes = list(size = 7))) +
  geom_path(aes(x=xi2, y=value, color=variable), data=all_temp_long,show.legend=TRUE) +
  labs(color="") +
  geom_abline(slope=0,intercept=28.1,linetype="dashed")+
  geom_abline(slope=0,intercept=29.1,color="maroon") +
  scale_color_manual(values = c(`Site 3`,`Site 5`,`Site 8`,
                                `Site 15`,`Site 19`,`Site 25`,
                                `Site 27`,`Site 30`,`Site 32`,
                                `Site 34`,`Site 35`,`Site 40`))

jpeg(file="figures/Extended_Data/TechnicalResponse1.jpeg", height=8, width=15, units = "in",res = 300)
p
dev.off()

tiff(file="figures/Extended_Data/TechnicalResponse1.tiff", height=8, width=15, units = "in",res = 300)
p
dev.off()

png(file="figures/Extended_Data/TechnicalResponse1.png", height=8, width=15, units = "in",res = 300)
p
dev.off()

############create a map to be an inset ######################

### site data
sites<-read.csv('figures/ki_map_files/ki_sites_platy.csv')

#take out sites that did not have a seabird at them
sites<-sites[!sites$site=="14",]
sites<-sites[!sites$site=="37",]
sites<-sites[!sites$site=="38",]

psites<-c("3","5","8","15","19","25", "27", "30","32", "34", "35", "40")
site.cols<-as.data.frame(psites)
site.cols$col <- c(D_cols[["High"]],D_cols[["Low"]], D_cols[["Medium"]],  D_cols[["Very Low"]], D_cols[["Very Low"]], D_cols[["High"]], D_cols[["Very High"]],  D_cols[["Very High"]], D_cols[["Very High"]], D_cols[["Medium"]], D_cols[["Medium"]], D_cols[["Very High"]])
#site.cols$col<-c("#8dd3c7","#ffffb3","#bebada","#fb8072","#666699","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5")
sites$col<-site.cols$col[match(sites$site, site.cols$psites)]


### Sites sampled for platy paper
#pdf(file="KI_map_sites_temp_platyms.pdf")
#tiff(file="KI_map_sites_temp_platyms.tiff",width = 7, height = 7,units="in",res=300)
jpeg(file="figures/KI_map_sites_temp_platyms.jpeg",width = 7, height = 7,units="in",res=300)
source("figures/ki_map_files/KI_base_B&W.R")
points(sites$lon, sites$lat, bg=alpha(sites$col,0.9), pch=21, cex=3.0)
with(sites, text(lon, lat, label=site, cex=0.7))
text(-157.3, 1.9, "Bay\n of\n Wrecks", cex=1)
text(-157.52, 1.82, "Vaskess\n Bay", cex=1)

dev.off()

png(file="figures/KI_map_sites_temp_platyms.png",width = 7, height = 7,units="in",res=300)
source("figures/ki_map_files/KI_base_B&W.R")
points(sites$lon, sites$lat, bg=alpha(sites$col,0.9), pch=21, cex=3.0)
with(sites, text(lon, lat, label=site, cex=0.7))
text(-157.3, 1.9, "Bay\n of\n Wrecks", cex=1)
text(-157.52, 1.82, "Vaskess\n Bay", cex=1)

dev.off()



############## now going to inset it to the figure ##########################

require(grImport)
library(png)
library(grid)
#require(EBImage)
require(gridExtra)
#require(ReadImage)
require(magick)
library(here)

plot<-image_read("figures/Extended_Data/TechnicalResponse1.png")
map_raw<-image_read("figures/KI_map_sites_temp_platyms.png")

map<- map_raw%>%
  image_scale("1400") %>%
  image_annotate("Kiritimati Island", color = "black", size = 50, location = "+50+50", gravity = "northeast")

#combine the map and the plot
final_plot<-image_apply(map, function(x){image_composite(plot, x, offset = "+2520+840")})

#check it
#final_plot

image_write(final_plot, path = "figures/Extended_Data/TechnicalResponse1_2.png", format = "png")
image_write(final_plot, path = "figures/Extended_Data/TechnicalResponse1_2.jpeg", format = "jpeg")

