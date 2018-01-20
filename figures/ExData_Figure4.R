#### Making figures for the Platy manuscript #####
##### Beginning code (lines up to 35) copied from site.comparisons.R #####

rm(list=ls())

dev.off()

library(scales); library(ggplot2)
theme_set(theme_bw())

#####  Load data ####
load("data/temperature/KI_SB_temp_1hr.RData")

#############################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#         Figures           #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#############################

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

jpeg(file="figures/Extended Data/ExData_Figure4.jpeg", height=8, width=15, units = "in",res = 300)
p
dev.off()

tiff(file="figures/Extended Data/ExData_Figure4.tiff", height=8, width=15, units = "in",res = 300)
p
dev.off()

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
# 
# 
