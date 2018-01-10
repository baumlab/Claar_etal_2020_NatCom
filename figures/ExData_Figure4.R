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

site3_col <- "#8dd3c7"
site5_col <- "#ffffb3"
site8_col <- "#bebada"
site15_col <-  "#fb8072"
site25_col <- "#80b1d3"
site27_col <- "#fdb462"
site30_col <-  "#b3de69"
site32_col <- "#fccde5"
site34_col <- "#d9d9d9"
site35_col <- "#bc80bd"
site40_col <- "#ccebc5"
#ffed6f
  
## daily trend for each site on one graph for El Nino
p <- ggplot(site15_1hr, aes(x=xi2,temperature_1hr)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=10),
        legend.position = "top") +
  scale_x_datetime(limits=c(startday,endday),expand=c(0,0),date_breaks = "1 month", date_labels = "%b-%Y") +
  labs(x="") + 
  scale_y_continuous(limits=c(24.5,31),breaks = c(25,26,27,28,29,30,31),expand=c(0,0),name="Temperature (°C)") +
  geom_line(col=site15_col,show.legend = TRUE) + 
  geom_line(data=site3_1hr,col=site3_col,show.legend = TRUE) + 
  geom_line(data=site5_1hr,col=site5_col) + 
  geom_line(data=site8_1hr,col=site8_col) +
  geom_line(data=site25_1hr,col=site25_col) +
  geom_line(data=site27_1hr,col=site27_col) +
  geom_line(data=site30_1hr,col=site30_col) +
  geom_line(data=site32_1hr,col=site32_col) +
  geom_line(data=site34_1hr,col=site34_col) +
  geom_line(data=site35_1hr,col=site35_col) +
  geom_line(data=site40_1hr,col=site40_col) +
  geom_abline(slope=0,intercept=28.1,linetype="dashed")+
  geom_abline(slope=0,intercept=29.1,color="maroon")+
  scale_color_manual(values = c(site15_col,site3_col,site5_col,site8_col,site25_col,site27_col, site30_col,site32_col,site34_col,site35_col,site40_col))

jpeg(file="figures/Extended Data/ExData_Figure4.jpeg", height=8, width=15, units = "in",res = 300)
p
dev.off()

tiff(file="figures/Extended Data/ExData_Figure4.tiff", height=8, width=15, units = "in",res = 300)
p
dev.off()

