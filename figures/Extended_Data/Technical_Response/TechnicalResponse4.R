library(ggplot2)
library(scales); library(ggplot2);library(dichromat);library(maptools);library(scales);library(RColorBrewer);library(rgdal);library(ggplot2); library(plyr);library(dplyr);library(reshape2)
library(magick)

load("data/temperature/NOAA_5km.RData")

# D_cols <- c("Very High"="#8c510a", "High"="#d8b365", "Medium"="#c7eae5", "Low"="#5ab4ac", "Very Low"="#01665e")
R_cols <- c("North lagoon" = "#fc8d62", "North shore" = "#e78ac3", "South lagoon"= "#ffd92f", "Vaskess Bay" = "#e5c494", "Bay of Wrecks" = "#b3b3b3")
theme_set(theme_bw())


northshore <- R_cols[["North shore"]]
vaskesbay <- R_cols[["Vaskess Bay"]]
southlagoon <- R_cols[["South lagoon"]]
bayofwrecks <-  R_cols[["Bay of Wrecks"]]
northlagoon <- R_cols[["North lagoon"]]

startday <- as.POSIXct("2015-01-01")
endday <- as.POSIXct("2016-05-01")

KI2014 <- as.POSIXct("2014-09-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-08 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")

region_temp_long <- melt(dhw_dist, id="date") 
str(region_temp_long)

levels(region_temp_long$variable) <- c("dhw_very_high","dhw_high","dhw_medium","dhw_low","dhw_very_low")

p <- ggplot(dhw_dist) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=18),
        text = element_text(size=24),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24),
        legend.position = c(0.515,0.23),
        legend.margin = margin(0,3,0,0)) +
  # geom_point(aes(x=date,y=dhw_high),color=R_cols[["North shore"]]) +
  # geom_point(aes(x=date,y=dhw_low),color=R_cols[["Vaskess Bay"]]) +
  # geom_point(aes(x=date,y=dhw_medium),color=R_cols[["South lagoon"]]) +
  # geom_point(aes(x=date,y=dhw_very_high),color=R_cols[["North lagoon"]]) +
  # geom_point(aes(x=date,y=dhw_very_low),color=R_cols[["Bay of Wrecks"]]) +
  geom_path(aes(x=date, y=value, color=variable), data=region_temp_long,show.legend=TRUE) +
  geom_vline(xintercept=KI2015a) +
  geom_vline(xintercept=KI2015b) +
  geom_vline(xintercept=KI2015c) +
  geom_vline(xintercept=KI2016a) +
  geom_text(aes(x=KI2015a+1000000, y=25),label="ii",size=10) +
  geom_text(aes(x=KI2015b+1000000, y=25),label="iii",size=10) +
  geom_text(aes(x=KI2015c+1000000, y=25),label="iv",size=10) +
  geom_text(aes(x=KI2016a+1000000, y=25),label="v",size=10) +
  scale_x_datetime(limits=c(startday,endday),
                   expand=c(0,0),date_breaks = "3 months", date_labels = "%b-%Y") +
  labs(x="") + 
  scale_y_continuous(limits=c(0,26),breaks = c(seq(0,25,5)),
                     expand=c(0,0),name="Degree Heating Weeks") +
  guides(color = guide_legend(override.aes = list(size = 7))) +
  scale_color_manual(values = c(`northlagoon`, `northshore`, `southlagoon`,
                                `vaskesbay`,`bayofwrecks`),
                     labels = c("North of lagoon", "North shore", 
                                "South of lagoon", "Vaskess Bay", "Bay of Wrecks"),
                     name = "Region")
  p

jpeg(file="figures/Extended_Data/Technical_Response/TechnicalResponse4_NOAA_temp.jpeg", height=6, width=15, units = "in",res = 300)
p
dev.off()

plot<-image_read("figures/Extended_Data/Technical_Response/TechnicalResponse4_NOAA_temp.jpeg")
map_raw<-image_read("figures/Extended_Data/Technical_Response/KI_regions.jpg")

map<- map_raw%>%
  image_scale("1000") %>%
  image_annotate("Kiritimati Island", color = "black", size = 50, location = "+50+50", gravity = "northeast")

#combine the map and the plot
final_plot<-image_apply(map, function(x){image_composite(plot, x, offset = "+2860+580")})

#check it
# final_plot

# image_write(final_plot, path = "figures/Extended_Data/Figure_S2.png", format = "png")
image_write(final_plot, path = "figures/Extended_Data/Figure_S2_C.jpeg", format = "jpeg")
image_write(final_plot, path = "figures/Extended_Data/Figure_S2_C.tiff", format = "tiff")

