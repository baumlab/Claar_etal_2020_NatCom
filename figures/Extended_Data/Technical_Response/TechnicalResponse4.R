library(ggplot2)
load("data/temperature/NOAA_5km.RData")

D_cols <- c("Very High"="#8c510a", "High"="#d8b365", "Medium"="#c7eae5", "Low"="#5ab4ac", "Very Low"="#01665e")
theme_set(theme_bw())


northshore <- D_cols[["High"]]
vaskesbay <- D_cols[["Low"]]
southlagoon <- D_cols[["Medium"]]
bayofwrecks <-  D_cols[["Very Low"]]
northlagoon <- D_cols[["Very High"]]

startday <- as.POSIXct("2015-01-01")
endday <- as.POSIXct("2016-05-01")

KI2014 <- as.POSIXct("2014-09-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-08 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")


p <- ggplot(dhw_dist) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=18),
        text = element_text(size=24),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24)) +
  geom_point(aes(x=date,y=dhw_high),color=D_cols[["High"]]) +
  geom_point(aes(x=date,y=dhw_low),color=D_cols[["Low"]]) +
  geom_point(aes(x=date,y=dhw_medium),color=D_cols[["Medium"]]) +
  geom_point(aes(x=date,y=dhw_very_high),color=D_cols[["Very High"]]) +
  geom_point(aes(x=date,y=dhw_very_low),color=D_cols[["Very Low"]]) +
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
                     expand=c(0,0),name="Degree Heating Weeks")

jpeg(file="figures/Extended_Data/TechnicalResponse4_NOAA.jpeg", height=6, width=15, units = "in",res = 300)
p
dev.off()

