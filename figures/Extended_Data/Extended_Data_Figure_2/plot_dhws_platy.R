rm(list=ls())

library(ggplot2)
library(gridExtra)

load("../KI_temperature_insitu_NOAA/data/DHW_all.RData")
load("../KI_temperature_insitu_NOAA/data/NOAA_DHW_5km.RData")

# region.cols<-c("VaskessBay" = "#5F4690","SouthLagoon"="#1D6996",
#                "MidLagoon"="#0F8554","NorthLagoon"="#EDAD08",
#                "NorthShore"="#E17C05","BayofWrecks"="#CC503E")

xlim1 <- as.POSIXct("2011/6/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim2 <- as.POSIXct("2017/1/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim3 <- as.POSIXct("2015/2/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim4 <- as.POSIXct("2016/9/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")

NOAA_DHW <- ggplot()+ 
  theme_classic()+
  geom_hline(yintercept = 4,linetype="dashed")+
  geom_hline(yintercept = 8,linetype="dashed")+
  geom_hline(yintercept = 12,linetype="dashed")+
  geom_hline(yintercept = 16,linetype="dashed")+
  geom_hline(yintercept = 20,linetype="dashed")+
  geom_hline(yintercept = 24,linetype="dashed")+
  geom_hline(yintercept = 28,linetype="dashed")+
  geom_line(aes(x=date,y=dhw),color="#5F4690",data=dhw_vaskess)+
  geom_line(aes(x=date,y=dhw),color="#1D6996",data=dhw_southlagoon)+
  geom_line(aes(x=date,y=dhw),color="#0F8554",data=dhw_lagoonface)+
  geom_line(aes(x=date,y=dhw),color="#EDAD08",data=dhw_northlagoon)+
  geom_line(aes(x=date,y=dhw),color="#E17C05",data=dhw_northshore)+
  geom_line(aes(x=date,y=dhw),color="#CC503E",data=dhw_BOW)+
  scale_x_datetime(name=NULL, expand=c(0,0), limits=c(xlim3,xlim4),
                   date_labels = "%b-%Y",date_breaks = "3 months")+
  scale_y_continuous(name="DHW (ºC-week)",limits=c(0,25.6),expand=c(0,0), 
                     breaks = c(0,4,8,12,16,20,24))

pdf(file = "figures/Extended_Data/KI_DHWs.pdf", 
    width = 5.5, height = 4.5, useDingbats = FALSE)
NOAA_DHW
dev.off()
