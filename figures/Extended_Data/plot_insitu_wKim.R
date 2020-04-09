# Clear working environment
rm(list=ls())

# Load necessary packages
library(ggplot2)
library(gridExtra)

# Load necessary data
load("../KI_temperature_insitu_NOAA/data/KI_SB_temp_wKim_1d.RData")
load("../KI_temperature_insitu_NOAA/data/NOAA_CoralTemp_2011_2018.RData")

region.cols<-c("VaskessBay" = "#5F4690","SouthLagoon"="#1D6996",
               "MidLagoon"="#0F8554","NorthLagoon"="#EDAD08",
               "NorthShore"="#E17C05","BayofWrecks"="#CC503E")
xlim3 <- as.POSIXct("2015/2/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim4 <- as.POSIXct("2016/9/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")

KI2014 <- as.POSIXct("2014-09-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015d <- as.POSIXct("2015-11-06 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-08 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2017a <- as.POSIXct("2017-07-15 00:00:00", format="%Y-%m-%d %H:%M:%S")


insitu_SST <- ggplot()+ 
  theme_classic()+
  geom_line(aes(x=xi3,y=temperature_1d),color="#5F4690",
            data=vaskesbay_1d_wKim)+
  geom_line(aes(x=xi3,y=temperature_1d),color="#1D6996",
            data=southlagoon_1d_wKim)+
  geom_line(aes(x=xi3,y=temperature_1d),color="#0F8554",
            data=lagoonface_1d_wKim)+
  geom_line(aes(x=xi3,y=temperature_1d),color="#EDAD08",
            data=northlagoon_1d_wKim)+
  geom_line(aes(x=xi3,y=temperature_1d),color="#E17C05",
            data=northshore_1d_wKim)+
  geom_line(aes(x=xi3,y=temperature_1d),color="#CC503E",
            data=bayofwrecks_1d_wKim)+
  geom_hline(yintercept=27.6,color="#5F4690", alpha=0.4)+
  geom_hline(yintercept=27.4,color="#1D6996", alpha=0.4)+
  geom_hline(yintercept=27.44,color="#0F8554", alpha=0.4)+
  geom_hline(yintercept=27.36,color="#EDAD08", alpha=0.4)+
  geom_hline(yintercept=27.58,color="#E17C05", alpha=0.4)+
  geom_hline(yintercept=28.03,color="#CC503E", alpha=0.4)+
  geom_vline(xintercept=KI2015b,color="darkgray",linetype="dashed")+
  geom_vline(xintercept=KI2015c,color="darkgray",linetype="dashed")+
  geom_vline(xintercept=KI2015d,color="darkgray",linetype="dashed")+
  geom_vline(xintercept=KI2016a,color="darkgray",linetype="dashed")+
  geom_vline(xintercept=KI2017a,color="darkgray",linetype="dashed")+
  scale_x_datetime(name=NULL, expand=c(0,0), limits=c(xlim3,xlim4),
                   date_labels = "%b-%Y",date_breaks = "3 months")+
  scale_y_continuous(name="Temperature (ºC)",limits=c(24,31),expand=c(0,0), 
                     breaks = c(24,25,26,27,28,29,30,31,32))
insitu_SST


pdf(file = "figures/Extended_Data/KI_insitu_wKim.pdf", 
    width = 9.5, height = 3.25, useDingbats = FALSE)
insitu_SST
dev.off()
