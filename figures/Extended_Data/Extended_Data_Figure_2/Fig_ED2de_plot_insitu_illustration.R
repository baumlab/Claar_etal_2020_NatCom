# Load necessary packages
library(ggplot2)
library(gridExtra)

# Load necessary data
load("data/temperature/KI_SB_temp_wKim_1d.RData") # From https://github.com/daniclaar/KI_temperature_insitu_NOAA/blob/master/data/KI_SB_temp_wKim_1d.RData

region.cols<-c("VaskessBay" = "#5F4690","SouthLagoon"="#1D6996",
               "MidLagoon"="#0F8554","NorthLagoon"="#EDAD08",
               "NorthShore"="#E17C05","BayofWrecks"="#CC503E")

xlim3 <- as.POSIXct("2015/9/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim4 <- as.POSIXct("2015/9/30 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim9 <- as.POSIXct("2014/9/1 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")
xlim10 <- as.POSIXct("2014/9/30 00:00:00",format="%Y/%m/%d %H:%M:%S",
                    tz="Pacific/Kiritimati")

insitu_SST_ill_EN <- ggplot()+ 
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
  scale_x_datetime(name=NULL, expand=c(0,0), limits=c(xlim3,xlim4),
                   date_labels = "%d-%b-%Y",date_breaks = "1 week")+
  scale_y_continuous(name="Temperature (ºC)",limits=c(24,31),expand=c(0,0), 
                     breaks = c(24,25,26,27,28,29,30,31,32))
insitu_SST_ill_EN

pdf(file = "figures/Extended_Data/Extended_Data_Figure_2/KI_insitu_ill_EN.pdf", 
    width = 4.5, height = 3, useDingbats = FALSE)
insitu_SST_ill_EN
dev.off()


insitu_SST_ill_nonEN <- ggplot()+ 
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
  scale_x_datetime(name=NULL, expand=c(0,0), limits=c(xlim9,xlim10),
                   date_labels = "%d-%b-%Y",date_breaks = "1 week")+
  scale_y_continuous(name="Temperature (ºC)",limits=c(24,31),expand=c(0,0), 
                     breaks = c(24,25,26,27,28,29,30,31,32))
insitu_SST_ill_nonEN

pdf(file = "figures/Extended_Data/Extended_Data_Figure_2/KI_insitu_SST_ill_nonEN.pdf", 
    width = 4.5, height = 3, useDingbats = FALSE)
insitu_SST_ill_nonEN
dev.off()
