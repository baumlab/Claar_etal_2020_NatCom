# Clear working environment
rm(list=ls())

# Load necessary packages
library(ggplot2)
library(gridExtra)
library(lubridate)
library(tidyverse)

# Load necessary data
load("../KI_temperature_insitu_NOAA/data/KI_SB_temp_wKim_1hr.RData")
region.cols<-c("VaskessBay" = "#5F4690","SouthLagoon"="#1D6996",
               "MidLagoon"="#0F8554","NorthLagoon"="#EDAD08",
               "NorthShore"="#E17C05","BayofWrecks"="#CC503E")

region <- list()
temp_calcs <- function(region_data){ 
  temp_calcs <- region_data %>% 
  group_by(Daily = as.Date(xi2)) %>% 
    filter(Daily >= as.Date("2011-01-01"))%>% 
    filter(Daily <= as.Date("2015-04-01")) %>% 
  summarise_at(c("temperature_1hr"),
               funs(mean=mean(., na.rm = TRUE),
                    min=(min(., na.rm = TRUE)),
                    max=(min(., na.rm = TRUE)),
                    range=(max(., na.rm = TRUE)-min(., na.rm = TRUE))))
  temp_calcs$range[temp_calcs$range=="-Inf"] <- NaN
  region["mean"] <- mean(temp_calcs$range,na.rm=TRUE)
  region["sd"] <- sd(temp_calcs$range,na.rm=TRUE)
  region
}

BOW <- as.data.frame(temp_calcs(region_data=bayofwrecks_1hr_wKim))
LF <- as.data.frame(temp_calcs(region_data=lagoonface_1hr_wKim))
NL <- as.data.frame(temp_calcs(region_data=northlagoon_1hr_wKim))
NS <- as.data.frame(temp_calcs(region_data=northshore_1hr_wKim))
SL <- as.data.frame(temp_calcs(region_data=southlagoon_1hr_wKim))
VB <- as.data.frame(temp_calcs(region_data=vaskesbay_1hr_wKim))

all_regions <- data.frame(t(data.frame(VB=t(VB),
                                       SL=t(SL),
                                       LF=t(LF),
                                       NL=t(NL), 
                                       NS=t(NS),
                                       BOW=t(BOW))))
all_regions$region <- as.factor(rownames(all_regions))
all_regions$region <- factor(all_regions$region,
                             levels(all_regions$region)[c(6,5,2,3,4,1)])

daily_var_nonEN <- ggplot(all_regions)+
  theme_classic()+theme(legend.position = "none")+
  geom_point(aes(x=region,y=mean, color=region),size=5,alpha=0.5)+
  geom_linerange(aes(x=region,ymin=(mean-sd),ymax=(mean+sd),color=region))+
  scale_color_manual(values=c("VB" = "#5F4690","SL"="#1D6996",
                         "LF"="#0F8554","NL"="#EDAD08",
                         "NS"="#E17C05","BOW"="#CC503E"))+
  scale_x_discrete(name="Region")+
  scale_y_continuous(name="Temperature (ºC)",limits=c(0,0.8))
daily_var_nonEN

pdf(file = "figures/Extended_Data/KI_insitu_daily_var_nonEN.pdf", 
    width = 4.5, height = 2.5, useDingbats = FALSE)
daily_var_nonEN
dev.off()

###########
# BOW_temp_calcs <- bayofwrecks_1hr_wKim %>% 
#   group_by(Daily = as.Date(xi2)) %>% 
#   summarise_at(c("temperature_1hr"),funs(mean=mean(., na.rm = TRUE),
#                       min=(min(., na.rm = TRUE)),
#                       max=(min(., na.rm = TRUE)),
#                       range=(max(., na.rm = TRUE)-min(., na.rm = TRUE))))
# BOW_temp_calcs$range[BOW_temp_calcs$range=="-Inf"] <- NaN
# 
# BOW_range_mean <- mean(BOW_temp_calcs$range,na.rm=TRUE)
# BOW_range_sd <- sd(BOW_temp_calcs$range,na.rm=TRUE)

temp_calcs_EN <- function(region_data){ 
  temp_calcs <- region_data %>% 
    group_by(Daily = as.Date(xi2)) %>% 
    filter(Daily >= as.Date("2015-04-01"))%>% 
    filter(Daily <= as.Date("2016-04-01")) %>% 
    summarise_at(c("temperature_1hr"),
                 funs(mean=mean(., na.rm = TRUE),
                      min=(min(., na.rm = TRUE)),
                      max=(min(., na.rm = TRUE)),
                      range=(max(., na.rm = TRUE)-min(., na.rm = TRUE))))
  temp_calcs$range[temp_calcs$range=="-Inf"] <- NaN
  region["mean"] <- mean(temp_calcs$range,na.rm=TRUE)
  region["sd"] <- sd(temp_calcs$range,na.rm=TRUE)
  region
}

BOW_EN <- as.data.frame(temp_calcs_EN(region_data=bayofwrecks_1hr_wKim))
LF_EN <- as.data.frame(temp_calcs_EN(region_data=lagoonface_1hr_wKim))
NL_EN <- as.data.frame(temp_calcs_EN(region_data=northlagoon_1hr_wKim))
NS_EN <- as.data.frame(temp_calcs_EN(region_data=northshore_1hr_wKim))
SL_EN <- as.data.frame(temp_calcs_EN(region_data=southlagoon_1hr_wKim))
VB_EN <- as.data.frame(temp_calcs_EN(region_data=vaskesbay_1hr_wKim))

all_regions_EN <- data.frame(t(data.frame(BOW_EN=t(BOW_EN),
                                       LF_EN=t(LF_EN),
                                       NL_EN=t(NL_EN),
                                       NS_EN=t(NS_EN),
                                       SL_EN=t(SL_EN),
                                       VB_EN=t(VB_EN))))
all_regions_EN$region <- as.factor(rownames(all_regions_EN))
all_regions_EN$region <- factor(all_regions_EN$region,
                             levels(all_regions_EN$region)[c(6,5,2,3,4,1)])


daily_var_EN <- ggplot(all_regions_EN)+
  theme_classic()+theme(legend.position = "none")+
  geom_point(aes(x=region,y=mean, color=region),size=5,alpha=0.5)+
  geom_linerange(aes(x=region,ymin=(mean-sd),ymax=(mean+sd),color=region))+
  scale_color_manual(values=c("VB_EN" = "#5F4690","SL_EN"="#1D6996",
                              "LF_EN"="#0F8554","NL_EN"="#EDAD08",
                              "NS_EN"="#E17C05","BOW_EN"="#CC503E"))+
                              scale_x_discrete(labels=c("VB","SL","LF",
                                                        "NL","NS","BOW"),
                                               name="Region")+
  scale_y_continuous(name="Temperature (ºC)",limits=c(0,0.8))
daily_var_EN

pdf(file = "figures/Extended_Data/KI_insitu_daily_var_EN.pdf", 
    width = 4.5, height = 2.5, useDingbats = FALSE)
daily_var_EN
dev.off()
