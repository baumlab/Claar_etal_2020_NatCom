###This script is to test for an effect of environmental variables on symbiont assemblage for each species separately
=======

# Load necessary libraries
library(readxl)
library(tidyverse)
library(arm)

# Load necessary data
env<-read_excel("data/environmental_parameters/KI_env_all.xlsx")
logist<-read_excel("data/Logistic_regression_data/LogisticData.xlsx")

#Run the following code only to remove sites that did not have reliable wave data
#For all other analyses, do not run this chunk
#env2 <- filter(env, wave_wind_fetch_sat == "0")
#env<-env2

# Rename column
colnames(logist)[3]<-"site"

# Change to numeric 
env$visibility_m<-env$visibility_m %>% as.numeric
env$ysi_salinity_mean_1m<-env$ysi_salinity_mean_1m %>% as.numeric()
env$ysi_pH_mean_1m<-env$ysi_pH_mean_1m %>% as.numeric()
env$ysi_DO_mean_1m<-env$ysi_DO_mean_1m %>% as.numeric()
env$npp_mean_sat<-env$npp_mean_sat %>% as.numeric()
env$npp_max_sat<-env$npp_max_sat %>% as.numeric()
env$wave_mean_sat<-env$wave_mean_sat %>% as.numeric()

# Summarize env parameters by site
env_avg<-env %>% 
  group_by(site) %>%
  summarise(vis=mean(visibility_m, na.rm=TRUE), 
            sal=mean(ysi_salinity_mean_1m, na.rm=TRUE),
            pH=mean(ysi_pH_mean_1m, na.rm=TRUE), 
            DO_sat=mean(ysi_DO_mean_1m, na.rm=TRUE),
            NPPmean=mean(npp_mean_sat, na.rm=TRUE), 
            NPPmax=mean(npp_max_sat, na.rm=TRUE),
            wave=mean(wave_mean_sat, na.rm=TRUE))
dat<-left_join(env_avg, logist, by = "site")
dat$ProportionD_before<-as.numeric(dat$ProportionD_before)


# Subset by coral species
dat1<-subset(dat,Coral_Species=="Platygyra")
dat2<-subset(dat,Coral_Species=="Favites")

#Filter to subset for wave exposure


# Run glms
bayesglm(ProportionD_before ~ Disturbance_sqrt, data=dat1, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ vis+Disturbance_sqrt, data=dat1, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ sal+Disturbance_sqrt, data=dat1, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ pH+Disturbance_sqrt, data=dat1, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ DO_sat+Disturbance_sqrt, 
         data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ wave+Disturbance_sqrt, data=dat1, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmean+Disturbance_sqrt, data=dat1, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmax+Disturbance_sqrt, data=dat1, 
         family="quasibinomial") %>% summary()

# Plot
ggplot(data=dat1, aes(y=ProportionD_before, x=NPPmax) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=quasibinomial), 
              col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion Durusdinium")))+
  xlab("Human disturbance level")

# Run beta regression
betareg(vis~ProportionD_before, data=dat2) %>% summary()

# Run glms
bayesglm(ProportionD_before ~ vis+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ sal+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ pH+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ DO_sat+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ wave+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmean+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmax+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary() 
bayesglm(ProportionD_before ~ NPPmax+Disturbance_sqrt, data=dat2, 
         family="quasibinomial") %>% summary() 

