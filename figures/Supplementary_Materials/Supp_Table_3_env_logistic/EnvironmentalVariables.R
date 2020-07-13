####This R script is to test for an effect of environmental variables on symbiont assemblage
env<-read_excel("KI_env_all.xlsx")
logist<-read_excel("data/Updated_Starko/LogisticData.xlsx")
colnames(logist)[3]<-"site"

env$visibility_m<-env$visibility_m %>% as.numeric
env$ysi_salinity_mean_1m<-env$ysi_salinity_mean_1m %>% as.numeric()
env$ysi_pH_mean_1m<-env$ysi_pH_mean_1m %>% as.numeric()
env$ysi_DO_mean_1m<-env$ysi_DO_mean_1m %>% as.numeric()
env$npp_mean_sat<-env$npp_mean_sat %>% as.numeric()
env$npp_max_sat<-env$npp_max_sat %>% as.numeric()
env$wave_mean_sat<-env$wave_mean_sat %>% as.numeric()

env_avg<-env %>% 
  group_by(site) %>%
  summarise(vis=mean(visibility_m, na.rm=TRUE), sal=mean(ysi_salinity_mean_1m, na.rm=TRUE),
            pH=mean(ysi_pH_mean_1m, na.rm=TRUE), DO_sat=mean(ysi_DO_mean_1m, na.rm=TRUE),
            NPPmean=mean(npp_mean_sat, na.rm=TRUE), NPPmax=mean(npp_max_sat, na.rm=TRUE),
            wave=mean(wave_mean_sat, na.rm=TRUE))

dat<-left_join(env_avg, logist, by = "site")
dat$ProportionD_before<-dat$ProportionD_before %>% as.numeric()

dat1<-subset(dat,Coral_Species=="Platygyra")
dat2<-subset(dat,Coral_Species=="Favites")

bayesglm(ProportionD_before ~ Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ vis+Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ sal+Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ pH+Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ DO_sat+Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ wave+Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmean+Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmax+Disturbance_sqrt, data=dat1, family="quasibinomial") %>% summary()

ggplot(data=dat1, aes(y=ProportionD_before, x=NPPmax) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=quasibinomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion Durusdinium")))+
  xlab("Human disturbance level")

betareg(vis~ProportionD_before, data=dat2) %>% summary()


bayesglm(ProportionD_before ~ vis+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ sal+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ pH+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ DO_sat+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ wave+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmean+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary()
bayesglm(ProportionD_before ~ NPPmax+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary() 
bayesglm(ProportionD_before ~ NPPmax+Disturbance_sqrt, data=dat2, family="quasibinomial") %>% summary() 


ggplot(data=dat2, aes(y=Disturbance_sqrt, x=NPPmax) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=quasibinomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion Durusdinium")))+
  xlab("Human disturbance level")

