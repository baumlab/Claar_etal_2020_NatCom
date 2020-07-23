####This R script is to test for relationships between environmental variables and disturbance;
###it is also used to test for an effect of environmental variables on symbiont assemblage

# Load necessary libraries
library(readxl)

# Load necessary data
env<-read_excel("data/environmental_parameters/KI_env_all.xlsx")
logist<-read_excel("data/Logistic_regression_data/LogisticData.xlsx")

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

# Change to numeric
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

###Sediment cover
sed<-read.csv("data/environmental_parameters/KI_percent_cover_sedient_beforeEN.csv")

sed_true<-subset(sed, coralnet.tag=="Sediment") %>% 
  group_by(site) %>%
  summarise(percent.sed=mean(percent.cover))

sed_sand<-subset(sed, coralnet.tag=="Sand")   %>% group_by(site) %>%
  summarise(percent.sed=mean(percent.cover))

sed_comb<-sed %>% group_by(site, Field.Season) %>%
  summarise(percent.sed=sum(percent.cover)) 
sed_comb<-sed_comb %>% group_by(site) %>%
  summarise(percent.sed=mean(percent.sed))


par(mfrow=c(2,1), mar=c(4,4,1,1))
sed_dist<-left_join(sed_true, dat2, by="site")
plot(as.numeric(percent.sed)~as.numeric(Disturbance_sqrt), data=sed_dist, pch=19, las=1, xlab="Human Disturbance", ylab="Percent Sediment",ylim=c(0,55))
abline(lm(as.numeric(percent.sed)~as.numeric(Disturbance_sqrt), data=sed_dist), lty=2, lwd=2)
text(y=52, x=14, "Excluding sand")

lm(percent.sed~Disturbance_sqrt,data=sed_dist2) %>% summary()

sed1<-ggplot(sed_dist, aes(x = Disturbance_sqrt, y = percent.sed))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot()+
  xlab("Human disturbance")+
  ylab("Percent cover sediment") + ylim(0,60)
  

sed_dist2<-left_join(sed_comb, dat2, by="site")
plot(as.numeric(percent.sed)~as.numeric(Disturbance_sqrt), data=sed_dist, pch=19, las=1, xlab="Human Disturbance", ylab="Percent Sediment",ylim=c(0,55))
abline(lm(as.numeric(percent.sed)~as.numeric(Disturbance_sqrt), data=sed_dist), lty=2, lwd=2)
text(y=52, x=14, "Including sand")

sed2<-ggplot(sed_dist2, aes(x = Disturbance_sqrt, y = percent.sed))+
  geom_point(cex=3)+
  stat_smooth(method="glm", col="black", se=FALSE)  +
  theme_cowplot() + xlab("Human disturbance")+
  ylab("Percent cover sediment") + ylim(0,60)



##read in microbial count data
microbe<-read.csv("data/environmental_parameters/MI_counts.csv")
microbe$Site<-as.character(microbe$Site)
microbe_sum<-microbe %>% group_by(Site, human_disturbance) %>% 
  summarize(Cells = mean(cells_per_ml), sd = sd(cells_per_ml))
MI<-ggplot(microbe_sum, aes(x=Site, y=Cells, group=human_disturbance, color=human_disturbance)) + 
  geom_pointrange(aes(ymin=Cells-sd, ymax=Cells+sd), cex=1.1, show.legend=FALSE)+
  theme_cowplot()+ scale_color_manual(values=c("black", "darkgrey"))+xlab("Site")+ylab("Cells per mL")

aov(cells_per_ml~as.factor(Site), data=microbe) %>% TukeyHSD()


#make panel
plot_grid(sed1,sed2,vi,MI)

# Subset by coral species
dat1<-subset(dat,Coral_Species=="Platygyra")
dat2<-subset(dat,Coral_Species=="Favites")

# Plot
ggplot(data=dat2, aes(y=Disturbance_sqrt, x=NPPmax) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=quasibinomial), 
              col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("NPP max")))+
  xlab("Human disturbance level")

=======
# Subset by coral species
dat1<-subset(dat,Coral_Species=="Platygyra")
dat2<-subset(dat,Coral_Species=="Favites")

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


