#Load libraries
library(tidyverse)
library(readxl)

##This is code to make logistic regressions and heatmaps
##Written by Sam Starko (Last Updated Mar 6, 2020)

################################################################################
####################LOGISTIC REGRESSIONS########################################
################################################################################

#Import data (note that I have not moved the data file into the repo - it is in dropbox (under "Updated Data") and is
#still being updated - thus, file.choose)
log.data<-read_excel(file.choose(), sheet="Logistic")


#Make columns numeric
log.data$ProposedSurvival_Status<-as.numeric(log.data$ProposedSurvival_Status)
log.data$ProportionD_before<-as.numeric(log.data$ProportionD_before)
log.data$Bleached_2015C<-as.numeric(log.data$Bleached_2015C)
log.data$Bleached_2016A<-as.numeric(log.data$Bleached_2016A)
log.data$Disturbance_sqrt<-as.numeric(log.data$Disturbance_sqrt)
log.data$Survival_Status_liberal<-as.numeric(log.data$Survival_Status_liberal)
log.data$Survival_Status_conservative<-as.numeric(log.data$Survival_Status_conservative)


log.data_platy<-subset(log.data, Coral_Species=="Platygyra")
log.data_Fav<-subset(log.data, Coral_Species=="Favites")
log.data_hyd <-subset(log.data, Coral_Species=="Hydnophora")

#bayesglm of survival versus proportion D before in 2015c or before
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data,
    family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionD_before)~Disturbance_sqrt,data=log.data_platy, las=1, ylab="Proportion Durusdinium",xlab="Number of people 2km (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Disturbance on symbionts")
fit2<-bayesglm(ProportionD_before~Disturbance_sqrt,data=log.data_platy,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)


#Plot logistic regression - Survival versus D proportion
plot(jitter(ProposedSurvival_Status,0.1)~ProportionD_before,data=log.data_platy, las=1, xlab="Proportion Durusdinium",ylab="Proportion dead",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on survival")
fit2<-bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_platy,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
fit2 %>% summary()

#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(Bleached_2015C,0.1)~ProportionD_before,data=log.data_platy, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-bayesglm(Bleached_2015C~ProportionD_before,data=log.data_platy,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(Bleached_2015C,0.1)~ProportionD_before,data=log.data_platy, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-bayesglm(Bleached_2015C~ProportionD_before,data=log.data_platy,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)



########FAVITES


#bayesglm of survival versus proportion D before in 2015c or before
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_Fav,
    family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionD_before)~Disturbance_sqrt,data=log.data_Fav, las=1, ylab="Proportion Durusdinium",xlab="Number of people 2km (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Disturbance on symbionts")
fit2<-bayesglm(ProportionD_before~Disturbance_sqrt,data=log.data_Fav,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)


#Plot logistic regression - Survival versus D proportion
plot(jitter(ProposedSurvival_Status,0.1)~ProportionD_before,data=log.data_Fav, las=1, xlab="Proportion Durusdinium",ylab="Proportion dead",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on survival")
fit2<-bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_Fav,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
fit2 %>% summary()

#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(Bleached_2015C,0.1)~ProportionD_before,data=log.data_Fav, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-bayesglm(Bleached_2015C~ProportionD_before,data=log.data_Fav,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)


########Hydnophora


#bayesglm of survival versus proportion D before in 2015c or before
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_hyd,
    family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionD_before)~Disturbance_sqrt,data=log.data_hyd, las=1, ylab="Proportion Durusdinium",xlab="Number of people 2km (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Disturbance on symbionts")
fit2<-bayesglm(ProportionD_before~Disturbance_sqrt,data=log.data_hyd,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

#Plot logistic regression - Survival versus D proportion
plot(jitter(ProposedSurvival_Status,0.1)~ProportionD_before,data=log.data_hyd, las=1, xlab="Proportion Durusdinium",ylab="Proportion dead",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on survival")
fit2<-bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_hyd,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
fit2 %>% summary()

#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(Bleached_2015C,0.1)~ProportionD_before,data=log.data_hyd, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-bayesglm(Bleached_2015C~ProportionD_before,data=log.data_hyd,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)


#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(ProposedSurvival_Status,0.1)~Bleached_2015C,data=log.data_hyd, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-bayesglm(ProportionD_before~Bleached_2015C,data=log.data_hyd,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Bleached_2015C=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Bleached_2015C=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Bleached_2015C=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)



################################################################################
################################HEATMAPS########################################
################################################################################

#####Heat maps with every expedition
#Import heatmap data
hm.data<-read_excel(file.choose(), sheet="HM_data")
hm.data %>% View()
hm.data.platy<-subset(hm.data, Coral_Species=="Platygyra")
hm.data.fav<-subset(hm.data, Coral_Species=="Favites")

##Platygyra
platy.map.d<-gather(hm.data.platy, key = "Expedition", value="Symbiont", 8:14, factor_key=TRUE) 

hex <- c("white", "lightgrey", "#429EB5", "#EE4100", "black", "black", "black","#E7C41D","#E77F02","#C8A2C8","#93BC9E")
hex <- c("white", "lightgrey", "#2165AC", "#B63238", "black", "black", "black","#F8C431","#E77F02","darkorchid4","#93BC9E")
ggplot(platy.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time point", y = "Species", fill = "Rank") 

#Favites
fav.map.d<-gather(hm.data.fav, key = "Expedition", value="Symbiont", 8:14, factor_key=TRUE) 

hex <- c("white", "lightgrey", "lightblue", "red", "black", "black", "black","yellow","orange","purple")
ggplot(fav.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Disturbance Category", y = "Species", fill = "Rank") 


#################Heatmaps with just "Before", "Early", "Late" and "After"

hm.data<-read_excel(file.choose(), sheet="HM_data_collapsed")
hm.data %>% View()
hm.data.platy<-subset(hm.data, Coral_Species=="Platygyra")
hm.data.fav<-subset(hm.data, Coral_Species=="Favites")

##Platygyra
platy.map.d<-gather(hm.data.platy, key = "Expedition", value="Symbiont", 8:11, factor_key=TRUE) 
##OLD VERSION hex <- c("white", "lightgrey", "lightblue", "red", "black", "black", "black","yellow","orange","purple","green")

##Danielle, this is the latest version
hex <- c("white", "lightgrey", "#2165AC", "#B63238", "black", "black", "black","#F8C431","#E77F02","darkorchid4","#93BC9E")

ggplot(platy.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time period", y = "Colony", fill = "Rank") 

#Favites
fav.map.d<-gather(hm.data.fav, key = "Expedition", value="Symbiont", 8:11, factor_key=TRUE) 

hex <- c("white", "lightgrey", "lightblue", "red", "black", "black", "black","yellow","orange","purple")
ggplot(fav.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time period", y = "Colony", fill = "Rank") 








