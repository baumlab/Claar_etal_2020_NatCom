#Load libraries
library(tidyverse)
library(readxl)
library(arm)

##This is code to make logistic regressions and heatmaps
##Written by Sam Starko (Last Updated Mar 6, 2020)

################################################################################
####################LOGISTIC REGRESSIONS########################################
################################################################################

#Import data (note that I have not moved the data file into the repo - it is in dropbox (under "Updated Data") and is
#still being updated - thus, file.choose)
log.data<-read_excel(file.choose(), sheet="Logistic")
log.data<-read_excel(file.choose())

#Make columns numeric
log.data$ProposedSurvival_Status<-as.numeric(log.data$ProposedSurvival_Status)
log.data$ProportionD_before<-as.numeric(log.data$ProportionD_before)
log.data$Bleached_2015C<-as.numeric(log.data$Bleached_2015C)
log.data$Bleached_2016A<-as.numeric(log.data$Bleached_2016A)
log.data$Disturbance_sqrt<-as.numeric(log.data$Disturbance_sqrt)
log.data$Survival_Status_liberal<-as.numeric(log.data$Survival_Status_liberal)
log.data$Survival_Status_conservative<-as.numeric(log.data$Survival_Status_conservative)
log.data$ProportionC_before<-as.numeric(log.data$ProportionC_before)
log.data$ProportionC_after<-as.numeric(log.data$ProportionC_after)
log.data$ProportionD_2015c<-as.numeric(log.data$ProportionD_2015c)
log.data$ProportionD_truebefore<-as.numeric(log.data$ProportionD_truebefore)


log.data$Bleached_2015C<-as.numeric(log.data$Bleached_2015C)
log.data$ProportionD_2015c<-as.numeric(log.data$ProportionD_2015c)
log.data$Bleached_firstsample<-as.numeric(log.data$Bleached_firstsample)
log.data_platy<-subset(log.data, Coral_Species=="Platygyra")
log.data_Fav<-subset(log.data, Coral_Species=="Favites")
log.data_hyd <-subset(log.data, Coral_Species=="Hydnophora")

#bayesglm of survival versus proportion D before in 2015c or before
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data,
    family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionD_before)~Disturbance_sqrt,data=log.data_platy, las=1, ylab="Proportion Durusdinium",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Disturbance on symbionts")
fit2<-bayesglm(ProportionD_before~Disturbance_sqrt,data=log.data_platy,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

#Plot logistic regression - Survival versus D proportion
plot(jitter(ProposedSurvival_Status,0.1)~ProportionD_before,data=log.data_platy, las=1, xlab="Proportion Durusdinium",ylab="Proportion dead",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on survival")
fit2<-bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_platy,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
fit2 %>% summary()

#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(Bleached_2015C,0.1)~ProportionD_before,data=log.data_platy, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-glm(Bleached_2015C~ProportionD_2015c,data=log.data_platy,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_2015c=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_2015c=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_2015c=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)



########FAVITES


#bayesglm of survival versus proportion D before in 2015c or before
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_Fav,
    family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionD_before)~Disturbance_sqrt,data=log.data_Fav, las=1, ylab="Proportion Durusdinium",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Disturbance on symbionts")
fit2<-bayesglm(ProportionD_before~Disturbance_sqrt,data=log.data_Fav,family=quasibinomial(link="logit"), maxit=1000)
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

#Plot logistic regression - Survival versus D proportion
plot(jitter(ProposedSurvival_Status,0.1)~ProportionD_before,data=log.data_Fav, las=1, xlab="Proportion Durusdinium",ylab="Proportion dead",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on survival")
fit2<-bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_Fav,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
fit2 %>% summary()

#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(Bleached_2015C,0.1)~ProportionD_before,data=log.data_Fav, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-glm(Bleached_2015C~ProportionD_2015c,data=log.data_Fav,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)



##################################################
###########ggplot logistic regressions############
##################################################

######PLATYGYRA
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_truebefore>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)

##Durusdinium versus Disturbance
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_before>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)
P1<-ggplot(data=log.data_platy, aes(y=ProportionD_before, x=Disturbance_sqrt,color=starting) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  xlab("Human disturbance level")



##Durusdinium versus bleaching
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_2015c>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)
P2<-ggplot(data=log.data_platy, aes(x=ProportionD_2015c, y=Bleached_2015C,color=starting) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion bleached early")


##Durusdinium versus Survival
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_before>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)
P3<-ggplot(data=log.data_platy, aes(x=ProportionD_before, y=ProposedSurvival_Status,color=starting) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion alive")


######FAVITES
log.data_Fav$starting<-ifelse(log.data_Fav$ProportionD_before>0.5,1,0)
log.data_Fav$starting <- as.factor(log.data_Fav$starting)

##Durusdinium versus Disturbance
F1<-ggplot(data=log.data_Fav, aes(y=ProportionD_before, x=Disturbance_sqrt,color=starting) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=quasibinomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  xlab("Human disturbance level")

##Durusdinium versus bleaching
log.data_Fav$starting<-ifelse(log.data_Fav$ProportionD_2015c>0.5,1,0)
log.data_Fav$starting <- as.factor(log.data_Fav$starting)
F2<-ggplot(data=log.data_Fav, aes(x=ProportionD_2015c, y=Bleached_2015C,color=starting) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion bleached early")


##Durusdinium versus Survival
log.data_Fav$starting<-ifelse(log.data_Fav$ProportionD_before>0.5,1,0)
log.data_Fav$starting <- as.factor(log.data_Fav$starting)
F3<-ggplot(data=log.data_Fav, aes(x=ProportionD_before, y=ProposedSurvival_Status,color=starting) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion alive")



save(P1,P2,P3,F1,F2,F3, file="Platy_Favites_LogisticPlots.RData")




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


###Logistic regressions across disturbance gradient

#Platy

#bayesglm of survival versus proportion D before in 2015c or before
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data,
         family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionC_before)~Disturbance_sqrt,data=log.data_platy, las=1, ylab="Proportion Cladocopium",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Before El Nino")
fit2<-bayesglm(ProportionC_before~Disturbance_sqrt,data=log.data_platy,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

plot(jitter(ProportionC_after)~Disturbance_sqrt,data=log.data_platy, las=1, ylab="Proportion Cladocopium",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="After El Nino")
fit2<-bayesglm(ProportionC_after~Disturbance_sqrt,data=log.data_platy,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

plot(ProposedSurvival_Status~Disturbance_sqrt,data=log.data_platy, las=1, ylab="Proportion Surviving",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Survival")
fit2<-bayesglm(ProposedSurvival_Status~Disturbance_sqrt,data=log.data_platy,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

plot(Bleached_2015C~Disturbance_sqrt,data=log.data_platy, las=1, ylab="Proportion Surviving",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Survival")
fit2<-bayesglm(Bleached_2015C~Disturbance_sqrt,data=log.data_platy,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

plot(Bleached_2016A~ProportionD_before,data=log.data_platy, las=1, ylab="Proportion Surviving",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Survival")
fit2<-bayesglm(Bleached_2016A~ProportionD_before,data=log.data_platy,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)



#Favites

#bayesglm of survival versus proportion D before in 2015c or before
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data,
         family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionC_before)~Disturbance_sqrt,data=log.data_Fav, las=1, ylab="Proportion Cladocopium",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Before El Nino")
fit2<-bayesglm(ProportionC_before~Disturbance_sqrt,data=log.data_Fav,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

plot(jitter(ProportionC_after)~Disturbance_sqrt,data=log.data_Fav, las=1, ylab="Proportion Cladocopium",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="After El Nino")
fit2<-bayesglm(ProportionC_after~Disturbance_sqrt,data=log.data_Fav,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

plot(ProposedSurvival_Status~Disturbance_sqrt,data=log.data_Fav, las=1, ylab="Proportion Surviving",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Survival")
fit2<-bayesglm(ProposedSurvival_Status~Disturbance_sqrt,data=log.data_Fav,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)


plot(Bleached_2015C~Disturbance_sqrt,data=log.data_Fav, las=1, ylab="Proportion Surviving",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Survival")
fit2<-bayesglm(Bleached_2015C~Disturbance_sqrt,data=log.data_Fav,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)

plot(Bleached_2015C~ProportionD_before,data=log.data_Fav, las=1, ylab="Proportion Surviving",xlab="Chronic Disturbance (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Survival")
fit2<-bayesglm(Bleached_2015C~ProportionD_before,data=log.data_Fav,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
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
  labs(x = "Time point", y = "Species", fill = "Rank") +scale_y_continuous(position = "right")

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

g<-ggplot(platy.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time period", y = "Colony", fill = "Rank") + 
  scale_y_continuous(position="right")+
  theme(legend.position = "none") 
  
  

#Favites
fav.map.d<-gather(hm.data.fav, key = "Expedition", value="Symbiont", 8:11, factor_key=TRUE) 

hex <- c("white", "lightgrey", "#2165AC", "#B63238", "black", "black", "black","#F8C431","#E77F02","darkorchid4")
g1<-ggplot(fav.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time period", y = "Colony", fill = "Rank") + 
  scale_y_continuous(position="right")+
  theme(legend.position = "none") 





############SWITCHING SUPPLEMENTARY FIGURES

##Platygyra
switch<-read_excel(file.choose(),sheet="Switch_figure2")
switch<-subset(switch,Coral_Species=="Platygyra" )
switch$ProportionD_before<-as.numeric(switch$ProportionD_before)
switch$ProportionD_early<-as.numeric(switch$ProportionD_early)
switch$Proportion_late<-as.numeric(switch$Proportion_late)
switch$Proportion_after<-as.numeric(switch$Proportion_after)

switch$Bleached_before<-as.numeric(switch$Bleached_before)
switch$Bleached_early<-as.numeric(switch$Bleached_early)
switch$Bleached_late<-as.numeric(switch$Bleached_late)
switch$Bleached_after<-as.numeric(switch$Bleached_after)

avg.Dbefore<-switch$ProportionD_before %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Dearly<-switch$ProportionD_early %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Dlate<-switch$Proportion_late %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Dafter<-switch$Proportion_after %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_before<-switch$Bleached_before %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_early<-switch$Bleached_early %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_late<-switch$Bleached_late %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_after<-switch$Bleached_after %>% as.numeric() %>% mean(,na.rm=TRUE)

n<-length(switch$ProportionD_before[!is.na(switch$ProportionD_before)])
SE.Dbefore<-sqrt(avg.Dbefore/n*(1-(avg.Dbefore/n))/n)

n<-length(switch$ProportionD_early[!is.na(switch$ProportionD_early)])
SE.Dearly<-sqrt(avg.Dearly/n*(1-(avg.Dearly/n))/n)

n<-length(switch$Proportion_late[!is.na(switch$Proportion_late)])
SE.Dlate<-sqrt(avg.Dlate/n*(1-(avg.Dlate/n))/n)

n<-length(switch$Proportion_after[!is.na(switch$Proportion_after)])
SE.Dafter<-sqrt(avg.Dafter/n*(1-(avg.Dafter/n))/n)

Timepoint<-c("before","early","late","after") %>% factor(levels=c("before","early","late","after"))
Average<-c(avg.Dbefore,avg.Dearly,avg.Dlate, avg.Dafter)
SE<-c(SE.Dbefore,SE.Dearly, SE.Dlate, SE.Dafter)

Durusdinium.df<-data.frame(Timepoint,Average, SE)



n<-length(switch$Bleached_before[!is.na(switch$Bleached_before)])
SE.Bleached_before<-sqrt(avg.Bleach_before/n*(1-(avg.Bleach_before/n))/n)

n<-length(switch$Bleached_early[!is.na(switch$Bleached_early)])
SE.Bleached_early<-sqrt(avg.Bleach_before/n*(1-(avg.Bleach_before/n))/n)

n<-length(switch$Bleached_late[!is.na(switch$Bleached_late)])
SE.Bleached_late<-sqrt(avg.Bleach_late/n*(1-(avg.Bleach_late/n))/n)

n<-length(switch$Bleached_after[!is.na(switch$Bleached_after)])
SE.Bleached_after<-sqrt(avg.Bleach_after/n*(1-(avg.Bleach_after/n))/n)

Timepoint<-c("before","early","late","after") %>% factor(levels=c("before","early","late","after"))
Average<-c(avg.Bleach_before,avg.Bleach_early,avg.Bleach_late, avg.Bleach_after)
SE<-c(SE.Bleached_before,SE.Bleached_early, SE.Bleached_late, SE.Bleached_after)


Bleached.df<-data.frame(Timepoint, Average, SE)

par(mfrow=c(1,1),mar=c(5,5,1,5))
stripchart(Average~Timepoint, data=Durusdinium.df, pch=19, cex=2, vertical=TRUE, ylim=c(0,1.1),ylab="Proportion of colonies", xlab="Heat stress timepoint", col=makeTransparent("gold"), las=1)+
lines(Average~Timepoint, Durusdinium.df, lwd=3, col="gold")+
arrows(c(1:4), Durusdinium.df$Average-1.96*Durusdinium.df$SE,c(1:4), Durusdinium.df$Average+1.96*Durusdinium.df$SE, length=0.1, angle=90, code=3, col="gold")+
points(Average~Timepoint, data=Bleached.df, pch=19, cex=2, col=makeTransparent("darkorchid4"))+
lines(Average~Timepoint, Bleached.df, lwd=3, col="darkorchid4")+
arrows(c(1:4), Bleached.df$Average-1.96*Bleached.df$SE,c(1:4), Bleached.df$Average+1.96*Bleached.df$SE, length=0.1, angle=90, code=3, col="darkorchid4")
legend(c(1,1.1), col=c("darkorchid4","gold"), pch=c(19,19),  legend=c("Bleached",expression(paste(italic("Durusdinium"), " ","dominated"))),bty="n")

expression(paste(italic("Durusdinium"))

####Favites
switch<-read_excel(file.choose(),sheet="Switch_figure2")
switch<-subset(switch,Coral_Species=="Favites" )
switch$ProportionD_before<-as.numeric(switch$ProportionD_before)
switch$ProportionD_early<-as.numeric(switch$ProportionD_early)
switch$Proportion_late<-as.numeric(switch$Proportion_late)
switch$Proportion_after<-as.numeric(switch$Proportion_after)

switch$Bleached_before<-as.numeric(switch$Bleached_before)
switch$Bleached_early<-as.numeric(switch$Bleached_early)
switch$Bleached_late<-as.numeric(switch$Bleached_late)
switch$Bleached_after<-as.numeric(switch$Bleached_after)

avg.Dbefore<-switch$ProportionD_before %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Dearly<-switch$ProportionD_early %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Dlate<-switch$Proportion_late %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Dafter<-switch$Proportion_after %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_before<-switch$Bleached_before %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_early<-switch$Bleached_early %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_late<-switch$Bleached_late %>% as.numeric() %>% mean(,na.rm=TRUE)
avg.Bleach_after<-switch$Bleached_after %>% as.numeric() %>% mean(,na.rm=TRUE)

n<-length(switch$ProportionD_before[!is.na(switch$ProportionD_before)])
SE.Dbefore<-sqrt(avg.Dbefore/n*(1-(avg.Dbefore/n))/n)

n<-length(switch$ProportionD_early[!is.na(switch$ProportionD_early)])
SE.Dearly<-sqrt(avg.Dearly/n*(1-(avg.Dearly/n))/n)

n<-length(switch$Proportion_late[!is.na(switch$Proportion_late)])
SE.Dlate<-sqrt(avg.Dlate/n*(1-(avg.Dlate/n))/n)

n<-length(switch$Proportion_after[!is.na(switch$Proportion_after)])
SE.Dafter<-sqrt(avg.Dafter/n*(1-(avg.Dafter/n))/n)

Timepoint<-c("before","early","late","after") %>% factor(levels=c("before","early","late","after"))
Average<-c(avg.Dbefore,avg.Dearly,avg.Dlate, avg.Dafter)
SE<-c(SE.Dbefore,SE.Dearly, SE.Dlate, SE.Dafter)

Durusdinium.df<-data.frame(Timepoint,Average, SE)



n<-length(switch$Bleached_before[!is.na(switch$Bleached_before)])
SE.Bleached_before<-sqrt(avg.Bleach_before/n*(1-(avg.Bleach_before/n))/n)

n<-length(switch$Bleached_early[!is.na(switch$Bleached_early)])
SE.Bleached_early<-sqrt(avg.Bleach_before/n*(1-(avg.Bleach_before/n))/n)

n<-length(switch$Bleached_late[!is.na(switch$Bleached_late)])
SE.Bleached_late<-sqrt(avg.Bleach_late/n*(1-(avg.Bleach_late/n))/n)

n<-length(switch$Bleached_after[!is.na(switch$Bleached_after)])
SE.Bleached_after<-sqrt(avg.Bleach_after/n*(1-(avg.Bleach_after/n))/n)

Timepoint<-c("before","early","late","after") %>% factor(levels=c("before","early","late","after"))
Average<-c(avg.Bleach_before,avg.Bleach_early,avg.Bleach_late, avg.Bleach_after)
SE<-c(SE.Bleached_before,SE.Bleached_early, SE.Bleached_late, SE.Bleached_after)


Bleached.df<-data.frame(Timepoint, Average, SE)

par(mfrow=c(1,1),mar=c(5,5,1,5))
stripchart(Average~Timepoint, data=Durusdinium.df, pch=19, cex=2, vertical=TRUE, ylim=c(0,1.1), xlab="Heat stress timepoint",ylab="Proportion of colonies", col="gold", las=1)
lines(Average~Timepoint, Durusdinium.df, lwd=3, col="gold")
arrows(c(1:4), Durusdinium.df$Average-1.96*Durusdinium.df$SE,c(1:4), Durusdinium.df$Average+1.96*Durusdinium.df$SE, length=0.1, angle=90, code=3, col="gold")
points(Average~Timepoint, data=Bleached.df, pch=19, cex=2, col="purple")
lines(Average~Timepoint, Bleached.df, lwd=3, col="purple")
arrows(c(1:4), Bleached.df$Average-1.96*Bleached.df$SE,c(1:4), Bleached.df$Average+1.96*Bleached.df$SE, length=0.1, angle=90, code=3, col="purple")
axis(4, las=1)


###Baseline bleaching
###Platygyra
switch<-read_excel(file.choose())
#switch<-read_excel(file.choose(),sheet="Switch_figure")
switch<-log.data
#switch$Bleaching_2014<-as.numeric(switch$Bleaching_2014)
#switch$Bleaching_2014_bin<-ifelse(switch$Bleaching_2014>1,1,0)

#switch$Bleaching_2015a<-as.numeric(switch$Bleaching_2015a)
#switch$Bleaching_2015a_bin<-ifelse(switch$Bleaching_2015a>1,1,0)

#switch$Bleaching_2015b<-as.numeric(switch$Bleaching_2015b)
#switch$Bleaching_2015b_bin<-ifelse(switch$Bleaching_2015b>1,1,0)

switch$Bleached_first_encounter<-as.numeric(switch$Bleached_first_encounter)
switch$Bleached_first_encounter_bin<-ifelse(switch$Bleached_first_encounter>1,1,0)

switch$Coral_Species<-switch$Coral_Species %>% factor(, levels=c("Platygyra", "Favites"))
##Bleaching versus Disturbance
ggplot(data=switch, aes(y=Bleached_first_encounter_bin, x=Disturbance_sqrt) ) +
  facet_wrap(~Coral_Species, scales='free')+
  geom_jitter(height=0.02,cex=3) +
  theme_cowplot(12)+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion Bleached")))+
  xlab("Human disturbance level")

fit2<-glm(as.factor(Bleached_first_encounter_bin)~Disturbance_sqrt,data=switch[switch$Coral_Species=="Platygyra",],family=binomial(link="logit"))
summary(fit2)

fit2<-bayesglm(as.factor(Bleached_first_encounter_bin)~Disturbance_sqrt,data=switch[switch$Coral_Species=="Favites",],family=binomial(link="logit"))
summary(fit2)

ggplot(data=switch, aes(y=Bleaching_2015b_bin, x=Disturbance_sqrt) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion Bleached")))+
  xlab("Human disturbance level")

fit2<-glm(as.factor(Bleaching_2014)~Disturbance_sqrt,data=switch,family=binomial(link="logit"))
summary(fit2)


###Favites
switch<-read_excel(file.choose(),sheet="Switch_figure")
switch<-subset(switch,Coral_Species=="Favites" )
switch$Bleaching_2014<-as.numeric(switch$Bleaching_2014)
switch$Bleaching_2014_bin<-ifelse(switch$Bleaching_2014>1,1,0)

switch$Bleaching_2015a<-as.numeric(switch$Bleaching_2015a)
switch$Bleaching_2015a_bin<-ifelse(switch$Bleaching_2015a>1,1,0)

switch$Bleaching_2015b<-as.numeric(switch$Bleaching_2015b)
switch$Bleaching_2015b_bin<-ifelse(switch$Bleaching_2015b>1,1,0)


##Bleaching versus Disturbance
ggplot(data=switch, aes(y=Bleaching_2014_bin, x=Disturbance_sqrt) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion Bleached")))+
  xlab("Human disturbance level")

fit2<-bayesglm(as.factor(Bleaching_2015b)~Disturbance_sqrt,data=switch,family=binomial(link="logit"))
summary(fit2)

ggplot(data=switch, aes(y=Bleaching_2015b_bin, x=Disturbance_sqrt) ) +
  geom_jitter(height=0.02,cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c("#2165AC", "#B63238"))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion Bleached")))+
  xlab("Human disturbance level")

fit2<-glm(as.factor(Bleaching_2014)~Disturbance_sqrt,data=switch,family=binomial(link="logit"))
summary(fit2)


