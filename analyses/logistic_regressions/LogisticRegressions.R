#Load libraries
library(tidyverse)
library(readxl)
library(arm)
library(bayou)

##This is code to make logistic regressions and heatmaps
##Written by Sam Starko (Last Updated Mar 6, 2020)

################################################################################
####################LOGISTIC REGRESSIONS########################################
################################################################################

#Import data (note that I have not moved the data file into the repo - it is in dropbox (under "Updated Data") and is
#still being updated - thus, file.choose)
log.data<-read_csv("data/Logistic_regression_data/Logistic.csv")

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

#Subset data by species
log.data_platy<-subset(log.data, Coral_Species=="Platygyra")
log.data_Fav<-subset(log.data, Coral_Species=="Favites")


##################FITTING LOGISTIC MODELS WITH PLATYGYRA DATA###########################

#bayesglm of survival versus proportion D before in 2015c or before (both species)
bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data,
    family=binomial(link="logit")) %>% summary()

#Fit model testing for an effect of disturbance on the proportion of reads that were Durusdinium
fit<-bayesglm(ProportionD_before~Disturbance_sqrt,data=log.data_platy,family=quasibinomial(link="logit"))
summary(fit)

#Fit model testing for an effect of Durusdinium proportion on survival
fit2<-bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_platy,family=binomial(link="logit"))
summary(fit2)

#Plot logistic regression - Bleaching 2015c versus D proportion
fit3<-bayesglm(Bleached_2015C~ProportionD_2015c,data=log.data_platy,family=binomial(link="logit"))
summary(fit3)

##################FITTING LOGISTIC MODELS WITH FAVITES PENTAGONA DATA###########################


#Logistic regression - Proportion D versus human disturbance 
fit<-bayesglm(ProportionD_before~Disturbance_sqrt,data=log.data_Fav,family=quasibinomial(link="logit"), maxit=1000)
summary(fit)

#Logistic regression - Survival versus D proportion
fit2<-bayesglm(ProposedSurvival_Status~ProportionD_before,data=log.data_Fav,family=binomial(link="logit"))
summary(fit2)

#Logistic regression - Bleaching 2015c versus D proportion
fit3<-glm(Bleached_2015C~ProportionD_2015c,data=log.data_Fav,family=binomial(link="logit"))
summary(fit3)


##################################################
###########ggplot logistic regressions############
##################################################

######PLATYGYRA
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_before>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)

##Durusdinium versus Disturbance
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_before>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)
P1<-ggplot(data=log.data_platy, aes(y=ProportionD_before, x=Disturbance_sqrt,color=starting) ) +
  geom_point(cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  xlab("Human disturbance level")

P1

##Durusdinium versus bleaching
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_2015c>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)
P2<-ggplot(data=log.data_platy, aes(x=ProportionD_2015c, y=Bleached_2015C,color=starting) ) +
  geom_point(cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion bleached early")
P2

##Durusdinium versus Survival
log.data_platy$starting<-ifelse(log.data_platy$ProportionD_before>0.5,1,0)
log.data_platy$starting <- as.factor(log.data_platy$starting)
P3<-ggplot(data=log.data_platy, aes(x=ProportionD_before, y=1-ProposedSurvival_Status,color=starting) ) +
  geom_point(cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion dead")

P3


######FAVITES PENTAGONA
log.data_Fav$starting<-ifelse(log.data_Fav$ProportionD_before>0.5,1,0)
log.data_Fav$starting <- as.factor(log.data_Fav$starting)

##Durusdinium versus Disturbance
F1<-ggplot(data=log.data_Fav, aes(y=ProportionD_before, x=Disturbance_sqrt,color=starting) ) +
  geom_point(cex=3) +
  theme_classic()+
  stat_smooth(method="bayesglm", method.args=list(family=quasibinomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  theme(legend.position = "none")+
  ylab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  xlab("Human disturbance level")
F1


##Durusdinium versus bleaching
log.data_Fav$starting<-ifelse(log.data_Fav$ProportionD_2015c>0.5,1,0)
log.data_Fav$starting <- as.factor(log.data_Fav$starting)
F2<-ggplot(data=log.data_Fav, aes(x=ProportionD_2015c, y=Bleached_2015C,color=starting) ) +
  geom_point(cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion bleached early")

F2
##Durusdinium versus Survival
log.data_Fav$starting<-ifelse(log.data_Fav$ProportionD_before>0.5,1,0)
log.data_Fav$starting <- as.factor(log.data_Fav$starting)
F3<-ggplot(data=log.data_Fav, aes(x=ProportionD_before, y=1-ProposedSurvival_Status,color=starting) ) +
  geom_point(cex=3) +
  theme_classic()+
  stat_smooth(method="glm", method.args=list(family=binomial), col="black", se=FALSE, linetype="dashed")  +
  scale_color_manual(values=c(makeTransparent("#2165AC"), makeTransparent("#B63238")))+ 
  theme(legend.position = "none")+
  xlab(expression(paste("Proportion", " ",italic("Durusdinium"))))+
  ylab("Proportion dead")
F3


save(P1,P2,P3,F1,F2,F3, file="analyses/logistic_regressions/Platy_Favites_LogisticPlots.RData")



