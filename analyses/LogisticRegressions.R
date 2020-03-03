#Load libraries
library(tidyverse)
library(readxl)

#Import data (note that I have not moved the data file into the repo - it is in dropbox and is
#still being updated - thus, file.choose)
log.data<-read_excel(file.choose(), sheet="Logistic")

#Make columns numeric
log.data$ProposedSurvival_Status<-as.numeric(log.data$ProposedSurvival_Status)
log.data$ProportionD_before<-as.numeric(log.data$ProportionD_before)
log.data$Bleached_2015C<-as.numeric(log.data$Bleached_2015C)
log.data$Bleached_2016A<-as.numeric(log.data$Bleached_2016A)
log.data$Disturbance_sqrt<-as.numeric(log.data$Disturbance_sqrt)


#glm of survival versus proportion D before in 2015c or before
glm(ProposedSurvival_Status~ProportionD_before,data=log.data,
    family=binomial(link="logit")) %>% summary()

par(mfrow=c(3,1), mar=c(4,4,2.5,1))
#Plot logistic regression - Proportion D versus human disturbance (Number of people within 2km)
plot(jitter(ProportionD_before)~Disturbance_sqrt,data=log.data, las=1, ylab="Proportion Durusdinium",xlab="Number of people 2km (sqrt)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Disturbance on symbionts")
fit2<-glm(ProportionD_before~Disturbance_sqrt,data=log.data,family=quasibinomial(link="logit"))
curve(predict(fit2,data.frame(Disturbance_sqrt=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(Disturbance_sqrt=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)


#Plot logistic regression - Survival versus D proportion
plot(jitter(ProposedSurvival_Status,0.1)~ProportionD_before,data=log.data, las=1, xlab="Proportion Durusdinium",ylab="Proportion dead",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on survival")
fit2<-glm(ProposedSurvival_Status~ProportionD_before,data=log.data,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)

#Plot logistic regression - Bleaching 2015c versus D proportion
plot(jitter(Bleached_2015C,0.1)~ProportionD_before,data=log.data, las=1, xlab="Proportion Durusdinium",ylab="Proportion bleached (2015c)",col="black",pch=19, cex=0.8, lwd=3, main="Effect of Durusdinium on bleaching")
fit2<-glm(Bleached_2015C~ProportionD_before,data=log.data,family=binomial(link="logit"))
curve(predict(fit2,data.frame(ProportionD_before=x),type="resp"),add=TRUE, col="black", lwd=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*1+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
curve((predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$se.fit*(-1)+predict(fit2,data.frame(ProportionD_before=x),type="resp", se=TRUE)$fit),add=TRUE, col="black", lwd=1, lty=2)
summary(fit2)



