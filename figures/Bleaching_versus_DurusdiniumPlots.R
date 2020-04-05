
####This script is to make the figures showing proportion of colonies bleaching versus 
###dominated by Durusdinium
###By Sam Starko - Mar 12, 2020
###The data are in "Updated Data" folder in the dropbox
###This code is not concise but it works!!!

##Platygyra
##Import data and subset
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


###MAKE PLOT
par(mfrow=c(1,1),mar=c(5,5,1,5))
stripchart(Average~Timepoint, data=Durusdinium.df, pch=19, cex=2, vertical=TRUE, ylim=c(0,1.1),ylab="Proportion of colonies", xlab="Heat stress timepoint", col=makeTransparent("#B63238"), las=1)+
  lines(Average~Timepoint, Durusdinium.df, lwd=3, col="#B63238")+
  arrows(c(1:4), Durusdinium.df$Average-1.96*Durusdinium.df$SE,c(1:4), Durusdinium.df$Average+1.96*Durusdinium.df$SE, length=0.1, angle=90, code=3, col="#B63238")+
  points(Average~Timepoint, data=Bleached.df, pch=19, cex=2, col=makeTransparent("gold"))+
  lines(Average~Timepoint, Bleached.df, lwd=3, col="gold")+
  arrows(c(1:4), Bleached.df$Average-1.96*Bleached.df$SE,c(1:4), Bleached.df$Average+1.96*Bleached.df$SE, length=0.1, angle=90, code=3, col="gold")
  polygon(x=c())
legend(c(1,1.1), col=c("gold","#B63238"), pch=c(19,19),  legend=c("Bleached",expression(paste(italic("Durusdinium"), " ","dominated"))),bty="n")



####Favites
##Import and subset data
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


###MAKE PLOT
par(mfrow=c(1,1),mar=c(5,5,1,5))
stripchart(Average~Timepoint, data=Durusdinium.df, pch=19, cex=2, vertical=TRUE, ylim=c(0,1.1),ylab="Proportion of colonies", xlab="Heat stress timepoint", col=makeTransparent("#B63238"), las=1)+
  lines(Average~Timepoint, Durusdinium.df, lwd=3, col="#B63238")+
  arrows(c(1:4), Durusdinium.df$Average-1.96*Durusdinium.df$SE,c(1:4), Durusdinium.df$Average+1.96*Durusdinium.df$SE, length=0.1, angle=90, code=3, col="#B63238")+
  points(Average~Timepoint, data=Bleached.df, pch=19, cex=2, col=makeTransparent("gold"))+
  lines(Average~Timepoint, Bleached.df, lwd=3, col="gold")+
  arrows(c(1:4), Bleached.df$Average-1.96*Bleached.df$SE,c(1:4), Bleached.df$Average+1.96*Bleached.df$SE, length=0.1, angle=90, code=3, col="gold")


           