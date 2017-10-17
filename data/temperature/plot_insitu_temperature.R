# Clear your environment
rm(list=ls())

# Load necessary packages
library(zoo)

# Reset graphical parameters
dev.off()

# Load in Data
# You will need to be in the KI_Platy directory for this to work
load(file="data/temperature/KI_SB_temp_1hr.RData")

# Open a figure
tiff(file="data/temperature/insitu_temp.tiff",width = 8, height = 4,units="in",res=300)


startdate <- as.POSIXct("2014-08-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
enddate <- as.POSIXct("2016-04-05 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
marchfs <- as.POSIXct("2016-03-15 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")

KI_allsites_1hr <- KI_allsites_1hr[which(KI_allsites_1hr$xi2>startdate),]
KI_allsites_1hr <- KI_allsites_1hr[which(KI_allsites_1hr$xi2<enddate),]

plot(KI_allsites_1hr,type='l',ylim=c(25,31.5),xlim=c(startdate,enddate),xlab="Time",ylab="Temperature",xaxs="i",xaxt='n')

axis.POSIXct(side=1,KI_allsites_1hr$xi2,cex.axis=0.93,tck=0.05,lwd.ticks=2,labels=FALSE)
axis.POSIXct(side=1,at=seq(KI_allsites_1hr$xi2[1],KI_allsites_1hr$xi2[14710],by="month"),KI_allsites_1hr$xi2,tck=0.03,cex.axis=0.93,labels=c("","","Oct","","","","","","Apr","","","Jul","","","Oct","","","","","","Apr"),lwd.ticks=1.5,padj=-1.5)
axis.POSIXct(side=1,KI_allsites_1hr$xi2,cex.axis=0.93,tck=0,padj=-1.5)

abline(28.1366,0,lty=2)
abline(29,0,col="orange")
abline(30,0,col='red')
abline(31,0, col='maroon')
abline(v=marchfs, col='blue')

dev.off()
