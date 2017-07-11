# Reset graphical parameters
dev.off()

# Clear your environment
rm(list=ls())

# Load necessary packages
library(imager)

# Load in Data
load("../data/KI_SB_temp_DHW.RData")
load("../data/KI_satellite_heat.RData")


tiff(file="../figures/Figure1.tiff",width = 8, height = 4,units="in",res=300)

# Set both inner and outer margins to 0
par(oma=c(0,0,0,0),mar=c(2,2.5,1.5,1.5))
# Setup layout for single top panel and 5 bottom panels
layout(matrix(c(1,1,1,1,1,1,2,3,4,5,6,7), nrow=2, ncol=6, byrow = TRUE), heights=c(0.5,0.25))
# Plot top panel
# Set a start and end date for plotting
startdate <- as.POSIXct("2014-08-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
enddate <- as.POSIXct("2016-11-19 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# Truncate the data from startdate to enddate
KI_heat <- KI_allsites_DHW[which(KI_allsites_DHW$xi3>startdate),]
KI_heat <- KI_heat[which(KI_heat$xi3<enddate),]
KI_satellite_heat <- KI_satellite_heat[which(KI_satellite_heat$time>as.Date(startdate)),]
KI_satellite_heat <- KI_satellite_heat[which(KI_satellite_heat$time<as.Date(enddate)),]
# Rename columns
colnames(KI_heat)<- c("time","dhw")

cbar <- read.csv("../figures/cmap_enso.csv",header=F)
dhw.floor <- floor(KI_heat$dhw)+1
dhw.cc <- cbar[dhw.floor,]
dhw.cc.rgb <- rgb(dhw.cc)
from <- KI_heat$time-1.75*86400
to <- KI_heat$time+1.75*86400

#################################################################################
# polycurve function
# http://www.fromthebottomoftheheap.net/2013/01/11/shading-regions-under-a-curve/
polyCurve <- function(x, y, from, to, n = 50, miny,
                      col = "red", border = col) {
  drawPoly <- function(fun, from, to, n = 50, miny, col, border) {
    Sq <- seq(from = from, to = to, length = n)
    polygon(x = c(Sq[1], Sq, Sq[n]),
            y = c(miny, fun(Sq), miny),
            col = col, border = border)
  }
  lf <- length(from)
  stopifnot(identical(lf, length(to)))
  if(length(col) != lf)
    col <- rep(col, length.out = lf)
  if(length(border) != lf)
    border <- rep(border, length.out = lf)
  if(missing(miny))
    miny <- min(y)
  interp <- approxfun(x = x, y = y)
  mapply(drawPoly, from = from, to = to, col = col, border = border,
         MoreArgs = list(fun = interp, n = n, miny = miny))
  invisible()
}
#################################################################################

# Colours for shading
# cols <- c("yellow", "orange", "darkorange", "red", "#7d0000", "red", "darkorange", "orange", "yellow")
cols <- c(rgb(cbar)[1], rgb(cbar)[4], rgb(cbar)[8], rgb(cbar)[12], rgb(cbar)[24])

KI2014 <- as.POSIXct("2014-09-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-08 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")

# Plot with the polycurve function
with(KI_heat, plot(KI_heat$time,KI_heat$dhw, type="l", xlab="", ylab="", ylim=c(0,26),cex.axis=1,cex.lab=1.2,yaxs="i",xaxs="i",lwd=3,xaxt='n',yaxt='n',
                   panel.first = # Panel first allows ablines to be plotted before polycurve, looks nicer.
                     c(abline(4,0,col=cols[2],lwd=3),
                       abline(8,0,col=cols[3],lwd=3),
                       abline(12,0,col=cols[4],lwd=3),
                       abline(24,0,col=cols[5],lwd=3),
                       abline(v=KI2014,col="darkgray",lwd=2,lty=2),
                       abline(v=KI2015a,col="darkgray",lwd=2,lty=2),
                       abline(v=KI2015b,col="darkgray",lwd=2,lty=2),
                       abline(v=KI2015c,col="darkgray",lwd=2,lty=2),
                       abline(v=KI2016a,col="darkgray",lwd=2,lty=2),
                       abline(v=KI2016b,col="darkgray",lwd=2,lty=2),
                       polyCurve(KI_heat$time, KI_heat$dhw, from = from, to = to, miny = 0,
                                 col = dhw.cc.rgb)
                       )))
title(ylab="Degree Heating Weeks", line=1.2, cex.lab=1.2)
# mtext(4,text="Degree Heating Week",line=10,cex=0.5) # Label y axis
axis.POSIXct(side=1,KI_heat$time,cex.axis=0.93,tck=0.05,lwd.ticks=2,labels=FALSE)
axis.POSIXct(side=1,at=seq(KI_heat$time[1],KI_heat$time[240],by="month"),KI_heat$time,tck=0.03,cex.axis=0.93,labels=c("","","Oct","","","","","","Apr","","","Jul","","","Oct","","","","","","Apr","","","Jul","","","Oct",""),lwd.ticks=1.5,padj=-1.5)
axis.POSIXct(side=1,KI_heat$time,cex.axis=0.93,tck=0,padj=-1.5)
Y <- c(0,5,10,15,20,25)
axis(side=2,at=Y,cex.axis=0.93,tck=0.03, lwd.ticks=1.5, las=2,hadj=0)
mtext("a",side=2, line=1.5,cex=1.2,las=2,padj=-7.7,font=2) # Add label for figure, specify size
mtext("b",side=2, line=1.5,cex=1.2,las=2,padj=9.1, font=2) # Add label for figure, specify size
mtext("i",side=2, line=-1.75, cex=1, las=2,padj=-7.8)
mtext("ii",side=2, line=-11.35, cex=1, las=2,padj=-7.8)
mtext("iii",side=2, line=-18.8, cex=1, las=2,padj=-7.8)
mtext("iiii",side=2, line=-23.9, cex=1, las=2,padj=-7.8)
mtext("v",side=2, line=-40.4, cex=1, las=2,padj=-7.8)
mtext("vi",side=2, line=-55.85, cex=1, las=2,padj=-7.8)


par(mar=c(0.1,0.1,0.1,0.1))

# Plot image panels using function created above
img_KI2014_site35_99 <- load.image('../figures/coral99/KI2014_site35_99.jpg')
img_KI2015a_site35_99_after <- load.image('../figures/coral99/KI2015a_site35_99_after.jpg')
img_KI2015b_site35_99 <- load.image('../figures/coral99/KI2015b_site35_99.jpg')
img_KI2015c_site35_99 <- load.image('../figures/coral99/KI2015c_site35_99.jpg')
img_KI2016a_site35_99 <- load.image('../figures/coral99/KI2016a_site35_99.jpg')
img_KI2016b_site35_99 <- load.image('../figures/coral99/KI2016b_site35_99.jpg')

plot(img_KI2014_site35_99, axes=F)
mtext("i",adj=0.05,padj=1.6,col="white") #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_KI2015a_site35_99_after, axes=F)
mtext("ii",adj=0.05,padj=1.6,col="white") #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_KI2015b_site35_99,axes=F)
mtext("iii",adj=0.05,padj=1.6,col="white") #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_KI2015c_site35_99, axes=F)
mtext("iv",adj=0.05,padj=1.6,col="white") #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_KI2016a_site35_99, axes=F)
mtext("v",adj=0.05,padj=1.6,col="white") #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_KI2016b_site35_99, axes=F)
mtext("vi",adj=0.05,padj=1.6,col="white") #,side=2, line=-0.5,cex=1,las=2,padj=-2)

dev.off()
