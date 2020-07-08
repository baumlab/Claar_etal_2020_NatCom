# Load necessary packages
library(imager)

# Load in Data
# You will need to be in the KI_Platy directory for this to work
# load("data/temperature/KI_SB_temp_DHW.RData")
# load("data/temperature/KI_satellite_heat.RData")
# load(file="data/temperature/KI_SB_temp_1hr.RData")
# load(file="data/temperature/KI_SB_temp_DHW_allsites.RData")

#####################################################################
load("../KI_temperature_insitu_NOAA/data/KI_SB_temp_DHW_NOAAMMM_minOffsetnoEN.RData") # DHW calculated for temperature paper with in situ offset
load(file="data/temperature/KI_SB_temp_1hr.RData") # hourly temperature data

meanMMM <- mean(27.6, # VB # All values from temperature paper, 
                            # Claar et al 2019
                27.4, # SL
                27.44, # LF
                27.36, # NL
                27.58, # NS
                28.03)# BOW 

bleaching_threshold <- meanMMM+1 # Bleaching threshold is MMM + 1

# Calculate mean DHW on KI based on in situ data
KI_meanDHW <- cbind(bayofwrecks_DHW_minOffsetnoEN,
                    lagoonface_DHW_minOffsetnoEN$DHW,
                    northlagoon_DHW_minOffsetnoEN$DHW,
                    northshore_DHW_minOffsetnoEN$DHW,
                    southlagoon_DHW_minOffsetnoEN$DHW,
                    vaskesbay_DHW_minOffsetnoEN$DHW)
colnames(KI_meanDHW) <- c("time","bayofwrecks","lagoonface",
                          "northlagoon","northshore",
                          "southlagoon","vaskessbay")
KI_meanDHW$KI_meanDHW <- rowMeans(KI_meanDHW[2:7],na.rm = TRUE)

# Set a start and end date for plotting
startdate <- as.POSIXct("2014-08-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
enddate <- as.POSIXct("2016-11-19 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")

# Truncate time for DHW data
KI_heat <- KI_meanDHW[which(KI_meanDHW$time>startdate),]
KI_heat <- KI_heat[which(KI_heat$time<enddate),]
# Truncate time for temperature data
KI_allsites_1hr <- KI_allsites_1hr[which(KI_allsites_1hr$xi2>startdate),]
KI_allsites_1hr <- KI_allsites_1hr[which(KI_allsites_1hr$xi2<enddate),]

# Create new color palette to match old one (but able to take more values)
heat.palette <- colorRampPalette(c("#ffffff","#ffffc1","#ffff84",
                                   "#ffff46","#ffff09","#ffed00",
                                   "#ffd200","#ffb700","#ffa700",
                                   "#ff9700","#ff8600","#ff7600",
                                   "#ff6600","#ff5500","#ff4500",
                                   "#ff3300","#ff2400","#ff1100",
                                   "#ff0400","#ea0000","#d30000",
                                   "#bc0000","#a40000","#8c0000",
                                   "#750000","#5e0000","#460000",
                                   "#2f0000","#170000","#000000"))

dhw.floor <- floor(KI_heat$KI_meanDHW)+1 # Calculate which color goes where by 1 degree increments
heat.cbar <- heat.palette(32) # create a color palette with 32 colors based on our generated palette
dhw.cc <- heat.cbar[dhw.floor] # Match the color palette to the DHW values

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
cols <- c(heat.cbar[1], heat.cbar[4], heat.cbar[8], heat.cbar[12], heat.cbar[24])

# Set dates for each field season
KI2014 <- as.POSIXct("2014-09-01 00:00:00",
                     tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00",
                      tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00",
                      tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00",
                      tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015d <- as.POSIXct("2015-11-06 00:00:00",
                      tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00",
                      tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-08 00:00:00",
                      tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")

###################################################################################

############# Make tiff file ################
# Open tiff file
pdf(file="figures/Figure_2/Figure2_a.pdf",
    width = 7.2, height = 2,useDingbats = FALSE)

# Set both inner and outer margins to 0
par(oma=c(0,0,0,0),mar=c(1.5,3.5,0.25,2.5))
# Plot with the polycurve function
with(KI_heat, plot(KI_heat$time,KI_heat$KI_meanDHW, type="l", 
                   xlab="", ylab="", ylim=c(0,33),
                   cex.axis=1,cex.lab=1.2,
                   yaxs="i",xaxs="i",lwd=0.5,xaxt='n',yaxt='n', 
                   col="gray40",
                   panel.first = # Panel first allows ablines to be plotted before polycurve, looks nicer.
                     c(abline(v=KI2014,col="black",lwd=1,lty=2),
                       abline(v=KI2015a,col="black",lwd=1,lty=2),
                       abline(v=KI2015b,col="black",lwd=1,lty=2),
                       abline(v=KI2015c,col="black",lwd=1,lty=2),
                       abline(v=KI2015d,col="black",lwd=1,lty=2),
                       abline(v=KI2016a,col="black",lwd=1,lty=2),
                       abline(v=KI2016b,col="black",lwd=1,lty=2),
                       polyCurve(KI_heat$time, KI_heat$KI_meanDHW, 
                                 from = from, to = to, miny = 0,
                                 col = dhw.cc)
                     )))
Y <- c(0,5,10,15,20,25,30) # Set up y axis
# Add 2nd y axis
axis(side=2,at=Y,cex.axis=0.93,tck=0.03, lwd.ticks=1.5, las=2,hadj=0)

par(new=T) # To add the temperature data to the same plot
plot(KI_allsites_1hr,type='l',col="darkgray",
     ylim=c(25,31.5),xlim=c(startdate,enddate),
     xlab="",ylab="",xaxs="i",xaxt='n',yaxt="n") # Plot the temperature data
abline(bleaching_threshold,0,col=cols[5],lwd=2) # Bleaching threshold
abline(meanMMM,0,col="black") # Mean Monthly Maximum - MMM

title(ylab="Degree Heating Weeks 
  (°C-weeks)", line=1.2, cex.lab=1.2) # Label y axis
# Add in time axes (multiple axes added to allow for customization)
axis.POSIXct(side=1,KI_heat$time,cex.axis=0.93,
             tck=0.05,lwd.ticks=2,labels=FALSE)
axis.POSIXct(side=1,at=seq(KI_heat$time[1],KI_heat$time[240],
                           by="month"),KI_heat$time,tck=0.03,
             cex.axis=0.93,labels=c("","","Oct","","","","","","Apr",
                                    "","","Jul","","","Oct","","","",
                                    "","","Apr","","","Jul","","","Oct",""),
             lwd.ticks=1.5,padj=-1.5)
axis.POSIXct(side=1,KI_heat$time,cex.axis=0.93,tck=0,padj=-1.5)
Z <- c(26,27,28,29,30,31) # To be used as temperature y-axis values
axis(side=4,at=Z,cex.axis=0.93,tck=0.03, lwd.ticks=1.5, las=2,hadj=0.95)
mtext("i",side=2, line=-0.85, cex=1, las=2,padj=-5.6) # Add sampling date label
mtext("ii",side=2, line=-5.9, cex=1, las=2,padj=-5.6) # Add sampling date label
mtext("iii",side=2, line=-9.9, cex=1, las=2,padj=-5.6) # Add sampling date label
mtext("iv",side=2, line=-12.6, cex=1, las=2,padj=-5.6) # Add sampling date label
mtext("v",side=2, line=-16.4, cex=1, las=2,padj=-5.6) # Add sampling date label
mtext("vi",side=2, line=-21.4, cex=1, las=2,padj=-5.6) # Add sampling date label
mtext("vii",side=2, line=-29.6, cex=1, las=2,padj=-5.6) # Add sampling date label
mtext("Temperature (°C)",side=4, cex=0.75,line=1.25)
mtext("Mean Monthly Max.",side=2,line=-47.7,cex=0.72,las=2,padj=0.2) # Label MMM line
mtext("Bleaching Threshold",side=2,line=-47.7,cex=0.72,las=2,padj=-2.5) # Label bleaching threshold line

dev.off() # Close

###############################
## Logistic regression plots ##
load("Platy_Favites_LogisticPlots.RData")
# Named: P1, P2 and P3 for Platy and F1, F2 and F3 for Favites
library(arm)

pdf(file="figures/Figure_2/Figure2_platy_reg1.pdf",
    width = 4.5, height = 2,useDingbats = FALSE)
P2
dev.off()

pdf(file="figures/Figure_2/Figure2_fpenta_reg1.pdf",
    width = 4.5, height = 2,useDingbats = FALSE)
F2
dev.off()

pdf(file="figures/Figure_2/Figure2_platy_reg2.pdf",
    width = 4.5, height = 2,useDingbats = FALSE)
P3
dev.off()

pdf(file="figures/Figure_2/Figure2_fpenta_reg2.pdf",
    width = 4.5, height = 2,useDingbats = FALSE)
F3
dev.off()