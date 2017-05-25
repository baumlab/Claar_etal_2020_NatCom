# install.packages('jpeg')
# Function from http://stackoverflow.com/questions/9543343/plot-a-jpg-image-using-base-graphics-in-r
plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[2:1] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}

# Reset graphical parameters
dev.off()

tiff(file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/figures/Figure1.tiff",width = 8, height = 6,units="in",res=300)

# Set both inner and outer margins to 0
par(oma=c(0,0,0,0),mar=c(3,5,1.5,1.5))
# Setup layout for single top panel and 5 bottom panels
layout(matrix(c(1,1,1,1,1,2,3,4,5,6), nrow=2, ncol=5, byrow = TRUE), heights=c(0.5,0.2))
# Plot top panel
temp <- read.csv("../data/thistout_kiritimati.csv", sep=",", header=FALSE)
mtime <- read.csv("../data/timevec_kiritimati.csv", sep=",", header=FALSE)
dhw <- read.csv("../data/histout_kiritimati.csv", sep=",", header=FALSE)
time <- "NA"

KI_heat <- cbind(temp,mtime,dhw,time)
colnames(KI_heat) <- c("temp","mtime","dhw","time")
head(KI_heat)

matlab2POS = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  # This next string of functions is a complete disaster, but it works.
  # It tries to outsmart R by converting the secs value to a POSIXct value
  # in the UTC time zone, then converts that to a time/date string that 
  # should lose the time zone, and then it performs a second as.POSIXct()
  # conversion on the time/date string to get a POSIXct value in the user's 
  # specified timezone. Time zones are a goddamned nightmare.
  return(as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
                                        tz = 'UTC'), format = '%Y-%m-%d %H:%M', 
                             tz = 'UTC', usetz = FALSE), tz = timez))
}

KI_heat$time <- matlab2POS(KI_heat$mtime)
KI_heat$time <- as.Date(KI_heat$time, '%m/%d/%Y',tz="UTC")

# par(mfrow=c(1,1),bg="black",fg="white",col.axis="white",col.lab="white")
# <!-- plot(spline(KI_heat$time,KI_heat$temp), type="l", xlab="", ylab="", ylim=c(25,31), col="red") -->
# <!-- axis(2, col="black",lwd=2) -->
# <!-- mtext(2,text="Temperature",line=2) -->
# par(new=TRUE)

# KI_heat$Colour <- cut(KI_heat$dhw, breaks = c(-Inf, 4, 8, 24, Inf), labels = c("yellow", "orange", "red", "maroon"))
# KI_heat$Colour <- as.character(KI_heat$Colour)

plot(spline(KI_heat$time,KI_heat$dhw), type="l", xlab="", ylab="Degree Heating Weeks", ylim=c(0,30),cex.axis=1.5,cex.lab=2,lwd=4,xaxt='n')
abline(4,0,col="orange")
abline(8,0,col="darkorange")
abline(12,0,col="red")
abline(24,0,col="maroon")
# ggplot(KI_heat, aes(time, dhw,color=Colour)) + geom_point() + xlab("") + ylab("Degree Heating Weeks") + scale_color_gradient(low="black",high="white", na.value = "grey50")


# axis(4, col="black",lwd=2)
# abline(27.897,0,col="blue",lty=2)
mtext(4,text="Degree Heating Week",line=10,cex=1.5)
# legend("topleft",col=c("black"),lty=c(1),lwd=4,legend=c("Heat Stress (DHW)"),box.lwd = 0,cex=1.6)
axis.Date(side=1,KI_heat$time,cex.axis=1.5)
par(mar=c(0.1,0.1,0.1,0.1))

# Plot image panels using function created above
plot_jpeg('../figures/KI2014_site35_99.jpg')
plot_jpeg('../figures/KI2015a_site35_99_after.jpg')
plot_jpeg('../figures/KI2015c_site35_99.jpg')
plot_jpeg('../figures/KI2016a_site35_99.jpg')
plot_jpeg('../figures/KI2016b_site35_99.jpg')

dev.off()