
## Script for creating KI map for platy ms fall 2017
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy/figures")

library(dichromat)
library(maptools)
library(scales)
library(RColorBrewer)
library(rgdal)

### site data
sites<-read.csv('ki_map_files/ki_sites_platy.csv')

###village data
villages<-read.csv("ki_map_files/KI_villagesDCC_2015update.csv", header = TRUE) 

villages<-data.frame(c(1.989386333, 2.022090868, 1.983594483, 1.865048333, 2.05))
villages$lon<-c(-157.4760637, -157.4884092, -157.3683462, -157.5522183, -157.350)
villages$pop<-c(1879, 2311, 955, 441, 500)
villages$village<-c("London", "Tabwakea", "Banana", "Poland", "legend")
colnames(villages)[1]<-"lat"



### reordering levels for colouring plot
levels(sites$f.pressure)

sites$levels<-as.numeric(factor(sites$f.pressure, levels(factor(sites$f.pressure))[c(4,1,2,3,5)]))
sites$f.pressure<-factor(sites$f.pressure, levels(factor(sites$f.pressure))[c(4,1,2,3,5)])

## set palette for fishing pressure

#fishing.cols<-c("Very high"="#a6611a", "High medium"="#dfc27d", "Low"="#80cdc1", "Very low"="#018571") ## need a high
#fishing.cols<-c("Very high"="#a6611a", "High medium"="#80cdc1", "Low"="#018571", "Very low"="#dfc27d")
fishing.cols<-c("#a6611a","#a6611a","#dfc27d","#80cdc1","#018571")
fishing.cols<-as.data.frame(fishing.cols)
fishing.cols$f.pressure<-levels(sites$f.pressure)
sites$col<-fishing.cols$fishing.cols[match(sites$f.pressure, fishing.cols$f.pressure)]

#fishing.cols<-colorschemes$DarkRedtoBlue.12[c(12, 10, 9, 4, 3, 2)]   ##old
#palette(fishing.cols)


### Sites sampled for platy paper
#sites.to.keep<-c(5,14,8,35,34,32,27,30,25,3,38,15,19,37,40)
#sites_platy<-sites[sites$site%in%sites.to.keep,]
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy/figures")
pdf(file="KI_map_sites_platyms.pdf")
source("ki_map_files/KI_base_B&W.R")
points(sites$lon, sites$lat, bg=alpha(sites$col,0.7), pch=21, cex=3.0)
#with(sites_platy, text(lon, lat, label=site, cex=0.7))
legend(-157.25, 2.07,legend=levels(sites$f.pressure), pt.bg=c("#a6611a","#a6611a", "#dfc27d","#80cdc1","#018571"), pch=21, bty="n", pt.cex=2.0, cex=1)
text(-157.26, 2.025, "Human disturbance", srt=90, cex=1)
segments(-157.249, 1.994,-157.249, 2.058)
#title("Kiritimati sites by human disturbance")
text(-157.3, 1.9, "Bay\n of\n Wrecks", cex=1)
text(-157.52, 1.82, "Vaskes\n Bay", cex=1)
#text(-157.23, 1.84, "Baum Lab, University of Victoria \n baum@uvic.ca \n http://baumlab.weebly.com", cex=0.7)
#source("ki_map_files/KI_base_inset.R")

dev.off()

############### ok going to try to put villages on the same map as the sites ####################
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy/figures")
pdf(file="KI_map_platysites_villages.pdf")
source("ki_map_files/KI_base_B&W.R")

# village markers sized by population
symbols(villages$lon, villages$lat, circles=(villages$pop)/10, add=TRUE,inches=0.3, bg=alpha("red", 0.4))
## legend for village size
# symbols(-157.240,1.855 , 50, add=TRUE, inches=0.1, bg=alpha("red",0.3))
text(-157.264, 2.048, "Village with 500 people", cex=1.1)
#with(villages[!villages$village=="legend",], text(lon, lat, label=village, pos=1, offset=0.5, font=2, col="black"))
#title("Kiritimati human population")
points(sites$lon, sites$lat, bg=alpha(sites$col,0.8), pch=21, cex=3.0)
#with(sites_platy, text(lon, lat, label=site, cex=0.7))
legend(-157.25, 2.024,legend=levels(sites$f.pressure), pt.bg=c("#a6611a","#a6611a", "#dfc27d","#80cdc1","#018571"), pch=21, bty="n", pt.cex=2.0, cex=1)
text(-157.26, 1.98, "Human disturbance", srt=90, cex=1)
segments(-157.249, 1.94,-157.249, 2.019)
# points(sites$lon, sites$lat, bg=alpha(sites$levels,0.7), pch=21, cex=3)
# with(sites, text(lon, lat, label=site, cex=0.7))
text(-157.3, 1.9, "Bay\nof\nWrecks")
text(-157.52, 1.82, "Vaskes\nBay")
# text(-157.23, 1.84, "Baum Lab, University of Victoria \n baum@uvic.ca \n http://baumlab.weebly.com", cex=0.7)
#source("ki_map_files/KI_base_inset.R")
dev.off()

