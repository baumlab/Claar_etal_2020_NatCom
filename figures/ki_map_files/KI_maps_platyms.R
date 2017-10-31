
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
villages<-read.csv("ki_map_files/KI_villagesDCC_2015update.csv", header = TRUE) # you get an error but it works

villages<-data.frame(c(1.989386333, 2.022090868, 1.983594483, 1.865048333, 1.66, 1.69, 1.71))
villages$lon<-c(-157.4760637, -157.4884092, -157.3683462, -157.5522183, -157.5522183, -157.5522183, -157.5522183)
villages$pop<-c(1879, 2311, 955, 441, 1500, 1000, 500)
villages$village<-c("London", "Tabwakea", "Banana", "Poland", "legend1500", "legend1000", "legend500")
colnames(villages)[1]<-"lat"



### reordering levels for colouring plot
levels(sites$f.pressure)

sites$levels<-as.numeric(factor(sites$f.pressure, levels(factor(sites$f.pressure))[c(4,1,3,2,5)]))
sites$f.pressure<-factor(sites$f.pressure, levels(factor(sites$f.pressure))[c(4,1,3,2,5)])

## set palette for fishing pressure

fishing.cols<-c("#8c510a","#d8b365","#c7eae5","#5ab4ac","#01665e")
fishing.cols<-as.data.frame(fishing.cols)
fishing.cols$f.pressure<-levels(sites$f.pressure)
sites$col<-fishing.cols$fishing.cols[match(sites$f.pressure, fishing.cols$f.pressure)]


### Sites sampled for platy paper
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy/figures")
#pdf(file="KI_map_sites_platyms.pdf")
#tiff(file="KI_map_sites_platyms.tiff",width = 7, height = 7,units="in",res=300)
jpeg(file="KI_map_sites_platyms.jpeg",width = 7, height = 7,units="in",res=300)
source("ki_map_files/KI_base_B&W.R")
points(sites$lon, sites$lat, bg=alpha(sites$col,0.7), pch=21, cex=3.0)
#with(sites_platy, text(lon, lat, label=site, cex=0.7))
legend(-157.23, 2.08,legend=levels(sites$f.pressure), pt.bg=c("#8c510a","#d8b365","#c7eae5","#5ab4ac","#01665e"), pch=21, bty="n", pt.cex=2.0, cex=1)
text(-157.24, 2.035, "Human disturbance", srt=90, cex=1)
segments(-157.23, 2.004,-157.23, 2.068)
text(-157.3, 1.9, "Bay\n of\n Wrecks", cex=1)
text(-157.52, 1.82, "Vaskes\n Bay", cex=1)

dev.off()

############### ok going to try to put villages on the same map as the sites ####################
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy/figures")
#tiff(file="KI_map_platysites_villages.tiff",width = 7, height = 7,units="in",res=300)
#jpeg(file="KI_map_platysites_villages.jpeg",width = 7, height = 7,units="in",res=300)
pdf(file="KI_map_platysites_villages.pdf")
source("ki_map_files/KI_base_B&W.R")

# village markers sized by population
symbols(villages$lon, villages$lat, circles=(villages$pop)/10, add=TRUE,inches=0.3, bg=alpha("red", 0.4))
## legend for village size
text(-157.465, 1.66, "Village with 1500 people", cex=1.4)   
text(-157.465, 1.688, "Village with 1000 people", cex=1.4)   
text(-157.4685, 1.709, "Village with 500 people", cex=1.4)   
#with(villages[!villages$village=="legend",], text(lon, lat, label=village, pos=1, offset=0.5, font=2, col="black"))
points(sites$lon, sites$lat, bg=alpha(sites$col,0.8), pch=21, cex=3.0)
#with(sites_platy, text(lon, lat, label=site, cex=0.7))
legend(-157.23, 2.08,legend=levels(sites$f.pressure), pt.bg=c("#8c510a","#d8b365","#c7eae5","#5ab4ac","#01665e"), pch=21, bty="n", pt.cex=2.0, cex=1.4)
text(-157.24, 2.035, "Human disturbance", srt=90, cex=1.3)
segments(-157.23, 2.004,-157.23, 2.068)
text(-157.3, 1.9, "Bay\nof\nWrecks")
text(-157.52, 1.82, "Vaskes\nBay")
#source("ki_map_files/KI_base_inset.R")

dev.off()

