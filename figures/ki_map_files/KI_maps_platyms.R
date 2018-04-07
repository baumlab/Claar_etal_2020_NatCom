## Script for creating KI map for platy ms fall 2017
library(dichromat)
library(maptools)
library(scales)
library(RColorBrewer)
library(rgdal)
library(ggplot2)

### site data
sites<-read.csv('figures/ki_map_files/ki_sites_platy.csv')

###village data
villages<-read.csv("figures/ki_map_files/KI_villagesDCC_2015update.csv", header = TRUE) # you get an error but it works
villages<-data.frame(c(1.989386333, 2.022090868, 1.983594483, 1.865048333, 1.663, 1.69, 1.7135))
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

#tiff(file="figures/KI_map_platysites_villages_bigger.tiff",width = 7.6, height = 7.2,units="in",res=300)
#jpeg(file="figures/KI_map_platysites_villages_bigger.jpeg",width = 7.6, height = 7.2,units="in",res=300)
jpeg(file="figures/KI_map_platysites_villages_bigger_3.jpeg",width = 7.6, height = 7.2,units="in",res=300)  # just 'village'
#pdf(file="figures/KI_map_platysites_villages_bigger.pdf", width = 7.5, height =7)
source("figures/ki_map_files/KI_base_B&W_bigger.R")

# village markers sized by population
symbols(villages$lon, villages$lat, circles=(villages$pop)/10, add=TRUE,inches=0.3, bg=alpha("red", 0.4))
## legend for village size
text(-157.4835, 1.663, "1500 people", cex=0.69)   # just village
text(-157.4835, 1.6885, "1000 people", cex=0.69)   # just village
text(-157.4875, 1.714, "500 people", cex=0.69)   # just village
text(-157.588, 1.69, "Village", srt=90, cex=0.6)# just village
segments(-157.575, 1.663,-157.575, 1.714)  # just village
points(sites$lon, sites$lat, bg=alpha(sites$col,0.8), pch=21, cex=1.4) 
#with(sites_platy, text(lon, lat, label=site, cex=0.7))
legend(-157.265, 2.075,legend=levels(sites$f.pressure), pt.bg=c("#8c510a","#d8b365","#c7eae5","#5ab4ac","#01665e"), pch=21, bty="n", pt.cex=1.4, cex=0.6)
text(-157.275, 2.01, "Human disturbance", srt=90, cex=0.6)
segments(-157.263, 1.968,-157.263, 2.057)
text(-157.3, 1.88, "Bay of\nWrecks", cex = 0.5)
text(-157.53, 1.82, "Vaskess\nBay", cex = 0.5)
text(-157.592, 2.078, "B", font=2, cex=1.25)
#source("ki_map_files/KI_base_inset.R")

dev.off()


# 
# #######doing versions with the inset so the village legends need to be moved#############
# #tiff(file="KI_map_platysites_villages_inset_bigger.tiff",width = 7.6, height = 7.2,units="in",res=300)
# #jpeg(file="KI_map_platysites_villages_inset_bigger.jpeg",width = 7.5, height = 7,units="in",res=300)
# pdf(file="figures/KI_map_platysites_villages_inset_bigger.pdf", width = 7.5, height =7)
# source("figures/ki_map_files/KI_base_B&W_bigger.R")
# 
# # village markers sized by population
# symbols(villages$lon, villages$lat, circles=(villages$pop)/10, add=TRUE,inches=0.3, bg=alpha("red", 0.4))
# ## legend for village size
# text(-157.19, 1.85, "1500 people", cex=0.66)   
# text(-157.19, 1.877, "1000 people", cex=0.66)   
# text(-157.1945, 1.904, "500 people", cex=0.66)  
# points(sites$lon, sites$lat, bg=alpha(sites$col,0.8), pch=21, cex=1.4) 
# #with(sites_platy, text(lon, lat, label=site, cex=0.7))
# legend(-157.265, 2.075,legend=levels(sites$f.pressure), pt.bg=c("#8c510a","#d8b365","#c7eae5","#5ab4ac","#01665e"), pch=21, bty="n", pt.cex=1.4, cex=0.6)
# text(-157.275, 2.01, "Human disturbance", srt=90, cex=0.6)
# segments(-157.263, 1.968,-157.263, 2.057)
# text(-157.315, 1.88, "Bay of\nWrecks", cex = 0.45)
# text(-157.524, 1.825, "Vaskess\nBay", cex = 0.45)
# par(mar=c(1.3,0.9,0.5,0.5))
# source("figures/ki_map_files/KI_base_inset_forbigger.R")
# 
# dev.off()
# 
# 
