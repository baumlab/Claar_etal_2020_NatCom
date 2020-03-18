## Script for creating KI map inset
#setwd("/Users/jpwrobinson/Documents/git_repos/maps")
 setwd("/Users/KristinaTietjen/Documents/Git_Hub/maps")
# setwd("/Users/IMAC3/Documents/git-jpwrobinson/maps")
#setwd("/Users/jamiemcdevitt-irwin/Documents/Git_Repos/maps")

library(maptools)
library(maps)
library(scales)
library(RColorBrewer)
#install.packages("mapdata")
library(mapdata)
#install.packages("mapproj")
library(mapproj)
#install.packages("ggmap")
library(ggmap)
library(png)
#install.packages("grImport")
library(grImport)
 library(rgdal)
library(scales)
#install.packages("PBSmapping")
library(PBSmapping)







###complex inset
# world_shp<-readShapePoly("/Users/jpwrobinson/Documents/git_repos/maps/shapes/ne_110m_land/ne_110m_land")
# world_shp<-readShapePoly("/Users/IMAC3/Documents/git-jpwrobinson/maps/shapes/ne_110m_land/ne_110m_land")
 world_shp<-readShapePoly("/Users/KristinaTietjen/Documents/Git_Hub/maps/shapes/ne_110m_land/ne_110m_land")
#world_shp <- readShapePoly("/Users/jamiemcdevitt-irwin/Documents/Git_Repos/maps/shapes/ne_110m_land/ne_110m_land")

plot(world_shp, xlim=c(-180,-80),ylim=c(-40,40), col=alpha("#B6A178", 0.3), bg="white",cex=0.2, lwd=0.2)
box(lwd=1)

axis(1,lwd=0.2,padj = -4.2, at=c(-180, -100),cex.axis=0.5, tck=-0.05, labels=c(expression(paste(180, degree, "W")), expression(paste(100, degree, "W"))))
axis(2, las=0,lwd=0.2,padj = 3.2, at=c(-40, 0, 40), cex.axis=0.5, tck=-0.05, labels=c(expression(paste(40, degree, "S")), expression(paste(0, degree)), expression(paste(40, degree, "N"))))
#rect(-158, 1, -156, 3, border="red", lwd=2)
points(-157.4, 1.9, col=1, pch=2, cex=1.5)

