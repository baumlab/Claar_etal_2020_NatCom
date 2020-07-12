## Script for creating KI map inset
library(maptools)
library(maps)
library(scales)
library(RColorBrewer)
library(mapdata)
library(mapproj)
library(ggmap)
library(png)
library(grImport)
 library(rgdal)
library(scales)
library(PBSmapping)

world_shp<-readShapePoly("figures/Figure_1/ki_map_files/shapes/ne_110m_land/ne_110m_land")

plot(world_shp, xlim=c(-180,-80),ylim=c(-40,40), 
     col=alpha("#B6A178", 0.3), bg="white",cex=0.2, lwd=0.2)
box(lwd=1)

axis(1,lwd=0.2,padj = -4.2, at=c(-180, -100),cex.axis=0.5, 
     tck=-0.05, labels=c(expression(paste(180, degree, "W")), 
                         expression(paste(100, degree, "W"))))
axis(2, las=0,lwd=0.2,padj = 3.2, at=c(-40, 0, 40), cex.axis=0.5, 
     tck=-0.05, labels=c(expression(paste(40, degree, "S")), 
                         expression(paste(0, degree)), 
                         expression(paste(40, degree, "N"))))
points(-157.4, 1.9, col=1, pch=2, cex=1.5)

