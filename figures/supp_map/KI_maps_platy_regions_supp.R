# library(dichromat)
# library(maptools)
# library(scales)
# library(RColorBrewer)
# library(rgdal)
# library(ggplot2)

### site data
sites<-read.csv('figures/Figure_1/ki_map_files/ki_sites_platy.csv')

### reordering levels for colouring plot
levels(sites$region) # VB,SL,LF,NL,NS,BOW
sites$region<-factor(sites$region, levels(factor(sites$region))[c(6,5,2,3,4,1)])

# All colors from potential palette "#5F4690","#1D6996","#38A6A5","#0F8554","#73AF48","#EDAD08","#E17C05","#CC503E","#94346E","#6F4070","#994E95","#666666"
region.cols<-c("#5F4690","#1D6996","#0F8554","#EDAD08","#E17C05","#CC503E")
site.cols<-c("#5F4690","#1D6996","#0F8554","#EDAD08","#E17C05","#CC503E")
region.cols<-as.data.frame(region.cols)
region.cols$region<-levels(sites$region)
sites$col<-region.cols$region.cols[match(sites$region, region.cols$region)]

levels(sites$region) <- c("Vaskess Bay", "South Lagoon", "Mid Lagoon",
                          "North Lagoon", "North Shore", "Bay of Wrecks")

pdf(file="figures/supp_map/KI_map_platy_regions_inset_bigger.pdf", 
    width = 7.5, height =7, useDingbats = FALSE)
source("figures/Figure_1/ki_map_files/KI_base_B&W_bigger.R")

points(sites$lon, sites$lat, bg=alpha(sites$col,0.8), pch=21, cex=1.4) 
with(sites, text(lon, lat, label=site.simple, cex=0.3))
legend(-157.29, 2.085,legend=levels(sites$region), pt.bg=site.cols, pch=21, bty="n", pt.cex=1.4, cex=0.6)
text(-157.315, 1.88, "Bay of\nWrecks", cex = 0.45)
text(-157.515, 1.825, "Vaskess\nBay", cex = 0.45)
text(-157.455, 1.917, "Lagoon", cex = 0.45)
par(mar=c(1.3,0.9,0.5,0.5))
source("figures/Figure_1/ki_map_files/KI_base_inset_forbigger.R")

dev.off()

jpeg(filename = "figures/supp_map/KI_map_platy_regions_inset_bigger.jpg", width = 7.5, height =7, units="in", res=300)
source("figures/Figure_1/ki_map_files/KI_base_B&W_bigger.R")

points(sites$lon, sites$lat, bg=alpha(sites$col,0.8), pch=21, cex=1.4) 
with(sites, text(lon, lat, label=site.simple, cex=0.3))
legend(-157.29, 2.085,legend=levels(sites$region), pt.bg=site.cols, pch=21, bty="n", pt.cex=1.4, cex=0.6)
text(-157.315, 1.88, "Bay of\nWrecks", cex = 0.45)
text(-157.515, 1.825, "Vaskess\nBay", cex = 0.45)
text(-157.455, 1.917, "Lagoon", cex = 0.45)
par(mar=c(1.3,0.9,0.5,0.5))
source("figures/Figure_1/ki_map_files/KI_base_inset_forbigger.R")

dev.off()
