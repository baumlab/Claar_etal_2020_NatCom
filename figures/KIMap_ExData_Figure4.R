############create a map to be an inset for the temperature at each site plot ######################

#set wd
#setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy")

### site data
sites<-read.csv('figures/ki_map_files/ki_sites_platy.csv')

#take out sites that did not have a seabird at them
sites<-sites[!sites$site=="14",]
sites<-sites[!sites$site=="37",]
sites<-sites[!sites$site=="38",]

## set palette 

# `Site 3` <- "#8dd3c7"
# `Site 5` <- "#ffffb3"
# `Site 8` <- "#bebada"
# `Site 15` <-  "#fb8072"
# `Site 25` <- "#80b1d3"
# `Site 27` <- "#fdb462"
# `Site 30` <-  "#b3de69"
# `Site 32` <- "#fccde5"
# `Site 34` <- "#d9d9d9"
# `Site 35` <- "#bc80bd"
# `Site 40` <- "#ccebc5"

psites<-c("3","5","8","15","25", "27", "30","32", "34", "35", "40")
site.cols<-as.data.frame(psites)
site.cols$col<-c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5")
sites$col<-site.cols$col[match(sites$site, site.cols$psites)]


### Sites sampled for temperature

#saving the plot
#pdf(file="figures/KI_map_sites_temp_platyms.pdf")
png(file="figures/KI_map_sites_temp_platyms.png",width = 7, height = 7,units="in",res=300)
#tiff(file="figures/KI_map_sites_temp_platyms.tiff",width = 7, height = 7,units="in",res=300)
#jpeg(file="figures/KI_map_sites_temp_platyms.jpeg",width = 7, height = 7,units="in",res=300)

#opening the base plot
source("figures/ki_map_files/KI_base_B&W.R")    # this changes the wd

#plot things on the map
points(sites$lon, sites$lat, bg=alpha(sites$col,0.9), pch=21, cex=3.0)
with(sites, text(lon, lat, label=site, cex=0.7))
text(-157.3, 1.9, "Bay\n of\n Wrecks", cex=1)
text(-157.52, 1.82, "Vaskess\n Bay", cex=1)

#change the wd back 
setwd("/Users/KristinaTietjen/Documents/Git_Hub/KI_Platy")

#finalize the file creation
dev.off()
