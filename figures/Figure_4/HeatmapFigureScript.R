#This is the script to make Figure 4 
#Note that these plots were exported and disturbance categories were added to the figure manually

##Load libraries
library(tidyverse)
library(readxl)
library(arm)
library(bayou)

################################################################################
################################HEATMAPS########################################
################################################################################

####################Heat maps with every expedition#############################
#Note that these were not included in the paper but are interesting
#Import heatmap data
hm.data<-read_csv("data/Heatmap_datasheets/HM_data.csv")
#hm.data %>% View()
hm.data.platy<-subset(hm.data, Coral_Species=="Platygyra")
hm.data.fav<-subset(hm.data, Coral_Species=="Favites")

#####
##Gather dataframe to long form - Platygyra
platy.map.d<-gather(hm.data.platy, key = "Expedition", value="Symbiont", 8:14, factor_key=TRUE) 

###Assign colour scheme
#hex <- c("white", "lightgrey", "#429EB5", "#EE4100", "black", "black", "black","#E7C41D","#E77F02","#C8A2C8","#93BC9E")
hex <- c("white", "lightgrey", "#2165AC", "#B63238", "black", "black", "black","#F8C431","#E77F02","darkorchid4","#93BC9E")

#Make heatmap for Platygyra
ggplot(platy.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time point", y = "Species", fill = "Rank") +scale_y_continuous(position = "right")

#Gather dataframe to long form - Favites
fav.map.d<-gather(hm.data.fav, key = "Expedition", value="Symbiont", 8:14, factor_key=TRUE) 

hex <- c("white", "lightgrey", "#2165AC", "#B63238", "black", "black", "black","#F8C431","#E77F02","darkorchid4")
ggplot(fav.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time point", y = "Species", fill = "Rank") 


#################Heatmaps with just "Before", "Early", "Late" and "After" (Figure 4)##############
####Final versions

###Import data
hm.data<-read_excel("data/Heatmap_datasheets//HeatmapData.xlsx")
#hm.data %>% View()

##Subset by species
hm.data.platy<-subset(hm.data, Coral_Species=="Platygyra")
hm.data.fav<-subset(hm.data, Coral_Species=="Favites")

##Platygyra
platy.map.d<-gather(hm.data.platy, key = "Expedition", value="Symbiont", 8:11, factor_key=TRUE) 
##OLD VERSION hex <- c("white", "lightgrey", "lightblue", "red", "black", "black", "black","yellow","orange","purple","green")
hex <- c("white", "lightgrey", "#2165AC", "#B63238", "black", "black", "black","#F8C431","#E77F02","darkorchid4","#93BC9E")

g<-ggplot(platy.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time period", y = "Colony", fill = "Rank") + 
  scale_y_continuous(position="right")+
  theme(legend.position = "none") 

g


#Favites
fav.map.d<-gather(hm.data.fav, key = "Expedition", value="Symbiont", 8:11, factor_key=TRUE) 

hex <- c("white", "lightgrey", "#2165AC", "#B63238", "black", "black", "black","#F8C431","#E77F02","darkorchid4")
g1<-ggplot(fav.map.d, aes(Expedition, rev(order), fill= Symbiont)) +
  geom_tile() +
  scale_fill_gradientn(colours = (hex)) +
  labs(x = "Time period", y = "Colony", fill = "Rank") + 
  scale_y_continuous(position="right")+
  theme(legend.position = "none") 


g1

