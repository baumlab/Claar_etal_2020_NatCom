##This script is to generate ITS2 profile stacked barplots as part of supplementary analyses for
##Claar et al 2020

##Code written by Sam Starko

# Code to import and format datasets
##Load in required libraries
library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)
library(lattice)
library(vegan)
library(here)
library(ape)

##Read in dataframes###
DIV.meta<-read.csv("data/SymPortal/Metadata.csv")
DIV.genus<-read.csv("data/SymPortal/By_genus.csv")
DIV.raw<-read.csv("data/SymPortal/RawRead_matrix.csv")
tags<-read.csv("data/SymPortal/tags.csv")
colour.scheme<-read.csv("data/SymPortal/Colour_scheme.csv")


#Combine dataframes
DIV.combined<-full_join(DIV.raw2, DIV.meta2, by="sample_name")
DIV.combined.genus<-full_join(DIV.genus2, DIV.meta2, by="sample_name")

#Remove samples with less than 1000 sequences; isolate dates and years of interest
DIV.combined<-DIV.combined[DIV.combined$Sequence_count>250,]
DIV.combined<-DIV.combined[DIV.combined$expedition!="2018"&DIV.combined$expedition!="2019",]
DIV.combined<-DIV.combined[DIV.combined$coral_tag %in% tags$Coral_Tag,]
DIV.combined.genus<-DIV.combined.genus[DIV.combined.genus$Sequence_count>250,]


##Separate community matrix from meta-data information
mat<-DIV.combined[,2:159]
dev<-DIV.combined[,160]

#Convert matrix to proportion
DIV.matrix<-mapply("/", as.data.frame(mat), dev)
DIV.matrix<-as_tibble(cbind(DIV.combined[,1],DIV.combined[,178],DIV.matrix, DIV.combined[,181],DIV.combined[,"coral_tag"],DIV.combined[,"host_genus"], DIV.combined[,"El_nino"],DIV.combined[,"site_name"],DIV.combined[,"Disturbance"] ))
#Create metadata dataframe
meta<-as_tibble(cbind(DIV.combined[,1],DIV.combined[,161:184]))
meta$Disturbance_cat<-factor(meta$Disturbance_cat, levels=c("Very Low", "Low", "Medium", "High", "Very High"))


###Platygyra - format data
DIV.Platygyra<-DIV.matrix[meta$host_genus=="Platygyra",]
Platygyra.avg<-DIV.Platygyra[,2:161] %>%
  drop_na() %>%
  group_by(expedition, Disturbance_cat) %>%
  summarise_all(funs(mean))
Platygyra.avg2<-Platygyra.avg[,3:160]
Platygyra.avg3<-drop_na(Platygyra.avg2)[,colSums(drop_na(Platygyra.avg2))>0]
rowSums(Platygyra.avg3)
Platygyra.avg4<-cbind(as.data.frame(Platygyra.avg[,1:2]),Platygyra.avg3) 
Platygyra.long<-Platygyra.avg4 %>% gather( key="DIV", value="Abundance", 3:47)
Platygyra.long$Disturbance_cat<-factor(Platygyra.long$Disturbance_cat,levels=c("Very Low", "Low","Medium","High", "Very High"))
colour.platy<-colour.scheme[colSums(Platygyra.avg2)>0,]
colour.platy<-colour.platy[order(colour.platy$Alpha_ID),]

#MAKE plot for Platygyra
ggplot(Platygyra.long, aes(fill=DIV, y=Abundance, x=expedition)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Disturbance_cat)+
  theme(legend.title = element_text(size = 10),legend.text = element_text(size = 8), axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values=colour.platy$Color %>% as.vector())+theme_cowplot(7)

#Count samples
DIV.Platygyra$Dummy<-paste(DIV.Platygyra$Disturbance_cat, "_", DIV.Platygyra$expedition)

Platygyra.count<-DIV.Platygyra %>%
  drop_na() %>%
  group_by(expedition, Disturbance_cat) %>%
  summarise(count=count(Dummy))
Platygyra.count

DIV.Platygyra

###Favites - format data
DIV.Favites<-DIV.matrix[meta$host_genus=="Favites",]
Favites.avg<-DIV.Favites[,2:161] %>%
  drop_na() %>%
  group_by(expedition, Disturbance_cat) %>%
  summarise_all(funs(mean))
Favites.avg2<-Favites.avg[,3:160]
Favites.avg3<-drop_na(Favites.avg2)[,colSums(drop_na(Favites.avg2))>0]
rowSums(Favites.avg3)
Favites.avg4<-cbind(as.data.frame(Favites.avg[,1:2]),Favites.avg3) 
Favites.long<-Favites.avg4 %>% gather( key="DIV", value="Abundance", 3:43)
Favites.long$Disturbance_cat<-factor(Favites.long$Disturbance_cat,levels=c("Very Low", "Low","Medium","High", "Very High"))
colour.platy<-colour.scheme[colSums(Favites.avg2)>0,]
colour.platy<-colour.platy[order(colour.platy$Alpha_ID),]

#Make plot for Favites pentagona
ggplot(Favites.long, aes(fill=DIV, y=Abundance, x=expedition)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Disturbance_cat)+
  theme(legend.title = element_text(size = 10),legend.text = element_text(size = 8), axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values=colour.platy$Color %>% as.vector())+theme_cowplot(8)


#Count sample size
DIV.Favites$Dummy<-paste(DIV.Favites$Disturbance_cat, "_", DIV.Favites$expedition)

Favites.count<-DIV.Favites %>%
  drop_na() %>%
  group_by(expedition, Disturbance_cat) %>%
  summarise(count=count(Dummy))
Favites.count

