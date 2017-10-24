#### KI PLATY - FIGURE 2 ####

# Reset graphical parameters
dev.off()

# Clear your environment
rm(list=ls())

# Load necessary packages
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load in data
load("data/KI_seqs_f_coral_grouped.RData")
load("data/temperature/KI_SB_temp_DHW.RData")

# ### Prep time vector for plotting
# # Set dates for the field seasons (for plotting)
# KI2014 <- as.POSIXct("2014-09-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# KI2015a <- as.POSIXct("2015-01-20 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# KI2015b <- as.POSIXct("2015-05-10 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# KI2015c <- as.POSIXct("2015-07-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# KI2016a <- as.POSIXct("2016-03-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# # Set a start and end date for plotting
# startdate <- as.POSIXct("2014-08-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# enddate <- as.POSIXct("2016-11-19 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# # Truncate the heat data from startdate to enddate (so we can plot)
# KI_heat <- KI_allsites_DHW[which(KI_allsites_DHW$xi3>startdate),]
# KI_heat <- KI_heat[which(KI_heat$xi3<enddate),]
# # Rename columns
# colnames(KI_heat)<- c("time","dhw")
# # Set from-to dates for plotting
# from <- KI_heat$time-1.75*86400
# to <- KI_heat$time+1.75*86400
# 
# # Make dataframes for the field_season identifier (dates) and the POSIXct date
# dates <- t(data.frame("KI2014","KI2015a","KI2015b","KI2015c","KI2016a"))
# Pdates <- t(data.frame(KI2014,KI2015a,KI2015b,KI2015c,KI2016a))

# Extract the otu table and tax table into data frames
otu_platy <- data.frame(otu_table(phy97.f.c.platy.p))
otu_platy$otu <- row.names(otu_platy)
tax_platy <- data.frame(tax_table(phy97.f.c.platy.p))
sam_platy <- data.frame(sample_data(phy97.f.c.platy.p))
# Join together otu table an tax table
otu_tax_platy <- join_all(list(otu_platy, tax_platy),by = "otu", type="full")

# Aggregate by clade (i.e. collapse all types down to clade)
platy_clades <- aggregate(otu_tax_platy[,1:146], by=list(Clade=otu_tax_platy$clade), FUN=sum)

# Transpose
platy_clades.t <- t(platy_clades)
# Name columns
colnames(platy_clades.t) <- platy_clades.t[1,]
# Format data frame
platy_clades.t <- data.frame(platy_clades.t[2:146,])

# Add in sample data for plotting
platy_clades.t_sam <- merge(platy_clades.t,sam_platy, by=0,all=TRUE)

# head(platy_clades.t_sam)

# Make sure that 
platy_clades.t_sam$C <- as.numeric(as.character(platy_clades.t_sam$C))
platy_clades.t_sam$D <- as.numeric(as.character(platy_clades.t_sam$D))
platy_clades.t_sam$A <- as.numeric(as.character(platy_clades.t_sam$A))
platy_clades.t_sam$G <- as.numeric(as.character(platy_clades.t_sam$G))

followed_corals <- c("62","99","157","161","223","234","253","289","328","353","379","389","402","442","455","466","470","486","594","612","618","637","727","735","742","754","762","766","768","783","788","792","797","807","813","820","824","850","857","248_696")
platy_clades.t_sam_subset <- subset(platy_clades.t_sam, coral_tag %in% followed_corals)

ks <- c("alive","dead")
platy_clades.t_sam_subset2 <- subset(platy_clades.t_sam, Status %in% ks)

status_colors <- c("alive" = "#ca0020", "dead" = "#0571b0")

platy_clades.t_sam_subset2$Dist <- factor(platy_clades.t_sam_subset2$Dist, levels= c("VeryLow","Low","HighMed","VeryHigh"))

p <- ggplot(data=platy_clades.t_sam_subset2,aes(x=field_season,y=C),group=coral_tag)

p+geom_line(aes(group=coral_tag, color=Status))+scale_color_manual(values=status_colors)

p+geom_line(aes(group=coral_tag, color=Status))+facet_grid(.~Dist)+scale_color_manual(values=status_colors)

# p+geom_line(aes(group=coral_tag, color=Status))+stat_smooth(aes(group=Dist,color=Dist))+stat_summary(aes(group=Dist),geom="point",fun.y=mean,size=3)

p2 <- ggplot(data=platy_clades.t_sam_subset2,aes(x=field_season,y=D),group=coral_tag)

p2+geom_line(aes(group=coral_tag, color=Status))+scale_color_manual(values=status_colors)

p2+geom_line(aes(group=coral_tag, color=Status))+facet_grid(.~Dist)+scale_color_manual(values=status_colors)

tiff(file="figures/Fig2_v2_temp.tiff",width = 12, height = 6,units="in",res=300)
grid.arrange(ncol=2,p+geom_line(aes(group=coral_tag, color=Status))+scale_color_manual(values=status_colors)+theme(legend.position=c(0.15,0.2)),p2+geom_line(aes(group=coral_tag, color=Status))+scale_color_manual(values=status_colors,guide=FALSE))
dev.off()



p3 <- ggplot(data=platy_clades.t_sam_subset2,aes(x=field_season,y=A),group=coral_tag)

p3+geom_line(aes(group=coral_tag, color=Status))+scale_color_manual(values=status_colors)

p3+geom_line(aes(group=coral_tag, color=Status))+facet_grid(.~Dist)+scale_color_manual(values=status_colors)


p4 <- ggplot(data=platy_clades.t_sam_subset2,aes(x=field_season,y=G),group=coral_tag)

p4+geom_line(aes(group=coral_tag, color=Status))+scale_color_manual(values=status_colors)

p4+geom_line(aes(group=coral_tag, color=Status))+facet_grid(.~Dist)+scale_color_manual(values=status_colors)

