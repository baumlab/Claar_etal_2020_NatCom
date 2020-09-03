#code to make extended figure #5 for Claar et al.

#<---------------------------------
#<--remove items in the environment-->
#<---------------------------------
rm(list=ls())

#dev.off()

#<---------------------------------
#<--load required packages-->
#<---------------------------------
library(here)
library(ggplot2)
library(gridExtra)
library(grid)

#<--read in data-->
nov15 <- read.csv(here::here("data/Bleaching", "KI_2015d_Merulinidae.csv"))
coralnet <- read.csv(here::here("data/Bleaching", "data_Supplementary_Figure_8.csv")) 

#<---------------------------------
#<--set up color schemes for figures-->
#<---------------------------------

bleachcols.pub <- c("white", "black")

bleachcols.pub2 <- c("white", "gray75", "gray50", "black")

#<---------------------------------
#<--prepare bleaching in Nov. 2015 data-->
#<---------------------------------
head(nov15)
str(nov15)

###check columns because I know I was not careful with capitals etc

unique(nov15$site) #good
###although I am going to rename the bay of wrecks one to just BOW so it is shorter
nov15$site <- ifelse(nov15$site == "BayofWrecks_Loggers", "bow", nov15$site)
unique(nov15$site) #good

unique(nov15$species) #ok of course some to fix which i knew

nov15$species <- ifelse(nov15$species == "hydno", "Hydnophora", nov15$species)
nov15$species <- ifelse(nov15$species == "favia sp.", "Favia sp.", nov15$species)
nov15$species <- ifelse(nov15$species == "favia sp. ", "Favia sp.", nov15$species)
nov15$species <- ifelse(nov15$species == "Favia sp. ", "Favia sp.", nov15$species)
nov15$species <- ifelse(nov15$species == "favid", "Favid", nov15$species)
nov15$species <- ifelse(nov15$species == "f. pentagona", "F. pentagona", nov15$species)
nov15$species <- ifelse(nov15$species == "f pentagona", "F. pentagona", nov15$species)
nov15$species <- ifelse(nov15$species == "F pentagona", "F. pentagona", nov15$species)
nov15$species <- ifelse(nov15$species == "Favities", "Favites sp.", nov15$species)
nov15$species <- ifelse(nov15$species == "favites sp", "Favites sp.", nov15$species)
nov15$species <- ifelse(nov15$species == "Favia halicora", "Favites halicora", nov15$species)
nov15$species <- ifelse(nov15$species == "favia matthai", "Favia matthai", nov15$species)
nov15$species <- ifelse(nov15$species == "Hydno", "Hydnophora", nov15$species)
nov15$species <- ifelse(nov15$species == "platygyra sp.", "Platygyra sp.", nov15$species)

unique(nov15$species) 

unique(nov15$bleaching.proportion) #good to go

unique(nov15$cut.off.y.n) # good to go

head(nov15)

###ok now combine some of the species
nov15$species.comb <- nov15$species

nov15$species.comb <- ifelse((nov15$species == "Favia sp." | nov15$species == "Favia matthai"), "Favia spp", nov15$species.comb)

nov15$species.comb <- ifelse((nov15$species == "Favites halicora" | nov15$species == "Favites sp."), "Favites spp", nov15$species.comb)

unique(nov15$species.comb)

###add disturbance level
nov15$disturb <- "VH"
nov15$disturb <- ifelse(nov15$site == "8", "M", nov15$disturb)
nov15$disturb <- ifelse(nov15$site == "35", "M", nov15$disturb)
nov15$disturb <- ifelse(nov15$site == "bow", "VL", nov15$disturb)

unique(nov15$disturb)

head(nov15)

###add column of binary bleaching
nov15$bin.bleach <- 0
nov15$bin.bleach <- ifelse((nov15$bleaching.proportion == "3" | nov15$bleaching.proportion == "2"), 1, nov15$bin.bleach)
head(nov15)
tail(nov15)

###determine order of levels
str(nov15)
nov15$disturb <- as.factor(nov15$disturb)
levels(nov15$disturb)
nov15$disturb<-factor(nov15$disturb, levels = c("VH", "M", "VL")) 
levels(nov15$disturb)

nov15$species.comb <- as.factor(nov15$species.comb)
levels(nov15$species.comb)
nov15$species.comb<-factor(nov15$species.comb, levels = c("Platygyra sp.", "F. pentagona",  "Favia spp", "Favid", "Favites spp", "Hydnophora")) 
levels(nov15$species.comb)

#convert 4 (4 = recent (texture has not been eroded yet) partial mortality where living tissue is not bleached)) into 0
head(nov15)
str(nov15)
nov15$bleach.prop.no4 <- nov15$bleaching.proportion
nov15$bleach.prop.no4 <- ifelse(nov15$bleach.prop.no4 == "4", "0", nov15$bleach.prop.no4)
unique(nov15$bleach.prop.no4)

#take out 5
nov15 <- nov15[!nov15$bleaching.proportion == "5", ]
nov15 <- droplevels(nov15)
unique(nov15$bleach.prop.no4)

#make a table with the new column

nov.tbl <- plyr::count(nov15, c("site", "species.comb", "bleach.prop.no4"))

nov.tbl

### add disturbance level to the table
nov.pub <- nov.tbl
nov.pub$disturb <- nov15$disturb[match(nov.pub$site, nov15$site)]
str(nov.pub)

#remove the bow since there is not enough sample size there
unique(nov.pub$site)
nov.pub <- nov.pub[!nov.pub$site == "bow", ]
nov.pub <- droplevels(nov.pub)
unique(nov.pub$site)
head(nov.pub)

#subset for just platy and fpenta
nov.pub <- nov.pub[(nov.pub$species.comb == "Platygyra sp."| nov.pub$species.comb == "F. pentagona"), ]
nov.pub <- droplevels(nov.pub)
head(nov.pub)

##add the binary column
str(nov.pub)
nov.pub$bleach.bin <- "Healthy"
nov.pub$bleach.bin <- ifelse((nov.pub$bleach.prop.no4 == "2" | nov.pub$bleach.prop.no4 == "3"), "Bleached", nov.pub$bleach.bin)
head(nov.pub)

#sum up number of each species at the two disturbance levels
nov.pub.sum.disturb <- aggregate(freq ~ species.comb + disturb + bleach.bin, nov.pub, sum)
nov.pub.sum.disturb

#now the total for each species
nov.pub.sum <- aggregate(freq ~ species.comb + disturb, nov.pub, sum)
nov.pub.sum

#add that total to the other df
nov.pub.sum.disturb$species.disturb <- paste(nov.pub.sum.disturb$species.comb, nov.pub.sum.disturb$disturb, sep = ".")
nov.pub.sum$species.disturb <- paste(nov.pub.sum$species.comb, nov.pub.sum$disturb, sep = ".")
nov.pub.sum.disturb$disturb.tot <- nov.pub.sum$freq[match(nov.pub.sum.disturb$species.disturb, nov.pub.sum$species.disturb)]
nov.pub.sum.disturb

#now calculate proportions
nov.pub.sum.disturb$prop <- nov.pub.sum.disturb$freq/nov.pub.sum.disturb$disturb.tot
nov.pub.sum.disturb

###change the disturbance labels
unique(nov.pub.sum.disturb$disturb)
nov.pub.sum.disturb$disturb <- as.character(levels(nov.pub.sum.disturb$disturb))[nov.pub.sum.disturb$disturb]
nov.pub.sum.disturb$disturb <- ifelse(nov.pub.sum.disturb$disturb == "VH", "very high", nov.pub.sum.disturb$disturb)
nov.pub.sum.disturb$disturb <- ifelse(nov.pub.sum.disturb$disturb == "M", "medium", nov.pub.sum.disturb$disturb)
unique(nov.pub.sum.disturb$disturb)

#change structure and orders
str(nov.pub.sum.disturb)
nov.pub.sum.disturb$bleach.bin <- as.factor(nov.pub.sum.disturb$bleach.bin)
levels(nov.pub.sum.disturb$bleach.bin)
nov.pub.sum.disturb$disturb <- as.factor(nov.pub.sum.disturb$disturb)
levels(nov.pub.sum.disturb$disturb)
nov.pub.sum.disturb$disturb<-factor(nov.pub.sum.disturb$disturb, levels = c("very high", "medium"))
nov.pub.sum.disturb

#now split into two df for each species
nov.pub.sum.disturb.platy <- nov.pub.sum.disturb[nov.pub.sum.disturb$species.comb == "Platygyra sp.", ]
nov.pub.sum.disturb.fpenta <- nov.pub.sum.disturb[nov.pub.sum.disturb$species.comb == "F. pentagona", ]

#<---------------------------------
#<--make November 2015 figures-->
#<---------------------------------

#########Platy figure

platy.nov <- ggplot(nov.pub.sum.disturb.platy, aes(x = disturb, y = prop, fill = bleach.bin)) +
  geom_bar(stat='identity', position='stack', color = "black") +
  labs(x = "", y = "", fill = "Status")+
  scale_fill_manual(values = bleachcols.pub) +
  theme(axis.text.x = element_text(angle = 0, face = "italic"), plot.background=element_blank(), 
        panel.background = element_blank(), 
        strip.background = element_blank(), strip.text = element_text(size = 13),
        axis.line = element_line(color = "black"), legend.position="bottom", 
        axis.text = element_text(color="black", size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        axis.title = element_text(color = "black", size = 14),
        panel.border = element_blank()) +
  scale_y_continuous(expand = c(0,0)) + guides(fill=FALSE)

platy.nov


#########F. pentagona figure


fpenta.nov <- ggplot(nov.pub.sum.disturb.fpenta, aes(x = disturb, y = prop, fill = bleach.bin)) +
  geom_bar(stat='identity', position='stack', color = "black") +
  labs(x = "Human Disturbance Category", y = "", fill = "Status")+
  scale_fill_manual(values = bleachcols.pub) +
  theme(axis.text.x = element_text(angle = 0, face = "italic"), plot.background=element_blank(), 
        panel.background = element_blank(), 
        strip.background = element_blank(), strip.text = element_text(size = 13),
        axis.line = element_line(color = "black"), legend.position="bottom", 
        axis.text = element_text(color="black", size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        axis.title = element_text(color = "black", size = 14),
        panel.border = element_blank()) +
  scale_y_continuous(expand = c(0,0)) + guides(fill=FALSE)

fpenta.nov


#<---------------------------------
#<--prepare data from coralnet-->
#<---------------------------------

head(coralnet)
cn <- coralnet

unique(cn$Field.Season)
unique(cn$Site)
unique(cn$Species)

#take site out of site column
cn$site <- cn$Site
str(cn$site)
unique(cn$site)
cn$site <- ifelse(grepl("e8", cn$Site), "8", cn$site)
cn$site <- ifelse(grepl("e35", cn$Site), "35", cn$site)

head(cn)

#set order to species and check structure
str(cn)

cn$Field.Season <- as.factor(cn$Field.Season)
cn$Site <- as.factor(cn$Site)

cn$Species <- as.factor(cn$Species)
levels(cn$Species)
cn$Species <- factor(cn$Species, levels = c("Platygyra.spp", "Favites.pentagona"))
levels(cn$Species)

head(cn)

###now to create tables and calculate proportions etc
cn.sum <-aggregate(Cover ~  Species + Field.Season + Bleached, cn, sum)
head(cn.sum)

#calculate the number of points at each site for each species (healthy and bleached) so we can get proportion of bleached vr healthy
cn.sum.species <- aggregate(Cover ~ Species + Field.Season, cn, sum)
head(cn.sum.species)

#now add to the other dataframe 
cn.sum$season.spec <- paste(cn.sum$Field.Season, cn.sum$Species, sep = ".")
cn.sum.species$season.spec <- paste(cn.sum.species$Field.Season, cn.sum.species$Species, sep = ".")
cn.sum$total.season.spec <- cn.sum.species$Cover[match(cn.sum$season.spec, cn.sum.species$season.spec)]
head(cn.sum)

#now calculate proportion
cn.sum$prop <- cn.sum$Cover/cn.sum$total.season.spec
head(cn.sum)

#<---------------------------------
#<--make coralnet figures-->
#<---------------------------------

#########Platy figure
#cn.sum.c.pub.platy <- cn.sum.c.pub[cn.sum.c.pub$Species == "Platygyra.spp", ]
cn.sum.platy <- cn.sum[cn.sum$Species == "Platygyra.spp", ]

coralnet.platy <- ggplot(cn.sum.platy, aes(x = Field.Season, y = prop, fill = Bleached)) +
  geom_bar(stat='identity', position='stack', color = "black") +
  labs(x = "", y = "", fill = "Status")+
  scale_fill_manual(values = bleachcols.pub) +
  scale_x_discrete(labels = c("KI2015b" = "May 2015", "KI2015c" = "July 2015", "KI2015d" = "November 2015", "KI2016a" = "March 2016")) +
  theme(axis.text.x = element_text(angle = 0), plot.background=element_blank(), 
        panel.background = element_blank(), 
        strip.background = element_blank(), strip.text = element_text(size = 13),
        axis.line = element_line(color = "black"), legend.position="bottom", 
        axis.text = element_text(color="black", size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        axis.title = element_text(color = "black", size = 14),
        panel.border = element_blank()) +
  scale_y_continuous(expand = c(0,0)) + guides(fill = FALSE)

coralnet.platy

#########F. pentagona figure

cn.sum.fpent <- cn.sum[cn.sum$Species == "Favites.pentagona", ]

coralnet.fpent <- ggplot(cn.sum.fpent, aes(x = Field.Season, y = prop, fill = Bleached)) +
  geom_bar(stat='identity', position='stack', color = "black") +
  labs(x = "Time Point", y = "", fill = "Status")+
  scale_fill_manual(values = bleachcols.pub) +
  scale_x_discrete(labels = c("KI2015b" = "May 2015", "KI2015c" = "July 2015", "KI2015d" = "November 2015", "KI2016a" = "March 2016")) +
  theme(axis.text.x = element_text(angle = 0), plot.background=element_blank(), 
        panel.background = element_blank(), 
        strip.background = element_blank(), strip.text = element_text(size = 13),
        axis.line = element_line(color = "black"), legend.position="bottom", 
        axis.text = element_text(color="black", size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 13),
        axis.title = element_text(color = "black", size = 14),
        panel.border = element_blank()) +
  scale_y_continuous(expand = c(0,0))  

coralnet.fpent


#<---------------------------------
#<--combine to make final figure-->
#<---------------------------------

### open pdf
pdf("figures/Supplementary_Materials/Supplementary_Figure_8/Supplementary_Figure_8.pdf", width = 12)

### make a blank thing to add extra space to top of figure so panel labels fit nicely
r <- textGrob("")

### set up space for legend
g_legend <-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

### create legend
legend <- g_legend(coralnet.fpent)

###combine figures
grid.arrange(arrangeGrob(r, coralnet.platy, platy.nov, coralnet.fpent + theme(legend.position = "none"),  fpenta.nov, legend, 
                         layout_matrix = rbind(c(1,1,1,1,1),
                                               c(1,2,2,3,3),
                                               c(1,4,4,5,5),
                                               c(NA,NA,6,6,NA)), 
                         heights = rbind((unit(0.5, "in")),(unit(3, "in")), (unit(3, "in")), (unit(0.3, "in"))), 
                         widths = rbind((unit(0.08, "in")), (unit(0.5, "in")),(unit(0.5, "in")),(unit(0.5, "in")),(unit(0.5, "in")))))

###add y axis labels
line_1 <- expression("Proportion of randomly sampled benthic")
line_2 <- expression(paste("points at ", italic("medium"), " disturbance sites"))
###grid.text(expression(paste(atop("Proportion of randomly sampled", atop("benthic points at ",italic("medium")," disturbance sites"))), x = unit(0.03, "npc"), y = unit(0.56, "npc"), gp = gpar(fontsize=15), rot = 90)
grid.text(line_1, x = unit(0.025, "npc"), y = unit(0.56, "npc"), gp = gpar(fontsize=15), rot = 90 )
grid.text(line_2, x = unit(0.046, "npc"), y = unit(0.56, "npc"), gp = gpar(fontsize=15), rot = 90 )
grid.text("Proportion of individual colonies in November 2015", x = unit(0.525, "npc"), y = unit(0.55, "npc"), gp = gpar(fontsize=15), rot = 90)

###add panel labels
grid.text("a", x = unit(0.085, "npc"), y = unit(0.96, "npc"), gp = gpar(fontsize=18, fontface = "bold"))
grid.text("b", x = unit(0.57, "npc"), y = unit(0.96, "npc"), gp = gpar(fontsize=18, fontface = "bold"))
grid.text("c", x = unit(0.085, "npc"), y = unit(0.51, "npc"), gp = gpar(fontsize=18, fontface = "bold"))
grid.text("d", x = unit(0.57, "npc"), y = unit(0.51, "npc"), gp = gpar(fontsize=18, fontface = "bold"))

###add sample sizes
##panel a
grid.text("86", x = unit(0.155, "npc"), y = unit(0.935, "npc"), gp = gpar(fontsize=12))
grid.text("89", x = unit(0.255, "npc"), y = unit(0.935, "npc"), gp = gpar(fontsize=12))
grid.text("42", x = unit(0.355, "npc"), y = unit(0.935, "npc"), gp = gpar(fontsize=12))
grid.text("65", x = unit(0.45, "npc"), y = unit(0.935, "npc"), gp = gpar(fontsize=12))

##panel b
grid.text("19", x = unit(0.69, "npc"), y = unit(0.935, "npc"), gp = gpar(fontsize=12))
grid.text("88", x = unit(0.882, "npc"), y = unit(0.935, "npc"), gp = gpar(fontsize=12))

##panel c
grid.text("19", x = unit(0.155, "npc"), y = unit(0.495, "npc"), gp = gpar(fontsize=12))
grid.text("77", x = unit(0.255, "npc"), y = unit(0.495, "npc"), gp = gpar(fontsize=12))
grid.text("16", x = unit(0.355, "npc"), y = unit(0.495, "npc"), gp = gpar(fontsize=12))
grid.text("10", x = unit(0.45, "npc"), y = unit(0.495, "npc"), gp = gpar(fontsize=12))

##panel d
grid.text("4", x = unit(0.69, "npc"), y = unit(0.495, "npc"), gp = gpar(fontsize=12))
grid.text("31", x = unit(0.882, "npc"), y = unit(0.495, "npc"), gp = gpar(fontsize=12))

###close pdf
dev.off()
