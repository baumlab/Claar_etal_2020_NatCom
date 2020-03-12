library(ggplot2)
library(tidyverse)
library(imager)
library(gridExtra)
library(phyloseq)

##################################################################################
### KI Local Disturbance Map ###

# Load in and format KI map showing local human disturbance
# Use imager to load the image
img_KImap <- load.image('figures/KI_map_platysites_villages.jpg')
# was previously ...jpeg which made imager very mad and it didn't work.
# # Check that the data is in the correct format for the next step
# as.data.frame(img_KImap,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df2 <- as.data.frame(img_KImap,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
# # Double check that it worked
# head(df2,3)

# Turn image into a ggplot object for incorporating in multi-panel figure below
KImap <- ggplot(df2,aes(x,y))+geom_raster(aes(fill=rgb.val))+
  scale_fill_identity()+scale_y_reverse()+ 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.position="none", panel.background=element_blank(),
        panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        plot.margin = unit(c(-0.175,0,0,-0.1),c("in","in","in","in")))

##################################################################################
### Very Low Disturbance Photo ###

# Load in and format Very Low Disturbance example image
# Use imager to load the image
img_VLowDist <- load.image('figures/site15_jan15_DanielleClaar_IMG_4926_2.jpg')
# # Check that the data is in the correct format for the next step
# as.data.frame(img_VLowDist,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df3 <- as.data.frame(img_VLowDist,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
# # Double check that it worked
# head(df3,3)

# Turn image into a ggplot object for incorporating in multi-panel figure below
VLowDist <- ggplot(df3,aes(x,y)) + 
  geom_raster(aes(fill=rgb.val)) + 
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_identity()+scale_y_reverse(expand=c(0,0)) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),                                         axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),legend.position="none",                                            panel.border=element_rect(colour="black",fill=NA,size=1,linetype = "solid"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = unit(c(-0.17,0,0.05,-0.1),c("in","in","in","in")))


##################################################################################
### High Disturbance Photo ###

# Load in and format Very Low Disturbance example image
# Use imager to load the image
img_HighDist <- load.image('figures/highdisturbance_KI.jpg')
# # Check that the data is in the correct format for the next step
# as.data.frame(img_HighDist,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df4 <- as.data.frame(img_HighDist,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
# # Double check that it worked
# head(df4,3)

# Turn image into a ggplot object for incorporating in multi-panel figure below
HighDist <- ggplot(df4,aes(x,y)) + 
  geom_raster(aes(fill=rgb.val)) + 
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_identity()+scale_y_reverse(expand=c(0,0)) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),legend.position="none",                                            panel.background=element_blank(),
        panel.border=element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank(),plot.title = element_text("B"),
        plot.margin = unit(c(-0.17,0,0.1,-0.1),c("in","in","in","in"))) +  
  annotate("rect", xmin=0, ymin=0, xmax=120, ymax=130,color="white",fill="white") + 
  annotate("text", x = 72, y = 65, label = "A",fontface="bold", color="black")

###################################
samplelist <- read.csv("analyses/2020_analyses/ASV_ordination/samplelist.csv")

load("data/KI_Platy_f_coral_grouped_ASVs.RData")

D_cols <- c("VeryHigh"="#8c510a", "Medium"="#c7eae5", 
            "Low"="#5ab4ac", "VeryLow"="#01665e")

ord_samples <- samplelist$ord_sample

samplelist <- samplelist[!is.na(samplelist$ord_sample),]
samplelist_fpenta <- samplelist[samplelist$Coral_Species=="F. pentagona",]

ord_samples2 <- data.frame(samplelist_fpenta$ProposedSurvival_Status, 
                           row.names = samplelist_fpenta$ord_sample)

fpenta_ord_physeq0 <- subset_samples(phyASV.f.c,
                                     sample_data(phyASV.f.c)$SampleID %in% ord_samples)

fpenta_ord_physeq <- merge_phyloseq(fpenta_ord_physeq0,sample_data(ord_samples2))

colnames(sample_data(fpenta_ord_physeq))[colnames(sample_data(fpenta_ord_physeq))=="samplelist_fpenta.ProposedSurvival_Status"] <- "updated_status"

fpenta_ord_physeq <- subset_samples(fpenta_ord_physeq,
                                    sample_data(fpenta_ord_physeq)$updated_status!="unknown")

sample_data(fpenta_ord_physeq)$Dist <- factor(sample_data(fpenta_ord_physeq)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

fpenta_ord_physeq <- rarefy_even_depth(fpenta_ord_physeq, 
                                       sample.size = 1000)

fpenta_ord_CAP <- ordinate(fpenta_ord_physeq,method="CAP",
                           distance="wunifrac",formula= ~ field_season + Dist)

p1_penta <- plot_ordination(fpenta_ord_physeq, fpenta_ord_CAP,
                      shape="updated_status", color="Dist",type="samples",title="")

# Format ordination plot
p_fpenta_CAP <- p1_penta + 
  theme(legend.position=c(.83, .89),
        legend.box = "horizontal",legend.background = element_blank(), 
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),legend.key.height=unit(0.3,"line"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = unit(c(-0.25,0,0.15,0),c("in","in","in","in")),
        axis.title = element_text(size=8), 
        axis.text.x = element_text(size=8,margin = margin(b=5, t = 5)),
        axis.text.y = element_text(size=8,margin = margin(b=5, r = 5)), 
        axis.title.x = element_text(size=8,margin = margin(b=5, t = 0.1)),
        axis.title.y = element_text(size=8,margin = margin(b=5, r = 0.1)), 
        axis.ticks.length=unit(-0.05, "in")) + 
  geom_point(alpha=0.8,size=1.5) + 
  scale_shape_manual(name= "Coral Fate", values=c("alive"=19,"dead"=24),
                     labels=c("Survived","Died")) + 
  # scale_size_manual(values = c(5,1))+
  scale_color_manual(values=D_cols) +
  scale_fill_manual(values=D_cols) +
  # guides(color=F) +
  stat_ellipse(aes(group=updated_status), type = "norm",
               level=0.8,color=c("darkgray"),lty=2) +
  # annotate("text", x = -1.4, y = 4, label = "C",fontface="bold")
  NULL
p_fpenta_CAP

####################################

samplelist_platy <- samplelist[samplelist$Coral_Species=="P. ryukyuensis",]

ord_samples2 <- data.frame(samplelist_platy$ProposedSurvival_Status, 
                           row.names = samplelist_platy$ord_sample)

platy_ord_physeq0 <- subset_samples(phyASV.f.c,
                                    sample_data(phyASV.f.c)$SampleID %in% ord_samples)

platy_ord_physeq <- merge_phyloseq(platy_ord_physeq0,sample_data(ord_samples2))

colnames(sample_data(platy_ord_physeq))[colnames(sample_data(platy_ord_physeq))=="samplelist_platy.ProposedSurvival_Status"] <- "updated_status"

platy_ord_physeq <- subset_samples(platy_ord_physeq,sample_data(platy_ord_physeq)$updated_status!="unknown")

sample_data(platy_ord_physeq)$Dist <- factor(sample_data(platy_ord_physeq)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

platy_ord_physeq <- rarefy_even_depth(platy_ord_physeq, 
                                      sample.size = 1000)

platy_ord_CAP <- ordinate(platy_ord_physeq,method="CAP",
                          distance="wunifrac",formula= ~ field_season + Dist)

p1_platy <- plot_ordination(platy_ord_physeq, platy_ord_CAP,
                      shape="updated_status", color="Dist",type="samples",title="")

# Format ordination plot
p_platy_CAP <- p1_platy + 
  theme(legend.position=c(.83, .89),
        legend.box = "horizontal",legend.background = element_blank(), 
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),legend.key.height=unit(0.3,"line"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = unit(c(-0.25,0,0.15,0),c("in","in","in","in")),
        axis.title = element_text(size=8), 
        axis.text.x = element_text(size=8,margin = margin(b=5, t = 5)),
        axis.text.y = element_text(size=8,margin = margin(b=5, r = 5)), 
        axis.title.x = element_text(size=8,margin = margin(b=5, t = 0.1)),
        axis.title.y = element_text(size=8,margin = margin(b=5, r = 0.1)), 
        axis.ticks.length=unit(-0.05, "in")) + 
  geom_point(alpha=0.8,size=1.5) + 
  scale_shape_manual(name= "Coral Fate", values=c("alive"=19,"dead"=24),
                     labels=c("Survived","Died")) + 
  # scale_size_manual(values = c(5,1))+
  scale_color_manual(values=D_cols) +
  scale_fill_manual(values=D_cols) +
  # guides(color=F) +
  stat_ellipse(aes(group=updated_status), type = "norm",
               level=0.8,color=c("darkgray"),lty=2) +
  # annotate("text", x = -1.4, y = 4, label = "C",fontface="bold")
  NULL

p_platy_CAP

###################################

load("figures/Platy_Favites_LogisticPlots.RData")
# Named: P1, P2 and P3 for Platy and F1, F2 and F3 for Favites


# Open a jpg image
# jpeg(file="figures/Figure1_new.jpg",width = 7.2, height = 2.8,units="in",res=300)

# grid.arrange(arrangeGrob(arrangeGrob(HighDist,VLowDist,nrow=1,ncol=2,
#                                      widths=unit(c(1.17,1.17),c("in","in"))),
#                          KImap,
#                          ord_plot,
#                          ncol=3,widths=unit(c(1.4,2.8,2.6),
#                                             c("in","in","in"))),
#              nrow=1, heights=unit(c(2.6),c("in")),widths=unit(c(7.2),c("in")))

# dev.off()

library("patchwork")

layout <- "
AADDFF
AADDFF
BBEEGG
CCEEGG
"
# jpeg(file="figures/Figure1_new.jpg",
     width = 7.2, height = 5,units="in",res=300)
(KImap)/(HighDist/VLowDist)|(p_platy_CAP/P1)|(p_fpenta_CAP/F1)+
  plot_layout(design=layout)
# dev.off()

(P1)/(P1/F1)|(p_platy_CAP/P1)|(p_fpenta_CAP/F1)+
  plot_layout(design=layout)

jpeg(file="figures/Figure1_new.jpg",
     width = 7.2, height = 5,units="in",res=300)
KImap+HighDist+VLowDist+p_platy_CAP+P1+p_fpenta_CAP+F1+
  plot_layout(design=layout)
dev.off()
