# Reset graphical parameters
dev.off()

# Clear your environment
rm(list=ls())

# Load necessary packages
library(phyloseq)
library(vegan)
library(ggplot2)
library(phangorn)
library(gridExtra)
library(plyr)
library(dplyr)
library(imager)
library(ggrepel)

# Load in Data
# You will need to be in the KI_Platy directory for this to work
load("data/KI_seqs_f_coral_grouped.RData")


##################################################################################
### Formatting ###

# Set colors for disturbance levels
D_cols <- c("Very High"="#a6611a", "Medium"="#dfc27d", "Low"="#80cdc1", "Very Low"="#018571")
# D_cols <- c("Very High"="#e66101", "Medium"="#fdb863", "Low"="#b2abd2", "Very Low"="#5e3c99")

# Set ggplot2 basic parameters
theme_set(theme_bw())

# Rename "Disturbance" column
colnames(sample_data(phy97.f.c.platy.AD.before))[25] = "Disturbance"
# Capitalize the words "Alive" and "Dead"
sample_data(phy97.f.c.platy.AD.before)$Status <- gsub("alive","Alive",sample_data(phy97.f.c.platy.AD.before)$Status)
sample_data(phy97.f.c.platy.AD.before)$Status <- gsub("dead","Dead",sample_data(phy97.f.c.platy.AD.before)$Status)
# Rename Disturbance Levels for easy plotting
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("HighMed","Medium",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("VeryHigh","Very High",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("VeryLow","Very Low",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
# Set factor levels for "Disturbance"
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- factor(sample_data(phy97.f.c.platy.AD.before)$Disturbance, levels = c("Very Low","Low","Medium","Very High"))


##################################################################################
### Ordination Plot (CAP) ###

# Create ordination for plotting
ord.phy97.f.c.platy.AD.before.CAP <- ordinate(phy97.f.c.platy.AD.before,
                                              method="CAP",distance="wunifrac",
                                              formula= ~ field_season + Disturbance)

# # Plot this ordination - scree plot (Optional, helpful for looking at explained variance)
# plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,  shape="Status",type="scree",title="97% within-sample OTUs - CAP Platygyra only")

# Plot this ordination - Make plot using phyloseq & ggplot2
p1 <- plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,
                      shape="Status", color="Disturbance",type="samples",title="")
# Format ordination plot
ord_plot <- p1 + 
  geom_point() + 
      scale_shape_manual(values = c(9,20)) + scale_size_manual(values = c(0.75,1))+
      scale_color_manual(values=D_cols) + scale_fill_manual(values=D_cols) + guides(color=F) +
  theme(legend.position=c(.88, .89),
        legend.box = "horizontal",legend.background = element_blank(), 
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),legend.key.height=unit(0.3,"line"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = unit(c(-0.3,0,0,0),c("in","in","in","in")),
        axis.title = element_text(size=8), 
        axis.text.x = element_text(size=8,margin = margin(b=5, t = 5)),
        axis.text.y = element_text(size=8,margin = margin(b=5, r = 5)), 
        axis.title.x = element_text(size=8,margin = margin(b=5, t = 0.1)),
        axis.title.y = element_text(size=8,margin = margin(b=5, r = 0.1)), 
        axis.ticks.length=unit(-0.05, "in")) + 
  stat_ellipse(aes(group=Status), type = "t",level=0.95,color=c("darkgray"),lty=2)
  

# Disturbance centroids plot
dist_cent <- ggplot(d2, aes(CAP1,CAP2)) + geom_point() + geom_label_repel(aes(label=row.names(d2)),box.padding = 0.5,col=D_cols[c(4,3,2,1)]) + ylim(-0.35,0.2)

jpeg(file="figures/DisturbanceCentroids.jpg",width = 7.2, height = 4,units="in",res=300)
dist_cent
dev.off()

jpeg(file="figures/Disturbance_and_fieldseason_Centroids.jpg",width = 7.2, height = 4,units="in",res=300)
ggplot(d, aes(CAP1,CAP2)) + geom_point() + geom_label_repel(aes(label=row.names(d)),box.padding = 0.5,col=c("black","black","black",D_cols[c(4,3,2,1)])) + ylim(-0.35,0.2)
dev.off()

##################################################################################
### El Niño Global Map ###

# Load in and format world map showing El Niño
# Use imager to load the image
img_20152016ElNino <- load.image('figures/global_elnino_map/figure_1.jpg')
# # Check that the data is in the correct format for the next step
# as.data.frame(img_20152016ElNino,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df <- as.data.frame(img_20152016ElNino,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
# # Double check that it worked
# head(df,3)

# Turn image into a ggplot object for incorporating in multi-panel figure below
world_20152016ElNino <- ggplot(df,aes(x,y)) + 
  geom_raster(aes(fill=rgb.val)) + 
  scale_fill_identity()+scale_y_reverse() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),                                         axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),legend.position="none",                                            panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = unit(c(0,0,0,0),c("in","in","in","in")))

##################################################################################
### KI Local Disturbance Map ###

# Load in and format KI map showing local human disturbance
# Use imager to load the image
img_KImap <- load.image('figures/KI_map_platysites_villages.jpeg')
# # Check that the data is in the correct format for the next step
# as.data.frame(img_KImap,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df2 <- as.data.frame(img_KImap,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
# # Double check that it worked
# head(df2,3)

# Turn image into a ggplot object for incorporating in multi-panel figure below
KImap <- ggplot(df2,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+scale_y_reverse()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),                                      axis.text.y=element_blank(),axis.ticks=element_blank(),                                         axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),legend.position="none",                                            panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = unit(c(-0.2,0,0,-0.1),c("in","in","in","in")))


##################################################################################
### Very Low Disturbance Photo ###

# Load in and format Very Low Disturbance example image
# Use imager to load the image
img_VLowDist <- load.image('figures/VeryLowDisturbance_KI_cropped.jpg')
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
        plot.margin = unit(c(-0.09,0,0,-0.2),c("in","in","in","in")))

##################################################################################
### High Disturbance Photo ###

# Load in and format Very Low Disturbance example image
# Use imager to load the image
img_HighDist <- load.image('figures/highdisturbance_KI_cropped.jpg')
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
        plot.background=element_blank(),
        plot.margin = unit(c(-0.12,0,0,-0.2),c("in","in","in","in")))


##################################################################################
### Make the multi-panel plot ###

# Open a tiff image
tiff(file="figures/Figure3.tiff",width = 7.2, height = 4.07,units="in",res=300)

# lay=rbind(c(1,1,1),c(2,4,5),c(3,4,5))
# grid.arrange(world_20152016ElNino,HighDist,VLowDist,KImap,ord_plot,ncol=3, nrow=3, layout_matrix=lay, widths=unit(c(2,2,3.2),c("in","in","in")),heights=unit(c(1.6,1.15,1.15),c("in","in","in")))
grid.arrange(world_20152016ElNino,arrangeGrob(arrangeGrob(HighDist,VLowDist,nrow=2,heights=unit(c(1.05,1.05),c("in","in"))),KImap,ord_plot,ncol=3,widths=unit(c(1.5,2.1,3.1),c("in","in","in"))),nrow=2, heights=unit(c(1.67,2.2),c("in","in")),widths=unit(c(7.9),c("in")))
# plot_grid(world_20152016ElNino,HighDist,VLowDist,KImap,ord_plot, align = "v", nrow = 2, ncol=3, rel_heights = c(1/2, 1/4, 1/4), rel_widths=c(1,1/4,1/4,1/4,1/4))

dev.off()

# # Open a jpg image
# jpeg(file="figures/Figure3.jpg",width = 7.2, height = 3.97,units="in",res=300)
# 
# # lay=rbind(c(1,1,1),c(2,4,5),c(3,4,5))
# # grid.arrange(world_20152016ElNino,HighDist,VLowDist,KImap,ord_plot,ncol=3, nrow=3, layout_matrix=lay, widths=unit(c(2,2,3.2),c("in","in","in")),heights=unit(c(1.6,1.15,1.15),c("in","in","in")))
# grid.arrange(world_20152016ElNino,arrangeGrob(arrangeGrob(HighDist,VLowDist,nrow=2),KImap,ord_plot,ncol=3,widths=unit(c(1.7,2.1,3.1),c("in","in","in"))),nrow=2, heights=unit(c(1.67,2.1),c("in","in")),widths=unit(c(7.9),c("in")))
# # plot_grid(world_20152016ElNino,HighDist,VLowDist,KImap,ord_plot, align = "v", nrow = 2, ncol=3, rel_heights = c(1/2, 1/4, 1/4), rel_widths=c(1,1/4,1/4,1/4,1/4))
# 
# dev.off()

##################################################################################


# Test ordination
anova(ord.phy97.f.c.platy.AD.before.CAP)
# Use ordistep to assess which model terms should be included
finalmodel.platy.AD.before <- ordistep(ord.phy97.f.c.platy.AD.before.CAP, formula= ~ Year + Status +  Dist, direction = c("both"), Pin = 0.05, Pout = 0.1, pstep = 100, perm.max = 1000, steps = 50, trace = TRUE)
# Look at the ordistep final model
finalmodel.platy.AD.before
# Check for covariance
vif.cca(finalmodel.platy.AD.before)
# Test the full model
anova.cca(finalmodel.platy.AD.before)
# Test the model terms
anova.cca(finalmodel.platy.AD.before, by="terms")

# Open a tiff image - Make Fig 2 version 2
# tiff(file="figures/Figure3_v2.tiff",width = 7.2, height = 5.5,units="in",res=300)
# 
# lay=rbind(c(1,1),c(2,3))
# grid.arrange(world_20152016ElNino,KImap,ord_plot,ncol=2, nrow=2, layout_matrix=lay, widths=unit(c(3.2,3.6),c("in","in")),heights=unit(c(2.5,3),c("in","in")))
# 
# dev.off()

