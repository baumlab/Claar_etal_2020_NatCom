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
load("data/KI_Platy_f_coral_grouped_ASVs.RData")


##################################################################################
### Formatting ###

# Set colors for disturbance levels
D_cols <- c("Very High"="#8c510a", "High"="black", "Medium"="#c7eae5", "Low"="#5ab4ac", "Very Low"="#01665e")
# D_cols <- c("Very High"="#e66101", "Medium"="#fdb863", "Low"="#b2abd2", "Very Low"="#5e3c99")

# Set ggplot2 basic parameters
theme_set(theme_bw())

# Rename "Disturbance" column
colnames(sample_data(phyASV.f.c.platy.AD.before))[12] = "Disturbance"
# Capitalize the words "Alive" and "Dead"
sample_data(phyASV.f.c.platy.AD.before)$Status <- gsub("alive","Alive",sample_data(phyASV.f.c.platy.AD.before)$Status)
sample_data(phyASV.f.c.platy.AD.before)$Status <- gsub("dead","Dead",sample_data(phyASV.f.c.platy.AD.before)$Status)
# Rename Disturbance Levels for easy plotting
sample_data(phyASV.f.c.platy.AD.before)$Disturbance <- gsub("HighMed","Medium",sample_data(phyASV.f.c.platy.AD.before)$Disturbance)
sample_data(phyASV.f.c.platy.AD.before)$Disturbance <- gsub("VeryHigh","Very High",sample_data(phyASV.f.c.platy.AD.before)$Disturbance)
sample_data(phyASV.f.c.platy.AD.before)$Disturbance <- gsub("VeryLow","Very Low",sample_data(phyASV.f.c.platy.AD.before)$Disturbance)
# sample_data(phyASV.f.c.platy.AD.before)$Disturbance <- gsub("High","H",sample_data(phyASV.f.c.platy.AD.before)$Disturbance)
# Set factor levels for "Disturbance"
sample_data(phyASV.f.c.platy.AD.before)$Disturbance <- factor(sample_data(phyASV.f.c.platy.AD.before)$Disturbance, levels = c("Very Low","Low","Medium","High","Very High"))


##################################################################################
### Ordination Plot (CAP) ###

# Create ordination for plotting
ord.phyASV.f.c.platy.AD.before.CAP <- ordinate(phyASV.f.c.platy.AD.before,
                                              method="CAP",distance="bray",
                                              formula= ~ field_season + Disturbance)

# # Plot this ordination - scree plot (Optional, helpful for looking at explained variance)
# plot_ordination(phyASV.f.c.platy.AD.before, ord.phyASV.f.c.platy.AD.before.CAP,  shape="Status",type="scree",title="97% within-sample OTUs - CAP Platygyra only")

# Plot this ordination - Make plot using phyloseq & ggplot2
p1 <- plot_ordination(phyASV.f.c.platy.AD.before, ord.phyASV.f.c.platy.AD.before.CAP,
                      shape="Status", color="Disturbance",type="samples",title="")
# Format ordination plot
ord_plot <- p1 + 
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
  geom_point(alpha=0.5,size=1.5) + 
  scale_shape_manual(name= "Coral Fate", values=c("Alive"=19,"Dead"=24),
                     labels=c("Survived","Died")) + 
  # scale_size_manual(values = c(5,1))+
  scale_color_manual(values=D_cols) + 
  scale_fill_manual(values=D_cols) + 
  guides(color=F) +
  stat_ellipse(aes(group=Status), type = "t",level=0.95,color=c("darkgray"),lty=2) +
  annotate("text", x = -1.4, y = 4, label = "C",fontface="bold")

ord_plot

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
KImap <- ggplot(df2,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+scale_y_reverse()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),                                      axis.text.y=element_blank(),axis.ticks=element_blank(),                                         axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),legend.position="none",                                            panel.background=element_blank(),panel.border=element_blank(),
                                                                                                            panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                                                                                                            plot.background=element_blank(),
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


##################################################################################
### Make the multi-panel plot ###

# Open a tiff image
tiff(file="figures/Figure2_ASV.tiff",width = 7.2, height = 2.8,units="in",res=300)

grid.arrange(arrangeGrob(arrangeGrob(HighDist,VLowDist,nrow=2,heights=unit(c(1.17,1.17),c("in","in"))),KImap,ord_plot,ncol=3,widths=unit(c(1.4,2.8,2.6),c("in","in","in"))),nrow=1, heights=unit(c(2.6),c("in")),widths=unit(c(7.2),c("in")))

dev.off()

# Open a jpg image
jpeg(file="figures/Figure2_ASV.jpg",width = 7.2, height = 2.8,units="in",res=300)

grid.arrange(arrangeGrob(arrangeGrob(HighDist,VLowDist,nrow=2,heights=unit(c(1.17,1.17),c("in","in"))),KImap,ord_plot,ncol=3,widths=unit(c(1.4,2.8,2.6),c("in","in","in"))),nrow=1, heights=unit(c(2.6),c("in")),widths=unit(c(7.2),c("in")))

dev.off()

# Save as EPS
# setEPS(width=7.2,height=2.8)
# postscript("figures/Figure2.eps")
# grid.arrange(arrangeGrob(arrangeGrob(HighDist,VLowDist,nrow=2,heights=unit(c(1.17,1.17),c("in","in"))),KImap,ord_plot,ncol=3,widths=unit(c(1.4,2.8,2.6),c("in","in","in"))),nrow=1, heights=unit(c(2.6),c("in")),widths=unit(c(7.2),c("in")))
# dev.off()


##################################################################################


# Test ordination
anova(ord.phyASV.f.c.platy.AD.before.CAP)
# Use ordistep to assess which model terms should be included
finalmodel.platy.AD.before <- ordistep(ord.phyASV.f.c.platy.AD.before.CAP, formula= ~ Year + Status +  Dist, direction = c("both"), Pin = 0.05, Pout = 0.1, pstep = 100, perm.max = 1000, steps = 50, trace = TRUE)
# Look at the ordistep final model
finalmodel.platy.AD.before
# Check for covariance
vif.cca(finalmodel.platy.AD.before)
# Test the full model
anova.cca(finalmodel.platy.AD.before)
# Test the model terms
anova.cca(finalmodel.platy.AD.before, by="terms")
