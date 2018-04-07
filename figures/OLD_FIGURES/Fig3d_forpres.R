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
                      shape="Status", color="Status",type="samples",title="")
# Format ordination plot
ord_plot <- p1 + 
  geom_point(aes(color=Status)) + guides(shape=guide_legend(override.aes = list(size=3,linetype=0)))+
  scale_shape_manual(values = c(19,19)) + scale_size_manual(values = c(2.25,2.25))+
  scale_color_manual(values=c("#2c7bb6","#d7191c")) +
  theme(legend.position=c(.86, .83),
        legend.box = "horizontal",
        legend.background = element_rect(colour="white", fill="white"),
        legend.title = element_text(size=18),
        legend.text = element_text(size = 16,color="black"),
        legend.key.height=unit(1,"line"),
        legend.key = element_rect(fill = "white"), legend.spacing = unit(0.5,"line"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="white", fill="white"),
        plot.background = element_rect(colour="white", fill="white"),
        plot.margin = unit(c(-0.15,0.07,0.01,0.08),c("in","in","in","in")),
        axis.title = element_text(size=8), 
        axis.text.x = element_text(size=18,margin = margin(b=5, t = 5),color="black"),
        axis.text.y = element_text(size=18,margin = margin(b=5, r = 5),color="black"), 
        axis.title.x = element_text(size=18,margin = margin(b=5, t = 0.1),color="black"),
        axis.title.y = element_text(size=18,margin = margin(b=5, r = 0.1),color="black"), 
        axis.ticks.length=unit(-0.05, "in"), axis.line = element_line(color="black")) + 
  stat_ellipse(aes(group=Status), type = "t",level=0.95,lty=2)

jpeg(file="figures/Fig3d_forpres.jpg",width = 6, height = 4,units="in",res=300)
ord_plot
dev.off()


# Plot this ordination - Make plot using phyloseq & ggplot2
p2 <- plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,
                      color="Disturbance",type="samples",title="")

# Format ordination plot
ord_plot2 <- p2 + 
  geom_point() + 
  scale_size_manual(values = c(1.75,2.25))+ guides(shape=guide_legend(override.aes = list(size=3)))+
  scale_color_manual(values=D_cols) +
  theme(legend.position=c(.62, .76),
        legend.box = "horizontal",
        legend.background = element_rect(colour="white", fill="white"),
        legend.title = element_text(size=18),
        legend.text = element_text(size = 16,color="black"),
        legend.key.height=unit(1.2,"line"),
        legend.key = element_rect(fill = "white"), legend.spacing = unit(0.5,"line"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="white", fill="white"),
        plot.background = element_rect(colour="white", fill="white"),
        plot.margin = unit(c(-0.15,0.07,0.01,0.08),c("in","in","in","in")),
        axis.title = element_text(size=8), 
        axis.text.x = element_text(size=18,margin = margin(b=5, t = 5),color="black"),
        axis.text.y = element_text(size=18,margin = margin(b=5, r = 5),color="black"), 
        axis.title.x = element_text(size=18,margin = margin(b=5, t = 0.1),color="black"),
        axis.title.y = element_text(size=18,margin = margin(b=5, r = 0.1),color="black"), 
        axis.ticks.length=unit(-0.05, "in"), axis.line = element_line(color="black")) + 
  stat_ellipse(aes(group=Status), type = "t",level=0.95,color=c("gray50"),lty=2)

jpeg(file="figures/Fig3d_forpres2.jpg",width = 6, height = 4,units="in",res=300)
ord_plot2
dev.off()
