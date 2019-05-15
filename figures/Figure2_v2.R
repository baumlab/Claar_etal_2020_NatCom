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
D_cols <- c("Very High"="#8c510a", "High"="black", "Medium"="#c7eae5", "Low"="#5ab4ac", "Very Low"="#01665e")
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
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- factor(sample_data(phy97.f.c.platy.AD.before)$Disturbance, levels = c("Very Low","Low","Medium","High","Very High"))


##################################################################################
### Ordination Plot (CAP) ###

# Create ordination for plotting
ord.phy97.f.c.platy.AD.before.CAP <- ordinate(phy97.f.c.platy.AD.before,
                                              method="CAP",distance="bray",
                                              formula= ~ field_season + Disturbance)

# # Plot this ordination - scree plot (Optional, helpful for looking at explained variance)
# plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,  shape="Status",type="scree",title="97% within-sample OTUs - CAP Platygyra only")

# Plot this ordination - Make plot using phyloseq & ggplot2
p1 <- plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,
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
  annotate("text", x = -0.4, y = 4, label = "C",fontface="bold")
  
ord_plot

pdf(file="figures/Figure2_ordination.pdf",width = 2.6, height = 2.8,useDingbats = FALSE)
ord_plot
dev.off()

