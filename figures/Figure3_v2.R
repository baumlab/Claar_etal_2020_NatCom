rm(list=ls())

library(phyloseq)
library(vegan)
library(ggplot2)
library(phangorn)
library(gridExtra)
library(plyr)
library(dplyr)
library(imager)

load("data/KI_seqs_f_coral_grouped.RData")

# Set disturbance colors
D_cols <- c("Very High"="#a6611a", "Medium"="#dfc27d", "Low"="#80cdc1", "Very Low"="#018571")
# D_cols <- c("Very High"="#e66101", "Medium"="#fdb863", "Low"="#b2abd2", "Very Low"="#5e3c99")

ord.phy97.f.c.platy.AD.before.CAP <- ordinate(phy97.f.c.platy.AD.before,method="CAP",distance="wunifrac",formula= ~ field_season + Status + Dist)
# # Plot this ordination - scree plot
# plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,  shape="Status",type="scree",title="97% within-sample OTUs - CAP Platygyra only")

# Open a tiff image
tiff(file="figures/Figure3_v2.tiff",width = 7.2, height = 5.5,units="in",res=300)

colnames(sample_data(phy97.f.c.platy.AD.before))[25] = "Disturbance"
sample_data(phy97.f.c.platy.AD.before)$Status <- gsub("alive","Alive",sample_data(phy97.f.c.platy.AD.before)$Status)
sample_data(phy97.f.c.platy.AD.before)$Status <- gsub("dead","Dead",sample_data(phy97.f.c.platy.AD.before)$Status)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("HighMed","Medium",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("VeryHigh","Very High",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("VeryLow","Very Low",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- factor(sample_data(phy97.f.c.platy.AD.before)$Disturbance, levels = c("Very Low","Low","Medium","Very High"))
# Plot this ordination - sample plot
p1 <- plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,  shape="Status", color="Disturbance",type="samples",title="")
theme_set(theme_bw())
plot_p1 <- p1 + 
  geom_point(size=2)+ 
  theme(legend.position=c(.72, .85),
        legend.box = "horizontal",legend.background = element_blank(), 
        legend.title = element_text(size=10),
        legend.text = element_text(size = 8),legend.key.height=unit(0.5,"line"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = unit(c(-0.3,0,0,0),c("in","in","in","in")),
        axis.title = element_text(size=8), 
        axis.text.x = element_text(size=8,margin = margin(b=5, t = 5)),
        axis.text.y = element_text(size=8,margin = margin(b=5, r = 5)), 
        axis.ticks.length=unit(-0.05, "in")) + 
  stat_ellipse(aes(group=Status), type = "t",level=0.95,color=c("darkgray"),lty=2)+ 
  scale_color_manual(values=D_cols)

img_20152016ElNino <- load.image('figures/figure_309.png')
as.data.frame(img_20152016ElNino,wide="c") %>% head
df <- as.data.frame(img_20152016ElNino,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
head(df,3)
world_20152016ElNino <- ggplot(df,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+scale_y_reverse()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),                                          axis.text.y=element_blank(),axis.ticks=element_blank(),                                          axis.title.x=element_blank(),                                                                    axis.title.y=element_blank(),legend.position="none",                                             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),
                           plot.margin = unit(c(0,0,0,0),c("in","in","in","in")))

img_KImap <- load.image('figures/KI_map_coral_sites_NatGeo.jpg')
as.data.frame(img_KImap,wide="c") %>% head
df2 <- as.data.frame(img_KImap,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
head(df2,3)
KImap <- ggplot(df2,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+scale_y_reverse()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),                                          axis.text.y=element_blank(),axis.ticks=element_blank(),                                          axis.title.x=element_blank(),                                                                    axis.title.y=element_blank(),legend.position="none",                                             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank(),
          plot.margin = unit(c(-0.2,0,0,0),c("in","in","in","in")))


lay=rbind(c(1,1),c(2,3))
grid.arrange(world_20152016ElNino,KImap,plot_p1,ncol=2, nrow=2, layout_matrix=lay, widths=unit(c(3.2,3.6),c("in","in")),heights=unit(c(2.5,3),c("in","in")))

dev.off()

# Open a tiff image
tiff(file="figures/Figure3_v3.tiff",width = 7.2, height = 4.5,units="in",res=300)

img_VLowDist <- load.image('figures/VeryLowDisturbance_KI_cropped.jpg')
as.data.frame(img_VLowDist,wide="c") %>% head
df3 <- as.data.frame(img_VLowDist,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
head(df3,3)
VLowDist <- ggplot(df3,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+scale_y_reverse()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),                                          axis.text.y=element_blank(),axis.ticks=element_blank(),                                          axis.title.x=element_blank(),                                                                    axis.title.y=element_blank(),legend.position="none",                                             panel.background=element_blank(),panel.border=element_blank(),
             panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
             plot.background=element_blank(),
             plot.margin = unit(c(-0.1,0,0,0),c("in","in","in","in")))

img_HighDist <- load.image('figures/highdisturbance_KI_cropped.jpg')
as.data.frame(img_HighDist,wide="c") %>% head
df4 <- as.data.frame(img_HighDist,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
head(df4,3)
HighDist <- ggplot(df4,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()+scale_y_reverse()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),                                          axis.text.y=element_blank(),axis.ticks=element_blank(),
               axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),legend.position="none",                                            panel.background=element_blank(),panel.border=element_blank(),
               panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
               plot.background=element_blank(),
               plot.margin = unit(c(-0.1,0,0,0),c("in","in","in","in")))

lay=rbind(c(1,1,1),c(2,3,4),c(2,3,5))
grid.arrange(world_20152016ElNino,KImap,plot_p1,VLowDist,HighDist,ncol=3, nrow=3, layout_matrix=lay, widths=unit(c(2.1,2.7,1.9),c("in","in","in")),heights=unit(c(2.5,0.95,0.95),c("in","in","in")))

dev.off()


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
