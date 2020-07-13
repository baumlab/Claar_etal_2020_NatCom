library(ggplot2)
library(tidyverse)
library(imager)
library(gridExtra)
library(phyloseq)
library(patchwork)

##################################################################################
### KI Local Disturbance Map ###

# Load in and format KI map showing local human disturbance
# Use imager to load the image
img_KImap <- load.image('figures/Figure_1/KI_map_platysites_villages.jpg')

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
img_VLowDist <- load.image('figures/Figure_1/site15_jan15_DanielleClaar_IMG_4926_2.jpg')
# # Check that the data is in the correct format for the next step
# as.data.frame(img_VLowDist,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df3 <- as.data.frame(img_VLowDist,wide="c") %>% 
  mutate(rgb.val=rgb(c.1,c.2,c.3))
# # Double check that it worked
# head(df3,3)

# Turn image into a ggplot object for incorporating in multi-panel figure below
VLowDist <- ggplot(df3,aes(x,y)) + 
  geom_raster(aes(fill=rgb.val)) + 
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_identity()+scale_y_reverse(expand=c(0,0)) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),                                         axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),
        legend.position="none", 
        panel.border=element_rect(colour="black",fill=NA,size=1,
                                  linetype = "solid"),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = unit(c(-0.17,0,0.05,-0.1),c("in","in","in","in")))


##################################################################################
### High Disturbance Photo ###

# Load in and format Very Low Disturbance example image
# Use imager to load the image
img_HighDist <- load.image('figures/Figure_1/highdisturbance_KI.jpg')
# # Check that the data is in the correct format for the next step
# as.data.frame(img_HighDist,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df4 <- as.data.frame(img_HighDist,wide="c") %>% 
  mutate(rgb.val=rgb(c.1,c.2,c.3))
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
  annotate("rect", xmin=0, ymin=0, xmax=120, ymax=130,
           color="white",fill="white") + 
  annotate("text", x = 72, y = 65, label = "A",fontface="bold", color="black")

###################################

library(vegan)
set.seed(2020)
samplelist <- read.csv("figures/Figure_1/samplelist_Fig1.csv")
# Also produces stats for Supplementary Table 2

load("data/KI_Platy_f_coral_grouped_ASVs.RData")

D_cols <- c("VeryHigh"="#8c510a", "Medium"="#c7eae5", 
            "Low"="#5ab4ac", "VeryLow"="#01665e")

# Rename column
colnames(samplelist)[1] <- "Coral_Species"
# Extract samples for ordination
ord_samples <- samplelist$ord_sample

# Remove NAs
samplelist_noNA <- samplelist[!is.na(samplelist$ord_sample),]
# Subset only F. pentaona
samplelist_fpenta <- samplelist_noNA[samplelist_noNA$Coral_Species=="F. pentagona",]

# Subset F. pentagona samples to ordinate
ord_samples2 <- data.frame(samplelist_fpenta$ProposedSurvival_Status, 
                           row.names = samplelist_fpenta$ord_sample)
fpenta_ord_physeq0 <- subset_samples(phyASV.f.c,
                                     sample_data(phyASV.f.c)$SampleID %in%
                                       rownames(ord_samples2))

# Merge phyloseq for ordination
fpenta_ord_physeq <- merge_phyloseq(fpenta_ord_physeq0,
                                    sample_data(ord_samples2))

# Update survival status based on subsequent field seasons
colnames(sample_data(fpenta_ord_physeq))[colnames(sample_data(fpenta_ord_physeq))=="samplelist_fpenta.ProposedSurvival_Status"] <- "updated_status"

# Order factor levels of Disturbance
sample_data(fpenta_ord_physeq)$Dist <- factor(sample_data(fpenta_ord_physeq)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

# Define leeward/windward for each site on KI
sample_data(fpenta_ord_physeq)$leewind <- sample_data(fpenta_ord_physeq)$site
sample_data(fpenta_ord_physeq)$leewind <- gsub("38","windward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("35","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("34","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("32","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("30","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("27","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("25","windward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("15","windward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("14","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("8","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("5","leeward",sample_data(fpenta_ord_physeq)$leewind)
sample_data(fpenta_ord_physeq)$leewind <- gsub("3","windward",sample_data(fpenta_ord_physeq)$leewind)

# Rarefy to even depth of 1000 sequences
fpenta_ord_physeq <- rarefy_even_depth(fpenta_ord_physeq, 
                                       sample.size = 1000)

# Conduct constrained ordination with Disturbance and leeward/windward
fpenta_ord_CAP <- ordinate(fpenta_ord_physeq,method="CAP",
                           distance="wunifrac",formula= ~ Dist + leewind)
# Run anova
anova(fpenta_ord_CAP)
anova.cca(fpenta_ord_CAP,by="terms")

# Use ordistep to conduct backward stepwise model selection
fpenta.finalmodel <- ordistep(fpenta_ord_CAP, formula= ~ Dist + leewind,
                              direction = c("both"), 
                              Pin = 0.05, Pout = 0.1, pstep = 100, 
                              perm.max = 1000, steps = 50, trace = TRUE)
# Run anova
anova(fpenta.finalmodel)
anova.cca(fpenta.finalmodel,by="terms")

# Plot the ordination
p1_penta <- plot_ordination(fpenta_ord_physeq, fpenta_ord_CAP,
                      color="Dist",type="samples",title="")

# Format ordination plot
p_fpenta_CAP <- p1_penta + 
  theme_classic()+
  geom_point(size=5,alpha=0.7,shape=18) + 
  scale_color_manual(values=D_cols) +
  scale_fill_manual(values=D_cols) +
  guides(color=F) +
  NULL
p_fpenta_CAP

# Make figures
jpeg(file="figures/Figure_1/Figure1_fpenta_CAP.jpg",
     width = 3, height = 3,units="in",res=300)
p_fpenta_CAP
dev.off()

pdf(file="figures/Figure_1/Figure1_fpenta_CAP.pdf",
     width = 3, height = 3,useDingbats = FALSE)
p_fpenta_CAP
dev.off()

####################################
# Same as above for F. pentagona, but for P. ryukyuensis
ord_samples <- samplelist$ord_sample

samplelist_noNA <- samplelist[!is.na(samplelist$ord_sample),]
samplelist_platy <- samplelist_noNA[samplelist_noNA$Coral_Species=="P. ryukyuensis",]

ord_samples2 <- data.frame(samplelist_platy$ProposedSurvival_Status, 
                           row.names = samplelist_platy$ord_sample)

platy_ord_physeq0 <- subset_samples(phyASV.f.c,
                                     sample_data(phyASV.f.c)$SampleID %in%
                                      rownames(ord_samples2))

platy_ord_physeq <- merge_phyloseq(platy_ord_physeq0,sample_data(ord_samples2))

colnames(sample_data(platy_ord_physeq))[colnames(sample_data(platy_ord_physeq))=="samplelist_platy.ProposedSurvival_Status"] <- "updated_status"

sample_data(platy_ord_physeq)$Dist <- factor(sample_data(platy_ord_physeq)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

sample_data(platy_ord_physeq)$leewind <- sample_data(platy_ord_physeq)$site
sample_data(platy_ord_physeq)$leewind <- gsub("38","windward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("35","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("34","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("32","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("30","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("27","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("25","windward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("15","windward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("14","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("8","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("5","leeward",sample_data(platy_ord_physeq)$leewind)
sample_data(platy_ord_physeq)$leewind <- gsub("3","windward",sample_data(platy_ord_physeq)$leewind)

platy_ord_physeq<- rarefy_even_depth(platy_ord_physeq, 
                                      sample.size = 1000)

platy_ord_CAP <- ordinate(platy_ord_physeq,method="CAP",
                          distance="wunifrac",formula= ~ Dist + leewind)

anova(platy_ord_CAP)
anova.cca(platy_ord_CAP,by="terms")

platy.finalmodel <- ordistep(platy_ord_CAP, formula= ~ Dist + leewind,
                              direction = c("both"), 
                              Pin = 0.05, Pout = 0.1, pstep = 100, 
                              perm.max = 1000, steps = 50, trace = TRUE)
anova(platy.finalmodel)
anova.cca(platy.finalmodel,by="terms")


p1_platy <- plot_ordination(platy_ord_physeq, platy_ord_CAP,
                      color="Dist",type="samples",title="")

# Format ordination plot
p_platy_CAP <- p1_platy + 
  theme_classic()+
  geom_point(size=5,alpha=0.7,shape=18) + 
  scale_color_manual(values=D_cols) +
  scale_fill_manual(values=D_cols) +
  guides(color=F) +
  NULL

p_platy_CAP

# Make figure
jpeg(file="figures/Figure_1/Figure1_platy_CAP.jpg",
     width = 3, height = 3,units="in",res=300)
p_platy_CAP
dev.off()

pdf(file="figures/Figure_1/Figure1_platy_CAP.pdf",
     width = 3, height = 3,useDingbats = FALSE)
p_platy_CAP
dev.off()


###################################
load("analyses/logistic_regressions/Platy_Favites_LogisticPlots.RData")
# Named: P1, P2 and P3 for Platy and F1, F2 and F3 for Favites
library(arm)
jpeg(file="figures/Figure_1/Figure1_platy_reg.jpg",
     width = 4.5, height = 2.25,units="in",res=300)
P1
dev.off()

pdf(file="figures/Figure_1/Figure1_platy_reg.pdf",
     width = 4.5, height = 2.25,useDingbats = FALSE)
P1
dev.off()

pdf(file="figures/Figure_1/Figure1_fpenta_reg.pdf",
    width = 4.5, height = 2.25,useDingbats = FALSE)
F1
dev.off()

