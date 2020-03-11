rm(list=ls())
samplelist <- read.csv("analyses/2020_analyses/ASV_ordination/samplelist.csv")

load("data/KI_Platy_f_coral_grouped_ASVs.RData")

D_cols <- c("VeryHigh"="#8c510a", "Medium"="#c7eae5", "Low"="#5ab4ac", "VeryLow"="#01665e")

ord_samples <- samplelist$ord_sample

samplelist <- samplelist[!is.na(samplelist$ord_sample),]
samplelist_platy <- samplelist[samplelist$Coral_Species=="P. ryukyuensis",]

ord_samples2 <- data.frame(samplelist_platy$ProposedSurvival_Status, 
                           row.names = samplelist_platy$ord_sample)

platy_ord_physeq0 <- subset_samples(phyASV.f.c,
                                    sample_data(phyASV.f.c)$SampleID %in% ord_samples)

platy_ord_physeq <- merge_phyloseq(platy_ord_physeq0,sample_data(ord_samples2))

colnames(sample_data(platy_ord_physeq))[colnames(sample_data(platy_ord_physeq))=="samplelist_platy.ProposedSurvival_Status"] <- "updated_status"

platy_ord_physeq <- subset_samples(platy_ord_physeq,sample_data(platy_ord_physeq)$updated_status!="unknown")

sample_data(platy_ord_physeq)$Dist <- factor(sample_data(platy_ord_physeq)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

sample_data(platy_ord_physeq)$leewind <- sample_data(platy_ord_physeq)$site
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

sample_data(platy_ord_physeq)$region <- sample_data(platy_ord_physeq)$site
sample_data(platy_ord_physeq)$region <- gsub("35","west",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("34","west",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("32","west",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("30","west",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("27","west",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("25","north",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("15","east",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("14","south",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("8","west",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("5","south",sample_data(platy_ord_physeq)$region)
sample_data(platy_ord_physeq)$region <- gsub("3","north",sample_data(platy_ord_physeq)$region)


platy_ord_physeq <- rarefy_even_depth(platy_ord_physeq, 
                                      sample.size = 1000)

platy_ord_CAP <- ordinate(platy_ord_physeq,method="CAP",
                          distance="wunifrac",formula= ~ Dist + leewind)

p1 <- plot_ordination(platy_ord_physeq, platy_ord_CAP,
                      shape="updated_status", color="Dist",type="samples",title="")

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

ord_plot

# Open a jpg image
# jpeg(file="figures/Figure2c_ASV_updated_rare100.jpg",width = 6, height = 6,units="in",res=300)
# ord_plot
# dev.off()


# Test ordination
anova(platy_ord_CAP)
anova.cca(platy_ord_CAP, by="terms")
# Use ordistep to assess which model terms should be included
finalmodel.platy.AD.before <- ordistep(platy_ord_CAP, formula= ~ Year + updated_status +  Dist, direction = c("both"), Pin = 0.05, Pout = 0.1, pstep = 100, perm.max = 1000, steps = 50, trace = TRUE)
# Look at the ordistep final model
finalmodel.platy.AD.before
anova(finalmodel.platy.AD.before)
# Check for covariance
vif.cca(finalmodel.platy.AD.before)
# Test the full model
anova.cca(finalmodel.platy.AD.before)
# Test the model terms
anova.cca(finalmodel.platy.AD.before, by="terms")


platy_ord_CAP_status <- ordinate(platy_ord_physeq,method="CAP",
                                 distance="wunifrac",formula= ~ field_season + updated_status)

p2 <- plot_ordination(platy_ord_physeq, platy_ord_CAP_status,
                      shape="updated_status", color="Dist",
                      type="samples",title="")

# Format ordination plot
ord_plot <- p2 + 
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

ord_plot

# Test ordination
anova(platy_ord_CAP_status)
anova.cca(platy_ord_CAP_status, by="terms")
plot(platy_ord_CAP_status)
