rm(list=ls())
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

p1 <- plot_ordination(fpenta_ord_physeq, fpenta_ord_CAP,
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
jpeg(file="figures/Figure2c_ASV_fpenta_rare1000.jpg",width = 6, height = 6,units="in",res=300)
ord_plot
dev.off()

# Test ordination
anova(fpenta_ord_CAP)
anova.cca(fpenta_ord_CAP, by="terms")
# Use ordistep to assess which model terms should be included
finalmodel.fpenta.AD.before <- ordistep(fpenta_ord_CAP, formula= ~ Year + Status +  Dist, direction = c("both"), Pin = 0.05, Pout = 0.1, pstep = 100, perm.max = 1000, steps = 50, trace = TRUE)
# Look at the ordistep final model
finalmodel.fpenta.AD.before
anova(finalmodel.fpenta.AD.before)
# Check for covariance
vif.cca(finalmodel.fpenta.AD.before)
# Test the full model
anova.cca(finalmodel.fpenta.AD.before)
# Test the model terms
anova.cca(finalmodel.fpenta.AD.before, by="terms")

