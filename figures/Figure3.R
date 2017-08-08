rm(list=ls())

library(phyloseq)
library(vegan)
library(ggplot2)
library(phangorn)

load("data/KI_seqs_f_coral_grouped.RData")

ord.phy97.f.c.platy.AD.before.CAP <- ordinate(phy97.f.c.platy.AD.before,method="CAP",distance="wunifrac",formula= ~ field_season + Status + Dist)
# Plot this ordination - scree plot
plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,  shape="Status",type="scree",
                title="97% within-sample OTUs - CAP Platygyra only")

# Open a tiff image
tiff(file="figures/Figure3.tiff",width = 6, height = 4,units="in",res=300)
colnames(sample_data(phy97.f.c.platy.AD.before))[25] = "Disturbance"
sample_data(phy97.f.c.platy.AD.before)$Status <- gsub("alive","Alive",sample_data(phy97.f.c.platy.AD.before)$Status)
sample_data(phy97.f.c.platy.AD.before)$Status <- gsub("dead","Dead",sample_data(phy97.f.c.platy.AD.before)$Status)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("HighMed","Medium",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("VeryHigh","Very High",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
sample_data(phy97.f.c.platy.AD.before)$Disturbance <- gsub("VeryLow","Very Low",sample_data(phy97.f.c.platy.AD.before)$Disturbance)
# Plot this ordination - sample plot
p1 <- plot_ordination(phy97.f.c.platy.AD.before, ord.phy97.f.c.platy.AD.before.CAP,  shape="Status", color="Disturbance",type="samples",title="")
theme_set(theme_bw())
p1 + geom_point(size=2)+ theme(legend.position=c(.83, .86),legend.box = "horizontal",legend.background = element_blank(),legend.text = element_text(size = 8),legend.key.height=unit(0.75,"line")) + stat_ellipse(aes(group=Status), type = "t",level=0.95,color=c("darkgray"),lty=2)
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
