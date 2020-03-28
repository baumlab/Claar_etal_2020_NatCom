rm(list=ls())

samplelist <- read.csv("analyses/2020_analyses/ASV_ordination/samplelist_Fig1.csv")

load("data/KI_Platy_f_coral_grouped_ASVs.RData")

D_cols <- c("VeryHigh"="#8c510a", "Medium"="#c7eae5", 
            "Low"="#5ab4ac", "VeryLow"="#01665e")

set.seed(2020)

colnames(samplelist)[1] <- "Coral_Species"
ord_samples <- samplelist$ord_sample

samplelist_noNA <- samplelist[!is.na(samplelist$ord_sample),]
samplelist_platy <- samplelist_noNA[samplelist_noNA$Coral_Species=="P. ryukyuensis",]

ord_samples2 <- data.frame(samplelist_platy$ProposedSurvival_Status, 
                           row.names = samplelist_platy$ord_sample)
ord_samples3 <- data.frame(samplelist_platy$Survival_Status_conservative, 
                           row.names = samplelist_platy$ord_sample)
ord_samples4 <- data.frame(samplelist_platy$Survival_Status_liberal, 
                           row.names = samplelist_platy$ord_sample)

platy_ord_physeq0 <- subset_samples(phyASV.f.c,
                                     sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples2))

platy_ord_physeq <- merge_phyloseq(platy_ord_physeq0,sample_data(ord_samples2))

colnames(sample_data(platy_ord_physeq))[colnames(sample_data(platy_ord_physeq))=="samplelist_platy.ProposedSurvival_Status"] <- "updated_status"


platy_ord_physeq00 <- subset_samples(phyASV.f.c,
                                      sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples3))

platy_ord_physeq_cons <- merge_phyloseq(platy_ord_physeq00,sample_data(ord_samples3))

colnames(sample_data(platy_ord_physeq_cons))[colnames(sample_data(platy_ord_physeq_cons))=="samplelist_platy.Survival_Status_conservative"] <- "updated_status_conservative"

platy_ord_physeq000 <- subset_samples(phyASV.f.c,
                                       sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples4))

platy_ord_physeq_lib <- merge_phyloseq(platy_ord_physeq000,sample_data(ord_samples4))

colnames(sample_data(platy_ord_physeq_lib))[colnames(sample_data(platy_ord_physeq_lib))=="samplelist_platy.Survival_Status_liberal"] <- "updated_status_liberal"

sample_data(platy_ord_physeq)$Dist <- factor(sample_data(platy_ord_physeq)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

sample_data(platy_ord_physeq_cons)$Dist <- factor(sample_data(platy_ord_physeq_cons)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

sample_data(platy_ord_physeq_lib)$Dist <- factor(sample_data(platy_ord_physeq_lib)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

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

sample_data(platy_ord_physeq_cons)$leewind <- sample_data(platy_ord_physeq_cons)$site
sample_data(platy_ord_physeq_cons)$leewind <- gsub("35","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("34","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("32","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("30","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("27","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("25","windward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("15","windward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("14","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("8","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("5","leeward",sample_data(platy_ord_physeq_cons)$leewind)
sample_data(platy_ord_physeq_cons)$leewind <- gsub("3","windward",sample_data(platy_ord_physeq_cons)$leewind)

sample_data(platy_ord_physeq_lib)$leewind <- sample_data(platy_ord_physeq_lib)$site
sample_data(platy_ord_physeq_lib)$leewind <- gsub("35","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("34","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("32","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("30","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("27","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("25","windward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("15","windward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("14","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("8","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("5","leeward",sample_data(platy_ord_physeq_lib)$leewind)
sample_data(platy_ord_physeq_lib)$leewind <- gsub("3","windward",sample_data(platy_ord_physeq_lib)$leewind)

platy_ord_physeq <- rarefy_even_depth(platy_ord_physeq, 
                                       sample.size = 1000)
platy_ord_physeq_cons <- rarefy_even_depth(platy_ord_physeq_cons, 
                                            sample.size = 1000)
platy_ord_physeq_lib <- rarefy_even_depth(platy_ord_physeq_lib, 
                                           sample.size = 1000)

platy_ord_CAP <- ordinate(platy_ord_physeq,method="CAP",
                           distance="wunifrac",formula= ~ updated_status)
platy_ord_CAP_cons <- ordinate(platy_ord_physeq_cons,method="CAP",
                                distance="wunifrac",formula= ~ updated_status_conservative)
platy_ord_CAP_lib <- ordinate(platy_ord_physeq_lib,method="CAP",
                               distance="wunifrac",formula= ~ updated_status_liberal)

anova(platy_ord_CAP)
anova(platy_ord_CAP_cons)
anova(platy_ord_CAP_lib)

