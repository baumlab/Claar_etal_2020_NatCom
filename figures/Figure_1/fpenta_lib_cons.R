samplelist <- read.csv("analyses/2020_analyses/ASV_ordination/samplelist_Fig1.csv")

load("data/KI_Platy_f_coral_grouped_ASVs.RData")

D_cols <- c("VeryHigh"="#8c510a", "Medium"="#c7eae5", 
            "Low"="#5ab4ac", "VeryLow"="#01665e")


colnames(samplelist)[1] <- "Coral_Species"
ord_samples <- samplelist$ord_sample

samplelist_noNA <- samplelist[!is.na(samplelist$ord_sample),]
samplelist_fpenta <- samplelist_noNA[samplelist_noNA$Coral_Species=="F. pentagona",]

ord_samples2 <- data.frame(samplelist_fpenta$ProposedSurvival_Status, 
                           row.names = samplelist_fpenta$ord_sample)
ord_samples3 <- data.frame(samplelist_fpenta$Survival_Status_conservative, 
                           row.names = samplelist_fpenta$ord_sample)
ord_samples4 <- data.frame(samplelist_fpenta$Survival_Status_liberal, 
                           row.names = samplelist_fpenta$ord_sample)

fpenta_ord_physeq00 <- subset_samples(phyASV.f.c,
                                     sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples3))

fpenta_ord_physeq_cons <- merge_phyloseq(fpenta_ord_physeq00,sample_data(ord_samples3))

colnames(sample_data(fpenta_ord_physeq_cons))[colnames(sample_data(fpenta_ord_physeq_cons))=="samplelist_fpenta.Survival_Status_conservative"] <- "updated_status_conservative"

fpenta_ord_physeq000 <- subset_samples(phyASV.f.c,
                                      sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples4))

fpenta_ord_physeq_lib <- merge_phyloseq(fpenta_ord_physeq000,sample_data(ord_samples4))

colnames(sample_data(fpenta_ord_physeq_lib))[colnames(sample_data(fpenta_ord_physeq_lib))=="samplelist_fpenta.Survival_Status_liberal"] <- "updated_status_liberal"

sample_data(fpenta_ord_physeq_cons)$Dist <- factor(sample_data(fpenta_ord_physeq_cons)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

sample_data(fpenta_ord_physeq_lib)$Dist <- factor(sample_data(fpenta_ord_physeq_lib)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

sample_data(fpenta_ord_physeq_cons)$leewind <- sample_data(fpenta_ord_physeq_cons)$site
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("35","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("34","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("32","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("30","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("27","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("25","windward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("15","windward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("14","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("8","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("5","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("3","windward",sample_data(fpenta_ord_physeq_cons)$leewind)

sample_data(fpenta_ord_physeq_lib)$leewind <- sample_data(fpenta_ord_physeq_lib)$site
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("35","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("34","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("32","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("30","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("27","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("25","windward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("15","windward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("14","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("8","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("5","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("3","windward",sample_data(fpenta_ord_physeq_lib)$leewind)

fpenta_ord_physeq_cons <- rarefy_even_depth(fpenta_ord_physeq_cons, 
                                       sample.size = 1000)
fpenta_ord_physeq_lib <- rarefy_even_depth(fpenta_ord_physeq_lib, 
                                            sample.size = 1000)

fpenta_ord_CAP_cons <- ordinate(fpenta_ord_physeq_cons,method="CAP",
                           distance="wunifrac",formula= ~ Dist + leewind)
fpenta_ord_CAP_lib <- ordinate(fpenta_ord_physeq_lib,method="CAP",
                                distance="wunifrac",formula= ~ Dist + leewind)

anova(fpenta_ord_CAP_cons)
anova(fpenta_ord_CAP_lib)

#############

samplelist <- read.csv("analyses/2020_analyses/ASV_ordination/samplelist_Fig1.csv")

load("data/KI_Platy_f_coral_grouped_ASVs.RData")

D_cols <- c("VeryHigh"="#8c510a", "Medium"="#c7eae5", 
            "Low"="#5ab4ac", "VeryLow"="#01665e")


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

platy_ord_physeq00 <- subset_samples(phyASV.f.c,
                                      sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples3))

platy_ord_physeq_cons <- merge_phyloseq(platy_ord_physeq00,sample_data(ord_samples3))

colnames(sample_data(platy_ord_physeq_cons))[colnames(sample_data(platy_ord_physeq_cons))=="samplelist_platy.Survival_Status_conservative"] <- "updated_status_conservative"

platy_ord_physeq000 <- subset_samples(phyASV.f.c,
                                       sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples4))

platy_ord_physeq_lib <- merge_phyloseq(platy_ord_physeq000,sample_data(ord_samples4))

colnames(sample_data(platy_ord_physeq_lib))[colnames(sample_data(platy_ord_physeq_lib))=="samplelist_platy.Survival_Status_liberal"] <- "updated_status_liberal"

sample_data(platy_ord_physeq_cons)$Dist <- factor(sample_data(platy_ord_physeq_cons)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

sample_data(platy_ord_physeq_lib)$Dist <- factor(sample_data(platy_ord_physeq_lib)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

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

platy_ord_physeq_cons <- rarefy_even_depth(platy_ord_physeq_cons, 
                                            sample.size = 1000)
platy_ord_physeq_lib <- rarefy_even_depth(platy_ord_physeq_lib, 
                                           sample.size = 1000)

platy_ord_CAP_cons <- ordinate(platy_ord_physeq_cons,method="CAP",
                                distance="wunifrac",formula= ~ Dist + leewind)
platy_ord_CAP_lib <- ordinate(platy_ord_physeq_lib,method="CAP",
                               distance="wunifrac",formula= ~ Dist + leewind)

anova(platy_ord_CAP_cons)
anova(platy_ord_CAP_lib)