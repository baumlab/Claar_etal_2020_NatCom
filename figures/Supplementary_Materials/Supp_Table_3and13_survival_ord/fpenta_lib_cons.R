# Read in sample list
samplelist <- read.csv("analyses/2020_analyses/ASV_ordination/samplelist_Fig1.csv")

# Load necessary data
load("data/KI_Platy_f_coral_grouped_ASVs.RData")

# Set colors
D_cols <- c("VeryHigh"="#8c510a", "Medium"="#c7eae5", 
            "Low"="#5ab4ac", "VeryLow"="#01665e")

# Set random seed for reproducibility
set.seed(2020)

# Rename species column
colnames(samplelist)[1] <- "Coral_Species"

# Extract ord sample names
ord_samples <- samplelist$ord_sample

# Remove NAs and subset only F. penta
samplelist_noNA <- samplelist[!is.na(samplelist$ord_sample),]
samplelist_fpenta <- samplelist_noNA[samplelist_noNA$Coral_Species=="F. pentagona",]

# Subset each assumption set (AS) - standard, conservative, liberal
ord_samples2 <- data.frame(samplelist_fpenta$ProposedSurvival_Status, 
                           row.names = samplelist_fpenta$ord_sample)
ord_samples3 <- data.frame(samplelist_fpenta$Survival_Status_conservative, 
                           row.names = samplelist_fpenta$ord_sample)
ord_samples4 <- data.frame(samplelist_fpenta$Survival_Status_liberal, 
                           row.names = samplelist_fpenta$ord_sample)

# Subset phyloseq to only include standard samples
fpenta_ord_physeq0 <- subset_samples(phyASV.f.c,
                                      sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples2))
fpenta_ord_physeq <- merge_phyloseq(fpenta_ord_physeq0,sample_data(ord_samples2))
# Update column name
colnames(sample_data(fpenta_ord_physeq))[colnames(sample_data(fpenta_ord_physeq))=="samplelist_fpenta.ProposedSurvival_Status"] <- "updated_status"

# Subset phyloseq to only include conservative samples
fpenta_ord_physeq00 <- subset_samples(phyASV.f.c,
                                     sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples3))
fpenta_ord_physeq_cons <- merge_phyloseq(fpenta_ord_physeq00,sample_data(ord_samples3))
# Update column name
colnames(sample_data(fpenta_ord_physeq_cons))[colnames(sample_data(fpenta_ord_physeq_cons))=="samplelist_fpenta.Survival_Status_conservative"] <- "updated_status_conservative"

# Subset phyloseq to only include liberal samples
fpenta_ord_physeq000 <- subset_samples(phyASV.f.c,
                                      sample_data(phyASV.f.c)$SampleID %in% rownames(ord_samples4))
fpenta_ord_physeq_lib <- merge_phyloseq(fpenta_ord_physeq000,sample_data(ord_samples4))
# Update column name
colnames(sample_data(fpenta_ord_physeq_lib))[colnames(sample_data(fpenta_ord_physeq_lib))=="samplelist_fpenta.Survival_Status_liberal"] <- "updated_status_liberal"

# Remove all colonies with "Unknown" status
fpenta_ord_physeq <- subset_samples(fpenta_ord_physeq,
                                    sample_data(fpenta_ord_physeq)$updated_status!="unknown")
fpenta_ord_physeq_lib <- subset_samples(fpenta_ord_physeq_lib,
                                    sample_data(fpenta_ord_physeq_lib)$updated_status!="unknown")
fpenta_ord_physeq_cons <- subset_samples(fpenta_ord_physeq_cons,
                                    sample_data(fpenta_ord_physeq_cons)$updated_status!="unknown")

# Order disturbance factor levels
sample_data(fpenta_ord_physeq)$Dist <- factor(sample_data(fpenta_ord_physeq)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))
sample_data(fpenta_ord_physeq_cons)$Dist <- factor(sample_data(fpenta_ord_physeq_cons)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))
sample_data(fpenta_ord_physeq_lib)$Dist <- factor(sample_data(fpenta_ord_physeq_lib)$Dist,levels=c("VeryLow","Low","Medium","VeryHigh"))

# Add leeward/windward column
sample_data(fpenta_ord_physeq)$leewind <- sample_data(fpenta_ord_physeq)$site
sample_data(fpenta_ord_physeq)$leewind <- gsub("38","leeward",sample_data(fpenta_ord_physeq)$leewind)
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

sample_data(fpenta_ord_physeq_cons)$leewind <- sample_data(fpenta_ord_physeq_cons)$site
sample_data(fpenta_ord_physeq_cons)$leewind <- gsub("38","leeward",sample_data(fpenta_ord_physeq_cons)$leewind)
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
sample_data(fpenta_ord_physeq_lib)$leewind <- gsub("38","leeward",sample_data(fpenta_ord_physeq_lib)$leewind)
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

# Rarefy to 1000 sequences
fpenta_ord_physeq <- rarefy_even_depth(fpenta_ord_physeq, 
                                            sample.size = 1000)
fpenta_ord_physeq_cons <- rarefy_even_depth(fpenta_ord_physeq_cons, 
                                       sample.size = 1000)
fpenta_ord_physeq_lib <- rarefy_even_depth(fpenta_ord_physeq_lib, 
                                            sample.size = 1000)

# Run constrained ordination
fpenta_ord_CAP <- ordinate(fpenta_ord_physeq,method="CAP",
                                distance="wunifrac",formula= ~ updated_status)
fpenta_ord_CAP_cons <- ordinate(fpenta_ord_physeq_cons,method="CAP",
                           distance="wunifrac",
                           formula= ~ updated_status_conservative)
fpenta_ord_CAP_lib <- ordinate(fpenta_ord_physeq_lib,method="CAP",
                                distance="wunifrac",
                               formula= ~ updated_status_liberal)
# Run anovas
anova(fpenta_ord_CAP)
anova(fpenta_ord_CAP_cons)
anova(fpenta_ord_CAP_lib)

