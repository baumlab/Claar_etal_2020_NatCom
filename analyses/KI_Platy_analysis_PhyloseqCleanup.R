# Platy analysis - Phyloseq Cleanup

# Import Libraries
library(stringr)
library(reshape2)
library(phyloseq)

# Clear workspace
rm(list=ls())

# Load filtered RData object from output of filter_notsym.R script
load("C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f.RData")
phy97.f <- phy.f # Rename phyloseq object for clarity

# Characterize sites by disturbance level
VeryHigh <- c(33,40,32,31,27,30,26)
High <- c(25,3,38,24)
HighMed <- c(9,34,35,8,14,6)
LowMed <- c(2,22,1,23)
Low <- c(7,13,12,4,36,5,37)
VeryLow <- c(10,21,11,20,16,15,39,19,18,17)

sample_data(phy97.f)$Site <- factor(sample_data(phy97.f)$Site)
sample_data(phy97.f)$Dist <- sample_data(phy97.f)$Site
sample_data(phy97.f)$Dist<- gsub ("34",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("35",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("14",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("15",as.character("VeryLow"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("19",as.character("VeryLow"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("12",as.character("Low"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("37",as.character("Low"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("27",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("30",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("32",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("26",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("31",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("40",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("33",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("38",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("25",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("24",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("3",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("5",as.character("Low"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("6",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("9",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("8",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
rm(VeryHigh,High,HighMed,LowMed,Low,VeryLow)

# Use merge_taxa to collapse OTUs with equivalent names ("hits")

# I tried this and it didn't work
#phy97.f.test <- tax_glom(phy97.f, taxrank=rank_names(phy97.f)[9])
#tax_glom(physeq=myData, taxrank=rank_names(myData)[2])
#identical(tax_table(phy97.f),tax_table(phy97.f.test))

# Ok fine I'll do it the long way around
C3k_AY589737 <- grep("C3k_AY589737", data.frame(tax_table(phy97.f))$hit)
phy97.f.c <- merge_taxa(phy97.f, eqtaxa=C3k_AY589737, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(C3k_AY589737)

D1_multiple <- grep("D1_multiple", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=D1_multiple, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(D1_multiple)

C42_AY258487 <- grep("C42_AY258487", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=C42_AY258487, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(C42_AY258487)

C15_AY239369 <- grep("C15_AY239369", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=C15_AY239369, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(C15_AY239369)

C3_multiple <- grep("C3_multiple", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=C3_multiple, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(C3_multiple)

I4_FN561562 <- grep("I4_FN561562", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=I4_FN561562, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(I4_FN561562)

G100_AB253788 <- grep("G100_AB253788", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=G100_AB253788, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(G100_AB253788)

F5.2c_AM748596 <- grep("F5.2c_AM748596", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=F5.2c_AM748596, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(F5.2c_AM748596)

F5.1a_AM748591 <- grep("F5.1a_AM748591", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=F5.1a_AM748591, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(F5.1a_AM748591)

F3.2a_AM748568 <- grep("F3.2a_AM748568", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=F3.2a_AM748568, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(F3.2a_AM748568)

C3b_AF499791 <- grep("C3b_AF499791", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=C3b_AF499791, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(C3b_AF499791)

C1051_AF195153 <- grep("C1051_AF195153", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=C1051_AF195153, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(C1051_AF195153)

C1232_EU118163 <- grep("C1232_EU118163", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=C1232_EU118163, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(C1232_EU118163)

A133_DQ174725 <- grep("A133_DQ174725", data.frame(tax_table(phy97.f.c))$hit)
phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=A133_DQ174725, archetype=1)
tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
tt[isna,"hit"] <- info
tax_table(phy97.f.c) <- as.matrix(tt)
rm(A133_DQ174725)

rm(info,isna,tt)

# Manually checking whether all equivalent taxa are collapsed
# t <- sort(data.frame(tax_table(phy97.f.c))$hit)
# t
# 
# Check if all equivalent taxa are collapsed
# x <- length(unique(data.frame(tax_table(phy97.f.c))$hit))
# y <- length(data.frame(tax_table(phy97.f.c))$hit)
# identical(x,y)

# Make a tax_table column for "clade"
# Rename rank_names (aka column names in phy97.f.c)
colnames(tax_table(phy97.f.c)) <- c(otu = "otu", sim = "sim", del = "del", ins = "ins", mis = "mis", len = "len", score = "score", hit = "hit", i = "clade")
# Copy "hit" into last column
tax_table(phy97.f.c)[,9] <- tax_table(phy97.f.c)[,8]

# Use gsub to replace full "hit" name with only clade letter
tax_table(phy97.f.c)[,9] <- gsub("^A.*", "A", tax_table(phy97.f.c)[,9])
tax_table(phy97.f.c)[,9] <- gsub("^B.*", "B", tax_table(phy97.f.c)[,9])
tax_table(phy97.f.c)[,9] <- gsub("^C.*", "C", tax_table(phy97.f.c)[,9])
tax_table(phy97.f.c)[,9] <- gsub("^D.*", "D", tax_table(phy97.f.c)[,9])
tax_table(phy97.f.c)[,9] <- gsub("^F.*", "F", tax_table(phy97.f.c)[,9])
tax_table(phy97.f.c)[,9] <- gsub("^G.*", "G", tax_table(phy97.f.c)[,9])
tax_table(phy97.f.c)[,9] <- gsub("^I.*", "I", tax_table(phy97.f.c)[,9])

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.p <- transform_sample_counts(phy97.f.c, function(x) x/sum(x))

# Subset coral data set into individual genus data sets (and water and sediment)
phy97.f.c.platy <- subset_samples(phy97.f.c,Coral_Species=="Platygyra_sp")
phy97.f.c.platy <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy) > 0, prune=TRUE)

phy97.f.c.fpenta <- subset_samples(phy97.f.c,Coral_Species=="Favites_pentagona")
phy97.f.c.fpenta <- subset_taxa(phy97.f.c.fpenta, taxa_sums(phy97.f.c.fpenta) > 0, prune=TRUE)

phy97.f.c.faviasp <- subset_samples(phy97.f.c,Coral_Species=="Favia_sp")
phy97.f.c.faviam <- subset_samples(phy97.f.c,Coral_Species=="Favia_matthai")
phy97.f.c.faviaall <- merge_phyloseq(phy97.f.c.faviasp,phy97.f.c.faviam)
phy97.f.c.faviaall <- subset_taxa(phy97.f.c.faviaall, taxa_sums(phy97.f.c.faviaall) > 0, prune=TRUE)

phy97.f.c.water <- subset_samples(phy97.f.c,Coral_Species=="water")
phy97.f.c.water <- subset_taxa(phy97.f.c.water, taxa_sums(phy97.f.c.water) > 0, prune=TRUE)

phy97.f.c.sediment <- subset_samples(phy97.f.c,Coral_Species=="sediment")
phy97.f.c.sediment <- subset_taxa(phy97.f.c.sediment, taxa_sums(phy97.f.c.sediment) > 0, prune=TRUE)

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.platy.p <- transform_sample_counts(phy97.f.c.platy, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.fpenta.p <- transform_sample_counts(phy97.f.c.fpenta, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.faviasp.p <- transform_sample_counts(phy97.f.c.faviasp, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.faviam.p <- transform_sample_counts(phy97.f.c.faviam, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.faviaall.p <- transform_sample_counts(phy97.f.c.faviaall, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.water.p <- transform_sample_counts(phy97.f.c.water, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.sediment.p <- transform_sample_counts(phy97.f.c.sediment, function(x) x/sum(x))

# Subset by site
phy97.f.c.site3 <- subset_samples(phy97.f.c,Site=="3")
phy97.f.c.site3 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site3) > 0, prune=TRUE)
phy97.f.c.site5 <- subset_samples(phy97.f.c,Site=="5")
phy97.f.c.site5 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site5) > 0, prune=TRUE)
phy97.f.c.site8 <- subset_samples(phy97.f.c,Site=="8")
phy97.f.c.site8 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site8) > 0, prune=TRUE)
phy97.f.c.site14 <- subset_samples(phy97.f.c,Site=="14")
phy97.f.c.site14 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site14) > 0, prune=TRUE)
phy97.f.c.site15 <- subset_samples(phy97.f.c,Site=="15")
phy97.f.c.site15 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site15) > 0, prune=TRUE)
phy97.f.c.site19 <- subset_samples(phy97.f.c,Site=="19")
phy97.f.c.site19 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site19) > 0, prune=TRUE)
phy97.f.c.site25 <- subset_samples(phy97.f.c,Site=="25")
phy97.f.c.site25 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site25) > 0, prune=TRUE)
phy97.f.c.site27 <- subset_samples(phy97.f.c,Site=="27")
phy97.f.c.site27 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site27) > 0, prune=TRUE)
phy97.f.c.site30 <- subset_samples(phy97.f.c,Site=="30")
phy97.f.c.site30 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site30) > 0, prune=TRUE)
phy97.f.c.site32 <- subset_samples(phy97.f.c,Site=="32")
phy97.f.c.site32 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site32) > 0, prune=TRUE)
phy97.f.c.site34 <- subset_samples(phy97.f.c,Site=="34")
phy97.f.c.site34 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site34) > 0, prune=TRUE)
phy97.f.c.site35 <- subset_samples(phy97.f.c,Site=="35")
phy97.f.c.site35 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site35) > 0, prune=TRUE)
phy97.f.c.site37 <- subset_samples(phy97.f.c,Site=="37")
phy97.f.c.site37 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site37) > 0, prune=TRUE)
phy97.f.c.site38 <- subset_samples(phy97.f.c,Site=="38")
phy97.f.c.site38 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site38) > 0, prune=TRUE)
phy97.f.c.site40 <- subset_samples(phy97.f.c,Site=="40")
phy97.f.c.site40 <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.site40) > 0, prune=TRUE)

# Save grouped data as RData file
save.image(file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f_grouped.RData")
