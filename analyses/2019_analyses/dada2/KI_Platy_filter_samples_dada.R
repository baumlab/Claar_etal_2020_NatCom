# Import Libraries
library(stringr)
library(reshape2)
library(phyloseq)
library(seqinr)
library(phangorn) 
library(caroline)
library(DECIPHER)


# Clear workspace
rm(list=ls())

# Load filtered RData object from output of filter_notsym.R script
load("analyses/2019_analyses/dada2/KI_Platy_dada.RData")

phy.f <- ps

sample_data(phy.f)$field_season <- sample_data(phy.f)$Year_Pre_Post
sample_data(phy.f)$field_season <- as.character(sample_data(phy.f)$field_season)
sample_data(phy.f)$field_season <- replace(sample_data(phy.f)$field_season, sample_data(phy.f)$field_season=="2014", "KI2014")
sample_data(phy.f)$field_season <- replace(sample_data(phy.f)$field_season, sample_data(phy.f)$field_season=="2015Jan_Pre", "KI2015a_Pre")
sample_data(phy.f)$field_season <- replace(sample_data(phy.f)$field_season, sample_data(phy.f)$field_season=="2015Jan_Post", "KI2015a_Post")
sample_data(phy.f)$field_season <- replace(sample_data(phy.f)$field_season, sample_data(phy.f)$field_season=="2015May", "KI2015b")
sample_data(phy.f)$field_season <- replace(sample_data(phy.f)$field_season, sample_data(phy.f)$field_season=="2015July", "KI2015c")
sample_data(phy.f)$field_season <- replace(sample_data(phy.f)$field_season, sample_data(phy.f)$field_season=="2016March", "KI2016a")

colnames(sample_data(phy.f))[colnames(sample_data(phy.f))=="Site"] <- "site"

################################### Filtering ######################################
phy.f <- subset_samples(phy.f, Coral_Species!="lobata")
phy.f <- subset_samples(phy.f, as.data.frame(sample_data(phy.f))$CoralTag != "1005")
phy.f <- subset_samples(phy.f, as.data.frame(sample_data(phy.f))$CoralTag != "1011")
phy.f <- subset_samples(phy.f, as.data.frame(sample_data(phy.f))$CoralTag != "1013")
phy.f <- subset_samples(phy.f, as.data.frame(sample_data(phy.f))$CoralTag != "1017")


############################## Site Formatting ####################################

# Characterize sites by disturbance level
VeryHigh <- c(30,31,32,27)
High <- c(1,6,26,38,40)
Medium <- c(7,8,12,13,14,22,25,33,34,35)
Low <- c(2,3,4,9,23,24)
VeryLow <- c(5,10,11,15,16,17,18,19,20,21,28,29,36,37,39)

# Make site a factor
sample_data(phy.f)$site <- factor(sample_data(phy.f)$site)
# Create a disturbance level column, start as site
sample_data(phy.f)$Dist <- sample_data(phy.f)$site

# Finish creating Dist
for (i in VeryHigh){
  if(i %in% unique(as.character(data.frame(sample_data(phy.f))$site)))
    (sample_data(phy.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("VeryHigh"), as.character(data.frame(sample_data(phy.f))$Dist), as.character("VeryHigh")))
}

for (i in High){
  if(i %in% unique(as.character(data.frame(sample_data(phy.f))$site)))
    (sample_data(phy.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("High"), as.character(data.frame(sample_data(phy.f))$Dist), as.character("High")))
}

for (i in Medium){
  if(i %in% unique(as.character(data.frame(sample_data(phy.f))$site)))
    (sample_data(phy.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("Medium"), as.character(data.frame(sample_data(phy.f))$Dist), as.character("Medium")))
}

for (i in Low){
  if(i %in% unique(as.character(data.frame(sample_data(phy.f))$site)))
    (sample_data(phy.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("Low"), as.character(data.frame(sample_data(phy.f))$Dist), as.character("Low")))
}

for (i in VeryLow){
  if(i %in% unique(as.character(data.frame(sample_data(phy.f))$site)))
    (sample_data(phy.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("VeryLow"), as.character(data.frame(sample_data(phy.f))$Dist), as.character("VeryLow")))
}

# Order levels
levels(sample_data(phy.f)$Dist) <- c("VeryHigh","High","Medium","Low","VeryLow")

###################### Physeq formatting and tree #####################################
# Assign new name for clarity
phyASV.f.c <- phy.f
phyASV.f.c <- prune_taxa(taxa_sums(phyASV.f.c)>0,phyASV.f.c)

# Write tax table for phylogenetically-informed diversity
# write.table(data.frame(tax_table(phyASV.f.c)), "data/tax_table.txt", row.names=T, quote=F)
# Write otu table for phylogenetically-informed diversity
# write.delim(data.frame(otu_table(phyASV.f.c)), "data/otu_table.tsv", quote = FALSE, row.names = T, sep = "\t")

ASV.As <- subset_taxa(phyASV.f.c,Genus=="g__Symbiodinium")
# ASV.Bs <- subset_taxa(phyASV.f.c,Genus=="g__Breviolum")
ASV.Cs <- subset_taxa(phyASV.f.c,Genus=="g__Cladocopium")
ASV.Ds <- subset_taxa(phyASV.f.c,Genus=="g__Durusdinium")
# ASV.Es <- subset_taxa(phyASV.f.c,Genus=="g__Effrenium")
# ASV.Fs <- subset_taxa(phyASV.f.c,Genus=="g__Fugacium")
ASV.Gs <- subset_taxa(phyASV.f.c,Genus=="g__Gerakladium")
# ASV.Hs <- subset_taxa(phyASV.f.c,Genus=="g__cladeH") # no seqs
# ASV.Is <- subset_taxa(phyASV.f.c,Genus=="g__cladeI")

ASV.seqs <- refseq(phyASV.f.c)
min(width(ASV.seqs))
max(width(ASV.seqs))

ASV.A.seqs <- refseq(ASV.As)
writeXStringSet(ASV.A.seqs, # write to a new FASTA file
                file="data/Bioinf/tree/ASV_A_tree_seqs.fasta")
ASV.A.seqs.aligned <- AlignSeqs(ASV.A.seqs)
writeXStringSet(ASV.A.seqs.aligned, # write the alignment to a new FASTA file
                file="data/Bioinf/tree/ASV_A_tree_seqs_aligned.fasta")
ASV.C.seqs <- refseq(ASV.Cs)
writeXStringSet(ASV.C.seqs, # write to a new FASTA file
                file="data/Bioinf/tree/ASV_C_tree_seqs.fasta")
ASV.C.seqs.aligned <- AlignSeqs(ASV.C.seqs)
writeXStringSet(ASV.C.seqs.aligned, # write the alignment to a new FASTA file
                file="data/Bioinf/tree/ASV_C_tree_seqs_aligned.fasta")
ASV.D.seqs <- refseq(ASV.Ds)
writeXStringSet(ASV.D.seqs, # write to a new FASTA file
                file="data/Bioinf/tree/ASV_D_tree_seqs.fasta")
ASV.D.seqs.aligned <- AlignSeqs(ASV.D.seqs)
writeXStringSet(ASV.D.seqs.aligned, # write the alignment to a new FASTA file
                file="data/Bioinf/tree/ASV_D_tree_seqs_aligned.fasta")
ASV.G.seqs <- refseq(ASV.Gs)
writeXStringSet(ASV.G.seqs, # write to a new FASTA file
                file="data/Bioinf/tree/ASV_G_tree_seqs.fasta")
ASV.G.seqs.aligned <- AlignSeqs(ASV.G.seqs)
writeXStringSet(ASV.G.seqs.aligned, # write the alignment to a new FASTA file
                file="data/Bioinf/tree/ASV_G_tree_seqs_aligned.fasta")

writeXStringSet(c(ASV.A.seqs.aligned,ASV.C.seqs.aligned,ASV.D.seqs.aligned,ASV.G.seqs.aligned), 
                file="data/Bioinf/tree/ASV_ALL_tree_seqs_aligned.fasta")

#https://rdrr.io/rforge/seqinr/man/dist.alignment.html
#returns sqrt of pairwise genetic distance, then squared the matrices
A.seqs <- read.alignment(file = "data/Bioinf/tree/ASV_A_tree_seqs_aligned.fasta", format= "fasta")
A.dis <- (as.matrix(dist.alignment(A.seqs, matrix = "identity" )))^2
write.csv(A.dis, file="data/Bioinf/tree/ASV_A_dis_matx.csv")

C.seqs <- read.alignment(file = "data/Bioinf/tree/ASV_C_tree_seqs_aligned.fasta", format= "fasta")
C.dis <- (as.matrix(dist.alignment(C.seqs, matrix = "identity" )))^2
write.csv(C.dis, file="data/Bioinf/tree/ASV_C_dis_matx.csv")

D.seqs <- read.alignment(file = "data/Bioinf/tree/ASV_D_tree_seqs_aligned.fasta", format= "fasta")
D.dis <- (as.matrix(dist.alignment(D.seqs, matrix = "identity" )))^2
write.csv(D.dis, file="data/Bioinf/tree/ASV_D_dis_matx.csv")

G.seqs <- read.alignment(file = "data/Bioinf/tree/ASV_G_tree_seqs_aligned.fasta", format= "fasta")
G.dis <- (as.matrix(dist.alignment(G.seqs, matrix = "identity" )))^2
write.csv(G.dis, file="data/Bioinf/tree/ASV_G_dis_matx.csv")

#give clade distances using average 28s distance from Pochon and Gates 2010
# A_B <- matrix(0.219, ncol=ncol(A.dis), nrow=nrow(B.dis), dimnames=list(rownames(B.dis), colnames(A.dis)))
A_C <- matrix(0.1960, ncol=ncol(A.dis), nrow=nrow(C.dis), dimnames=list(rownames(C.dis), colnames(A.dis)))
A_D <- matrix(0.1775, ncol=ncol(A.dis), nrow=nrow(D.dis), dimnames=list(rownames(D.dis), colnames(A.dis)))
# A_E <- matrix(0.2085, ncol=ncol(A.dis), nrow=nrow(E.dis), dimnames=list(rownames(E.dis), colnames(A.dis)))
# A_F <- matrix(0.2085, ncol=ncol(A.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(A.dis)))
A_G <- matrix(0.216, ncol=ncol(A.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(A.dis)))
# A_I <- matrix(0.205, ncol=ncol(A.dis), nrow=nrow(I.dis), dimnames=list(rownames(I.dis), colnames(A.dis)))
# B_C <- matrix(0.114, ncol=ncol(B.dis), nrow=nrow(C.dis), dimnames=list(rownames(C.dis), colnames(B.dis)))
# B_D <- matrix(0.1705, ncol=ncol(B.dis), nrow=nrow(D.dis), dimnames=list(rownames(D.dis), colnames(B.dis)))
# B_E <- matrix(0.1355, ncol=ncol(B.dis), nrow=nrow(E.dis), dimnames=list(rownames(E.dis), colnames(B.dis)))
# B_F <- matrix(0.1355, ncol=ncol(B.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(B.dis)))
# B_G <- matrix(0.21, ncol=ncol(B.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(B.dis)))
# B_I <- matrix(0.151, ncol=ncol(B.dis), nrow=nrow(I.dis), dimnames=list(rownames(I.dis), colnames(B.dis)))
C_D <- matrix(0.1520, ncol=ncol(C.dis), nrow=nrow(D.dis), dimnames=list(rownames(D.dis), colnames(C.dis)))
# C_E <- matrix(0.0945, ncol=ncol(C.dis), nrow=nrow(E.dis), dimnames=list(rownames(E.dis), colnames(C.dis)))
# C_F <- matrix(0.0945, ncol=ncol(C.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(C.dis)))
C_G <- matrix(0.187, ncol=ncol(C.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(C.dis)))
# C_I <- matrix(0.137, ncol=ncol(C.dis), nrow=nrow(I.dis), dimnames=list(rownames(I.dis), colnames(C.dis)))
# D_E <- matrix(0.1691, ncol=ncol(D.dis), nrow=nrow(E.dis), dimnames=list(rownames(E.dis), colnames(D.dis)))
# D_F <- matrix(0.1691, ncol=ncol(D.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(D.dis)))
D_G <- matrix(0.1795, ncol=ncol(D.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(D.dis)))
# D_I <- matrix(0.169125, ncol=ncol(D.dis), nrow=nrow(I.dis), dimnames=list(rownames(I.dis), colnames(D.dis)))
# E_F <- matrix(0.1691, ncol=ncol(E.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(E.dis)))
# E_G <- matrix(0.1691, ncol=ncol(E.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(E.dis)))
# E_I <- matrix(0.1691, ncol=ncol(E.dis), nrow=nrow(I.dis), dimnames=list(rownames(I.dis), colnames(E.dis)))
# F_G <- matrix(0.2072, ncol=ncol(F.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(F.dis)))
# F_I <- matrix(0.14925, ncol=ncol(F.dis), nrow=nrow(I.dis), dimnames=list(rownames(I.dis), colnames(F.dis)))
# G_I <- matrix(0.194, ncol=ncol(G.dis), nrow=nrow(I.dis), dimnames=list(rownames(I.dis), colnames(G.dis)))

#build ACDG matrix
col1 <- rbind(A.dis, A_C, A_D, A_G)
col2 <- rbind(matrix(NA, nrow=nrow(A.dis), ncol=ncol(C.dis), dimnames=list(rownames(A.dis), colnames(C.dis))), C.dis, C_D, C_G)
col3 <- rbind(matrix(NA, nrow=nrow(A.dis)+nrow(C.dis), ncol=ncol(D.dis), dimnames=list(c(rownames(A.dis), rownames(C.dis)), colnames(D.dis))), D.dis, D_G)
col4 <- rbind(matrix(NA, nrow=nrow(A.dis)+nrow(C.dis)+nrow(D.dis), ncol=ncol(G.dis), dimnames=list(c(rownames(A.dis), rownames(C.dis), rownames(D.dis)), colnames(G.dis))), G.dis)

ubermatrix <- cbind(col1, col2, col3, col4)
dim(ubermatrix)

#build tree
uber.tree <- phangorn::upgma(ubermatrix)
plot(uber.tree, main="UPGMA")

#write tree to file
write.tree(uber.tree, file="data/Bioinf/tree/uber.tre")

class(uber.tree)

# Slot uber tree into the phy_tree slot of the phyloseq object
phy_tree(phyASV.f.c) <- phy_tree(uber.tree)
plot_tree(phyASV.f.c, label.tips = "Genus", color = "Genus")

######################### Subset by compartment and calc seqs ##################################

# Transform sample counts to proportional abundance for downstream analyses
phyASV.f.c.p <- transform_sample_counts(phyASV.f.c, function(x) x/sum(x))

# Calculate number of sequences in phyASV.f.c
total_seqs <- sum(taxa_sums(phyASV.f.c))

############################## Subset out Platy #######################
# Subset coral data set into individual genus data sets
phyASV.f.c.platy <- subset_samples(phyASV.f.c,Coral_Species=="Platygyra_sp")
phyASV.f.c.platy <- subset_taxa(phyASV.f.c.platy, taxa_sums(phyASV.f.c.platy) > 0, prune=TRUE)
phyASV.f.c.platy.AD <- subset_samples(phyASV.f.c.platy,Status=="alive"|Status=="dead")

phyASV.f.c.platy.p <- transform_sample_counts(phyASV.f.c.platy, function(x) x/sum(x))

phyASV.f.c.platy.AD.before <- subset_samples(phyASV.f.c.platy.AD,field_season!="KI2016a", prune=TRUE)
phyASV.f.c.platy.AD.before <- subset_samples(phyASV.f.c.platy.AD.before,field_season!="KI2015c", prune=TRUE)
phyASV.f.c.platy.AD.after <- subset_samples(phyASV.f.c.platy.AD,field_season=="KI2016a", prune=TRUE)

############################### Subset by coral species/disturbance ##########################
# Subset by coral species and disturbance level
# phyASV.f.c.coral.Peyd <- subset_samples(phyASV.f.c.coral,sample_data(phyASV.f.c.coral)$Coral_Species=="Pocillopora_eydouxi")
# phyASV.f.c.coral.Peyd <- prune_taxa(taxa_sums(phyASV.f.c.coral.Peyd)>0,phyASV.f.c.coral.Peyd)
# phyASV.f.c.coral.MAeq <- subset_samples(phyASV.f.c.coral,sample_data(phyASV.f.c.coral)$Coral_Species=="Montipora_foliosa")
# phyASV.f.c.coral.MAeq <- prune_taxa(taxa_sums(phyASV.f.c.coral.MAeq)>0,phyASV.f.c.coral.MAeq)
# phyASV.f.c.coral.Plob <- subset_samples(phyASV.f.c.coral,sample_data(phyASV.f.c.coral)$Coral_Species=="Porites_lobata")
# phyASV.f.c.coral.Plob <- prune_taxa(taxa_sums(phyASV.f.c.coral.Plob)>0,phyASV.f.c.coral.Plob)

############################# Subset by Field Season #################################
# Subset by field season
# coral.before <- subset_samples(phyASV.f.c.coral, data.frame(sample_data(phyASV.f.c.coral))$field_season == "KI2014",prune=TRUE)
# coral.before <- subset_taxa(coral.before, taxa_sums(coral.before) > 0, prune=TRUE)
# 
# coral.storm <- subset_samples(phyASV.f.c.coral, data.frame(sample_data(phyASV.f.c.coral))$field_season == "KI2015a_Post", prune=TRUE)
# coral.storm <- subset_taxa(coral.storm, taxa_sums(coral.storm) > 0, prune=TRUE)
# 
# coral.after <- subset_samples(phyASV.f.c.coral, data.frame(sample_data(phyASV.f.c.coral))$field_season == "KI2015b", prune=TRUE)
# coral.after <- subset_taxa(coral.after, taxa_sums(coral.after) > 0, prune=TRUE)

############################ ASVs by compartment #########################
n.all.ASVs <- nrow(data.frame(tax_table(phyASV.f.c)))

all.ASVs <- rownames(data.frame(tax_table(phyASV.f.c)))

#################### Save grouped data as RData file ##########################
save(list=ls(),file="data/KI_Platy_f_coral_grouped_ASVs.RData")
save(list=c("phyASV.f.c.p"),file="data/KI_Platy_phyASVfcp_ASVs.RData")
