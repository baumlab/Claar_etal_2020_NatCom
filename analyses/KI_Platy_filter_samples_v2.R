# Import Libraries
library(stringr)
library(reshape2)
library(phyloseq)
library(seqinr)
library(phangorn) 
library(caroline)

# Clear workspace
rm(list=ls())

# Load filtered RData object from output of filter_notsym.R script
load("data/SymPortal/KI-platy-phyloseq-obj.RData")

phy.f.coral <- physeq

# Filter OTUs by minimum count
# Set threshold count
n <- 5
# Identify OTUs below threshold count
taxa <- taxa_sums(phy.f.coral)[which(taxa_sums(phy.f.coral) >= n)]
# Remove taxa below threshold count
phy.f.coral <- prune_taxa(names(taxa), phy.f.coral)
# Note no taxa were removed with this cutoff

# Filter samples by minimum count
# Set threshold number of reads
sn <- 1000
# Remove samples with fewer reads than threshold
phy.f.coral <- prune_samples(sample_sums(phy.f.coral)>=sn, phy.f.coral)

sample_data(phy.f.coral)$disturbance_level[sample_data(phy.f.coral)$disturbance_level =="vhogh"] <- "vhigh"
sample_data(phy.f.coral)$disturbance_level <- droplevels(sample_data(phy.f.coral)$disturbance_level)

levels(sample_data(phy.f.coral)$disturbance_level) <- c("vhigh","high","med","low","vlow")
levels(sample_data(phy.f.coral)$disturbance_level) <- c("VeryHigh","High","Medium","Low","VeryLow")

# Assign new name for clarity
phy97.f.c <- phy.f.coral

phy97.f.c <- subset_samples(phy97.f.c, as.data.frame(sample_data(phy97.f.c))$coral_tag != "1005")
phy97.f.c <- subset_samples(phy97.f.c, as.data.frame(sample_data(phy97.f.c))$coral_tag != "1011")
phy97.f.c <- subset_samples(phy97.f.c, as.data.frame(sample_data(phy97.f.c))$coral_tag != "1013")
phy97.f.c <- subset_samples(phy97.f.c, as.data.frame(sample_data(phy97.f.c))$coral_tag != "1017")

# Make "host_name" with genus + species
sample_data(phy97.f.c)$host_name <- paste(sample_data(phy97.f.c)$host_genus,sample_data(phy97.f.c)$host_species, sep="_")

# Make "field_season" 
sample_data(phy97.f.c)$field_season <- as.character(sample_data(phy97.f.c)$collection_date)
sample_data(phy97.f.c)$field_season[sample_data(phy97.f.c)$field_season=="2015-05-01"] <- "KI2015b"
sample_data(phy97.f.c)$field_season[sample_data(phy97.f.c)$field_season=="2014-08-01"] <- "KI2014"
sample_data(phy97.f.c)$field_season[sample_data(phy97.f.c)$field_season=="2015-07-01"] <- "KI2015c"
sample_data(phy97.f.c)$field_season[sample_data(phy97.f.c)$field_season=="2015-01-01"] <- "KI2015a"
sample_data(phy97.f.c)$field_season[sample_data(phy97.f.c)$field_season=="2016-03-01"] <- "KI2016a"


# #https://rdrr.io/rforge/seqinr/man/dist.alignment.html
# #returns sqrt of pairwise genetic distance, then squared the matrices
# A.seqs <- read.alignment(file = "data/Bioinf/tree/A_tree_seqs_aligned_clean.fasta", format= "fasta")
# 
# A.dis <- (as.matrix(dist.alignment(A.seqs, matrix = "identity" )))^2
# write.csv(A.dis, file="data/Bioinf/tree/A.dis.matx.csv")
# 
# C.seqs <- read.alignment(file = "data/Bioinf/tree/C_tree_seqs_aligned_clean.fasta", format= "fasta")
# C.dis <- (as.matrix(dist.alignment(C.seqs, matrix = "identity" )))^2
# write.csv(C.dis, file="data/Bioinf/tree/C.dis.matx.csv")
# 
# D.seqs <- read.alignment(file = "data/Bioinf/tree/D_tree_seqs_aligned_clean.fasta", format= "fasta")
# D.dis <- (as.matrix(dist.alignment(D.seqs, matrix = "identity" )))^2
# write.csv(D.dis, file="data/Bioinf/tree/D.dis.matx.csv")
# 
# # F.seqs <- read.alignment(file = "data/Bioinf/tree/F_tree_seqs_aligned_clean.fasta", format= "fasta")
# # F.dis <- (as.matrix(dist.alignment(F.seqs, matrix = "identity" )))^2
# # write.csv(F.dis, file="data/Bioinf/tree/F.dis.matx.csv")
# 
# G.seqs <- read.alignment(file = "data/Bioinf/tree/G_tree_seqs_aligned_clean.fasta", format= "fasta")
# G.dis <- (as.matrix(dist.alignment(G.seqs, matrix = "identity" )))^2
# write.csv(G.dis, file="data/Bioinf/tree/G.dis.matx.csv")
# 
# #give clade distances using average 28s distance from Pochon and Gates 2010
# A_C <- matrix(0.1960, ncol=ncol(A.dis), nrow=nrow(C.dis), dimnames=list(rownames(C.dis), colnames(A.dis)))
# A_D <- matrix(0.1775, ncol=ncol(A.dis), nrow=nrow(D.dis), dimnames=list(rownames(D.dis), colnames(A.dis)))
# # A_F <- matrix(0.2085, ncol=ncol(A.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(A.dis)))
# A_G <- matrix(0.216, ncol=ncol(A.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(A.dis)))
# C_D <- matrix(0.1520, ncol=ncol(C.dis), nrow=nrow(D.dis), dimnames=list(rownames(D.dis), colnames(C.dis)))
# # C_F <- matrix(0.0945, ncol=ncol(C.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(C.dis)))
# C_G <- matrix(0.187, ncol=ncol(C.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(C.dis)))
# # D_F <- matrix(0.1691, ncol=ncol(D.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(D.dis)))
# D_G <- matrix(0.1795, ncol=ncol(D.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(D.dis)))
# # F_G <- matrix(0.2072, ncol=ncol(F.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(F.dis)))
# 
# 
# #build ACDG matrix
# col1 <- rbind(A.dis, A_C, A_D, A_G)
# col2 <- rbind(matrix(NA, nrow=nrow(A.dis), ncol=ncol(C.dis), dimnames=list(rownames(A.dis), colnames(C.dis))), C.dis, C_D, C_G)
# col3 <- rbind(matrix(NA, nrow=nrow(A.dis)+nrow(C.dis), ncol=ncol(D.dis), dimnames=list(c(rownames(A.dis), rownames(C.dis)), colnames(D.dis))), D.dis, D_G)
# col4 <- rbind(matrix(NA, nrow=nrow(A.dis)+nrow(C.dis)+nrow(D.dis), ncol=ncol(G.dis), dimnames=list(c(rownames(A.dis), rownames(C.dis), rownames(D.dis)), colnames(G.dis))), G.dis)
# 
# ubermatrix <- cbind(col1, col2, col3, col4)
# dim(ubermatrix)
# 
# #build tree
# uber.tree <- phangorn::upgma(ubermatrix)
# plot(uber.tree, main="UPGMA")
# 
# #write tree to file
# write.tree(uber.tree, file="data/Bioinf/tree/uber.tre")
# 
# #use tree and OTU table for calculating beta_diversity.py
# #http://qiime.org/scripts/beta_diversity.html
# 
# class(uber.tree)
# 
# phy_tree(uber.tree)
# 
# # Slot uber tree into the phy_tree slot of the phyloseq object
# phy_tree(phy97.f.c) <- phy_tree(uber.tree)

# Change metadata "Status" for corals we found either alive or dead after sequence processing
sample_data(phy97.f.c)$outcome[which(data.frame(sample_data(phy97.f.c))$coral_tag=="234")] <- "dead"

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.p <- transform_sample_counts(phy97.f.c, function(x) x/sum(x))

# Subset coral data set into individual genus data sets
phy97.f.c.platy <- subset_samples(phy97.f.c,host_name=="platygyra_sp")
phy97.f.c.platy <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy) > 0, prune=TRUE)
phy97.f.c.platy.AD <- subset_samples(phy97.f.c.platy,outcome=="alive"|outcome=="dead")

phy97.f.c.fpenta <- subset_samples(phy97.f.c,host_name=="favites_pentagona")
phy97.f.c.fpenta <- subset_taxa(phy97.f.c.fpenta, taxa_sums(phy97.f.c.fpenta) > 0, prune=TRUE)
phy97.f.c.fpenta.AD <- subset_samples(phy97.f.c.fpenta,outcome=="alive"|outcome=="dead")

phy97.f.c.dipsp <- subset_samples(phy97.f.c,host_name=="dipsastraea_sp")
phy97.f.c.dipsp <- subset_taxa(phy97.f.c.dipsp, taxa_sums(phy97.f.c.dipsp) > 0, prune=TRUE)
phy97.f.c.dipsp.AD <- subset_samples(phy97.f.c.dipsp,outcome=="alive"|outcome=="dead")

phy97.f.c.dipmat <- subset_samples(phy97.f.c,host_name=="dipsastraea_matthai")
phy97.f.c.dipmat <- subset_taxa(phy97.f.c.dipmat, taxa_sums(phy97.f.c.dipmat) > 0, prune=TRUE)
phy97.f.c.dipmat.AD <- subset_samples(phy97.f.c.dipmat,outcome=="alive"|outcome=="dead")

phy97.f.c.plob <- subset_samples(phy97.f.c,host_name=="porites_lobata")
phy97.f.c.plob <- subset_taxa(phy97.f.c.plob, taxa_sums(phy97.f.c.plob) > 0, prune=TRUE)
phy97.f.c.plob.AD <- subset_samples(phy97.f.c.plob,outcome=="alive"|outcome=="dead")

phy97.f.c.hydno <- subset_samples(phy97.f.c,host_name=="hydnophora_microconos")
phy97.f.c.hydno <- subset_taxa(phy97.f.c.hydno, taxa_sums(phy97.f.c.hydno) > 0, prune=TRUE)
phy97.f.c.hydno.AD <- subset_samples(phy97.f.c.hydno,outcome=="alive"|outcome=="dead")

# Note, this is now the same as phy97.f.c - renaming and subsetting was completed before when water and sediment samples were included
phy97.f.c.coral <- phy97.f.c

# Subset coral samples that have been confirmed either alive or dead (remove unknowns/gone)
phy97.f.c.coral.AD <- subset_samples(phy97.f.c.coral,outcome=="alive"|outcome=="dead")

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.platy.p <- transform_sample_counts(phy97.f.c.platy, function(x) x/sum(x))
phy97.f.c.fpenta.p <- transform_sample_counts(phy97.f.c.fpenta, function(x) x/sum(x))
phy97.f.c.dipsp.p <- transform_sample_counts(phy97.f.c.dipsp, function(x) x/sum(x))
phy97.f.c.dipmat.p <- transform_sample_counts(phy97.f.c.dipmat, function(x) x/sum(x))
phy97.f.c.plob.p <- transform_sample_counts(phy97.f.c.plob, function(x) x/sum(x))
phy97.f.c.hydno.p <- transform_sample_counts(phy97.f.c.hydno, function(x) x/sum(x))

# Subset coral samples to only keep samples taken before the event (==KI2014 to May 2015)
phy97.f.c.coral.AD.before <- subset_samples(phy97.f.c.coral.AD,field_season!="KI2016a"|field_season!="KI2015c", prune=TRUE)
phy97.f.c.platy.AD.before <- subset_samples(phy97.f.c.platy.AD,field_season!="KI2016a"|field_season!="KI2015c", prune=TRUE)
phy97.f.c.fpenta.AD.before <- subset_samples(phy97.f.c.fpenta.AD,field_season!="KI2016a"|field_season!="KI2015c", prune=TRUE)
phy97.f.c.dipsp.AD.before <- subset_samples(phy97.f.c.dipsp.AD,field_season!="KI2016a"|field_season!="KI2015c", prune=TRUE)
phy97.f.c.dipmat.AD.before <- subset_samples(phy97.f.c.dipmat.AD,field_season!="KI2016a"|field_season!="KI2015c", prune=TRUE)
phy97.f.c.plob.AD.before <- subset_samples(phy97.f.c.plob.AD,field_season!="KI2016a"|field_season!="KI2015c", prune=TRUE)
phy97.f.c.hydno.AD.before <- subset_samples(phy97.f.c.hydno.AD,field_season!="KI2016a"|field_season!="KI2015c", prune=TRUE)

# Subset coral samples to only keep samples taken after the event (== 2016)
phy97.f.c.coral.AD.after <- subset_samples(phy97.f.c.coral.AD,field_season=="KI2016a", prune=TRUE)
phy97.f.c.platy.AD.after <- subset_samples(phy97.f.c.platy.AD,field_season=="KI2016a", prune=TRUE)
phy97.f.c.fpenta.AD.after <- subset_samples(phy97.f.c.fpenta.AD,field_season=="KI2016a", prune=TRUE)
phy97.f.c.dipsp.AD.after <- subset_samples(phy97.f.c.dipsp.AD,field_season=="KI2016a", prune=TRUE)
phy97.f.c.dipmat.AD.after <- subset_samples(phy97.f.c.dipmat.AD,field_season=="KI2016a", prune=TRUE)
phy97.f.c.plob.AD.after <- subset_samples(phy97.f.c.plob.AD,field_season=="KI2016a", prune=TRUE)
phy97.f.c.hydno.AD.after <- subset_samples(phy97.f.c.hydno.AD,field_season=="KI2016a", prune=TRUE)

# Calculate number of sequences in phy97.f.c
total_seqs <- sum(taxa_sums(phy97.f.c))

# Calculate number of corals in phy97.f.c
total_tags <- unique(data.frame(sample_data(phy97.f.c))$coral_tag)
total_sites <- unique(data.frame(sample_data(phy97.f.c))$site)
total_dist <- unique(data.frame(sample_data(phy97.f.c))$disturbance_level)

# Cleanup 
rm(a,b,c,i,nam,VeryHigh,VeryLow,phy.f,Low,High,Medium,phy.f.coral,A_C,A_D,A_G,A.dis,C_D,C_G,C.dis,col1,col2,col3,col4,D_G,D.dis,G.dis,A.seqs,C.seqs,D.seqs,G.seqs,n)

# Save grouped data as RData file
save(list = ls(all.names = TRUE), file = "data/KI_seqs_f_coral_grouped_all_symportal.RData")
save(phy97.f.c.coral.AD.before, phy97.f.c.p, 
     phy97.f.c.coral, phy97.f.c.coral.AD,
     phy97.f.c.platy.AD.before, phy97.f.c.platy.p, 
     phy97.f.c.platy, phy97.f.c.platy.AD,
     phy97.f.c.fpenta.AD.before, phy97.f.c.fpenta.p, 
     phy97.f.c.fpenta, phy97.f.c.fpenta.AD,  
     phy97.f.c.dipsp.AD.before, phy97.f.c.dipsp.p, 
     phy97.f.c.dipsp, phy97.f.c.dipsp.AD,
     phy97.f.c.dipmat.AD.before, phy97.f.c.dipmat.p, 
     phy97.f.c.dipmat, phy97.f.c.dipmat.AD,
     phy97.f.c.hydno.AD.before, phy97.f.c.hydno.p, 
     phy97.f.c.hydno, phy97.f.c.hydno.AD,
     phy97.f.c.plob.AD.before, phy97.f.c.plob.p, 
     phy97.f.c.plob, phy97.f.c.plob.AD,
     file = "data/KI_seqs_f_coral_grouped_symportal.RData")

