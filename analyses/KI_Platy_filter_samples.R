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
# load("C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f.RData")
# Loading the full sequence database - from the first two MiSeq Runs
load("C:/Users/Dani/Documents/Data_Analysis/KI_seqs/data/otus_97_bysample/KI_seqs_f.RData")

phy.f.coral <- phy.f

# Filter OTUs by minimum count
# Set threshold count
n <- 10
# Identify OTUs below threshold count
taxa <- taxa_sums(phy.f.coral)[which(taxa_sums(phy.f.coral) >= n)]
# Remove taxa below threshold count
phy.f.coral <- prune_taxa(names(taxa), phy.f.coral)

# Filter samples by minimum count
# Set threshold number of reads
sn <- 200
# Remove samples with fewer reads than threshold
phy.f.coral <- prune_samples(sample_sums(phy.f.coral)>=sn, phy.f.coral)


# Filter OTUs by minimum count again in case any dropped below threshold after filtering samples
# Identify OTUs below threshold count
taxa <- taxa_sums(phy.f.coral)[which(taxa_sums(phy.f.coral) >= n)]
# Remove taxa below threshold count
phy97.f <- prune_taxa(names(taxa), phy.f.coral)

# Characterize sites by disturbance level
VeryHigh <- c(33,40,32,31,27,30,26)
High <- c(25,3,38,24)
HighMed <- c(9,34,35,8,14,6)
LowMed <- c(2,22,1,23)
Low <- c(7,13,12,4,36,5,37)
VeryLow <- c(10,21,11,20,16,15,39,19,18,17)

sample_data(phy97.f)$Site <- factor(sample_data(phy97.f)$Site)
sample_data(phy97.f)$Dist <- sample_data(phy97.f)$Site

for (i in VeryHigh){
  if(i %in% unique(as.character(data.frame(sample_data(phy97.f))$Site)))
    (sample_data(phy97.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("VeryHigh")))
}

for (i in High){
  if(i %in% unique(as.character(data.frame(sample_data(phy97.f))$Site)))
    (sample_data(phy97.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("High"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High")))
}

for (i in HighMed){
  if(i %in% unique(as.character(data.frame(sample_data(phy97.f))$Site)))
    (sample_data(phy97.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed")))
}

for (i in LowMed){
  if(i %in% unique(as.character(data.frame(sample_data(phy97.f))$Site)))
    (sample_data(phy97.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("LowMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("LowMed")))
}

for (i in Low){
  if(i %in% unique(as.character(data.frame(sample_data(phy97.f))$Site)))
    (sample_data(phy97.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("Low"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("Low")))
}

for (i in VeryLow){
  if(i %in% unique(as.character(data.frame(sample_data(phy97.f))$Site)))
    (sample_data(phy97.f)$Dist<- gsub (paste("\\<",as.character(i),"\\>",sep=""),as.character("VeryLow"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("VeryLow")))
}

levels(sample_data(phy97.f)$Dist) <- c("VeryHigh","High","HighMed","Low","VeryLow")

# Assign new name for clarity
phy97.f.c <- phy97.f

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

# Write tax table for phylogenetically-informed diversity
write.table(data.frame(tax_table(phy97.f.c)), "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/tax_table.txt", row.names=T, quote=F)
# Write otu table for phylogenetically-informed diversity
write.delim(data.frame(otu_table(phy97.f.c)), "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otu_table.tsv", quote = FALSE, row.names = T, sep = "\t")

#https://rdrr.io/rforge/seqinr/man/dist.alignment.html
#returns sqrt of pairwise genetic distance, then squared the matrices
A.seqs <- read.alignment(file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/A_tree_seqs_aligned_clean.fasta", format= "fasta")

A.dis <- (as.matrix(dist.alignment(A.seqs, matrix = "identity" )))^2
write.csv(A.dis, file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/A.dis.matx.csv")

C.seqs <- read.alignment(file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/C_tree_seqs_aligned_clean.fasta", format= "fasta")
C.dis <- (as.matrix(dist.alignment(C.seqs, matrix = "identity" )))^2
write.csv(C.dis, file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/C.dis.matx.csv")

D.seqs <- read.alignment(file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/D_tree_seqs_aligned_clean.fasta", format= "fasta")
D.dis <- (as.matrix(dist.alignment(D.seqs, matrix = "identity" )))^2
write.csv(D.dis, file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/D.dis.matx.csv")

F.seqs <- read.alignment(file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/F_tree_seqs_aligned_clean.fasta", format= "fasta")
F.dis <- (as.matrix(dist.alignment(F.seqs, matrix = "identity" )))^2
write.csv(F.dis, file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/F.dis.matx.csv")

G.seqs <- read.alignment(file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/G_tree_seqs_aligned_clean.fasta", format= "fasta")
G.dis <- (as.matrix(dist.alignment(G.seqs, matrix = "identity" )))^2
write.csv(G.dis, file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/G.dis.matx.csv")

#give clade distances using average 28s distance from Pochon and Gates 2010
A_C <- matrix(0.1960, ncol=ncol(A.dis), nrow=nrow(C.dis), dimnames=list(rownames(C.dis), colnames(A.dis)))
A_D <- matrix(0.1775, ncol=ncol(A.dis), nrow=nrow(D.dis), dimnames=list(rownames(D.dis), colnames(A.dis)))
A_F <- matrix(0.2085, ncol=ncol(A.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(A.dis)))
A_G <- matrix(0.216, ncol=ncol(A.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(A.dis)))
C_D <- matrix(0.1520, ncol=ncol(C.dis), nrow=nrow(D.dis), dimnames=list(rownames(D.dis), colnames(C.dis)))
C_F <- matrix(0.0945, ncol=ncol(C.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(C.dis)))
C_G <- matrix(0.187, ncol=ncol(C.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(C.dis)))
D_F <- matrix(0.1691, ncol=ncol(D.dis), nrow=nrow(F.dis), dimnames=list(rownames(F.dis), colnames(D.dis)))
D_G <- matrix(0.1795, ncol=ncol(D.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(D.dis)))
F_G <- matrix(0.2072, ncol=ncol(F.dis), nrow=nrow(G.dis), dimnames=list(rownames(G.dis), colnames(F.dis)))


#build ACD matrix
col1 <- rbind(A.dis, A_C, A_D, A_F, A_G)
col2 <- rbind(matrix(NA, nrow=nrow(A.dis), ncol=ncol(C.dis), dimnames=list(rownames(A.dis), colnames(C.dis))), C.dis, C_D, C_F, C_G)
col3 <- rbind(matrix(NA, nrow=nrow(A.dis)+nrow(C.dis), ncol=ncol(D.dis), dimnames=list(c(rownames(A.dis), rownames(C.dis)), colnames(D.dis))), D.dis, D_F, D_G)
col4 <- rbind(matrix(NA, nrow=nrow(A.dis)+nrow(C.dis)+nrow(D.dis), ncol=ncol(F.dis), dimnames=list(c(rownames(A.dis), rownames(C.dis), rownames(D.dis)), colnames(F.dis))), F.dis, F_G)
col5 <- rbind(matrix(NA, nrow=nrow(A.dis)+nrow(C.dis)+nrow(D.dis)+nrow(F.dis), ncol=ncol(G.dis), dimnames=list(c(rownames(A.dis), rownames(C.dis), rownames(D.dis), rownames(F.dis)), colnames(G.dis))), G.dis)

ubermatrix <- cbind(col1, col2, col3, col4, col5)
dim(ubermatrix)

#build tree
uber.tree <- phangorn::upgma(ubermatrix)
plot(uber.tree, main="UPGMA")

#write tree to file
write.tree(uber.tree, file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/uber.tre")

#use tree and OTU table for calculating beta_diversity.py
#http://qiime.org/scripts/beta_diversity.html

class(uber.tree)

phy_tree(uber.tree)

# Slot uber tree into the phy_tree slot of the phyloseq object
phy_tree(phy97.f.c) <- phy_tree(uber.tree)

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.p <- transform_sample_counts(phy97.f.c, function(x) x/sum(x))

# Subset coral data set into individual genus data sets
phy97.f.c.platy <- subset_samples(phy97.f.c,Coral_Species=="Platygyra_sp")
phy97.f.c.platy <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy) > 0, prune=TRUE)
phy97.f.c.platy.AD <- subset_samples(phy97.f.c.platy,Status=="alive"|Status=="dead")

phy97.f.c.fpenta <- subset_samples(phy97.f.c,Coral_Species=="Favites_pentagona")
phy97.f.c.fpenta <- subset_taxa(phy97.f.c.fpenta, taxa_sums(phy97.f.c.fpenta) > 0, prune=TRUE)
phy97.f.c.fpenta.AD <- subset_samples(phy97.f.c.fpenta,Status=="alive"|Status=="dead")

# Note, this is now the same as phy97.f.c - renaming and subsetting was completed before when water and sediment samples were included
phy97.f.c.coral <- subset_samples(phy97.f.c,SampleType=="coral")
phy97.f.c.coral <- subset_taxa(phy97.f.c.coral, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)

# Subset coral samples that have been confirmed either alive or dead (remove unknowns/gone)
phy97.f.c.coral.AD <- subset_samples(phy97.f.c.coral,Status=="alive"|Status=="dead")

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.platy.p <- transform_sample_counts(phy97.f.c.platy, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.fpenta.p <- transform_sample_counts(phy97.f.c.fpenta, function(x) x/sum(x))

# Subset coral by site
for (i in unique(data.frame(sample_data(phy97.f.c.coral))$Site)){
  nam <- paste("phy97.f.c.coral.site",i,sep="")
  a <- eval(subset_samples(phy97.f.c.coral,Site==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}

# Subset platygyra only by site
for (i in unique(data.frame(sample_data(phy97.f.c.platy))$Site)){
  nam <- paste("phy97.f.c.platy.site",i,sep="")
  a <- eval(subset_samples(phy97.f.c.platy,Site==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}

# Subset fpenta only by site
for (i in unique(data.frame(sample_data(phy97.f.c.fpenta))$Site)){
  nam <- paste("phy97.f.c.fpenta.site",i,sep="")
  a <- eval(subset_samples(phy97.f.c.fpenta,Site==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}

# Subset coral by disturbance level
for (i in unique(data.frame(sample_data(phy97.f.c.coral))$Dist)){
  nam <- paste("phy97.f.c.coral.",i,sep="")
  a <- eval(subset_samples(phy97.f.c.coral,Dist==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}

# Subset platygyra only by Disturbance level
for (i in unique(data.frame(sample_data(phy97.f.c.platy))$Dist)){
  nam <- paste("phy97.f.c.platy.",i,sep="")
  a <- eval(subset_samples(phy97.f.c.platy,Dist==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}

# Subset fpenta only by disturbance level
for (i in unique(data.frame(sample_data(phy97.f.c.fpenta))$Dist)){
  nam <- paste("phy97.f.c.fpenta.",i,sep="")
  a <- eval(subset_samples(phy97.f.c.fpenta,Dist==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}


# Transform sample counts to proportional abundance for downstream analyses
for (i in unique(data.frame(sample_data(phy97.f.c.coral))$Site)){
  nam <- paste("phy97.f.c.coral.site",i,".p",sep="")
  c <- paste("phy97.f.c.coral.site",i,sep="")
  a <- eval(transform_sample_counts(get(c), function(x) x/sum(x)))
  assign(nam,a)
}

# Subset by Field Season
phy97.f.c.coral.2014 <- subset_samples(phy97.f.c.coral,Year=="2014")
phy97.f.c.coral.2014 <- subset_taxa(phy97.f.c.coral.2014, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2014.AD <- subset_samples(phy97.f.c.coral.2014,Status=="alive"|Status=="dead")
phy97.f.c.coral.2015Jan <- subset_samples(phy97.f.c.coral,Year=="2015Jan")
phy97.f.c.coral.2015Jan <- subset_taxa(phy97.f.c.coral.2015Jan, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2015Jan.AD <- subset_samples(phy97.f.c.coral.2015Jan,Status=="alive"|Status=="dead")
phy97.f.c.coral.2015May <- subset_samples(phy97.f.c.coral,Year=="2015May")
phy97.f.c.coral.2015May <- subset_taxa(phy97.f.c.coral.2015May, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2015May.AD <- subset_samples(phy97.f.c.coral.2015May,Status=="alive"|Status=="dead")
phy97.f.c.coral.2015July <- subset_samples(phy97.f.c.coral,Year=="2015July")
phy97.f.c.coral.2015July <- subset_taxa(phy97.f.c.coral.2015July, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2015July.AD <- subset_samples(phy97.f.c.coral.2015July,Status=="alive"|Status=="dead")
phy97.f.c.coral.2016March <- subset_samples(phy97.f.c.coral,Year=="2016March")
phy97.f.c.coral.2016March <- subset_taxa(phy97.f.c.coral.2016March, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2016March.AD <- subset_samples(phy97.f.c.coral.2016March,Status=="alive"|Status=="dead")

# Subset coral samples to only keep samples taken before the event (==2014 to May 2015)
phy97.f.c.coral.AD.before <- subset_samples(phy97.f.c.coral.AD,Year!="2016March", prune=TRUE)
phy97.f.c.coral.AD.before <- subset_samples(phy97.f.c.coral.AD.before,Year!="2015July", prune=TRUE)
# Subset Platygyra samples to only keep samples taken before the event (==2014 to May 2015)
phy97.f.c.platy.AD.before <- subset_samples(phy97.f.c.platy.AD,Year!="2016March", prune=TRUE)
phy97.f.c.platy.AD.before <- subset_samples(phy97.f.c.platy.AD.before,Year!="2015July", prune=TRUE)
# Subset fpenta samples to only keep samples taken before the event (==2014 to May 2015)
phy97.f.c.fpenta.AD.before <- subset_samples(phy97.f.c.fpenta.AD,Year!="2016March", prune=TRUE)
phy97.f.c.fpenta.AD.before <- subset_samples(phy97.f.c.fpenta.AD.before,Year!="2015July", prune=TRUE)
# Subset coral samples to only keep samples taken during/after the event (== July 2015 - 2016)
phy97.f.c.coral.AD.da <- subset_samples(phy97.f.c.coral.AD,Year=="2016March"|Year=="2015July", prune=TRUE)
# Subset Platygyra samples to only keep samples taken during/after the event (== July 2015 - 2016)
phy97.f.c.platy.AD.da <- subset_samples(phy97.f.c.platy.AD,Year=="2016March"|Year=="2015July", prune=TRUE)
# Subset FPenta samples to only keep samples taken during/after the event (== July 2015 - 2016)
phy97.f.c.fpenta.AD.da <- subset_samples(phy97.f.c.fpenta.AD,Year=="2016March"|Year=="2015July", prune=TRUE)

rm(a,b,c,i,nam,VeryHigh,VeryLow,phy.f,phy97.f,Low,LowMed,High,HighMed)

# Save grouped data as RData file
save(list = ls(all.names = TRUE), file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/KI_seqs_f_coral_grouped.RData")
