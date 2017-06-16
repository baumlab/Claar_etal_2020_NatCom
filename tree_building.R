rm(list=ls())

library(seqinr)
library(phangorn) 
library(caroline)

load("C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/KI_seqs_f_coral_grouped.RData")

#https://rdrr.io/rforge/seqinr/man/dist.alignment.html
#returns sqrt of pairwise genetic distance, then squared the matrices
A.seqs <- read.alignment(file = "data/A_tree_seqs_aligned_clean.fasta", format= "fasta")
A.dis <- (as.matrix(dist.alignment(A.seqs, matrix = "identity" )))^2
write.csv(A.dis, file="data/A.dis.matx.csv")

C.seqs <- read.alignment(file = "data/C_tree_seqs_aligned_clean.fasta", format= "fasta")
C.dis <- (as.matrix(dist.alignment(C.seqs, matrix = "identity" )))^2
write.csv(C.dis, file="data/C.dis.matx.csv")

D.seqs <- read.alignment(file = "data/D_tree_seqs_aligned_clean.fasta", format= "fasta")
D.dis <- (as.matrix(dist.alignment(D.seqs, matrix = "identity" )))^2
write.csv(D.dis, file="data/D.dis.matx.csv")

F.seqs <- read.alignment(file = "data/F_tree_seqs_aligned_clean.fasta", format= "fasta")
F.dis <- (as.matrix(dist.alignment(F.seqs, matrix = "identity" )))^2
write.csv(F.dis, file="data/F.dis.matx.csv")

G.seqs <- read.alignment(file = "data/G_tree_seqs_aligned_clean.fasta", format= "fasta")
G.dis <- (as.matrix(dist.alignment(G.seqs, matrix = "identity" )))^2
write.csv(G.dis, file="data/G.dis.matx.csv")

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
write.tree(uber.tree, file="data/uber.tre")

#use tree and OTU table for calculating beta_diversity.py
#http://qiime.org/scripts/beta_diversity.html

class(uber.tree)

phy_tree(uber.tree)

# Slot uber tree into the phy_tree slot of the phyloseq object
phy_tree(phy97.f.c) <- phy_tree(uber.tree)


