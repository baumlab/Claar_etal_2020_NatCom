# Load necessary packages
library(Hmisc)
library(corrplot)
library(RColorBrewer)

# Load in Platygyra phyloseq objects
load("data/KI_seqs_f_coral_grouped.RData")

# Extract the denovo name and hit for all unique hits
allhits_unique <- levels(data.frame(tax_table(phy97.f.c.platy))[,c(1,8)])
# Write a txt file with this info
write.table(allhits_unique, file="data/Bioinf/tree/allhits_unique.txt",row.names = FALSE, col.names = FALSE)

# Extract the denovo name and hit for all hits (some hit names will be replicated)
allhits <- data.frame(tax_table(phy97.f.c.platy))[,c(1,8)]
# Write a txt file with this info
write.table(allhits, file="data/Bioinf/tree/allhits.txt",row.names = FALSE, col.names = FALSE)
# Write a csv file with this info
write.csv(allhits, file="data/Bioinf/tree/allhits.csv",row.names = FALSE)

# Set the plot margins
par(mar=c(0.5,0.5,0.5,0.5))

# Basic plots I'm not using anymore
# plot(phy_tree(phy97.f.c.platy),main="UPGMA")
# plot(phy_tree(phy97.f.c.platy),main="UPGMA",type="fan")

# Plot phylogenetic tree for all representative sequences
plot_tree(phy97.f.c.platy,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")

# Subset only Clade C sequences
C_seqs = subset_taxa(phy97.f.c.platy, clade=="C")
C_seqs = prune_samples(sample_sums(C_seqs)>=1, C_seqs)

# Make a phylogenetic tree plot of only Clade C sequences, and write to jpg
jpeg(file="data/Bioinf/tree/tree_Conly.jpg",width=10, height=7,units="in", res=300)
plot_tree(C_seqs,color="field_season",label.tips = c("hit","otu"),size="abundance",ladderize = "left")
dev.off()

# Subset to only keep taxa with taxa_sums > 100
GT100 <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy)>100)
GT100 <- prune_samples(sample_sums(GT100)>=100,GT100)

# Plot phylogenetic tree, and use custom colors for each field season
plottree <- plot_tree(GT100,color="field_season",label.tips = "hit",size="abundance",ladderize = "left") + scale_color_manual(values=c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c"))

# Write this phylogenetic tree to jpg
jpeg(file="data/Bioinf/tree/tree_100.jpg",width=12, height=7,units="in", res=300)
plottree
dev.off()

# To make a side-by-side plot of denovo and hit names, first make two separate plots with branch tips labeled with "hit" and "otu" respectively
pt1 <- plot_tree(phy97.f.c.platy,color="field_season",label.tips = c("hit"),size="abundance",ladderize = "left") + guides(color=FALSE,size=FALSE,fill=FALSE,shape=FALSE)
pt2 <- plot_tree(phy97.f.c.platy,color="field_season",label.tips = c("otu"),size="abundance",ladderize = "left")

# Use grid.arrange to write a jpg with both figures side-by-side
jpeg(file="data/Bioinf/tree/tree_all.jpg",width=11, height=8.5,units="in", res=300)
grid.arrange(pt1,pt2,nrow=1)
dev.off()


# Helper function from http://joey711.github.io/phyloseq-demo/phyloseq-demo.html to convert phyloseq objects to vegan-friendly formatting
veganotu = function(physeq) {
  require("vegan")
  OTU = otu_table(physeq)
  if (taxa_are_rows(OTU)) {
    OTU = t(OTU)
  }
  return(as(OTU, "matrix"))
}

# Subset phyloseq object to all taxa with >50 sequences (I played around with this from 10 to 10000)
platy <- subset_taxa(physeq = phy97.f.c.platy,taxa_sums(phy97.f.c.platy) >50)

# Use veganotu function to convert platy to vegan-friendly formatting
platy.veg <- veganotu(platy)

# Calculate correlation matrix
platy.cor <- cor(platy.veg, method = c("pearson"))
# Use Hmisc package to calculate correlation matrix with statistical significance
platy.rcorr <- rcorr(as.matrix(platy.veg))
# platy.rcorr$P
# platy.rcorr$r

# Use corrplot package to plot correlation matrix in a human-readable format
# Set p > 0.05
corrplot(platy.rcorr$r, type="upper", order="AOE", 
         p.mat = platy.rcorr$P, sig.level = 0.05, insig = "blank", method="square",
         col = rev(brewer.pal(n = 8, name = "RdBu")),tl.col = "black")

# Write a jpg for the correlation matrix
jpeg(file="data/Bioinf/tree/otu_correlation_matrix.jpg",width=10, height=10,units="in", res=300)
corrplot(platy.rcorr$r, type="upper", order="AOE", 
         p.mat = platy.rcorr$P, sig.level = 0.05, insig = "blank", method="square",
         col = rev(brewer.pal(n = 8, name = "RdBu")),tl.col = "black")
dev.off()

# Plot phylogenetic tree, and use custom colors for status (Alive/Dead)
# GT100b <- subset_samples(GT100,sample_data(GT100)$field_season == "KI2014" | sample_data(GT100)$field_season == "KI2015a" | sample_data(GT100)$field_season == "KI2015b")
# plottree2 <- plot_tree(GT100b,color="Status",label.tips = "hit",size="abundance",ladderize = "left") + scale_color_manual(values=c("#2b83ba","#d7191c","lightgray","lightgray","lightgray"))
# plottree2
