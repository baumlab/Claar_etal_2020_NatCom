# Load necessary packages
library(Hmisc)
library(corrplot)
library(RColorBrewer)

rm(list=ls())

# Load in Platygyra phyloseq objects
load("data/KI_seqs_f_coral_grouped.RData")

timecols <- c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c")

# Set the plot margins
par(mar=c(0.5,0.5,0.5,0.5))# Subset to only keep taxa with taxa_sums > 100
GT100 <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy)>100)
GT100 <- prune_samples(sample_sums(GT100)>=100,GT100)
GT100.p <- transform_sample_counts(GT100, function(x) x/sum(x))

# otu_table(GT100.p) <- log10(otu_table(GT100.p))

# Plot phylogenetic tree, and use custom colors for each field season
plottree <- plot_tree(GT100.p,color="field_season",label.tips = "hit",size="abundance",ladderize = "left") + scale_color_manual(values=c(timecols[c(1,4,5,3,2)])) + scale_size_area(max_size=10) + labs(size = "Abundance (Proportion)")
plottree

# Write this phylogenetic tree to jpg
jpeg(file="figures/Extended Data/ExData_Figure5.jpg",width=18, height=7,units="in", res=300)
plottree
dev.off()

ki16 <- subset_samples(GT100.p,sample_data(GT100.p)$field_season == "KI2016a")
t16 <- subset_samples(ki16,data.frame(tax_table(ki16))$hit =="C15_AY239369", prune=TRUE)
tax_table(t16) <- subset_taxa(t16, data.frame(tax_table(t16))$hit =="C15_AY239369", prune=TRUE)

tc15 <- subset_taxa(t16,taxa_sums(t16)<.3)

plottree2 <- plot_tree(GT100.p,color="Status",label.tips = "hit",size="abundance",ladderize = "left") + scale_color_manual(values=c("#2b83ba","#d7191c", "lightgray", "lightgray", "lightgray")) + scale_size_area(max_size=10) + labs(size = "Abundance (Proportion)")
plottree2

