

load("data/KI_seqs_f_coral_grouped.RData")

allhits_unique <- levels(data.frame(tax_table(phy97.f.c.platy))[,c(1,8)])
write.table(allhits_unique, file="data/Bioinf/tree/allhits_unique.txt",row.names = FALSE, col.names = FALSE)

allhits <- data.frame(tax_table(phy97.f.c.platy))[,c(1,8)]
write.table(allhits, file="data/Bioinf/tree/allhits.txt",row.names = FALSE, col.names = FALSE)
write.csv(allhits, file="data/Bioinf/tree/allhits.csv",row.names = FALSE)


par(mar=c(0.5,0.5,0.5,0.5))
plot(phy_tree(phy97.f.c.platy),main="UPGMA")
plot(phy_tree(phy97.f.c.platy),main="UPGMA",type="fan")


plot_tree(phy97.f.c.platy,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")
names(sample_data(phy97.f.c.platy))

plot_tree(phy97.f.c.platy,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")

C15_seqs = subset_taxa(phy97.f.c.platy, hit==c("C15_AY239369"))
C15_seqs2 = subset_taxa(phy97.f.c.platy, hit==c("C15.6_FN563472"))
C15_seqs = prune_samples(sample_sums(C15_seqs)>=10, C15_seqs)
C15_seqs2 = prune_samples(sample_sums(C15_seqs2)>=1, C15_seqs2)

C31_seqs = subset_taxa(phy97.f.c.platy, hit==c("C31_AY258496"))
C31_seqs = prune_samples(sample_sums(C31_seqs)>=1, C31_seqs)

C_seqs = subset_taxa(phy97.f.c.platy, clade=="C")
C_seqs = prune_samples(sample_sums(C_seqs)>=1, C_seqs)

jpeg(file="data/Bioinf/tree/tree_Conly.jpg",width=10, height=7,units="in", res=300)
plot_tree(C_seqs,color="field_season",label.tips = c("hit","otu"),size="abundance",ladderize = "left")
dev.off()

myTaxa = names(sort(taxa_sums(phy97.f.c.platy), decreasing = TRUE)[1:20])
ex1 = prune_taxa(myTaxa, phy97.f.c.platy)
plot_tree(ex1,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")


GT100 <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy)>100)
GT100 <- prune_samples(sample_sums(GT100)>=100,GT100)
GT100
plot_tree(GT100,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")
plottree <- plot_tree(GT100,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")

cbar <- read.csv("figures/cmap_enso.csv",header=F)
cbar.rgb <- rgb(cbar)

plottree <- plottree + scale_color_manual(values=c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c"))

jpeg(file="data/Bioinf/tree/tree_100.jpg",width=12, height=7,units="in", res=300)
plottree
dev.off()

GT500 <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy)>500)
GT500 <- prune_samples(sample_sums(GT500)>=500,GT500)
GT500
plot_tree(GT500,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")


GT100.p <- subset_taxa(phy97.f.c.platy.p, taxa_sums(phy97.f.c.platy.p)>0.01)
GT100.p <- prune_samples(sample_sums(GT100.p)>=0.001,GT100.p)
GT100.p
plot_tree(GT100.p,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")
plottree <- plot_tree(GT100,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")

GT100 <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy)>100)
GT100 <- prune_samples(sample_sums(GT100)>=100,GT100)
GT100
plot_tree(GT100,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")
plottree <- plot_tree(GT100,color="field_season",label.tips = "hit",size="abundance",ladderize = "left")

tax_sums <- sort(taxa_sums(phy97.f.c.platy))

pt1 <- plot_tree(phy97.f.c.platy,color="field_season",label.tips = c("hit"),size="abundance",ladderize = "left") + guides(color=FALSE,size=FALSE,fill=FALSE,shape=FALSE)
pt2 <- plot_tree(phy97.f.c.platy,color="field_season",label.tips = c("otu"),size="abundance",ladderize = "left")

jpeg(file="data/Bioinf/tree/tree_all.jpg",width=11, height=8.5,units="in", res=300)
grid.arrange(pt1,pt2,nrow=1)
dev.off()


otus <- otu_table(phy97.f.c.platy)
otus_mat <- otus@.Data


veganotu = function(physeq) {
  require("vegan")
  OTU = otu_table(physeq)
  if (taxa_are_rows(OTU)) {
    OTU = t(OTU)
  }
  return(as(OTU, "matrix"))
}

psd <- data.frame(sample_data(phy97.f.c.platy))

platy <- subset_taxa(physeq = phy97.f.c.platy,taxa_sums(phy97.f.c.platy) >50)
platy.veg <- veganotu(platy)


platy.cor <- cor(platy.veg, method = c("pearson"))
platy.rcorr <- rcorr(as.matrix(platy.veg))

platy.rcorr$P
platy.rcorr$r
symnum(platy.cor)
install.packages("corrplot")
library(corrplot)
corrplot(platy.cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(platy.rcorr$r, type="upper", order="hclust", 
         p.mat = platy.rcorr$P, sig.level = 0.05, insig = "blank")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library("PerformanceAnalytics")
chart.Correlation(platy.cor, histogram=TRUE, pch=19)
