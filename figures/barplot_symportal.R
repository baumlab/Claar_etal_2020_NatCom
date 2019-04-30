rm(list=ls())

library(phyloseq)
library(gridExtra)
library(ggplot2)

load(file = "data/KI_seqs_f_coral_grouped_symportal.RData")

phy97.f.c.coral.AD.before.p <- transform_sample_counts(phy97.f.c.coral.AD.before, function(x) x/sum(x))

clade_cols <- c(A="yellow",C="blue",D="red",B="orange")
phy97.f.c.platy.AD.before.p <- transform_sample_counts(phy97.f.c.platy.AD.before, function(x) x/sum(x))
phy97.f.c.platy.AD.before.p.no2015c <- subset_samples(phy97.f.c.platy.AD.before.p,field_season!="KI2015c")
plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

phy97.f.c.fpenta.AD.before.p <- transform_sample_counts(phy97.f.c.fpenta.AD.before, function(x) x/sum(x))
phy97.f.c.fpenta.AD.before.p.no2015c <- subset_samples(phy97.f.c.fpenta.AD.before.p,field_season!="KI2015c")
plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)


phy97.f.c.dipmat.AD.before.p <- transform_sample_counts(phy97.f.c.dipmat.AD.before, function(x) x/sum(x))
phy97.f.c.dipmat.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipmat.AD.before.p,field_season!="KI2015c")
plot_bar(phy97.f.c.dipmat.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.dipmat.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

phy97.f.c.dipsp.AD.before.p <- transform_sample_counts(phy97.f.c.dipsp.AD.before, function(x) x/sum(x))
phy97.f.c.dipsp.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipsp.AD.before.p,field_season!="KI2015c")
plot_bar(phy97.f.c.dipsp.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.dipsp.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

phy97.f.c.hydno.AD.before.p <- transform_sample_counts(phy97.f.c.hydno.AD.before, function(x) x/sum(x))
phy97.f.c.hydno.AD.before.p.no2015c <- subset_samples(phy97.f.c.hydno.AD.before.p,field_season!="KI2015c")
plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

phy97.f.c.plob.AD.before.p <- transform_sample_counts(phy97.f.c.plob.AD.before, function(x) x/sum(x))
phy97.f.c.plob.AD.before.p.no2015c <- subset_samples(phy97.f.c.plob.AD.before.p,field_season!="KI2015c")
plot_bar(phy97.f.c.plob.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.plob.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

jpeg(filename = "figures/new_barplots_April2019/platy_clade_barplot.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
dev.off()

jpeg(filename = "figures/new_barplots_April2019/fpenta_clade_barplot.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
dev.off()

jpeg(filename = "figures/new_barplots_April2019/dipsp_clade_barplot.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.dipsp.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
dev.off()

jpeg(filename = "figures/new_barplots_April2019/dipmat_clade_barplot.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.dipmat.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
dev.off()

jpeg(filename = "figures/new_barplots_April2019/hydno_clade_barplot.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
dev.off()

jpeg(filename = "figures/new_barplots_April2019/hydno_its2type_barplot.jpg",width = 10, height= 12, units = "in", res=300)
colourCount = length(unique(data.frame(tax_table(phy97.f.c.hydno.AD.before.p.no2015c))$its2_type_profile))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
p1 <- plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="its2_type_profile")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette(colourCount))

colourCount2 = length(unique(data.frame(tax_table(phy97.f.c.hydno.AD.before.p.no2015c))$majority_its2_sequence))
getPalette2 = colorRampPalette(brewer.pal(9, "Set3"))
p2 <- plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette2(colourCount2))

grid.arrange(p1,p2,nrow=2)

dev.off()

jpeg(filename = "figures/new_barplots_April2019/platy_its2type_barplot.jpg",width = 10, height= 12, units = "in", res=300)
colourCount = length(unique(data.frame(tax_table(phy97.f.c.platy.AD.before.p.no2015c))$its2_type_profile))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
p1 <- plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="its2_type_profile")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette(colourCount))

colourCount2 = length(unique(data.frame(tax_table(phy97.f.c.platy.AD.before.p.no2015c))$majority_its2_sequence))
getPalette2 = colorRampPalette(brewer.pal(9, "Set3"))
p2 <- plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette2(colourCount2))

grid.arrange(p1,p2,nrow=2)

dev.off()

jpeg(filename = "figures/new_barplots_April2019/fpenta_its2type_barplot.jpg",width = 10, height= 12, units = "in", res=300)
colourCount = length(unique(data.frame(tax_table(phy97.f.c.fpenta.AD.before.p.no2015c))$its2_type_profile))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
p1 <- plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="its2_type_profile")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette(colourCount))

colourCount2 = length(unique(data.frame(tax_table(phy97.f.c.fpenta.AD.before.p.no2015c))$majority_its2_sequence))
getPalette2 = colorRampPalette(brewer.pal(9, "Set3"))
p2 <- plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette2(colourCount2))

grid.arrange(p1,p2,nrow=2)

dev.off()

jpeg(filename = "figures/new_barplots_April2019/dipsp_its2type_barplot.jpg",width = 10, height= 12, units = "in", res=300)
colourCount = length(unique(data.frame(tax_table(phy97.f.c.dipsp.AD.before.p.no2015c))$its2_type_profile))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
p1 <- plot_bar(phy97.f.c.dipsp.AD.before.p.no2015c,fill="its2_type_profile")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette(colourCount))

colourCount2 = length(unique(data.frame(tax_table(phy97.f.c.dipsp.AD.before.p.no2015c))$majority_its2_sequence))
getPalette2 = colorRampPalette(brewer.pal(9, "Set3"))
p2 <- plot_bar(phy97.f.c.dipsp.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette2(colourCount2))

grid.arrange(p1,p2,nrow=2)

dev.off()

jpeg(filename = "figures/new_barplots_April2019/dipmat_its2type_barplot.jpg",width = 10, height= 12, units = "in", res=300)
colourCount = length(unique(data.frame(tax_table(phy97.f.c.dipmat.AD.before.p.no2015c))$its2_type_profile))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
p1 <- plot_bar(phy97.f.c.dipmat.AD.before.p.no2015c,fill="its2_type_profile")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette(colourCount))

colourCount2 = length(unique(data.frame(tax_table(phy97.f.c.dipmat.AD.before.p.no2015c))$majority_its2_sequence))
getPalette2 = colorRampPalette(brewer.pal(9, "Set3"))
p2 <- plot_bar(phy97.f.c.dipmat.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = getPalette2(colourCount2))

grid.arrange(p1,p2,nrow=2)

dev.off()
