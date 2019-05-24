rm(list=ls())

library(phyloseq)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)

load(file = "data/KI_seqs_f_coral_grouped_symportal.RData")

phy97.f.c.coral.AD.before.p <- transform_sample_counts(phy97.f.c.coral.AD.before, function(x) x/sum(x))

clade_cols <- c(A="yellow",C="blue",D="red",B="orange")
phy97.f.c.platy.AD.before.p <- transform_sample_counts(phy97.f.c.platy.AD.before, function(x) x/sum(x))
phy97.f.c.platy.AD.before.p.no2015c <- subset_samples(phy97.f.c.platy.AD.before.p,field_season!="KI2015c")
phy97.f.c.platy.AD.before.p.no2015c <- subset_samples(phy97.f.c.platy.AD.before.p.no2015c,field_season!="KI2016a")
plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

phy97.f.c.fpenta.AD.before.p <- transform_sample_counts(phy97.f.c.fpenta.AD.before, function(x) x/sum(x))
phy97.f.c.fpenta.AD.before.p.no2015c <- subset_samples(phy97.f.c.fpenta.AD.before.p,field_season!="KI2015c")
phy97.f.c.fpenta.AD.before.p.no2015c <- subset_samples(phy97.f.c.fpenta.AD.before.p.no2015c,field_season!="KI2016a")
plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)


# phy97.f.c.dipmat.AD.before.p <- transform_sample_counts(phy97.f.c.dipmat.AD.before, function(x) x/sum(x))
# phy97.f.c.dipmat.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipmat.AD.before.p,field_season!="KI2015c")
# phy97.f.c.dipmat.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipmat.AD.before.p.no2015c,field_season!="KI2016a")
# plot_bar(phy97.f.c.dipmat.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
# plot_bar(phy97.f.c.dipmat.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)
# 
# phy97.f.c.dipsp.AD.before.p <- transform_sample_counts(phy97.f.c.dipsp.AD.before, function(x) x/sum(x))
# phy97.f.c.dipsp.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipsp.AD.before.p,field_season!="KI2015c")
# phy97.f.c.dipsp.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipsp.AD.before.p.no2015c,field_season!="KI2016a")
# plot_bar(phy97.f.c.dipsp.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
# plot_bar(phy97.f.c.dipsp.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

phy97.f.c.dipall.AD.before <- merge_phyloseq(phy97.f.c.dipmat.AD.before,phy97.f.c.dipsp.AD.before)

phy97.f.c.dipall.AD.before.p <- transform_sample_counts(phy97.f.c.dipall.AD.before, function(x) x/sum(x))
phy97.f.c.dipall.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipall.AD.before.p,field_season!="KI2015c")
phy97.f.c.dipall.AD.before.p.no2015c <- subset_samples(phy97.f.c.dipall.AD.before.p.no2015c,field_season!="KI2016a")
plot_bar(phy97.f.c.dipall.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.dipall.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)


phy97.f.c.hydno.AD.before.p <- transform_sample_counts(phy97.f.c.hydno.AD.before, function(x) x/sum(x))
phy97.f.c.hydno.AD.before.p.no2015c <- subset_samples(phy97.f.c.hydno.AD.before.p,field_season!="KI2015c")
phy97.f.c.hydno.AD.before.p.no2015c <- subset_samples(phy97.f.c.hydno.AD.before.p.no2015c,field_season!="KI2016a")
plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols)
plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols)

jpeg(filename = "figures/Supplementary_Materials/platy_barplot_clade_outcome.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Symbiodinium"))),expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()

jpeg(filename = "figures/Supplementary_Materials/platy_barplot_clade_dist.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Symbiodinium"))),expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()


jpeg(filename = "figures/Supplementary_Materials/fpenta_barplot_clade_outcome.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()

jpeg(filename = "figures/Supplementary_Materials/fpenta_barplot_clade_dist.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()


jpeg(filename = "figures/Supplementary_Materials/dipall_barplot_clade_outcome.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.dipall.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()

jpeg(filename = "figures/Supplementary_Materials/dipall_barplot_clade_dist.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.dipall.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()


jpeg(filename = "figures/Supplementary_Materials/hydno_barplot_clade_outcome.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="clade")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Symbiodinium"))),expression(paste(italic("Breviolum"))),expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()

jpeg(filename = "figures/Supplementary_Materials/hydno_barplot_clade_dist.jpg",width = 10, height= 6, units = "in", res=300)
plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="clade")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values=clade_cols,name="Genus",labels=c(expression(paste(italic("Symbiodinium"))),expression(paste(italic("Breviolum"))),expression(paste(italic("Cladocopium"))),expression(paste(italic("Durusdinium")))))
dev.off()

#################

# Making color ramp to include all types for consistency
phy97.f.c.coral.AD.before.p.no2015c <- subset_samples(phy97.f.c.coral.AD.before.p,field_season!="KI2015c")
phy97.f.c.coral.AD.before.p.no2015c <- subset_samples(phy97.f.c.coral.AD.before.p.no2015c,field_season!="KI2016a")

colourCount2 = length(unique(data.frame(tax_table(phy97.f.c.coral.AD.before.p.no2015c))$majority_its2_sequence))
its2maj_names <- unique(data.frame(tax_table(phy97.f.c.coral.AD.before.p.no2015c))$majority_its2_sequence)
getPalette2 = colorRampPalette(brewer.pal(9, "Set1"))
its2maj_cols <- data.frame(color = getPalette2(colourCount2))
its2maj_cols$majority_its2_sequence <- its2maj_names

its2maj_cols2 <- setNames(as.character(its2maj_cols$color), its2maj_cols$majority_its2_sequence)

####
jpeg(filename = "figures/Supplementary_Materials/hydno_barplot_its2type_outcome.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()

jpeg(filename = "figures/Supplementary_Materials/hydno_barplot_its2type_dist.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.hydno.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()

jpeg(filename = "figures/Supplementary_Materials/platy_barplot_its2type_outcome.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()

jpeg(filename = "figures/Supplementary_Materials/platy_barplot_its2type_dist.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.platy.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()


jpeg(filename = "figures/Supplementary_Materials/dipall_barplot_its2type_outcome.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.dipall.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()

jpeg(filename = "figures/Supplementary_Materials/dipall_barplot_its2type_dist.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.dipall.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()


jpeg(filename = "figures/Supplementary_Materials/fpenta_barplot_its2type_outcome.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("outcome",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()

jpeg(filename = "figures/Supplementary_Materials/fpenta_barplot_its2type_dist.jpg",width = 10, height= 6, units = "in", res=300)
p2 <- plot_bar(phy97.f.c.fpenta.AD.before.p.no2015c,fill="majority_its2_sequence")+facet_grid("disturbance_level",scales="free",space="free")+scale_fill_manual(values = its2maj_cols2)
p2
dev.off()
