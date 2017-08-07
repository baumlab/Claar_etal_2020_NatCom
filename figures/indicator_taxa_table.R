# Reset graphical parameters
dev.off()

# Clear your environment
rm(list=ls())

# Load necessary packages
library(imager)

# Load in data
load("data/KI_seqs_f_coral_grouped.RData")

########## ALL CORAL ############
abund_all <- as.data.frame(t(data.frame(otu_table(phy97.f.c.coral.AD))))
DA_all <- as.numeric(data.frame(sample_data(phy97.f.c.coral.AD))$Status)
# Group 1 is alive

indval_all = multipatt(abund_all, DA_all, control = how(nperm=999))
summary(indval_all)
indic_all.1 <- indval_all$sign[which(indval_all$sign$s.1>0),]
indic_all.2 <- indval_all$sign[which(indval_all$sign$s.2>0),]
indic_all.1 <- row.names(indval_all$sign[which(indval_all$sign$p.value<0.05 & indval_all$sign$s.1>0),])
indic_all.2 <- row.names(indval_all$sign[which(indval_all$sign$p.value<0.05 & indval_all$sign$s.2>0),])

indic_all.1_ID <- data.frame(tax_table(phy97.f.c.coral.AD))[which(data.frame(tax_table(phy97.f.c.coral.AD))$otu %in% indic_all.1),]
indic_all.1_ID <- indic_all.1_ID[,8:9]

indic_all.2_ID <- data.frame(tax_table(phy97.f.c.coral.AD))[which(data.frame(tax_table(phy97.f.c.coral.AD))$otu %in% indic_all.2),]
indic_all.2_ID <- indic_all.2_ID[,8:9]

############# ALL PLATY ###############
abund_platy <- as.data.frame(t(data.frame(otu_table(phy97.f.c.platy.AD))))
DA_platy <- as.numeric(data.frame(sample_data(phy97.f.c.platy.AD))$Status)
# Group 1 is alive

indval_platy = multipatt(abund_platy, DA_platy, control = how(nperm=999))
summary(indval_platy)
indic_platy.1 <- indval_platy$sign[which(indval_platy$sign$s.1>0),]
indic_platy.2 <- indval_platy$sign[which(indval_platy$sign$s.2>0),]
indic_platy.1 <- row.names(indval_platy$sign[which(indval_platy$sign$p.value<0.05 & indval_platy$sign$s.1>0),])
indic_platy.2 <- row.names(indval_platy$sign[which(indval_platy$sign$p.value<0.05 & indval_platy$sign$s.2>0),])

indic_platy.1_ID <- data.frame(tax_table(phy97.f.c.platy.AD))[which(data.frame(tax_table(phy97.f.c.platy.AD))$otu %in% indic_platy.1),]
indic_platy.1_ID <- indic_platy.1_ID[,8:9]

indic_platy.2_ID <- data.frame(tax_table(phy97.f.c.platy.AD))[which(data.frame(tax_table(phy97.f.c.platy.AD))$otu %in% indic_platy.2),]
indic_platy.2_ID <- indic_platy.2_ID[,8:9]

############ ALL FPENTA #################
abund_fpenta <- as.data.frame(t(data.frame(otu_table(phy97.f.c.fpenta.AD))))
DA_fpenta <- as.numeric(data.frame(sample_data(phy97.f.c.fpenta.AD))$Status)
# Group 1 is alive

indval_fpenta = multipatt(abund_fpenta, DA_fpenta, control = how(nperm=999))
summary(indval_fpenta)

indic_fpenta.1 <- indval_fpenta$sign[which(indval_fpenta$sign$s.1>0),]
indic_fpenta.2 <- indval_fpenta$sign[which(indval_fpenta$sign$s.2>0),]
indic_fpenta.1 <- row.names(indval_fpenta$sign[which(indval_fpenta$sign$p.value<0.05 & indval_fpenta$sign$s.1>0),])
indic_fpenta.2 <- row.names(indval_fpenta$sign[which(indval_fpenta$sign$p.value<0.05 & indval_fpenta$sign$s.2>0),])

indic_fpenta.1_ID <- data.frame(tax_table(phy97.f.c.fpenta.AD))[which(data.frame(tax_table(phy97.f.c.fpenta.AD))$otu %in% indic_fpenta.1),]
indic_fpenta.1_ID <- indic_fpenta.1_ID[,8:9]

indic_fpenta.2_ID <- data.frame(tax_table(phy97.f.c.fpenta.AD))[which(data.frame(tax_table(phy97.f.c.fpenta.AD))$otu %in% indic_fpenta.2),]
indic_fpenta.2_ID <- indic_fpenta.2_ID[,8:9]

############## FPENTA BEFORE ######################
abund_fpenta.before <- as.data.frame(t(data.frame(otu_table(phy97.f.c.fpenta.AD.before))))
DA_fpenta.before <- as.numeric(data.frame(sample_data(phy97.f.c.fpenta.AD.before))$Status)
# Group 1 is alive

indval_fpenta.before = multipatt(abund_fpenta.before, DA_fpenta.before, control = how(nperm=999))
summary(indval_fpenta.before)

indic_fpenta.before.1 <- indval_fpenta.before$sign[which(indval_fpenta.before$sign$s.1>0),]
indic_fpenta.before.2 <- indval_fpenta.before$sign[which(indval_fpenta.before$sign$s.2>0),]
indic_fpenta.before.1 <- row.names(indval_fpenta.before$sign[which(indval_fpenta.before$sign$p.value<0.05 & indval_fpenta.before$sign$s.1>0),])
indic_fpenta.before.2 <- row.names(indval_fpenta.before$sign[which(indval_fpenta.before$sign$p.value<0.05 & indval_fpenta.before$sign$s.2>0),])

indic_fpenta.before.1_ID <- data.frame(tax_table(phy97.f.c.fpenta.AD.before))[which(data.frame(tax_table(phy97.f.c.fpenta.AD.before))$otu %in% indic_fpenta.before.1),]
indic_fpenta.before.1_ID <- indic_fpenta.before.1_ID[,8:9]

indic_fpenta.before.2_ID <- data.frame(tax_table(phy97.f.c.fpenta.AD.before))[which(data.frame(tax_table(phy97.f.c.fpenta.AD.before))$otu %in% indic_fpenta.before.2),]
indic_fpenta.before.2_ID <- indic_fpenta.before.2_ID[,8:9]

############## PLATY BEFORE ######################
abund_platy.before <- as.data.frame(t(data.frame(otu_table(phy97.f.c.platy.AD.before))))
DA_platy.before <- as.numeric(data.frame(sample_data(phy97.f.c.platy.AD.before))$Status)
# Group 1 is alive

indval_platy.before = multipatt(abund_platy.before, DA_platy.before, control = how(nperm=999))
summary(indval_platy.before)

indic_platy.before.1 <- indval_platy.before$sign[which(indval_platy.before$sign$s.1>0),]
indic_platy.before.2 <- indval_platy.before$sign[which(indval_platy.before$sign$s.2>0),]
indic_platy.before.1 <- row.names(indval_platy.before$sign[which(indval_platy.before$sign$p.value<0.05 & indval_platy.before$sign$s.1>0),])
indic_platy.before.2 <- row.names(indval_platy.before$sign[which(indval_platy.before$sign$p.value<0.05 & indval_platy.before$sign$s.2>0),])

indic_platy.before.1_ID <- data.frame(tax_table(phy97.f.c.platy.AD.before))[which(data.frame(tax_table(phy97.f.c.platy.AD.before))$otu %in% indic_platy.before.1),]
indic_platy.before.1_ID <- indic_platy.before.1_ID[,8:9]

indic_platy.before.2_ID <- data.frame(tax_table(phy97.f.c.platy.AD.before))[which(data.frame(tax_table(phy97.f.c.platy.AD.before))$otu %in% indic_platy.before.2),]
indic_platy.before.2_ID <- indic_platy.before.2_ID[,8:9]

############## FPENTA DURING ######################
abund_fpenta.during <- as.data.frame(t(data.frame(otu_table(phy97.f.c.fpenta.AD.during))))
DA_fpenta.during <- as.numeric(data.frame(sample_data(phy97.f.c.fpenta.AD.during))$Status)
# Group 1 is alive

indval_fpenta.during = multipatt(abund_fpenta.during, DA_fpenta.during, control = how(nperm=999))
summary(indval_fpenta.during)

indic_fpenta.during.1 <- indval_fpenta.during$sign[which(indval_fpenta.during$sign$s.1>0),]
indic_fpenta.during.2 <- indval_fpenta.during$sign[which(indval_fpenta.during$sign$s.2>0),]
indic_fpenta.during.1 <- row.names(indval_fpenta.during$sign[which(indval_fpenta.during$sign$p.value<0.05 & indval_fpenta.during$sign$s.1>0),])
indic_fpenta.during.2 <- row.names(indval_fpenta.during$sign[which(indval_fpenta.during$sign$p.value<0.05 & indval_fpenta.during$sign$s.2>0),])

indic_fpenta.during.1_ID <- data.frame(tax_table(phy97.f.c.fpenta.AD.after))[which(data.frame(tax_table(phy97.f.c.fpenta.AD.after))$otu %in% indic_fpenta.during.1),]
indic_fpenta.during.1_ID <- indic_fpenta.during.1_ID[,8:9]

indic_fpenta.during.2_ID <- data.frame(tax_table(phy97.f.c.fpenta.AD.after))[which(data.frame(tax_table(phy97.f.c.fpenta.AD.after))$otu %in% indic_fpenta.during.2),]
indic_fpenta.during.2_ID <- indic_fpenta.during.2_ID[,8:9]

############## PLATY DURING ######################
abund_platy.during <- as.data.frame(t(data.frame(otu_table(phy97.f.c.platy.AD.during))))
DA_platy.during <- as.numeric(data.frame(sample_data(phy97.f.c.platy.AD.during))$Status)
# Group 1 is alive

indval_platy.during = multipatt(abund_platy.during, DA_platy.during, control = how(nperm=999))
summary(indval_platy.during)

indic_platy.during.1 <- indval_platy.during$sign[which(indval_platy.during$sign$s.1>0),]
indic_platy.during.2 <- indval_platy.during$sign[which(indval_platy.during$sign$s.2>0),]
indic_platy.during.1 <- row.names(indval_platy.during$sign[which(indval_platy.during$sign$p.value<0.05 & indval_platy.during$sign$s.1>0),])
indic_platy.during.2 <- row.names(indval_platy.during$sign[which(indval_platy.during$sign$p.value<0.05 & indval_platy.during$sign$s.2>0),])

indic_platy.during.1_ID <- data.frame(tax_table(phy97.f.c.platy.AD.after))[which(data.frame(tax_table(phy97.f.c.platy.AD.after))$otu %in% indic_platy.during.1),]
indic_platy.during.1_ID <- indic_platy.during.1_ID[,8:9]

indic_platy.during.2_ID <- data.frame(tax_table(phy97.f.c.platy.AD.after))[which(data.frame(tax_table(phy97.f.c.platy.AD.after))$otu %in% indic_platy.during.2),]
indic_platy.during.2_ID <- indic_platy.during.2_ID[,8:9]


## Print out results
indic_fpenta.before.1_ID
indic_fpenta.before.2_ID

indic_fpenta.during.1_ID
indic_fpenta.during.2_ID

indic_fpenta.1_ID
indic_fpenta.2_ID

indic_platy.before.1_ID
indic_platy.before.2_ID

indic_platy.during.1_ID
indic_platy.during.2_ID

indic_platy.1_ID
indic_platy.2_ID
