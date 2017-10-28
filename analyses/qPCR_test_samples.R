# Plot barplots of just the samples Ross included in qPCR test http://jrcunning.github.io/labnotebook/2017/10/26/testing-new-extractions-of-ki-platy-sds-archives.html

rm(list=ls())

load("data/KI_seqs_f_coral_grouped.RData")
names <- read.csv('data/Platygyra_SDS_Guan_Names.csv')

qpcr_test_names <- names[,2] # These are the FSYM names that match up with FQ samples Ross is testing. NOTE: 5 samples in 2014 have different number codes between FQ and FSYM (see corresponding list below, as well as data/Platygyra_SDS_Guan_Names.csv). All of the rest of the numbers match up between the SDS and Guanidinium archives.

qpcr_test <- subset_samples(phy97.f.c.platy.p,rownames(sample_data(phy97.f.c.platy.p)) %in% qpcr_test_names)

# "KI16aFSYM260" "KI16aFSYM264" are missing because they had <200 sequences in the MiSeq data and were removed during filtering.

p1 <- plot_bar(qpcr_test,fill="hit",title = "qPCR Test Samples") 
p2 <- plot_bar(qpcr_test,fill="clade",title = "qPCR Test Samples") 


jpeg(file="analyses/qPCR_test_samples.jpg",height=5, width=14,units = "in", res=300)
# Plot Platygyra and Favites pentagona with a shared legend
p2
dev.off()

# KI14FQ019
# KI14FQ037 KI14FSYM062
# KI14FQ072 KI14FSYM099
# KI14FQ098 KI14FSYM148
# KI14FQ104 KI14FSYM157
# KI14FQ107 KI14FSYM161
# KI15cFQ253
# KI15cFQ254
# KI15cFQ261
# KI15cFQ273
# KI15cFQ385
# KI15cFQ404
# KI15cFQ415
# KI15cFQ428
# KI15cFQ436
# KI15cFQ460
# KI16aFQ260
# KI16aFQ261
# KI16aFQ264
# KI16aFQ265
# KI16aFQ272