library(phyloseq)
library(reshape)

rm(list=ls())

load("data/KI_seqs_f_coral_grouped_all.RData")

phy97.f.c.coral.AD

head(sample_data(phy97.f.c.coral.AD))

#########################
otu_table(phy97.f.c.coral.AD)
tax_table(phy97.f.c.coral.AD)

# Extract each component of the phyloseq object for tree building
tax <- data.frame(tax_table(phy97.f.c.coral.AD))
otu <- data.frame(otu_table(phy97.f.c.coral.AD))
sam <- data.frame(sample_data(phy97.f.c.coral.AD))

# Determine whether each sample has any clade A (now Symbiodinium sensu stricto)
A_tax <- tax$otu[tax$clade=="A"] # Find otus that are in clade A
A_Y <- otu[(rownames(otu) %in% A_tax),] # Make a list of otus that are clade A
A_Y_sums <- colSums(A_Y) # Sum instances of clade A being present (a coral may have more than one OTU that is clade A)

# Determine whether each sample has any clade B (now Breviolum)
B_tax <- tax$otu[tax$clade=="B"]
B_Y <- otu[(rownames(otu) %in% B_tax),]
B_Y_sums <- colSums(B_Y)

# Determine whether each sample has any clade C (now Cladocopium)
C_tax <- tax$otu[tax$clade=="C"]
C_Y <- otu[(rownames(otu) %in% C_tax),]
C_Y_sums <- colSums(C_Y)

# Determine whether each sample has any clade D (now Durusdinium)
D_tax <- tax$otu[tax$clade=="D"]
D_Y <- otu[(rownames(otu) %in% D_tax),]
D_Y_sums <- colSums(D_Y)

# Determine whether each sample has any clade F (now Fugacium)
F_tax <- tax$otu[tax$clade=="F"]
F_Y <- otu[(rownames(otu) %in% F_tax),]
F_Y_sums <- colSums(F_Y)

# Determine whether each sample has any clade G (now Gerakladium)
G_tax <- tax$otu[tax$clade=="G"]
G_Y <- otu[(rownames(otu) %in% G_tax),]
G_Y_sums <- colSums(G_Y)

clade_sums <- data.frame(A_Y_sums, B_Y_sums,C_Y_sums,D_Y_sums,F_Y_sums,G_Y_sums)

clade_sums$dom <- colnames(clade_sums)[max.col(clade_sums,ties.method="first")]
clade_sums$dom <- gsub("_Y_sums","",clade_sums$dom)


#########################
samdat <- data.frame(sample_data(phy97.f.c.coral.AD))
head(samdat)

colnames(samdat)

samdat <- (samdat[,c(1:7,25)])
samdat$sample_name <- rownames(samdat)

rownames(clade_sums)==samdat$sample_name

samdat$dom <- clade_sums$dom

samdat <- samdat[,c(1:6,8:10)]
rownames(samdat) <- NULL
r_samdat <- reshape(samdat,
        idvar = c("coral_tag","SampleType","Coral_Species","site","Dist","Status"),
        timevar = c("field_season"),
        direction = "wide")

head(r_samdat)
r_samdat2 <- r_samdat[order(as.numeric(as.character(r_samdat$coral_tag))),]
head(r_samdat2)

r_samdat2$coral_tag
r_samdat$coral_tag

colnames(r_samdat2)
r_samdat3 <- r_samdat2[c("coral_tag","SampleType","Coral_Species",
                         "site","Dist","Status",
                         "sample_name.KI2014","sample_name.KI2015a",
                         "sample_name.KI2015b","sample_name.KI2015c",
                         "sample_name.KI2016a",
                         "dom.KI2014","dom.KI2015a",
                         "dom.KI2015b","dom.KI2015c",
                         "dom.KI2016a")]

# r_samdat3$have_before <- NA
head(r_samdat3)
r_samdat3$have_before <- ifelse(r_samdat3$sample_name.KI2014!="<NA>" | r_samdat3$sample_name.KI2015a!="<NA>" | r_samdat3$sample_name.KI2015b!="<NA>","Yes",NA)
r_samdat3$have_duringafter <- ifelse(r_samdat3$sample_name.KI2015c!="<NA>" | r_samdat3$sample_name.KI2016a!="<NA>","Yes",NA)

head(r_samdat3)

cols <- c("dom.KI2014","dom.KI2015a","dom.KI2015b")
r_samdat3$dom_before <- do.call(paste, c(r_samdat3[cols],sep=""))
r_samdat3$dom_before <- gsub("NA","",r_samdat3$dom_before)
r_samdat3$dom_before <- gsub("CCC","C",r_samdat3$dom_before)
r_samdat3$dom_before <- gsub("CC","C",r_samdat3$dom_before)
r_samdat3$dom_before <- gsub("DDD","D",r_samdat3$dom_before)
r_samdat3$dom_before <- gsub("DD","D",r_samdat3$dom_before)
r_samdat3$dom_before <- gsub("^$","NA",r_samdat3$dom_before)
r_samdat3$dom_before <- as.factor(r_samdat3$dom_before)

cols <- c("dom.KI2015c")
r_samdat3$dom_during <- do.call(paste, c(r_samdat3[cols],sep=""))
r_samdat3$dom_during <- gsub("NA","",r_samdat3$dom_during)
r_samdat3$dom_during <- gsub("CCC","C",r_samdat3$dom_during)
r_samdat3$dom_during <- gsub("CC","C",r_samdat3$dom_during)
r_samdat3$dom_during <- gsub("DDD","D",r_samdat3$dom_during)
r_samdat3$dom_during <- gsub("DD","D",r_samdat3$dom_during)
r_samdat3$dom_during <- gsub("^$","NA",r_samdat3$dom_during)
r_samdat3$dom_during <- as.factor(r_samdat3$dom_during)

cols <- c("dom.KI2016a")
r_samdat3$dom_after <- do.call(paste, c(r_samdat3[cols],sep=""))
r_samdat3$dom_after <- gsub("NA","",r_samdat3$dom_after)
r_samdat3$dom_after <- gsub("CCC","C",r_samdat3$dom_after)
r_samdat3$dom_after <- gsub("CC","C",r_samdat3$dom_after)
r_samdat3$dom_after <- gsub("DDD","D",r_samdat3$dom_after)
r_samdat3$dom_after <- gsub("DD","D",r_samdat3$dom_after)
r_samdat3$dom_after <- gsub("^$","NA",r_samdat3$dom_after)
r_samdat3$dom_after <- as.factor(r_samdat3$dom_after)

r_samdat4 <- r_samdat3
rownames(r_samdat4) <- r_samdat4$coral_tag
head(r_samdat4)
write.csv(r_samdat3, file = "figures/summarize_samples_by_tag_Table.csv",
          row.names = FALSE)
