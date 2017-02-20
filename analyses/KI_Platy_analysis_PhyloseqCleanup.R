# Platy analysis - Phyloseq Cleanup

# Import Libraries
library(stringr)
library(reshape2)
library(phyloseq)

# Clear workspace
rm(list=ls())

# Load filtered RData object from output of filter_notsym.R script
# load("C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f.RData")
load("KI_seqs_f.RData")
phy97.f <- phy.f # Rename phyloseq object for clarity

# Characterize sites by disturbance level
VeryHigh <- c(33,40,32,31,27,30,26)
High <- c(25,3,38,24)
HighMed <- c(9,34,35,8,14,6)
LowMed <- c(2,22,1,23)
Low <- c(7,13,12,4,36,5,37)
VeryLow <- c(10,21,11,20,16,15,39,19,18,17)

sample_data(phy97.f)$Site <- factor(sample_data(phy97.f)$Site)
sample_data(phy97.f)$Dist <- sample_data(phy97.f)$Site
sample_data(phy97.f)$Dist<- gsub ("34",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("35",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("14",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("15",as.character("VeryLow"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("19",as.character("VeryLow"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("12",as.character("Low"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("37",as.character("Low"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("27",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("30",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("32",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("26",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("31",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("40",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("33",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("38",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("25",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("24",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("3",as.character("VeryHigh"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("High"))
sample_data(phy97.f)$Dist<- gsub ("5",as.character("Low"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("6",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("9",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
sample_data(phy97.f)$Dist<- gsub ("8",as.character("HighMed"), as.character(data.frame(sample_data(phy97.f))$Dist), as.character("HighMed"))
rm(VeryHigh,High,HighMed,LowMed,Low,VeryLow)

# Create a loop to merge OTUs with identical hits
# Note that this is computationally slow and probably could be made much more efficient.
phy97.f.c <- phy97.f

for (i in data.frame(tax_table(phy97.f.c))$hit){
  i <- grep(i, data.frame(tax_table(phy97.f.c))$hit)
  phy97.f.c <- merge_taxa(phy97.f.c, eqtaxa=i, archetype=1)
  tt <- data.frame(tax_table(phy97.f.c), stringsAsFactors = F)
  isna <- rownames(tt)[is.na(tt$hit)]
  info <- data.frame(tax_table(phy97.f), stringsAsFactors = F)[isna, "hit"]
  tt[isna,"hit"] <- info
  tax_table(phy97.f.c) <- as.matrix(tt)
  rm(i)
}

rm(info,isna,tt)

# Manually checking whether all equivalent taxa are collapsed
# t <- sort(data.frame(tax_table(phy97.f.c))$hit)
# t
# 
# # Check if all equivalent taxa are collapsed
# x <- length(unique(data.frame(tax_table(phy97.f.c))$hit))
# y <- length(data.frame(tax_table(phy97.f.c))$hit)
# identical(x,y)

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

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.p <- transform_sample_counts(phy97.f.c, function(x) x/sum(x))

# Subset coral data set into individual genus data sets (and water and sediment)
phy97.f.c.platy <- subset_samples(phy97.f.c,Coral_Species=="Platygyra_sp")
phy97.f.c.platy <- subset_taxa(phy97.f.c.platy, taxa_sums(phy97.f.c.platy) > 0, prune=TRUE)

phy97.f.c.fpenta <- subset_samples(phy97.f.c,Coral_Species=="Favites_pentagona")
phy97.f.c.fpenta <- subset_taxa(phy97.f.c.fpenta, taxa_sums(phy97.f.c.fpenta) > 0, prune=TRUE)

phy97.f.c.faviasp <- subset_samples(phy97.f.c,Coral_Species=="Favia_sp")
phy97.f.c.faviam <- subset_samples(phy97.f.c,Coral_Species=="Favia_matthai")
phy97.f.c.faviaall <- merge_phyloseq(phy97.f.c.faviasp,phy97.f.c.faviam)
phy97.f.c.faviaall <- subset_taxa(phy97.f.c.faviaall, taxa_sums(phy97.f.c.faviaall) > 0, prune=TRUE)

phy97.f.c.coral <- subset_samples(phy97.f.c,SampleType=="coral")
phy97.f.c.coral <- subset_taxa(phy97.f.c.coral, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)

phy97.f.c.water <- subset_samples(phy97.f.c,SampleType=="water")
phy97.f.c.water <- subset_taxa(phy97.f.c.water, taxa_sums(phy97.f.c.water) > 0, prune=TRUE)

phy97.f.c.sediment <- subset_samples(phy97.f.c,SampleType=="sediment")
phy97.f.c.sediment <- subset_taxa(phy97.f.c.sediment, taxa_sums(phy97.f.c.sediment) > 0, prune=TRUE)

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.platy.p <- transform_sample_counts(phy97.f.c.platy, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.fpenta.p <- transform_sample_counts(phy97.f.c.fpenta, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.faviasp.p <- transform_sample_counts(phy97.f.c.faviasp, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.faviam.p <- transform_sample_counts(phy97.f.c.faviam, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.faviaall.p <- transform_sample_counts(phy97.f.c.faviaall, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.water.p <- transform_sample_counts(phy97.f.c.water, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.sediment.p <- transform_sample_counts(phy97.f.c.sediment, function(x) x/sum(x))
# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.coral.p <- transform_sample_counts(phy97.f.c.coral, function(x) x/sum(x))

# Log transform coral
phy97.f.c.coral.log <- transform_sample_counts(phy97.f.c.coral, function(x) (log10(x+1)))

# Subset coral by site
for (i in unique(data.frame(sample_data(phy97.f.c.coral))$Site)){
  nam <- paste("phy97.f.c.coral.site",i,sep="")
  a <- eval(subset_samples(phy97.f.c.coral,Site==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}

# Subset water by site
for (i in unique(data.frame(sample_data(phy97.f.c.water))$Site)){
  nam <- paste("phy97.f.c.water.site",i,sep="")
  a <- eval(subset_samples(phy97.f.c.water,Site==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}

# Subset sediment by site
for (i in unique(data.frame(sample_data(phy97.f.c.sediment))$Site)){
  nam <- paste("phy97.f.c.sediment.site",i,sep="")
  a <- eval(subset_samples(phy97.f.c.sediment,Site==i, prune=TRUE))
  assign(nam,a)
  b <- eval(subset_taxa(a, taxa_sums(a) > 0, prune=TRUE))
  assign(nam,b)
}


# Transform sample counts to proportional abundance for downstream analyses
for (i in print(ls(pattern="phy97.f.c.coral.site"))){
  nam <- paste(i,".p",sep="")
  a <- eval(transform_sample_counts(i, function(x) x/sum(x)))
  assign(nam,a)
}

X <- get(ls(pattern="phy97.f.c.coral.site"))

lapply(X, transform_sample_counts(X, function(x) x/sum(x)))

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.coral.site3.p <- transform_sample_counts(phy97.f.c.coral.site3, function(x) x/sum(x))
phy97.f.c.coral.site5.p <- transform_sample_counts(phy97.f.c.coral.site5, function(x) x/sum(x))
phy97.f.c.coral.site8.p <- transform_sample_counts(phy97.f.c.coral.site8, function(x) x/sum(x))
phy97.f.c.coral.site14.p <- transform_sample_counts(phy97.f.c.coral.site14, function(x) x/sum(x))
phy97.f.c.coral.site15.p <- transform_sample_counts(phy97.f.c.coral.site15, function(x) x/sum(x))
phy97.f.c.coral.site19.p <- transform_sample_counts(phy97.f.c.coral.site19, function(x) x/sum(x))
phy97.f.c.coral.site25.p <- transform_sample_counts(phy97.f.c.coral.site25, function(x) x/sum(x))
phy97.f.c.coral.site27.p <- transform_sample_counts(phy97.f.c.coral.site27, function(x) x/sum(x))
phy97.f.c.coral.site30.p <- transform_sample_counts(phy97.f.c.coral.site30, function(x) x/sum(x))
phy97.f.c.coral.site32.p <- transform_sample_counts(phy97.f.c.coral.site32, function(x) x/sum(x))
phy97.f.c.coral.site34.p <- transform_sample_counts(phy97.f.c.coral.site34, function(x) x/sum(x))
phy97.f.c.coral.site35.p <- transform_sample_counts(phy97.f.c.coral.site35, function(x) x/sum(x))
phy97.f.c.coral.site37.p <- transform_sample_counts(phy97.f.c.coral.site37, function(x) x/sum(x))
phy97.f.c.coral.site38.p <- transform_sample_counts(phy97.f.c.coral.site38, function(x) x/sum(x))
phy97.f.c.coral.site40.p <- transform_sample_counts(phy97.f.c.coral.site40, function(x) x/sum(x))

# phy97.f.c.water.site3.p <- transform_sample_counts(phy97.f.c.water.site3, function(x) x/sum(x))
phy97.f.c.water.site5.p <- transform_sample_counts(phy97.f.c.water.site5, function(x) x/sum(x))
phy97.f.c.water.site8.p <- transform_sample_counts(phy97.f.c.water.site8, function(x) x/sum(x))
# phy97.f.c.water.site14.p <- transform_sample_counts(phy97.f.c.water.site14, function(x) x/sum(x))
# phy97.f.c.water.site15.p <- transform_sample_counts(phy97.f.c.water.site15, function(x) x/sum(x))
# phy97.f.c.water.site19.p <- transform_sample_counts(phy97.f.c.water.site19, function(x) x/sum(x))
# phy97.f.c.water.site25.p <- transform_sample_counts(phy97.f.c.water.site25, function(x) x/sum(x))
phy97.f.c.water.site27.p <- transform_sample_counts(phy97.f.c.water.site27, function(x) x/sum(x))
phy97.f.c.water.site30.p <- transform_sample_counts(phy97.f.c.water.site30, function(x) x/sum(x))
# phy97.f.c.water.site32.p <- transform_sample_counts(phy97.f.c.water.site32, function(x) x/sum(x))
phy97.f.c.water.site34.p <- transform_sample_counts(phy97.f.c.water.site34, function(x) x/sum(x))
phy97.f.c.water.site35.p <- transform_sample_counts(phy97.f.c.water.site35, function(x) x/sum(x))
# phy97.f.c.water.site37.p <- transform_sample_counts(phy97.f.c.water.site37, function(x) x/sum(x))
# phy97.f.c.water.site38.p <- transform_sample_counts(phy97.f.c.water.site38, function(x) x/sum(x))
# phy97.f.c.water.site40.p <- transform_sample_counts(phy97.f.c.water.site40, function(x) x/sum(x))

# phy97.f.c.sediment.site3.p <- transform_sample_counts(phy97.f.c.sediment.site3, function(x) x/sum(x))
# phy97.f.c.sediment.site5.p <- transform_sample_counts(phy97.f.c.sediment.site5, function(x) x/sum(x))
# phy97.f.c.sediment.site8.p <- transform_sample_counts(phy97.f.c.sediment.site8, function(x) x/sum(x))
# phy97.f.c.sediment.site14.p <- transform_sample_counts(phy97.f.c.sediment.site14, function(x) x/sum(x))
# phy97.f.c.sediment.site15.p <- transform_sample_counts(phy97.f.c.sediment.site15, function(x) x/sum(x))
# phy97.f.c.sediment.site19.p <- transform_sample_counts(phy97.f.c.sediment.site19, function(x) x/sum(x))
# phy97.f.c.sediment.site25.p <- transform_sample_counts(phy97.f.c.sediment.site25, function(x) x/sum(x))
# phy97.f.c.sediment.site27.p <- transform_sample_counts(phy97.f.c.sediment.site27, function(x) x/sum(x))
phy97.f.c.sediment.site30.p <- transform_sample_counts(phy97.f.c.sediment.site30, function(x) x/sum(x))
# phy97.f.c.sediment.site32.p <- transform_sample_counts(phy97.f.c.sediment.site32, function(x) x/sum(x))
phy97.f.c.sediment.site34.p <- transform_sample_counts(phy97.f.c.sediment.site34, function(x) x/sum(x))
phy97.f.c.sediment.site35.p <- transform_sample_counts(phy97.f.c.sediment.site35, function(x) x/sum(x))
# phy97.f.c.sediment.site37.p <- transform_sample_counts(phy97.f.c.sediment.site37, function(x) x/sum(x))
# phy97.f.c.sediment.site38.p <- transform_sample_counts(phy97.f.c.sediment.site38, function(x) x/sum(x))
# phy97.f.c.sediment.site40.p <- transform_sample_counts(phy97.f.c.sediment.site40, function(x) x/sum(x))


# Subset by Field Season
phy97.f.c.2014 <- subset_samples(phy97.f.c,Year=="2014")
phy97.f.c.2014 <- subset_taxa(phy97.f.c.2014, taxa_sums(phy97.f.c) > 0, prune=TRUE)
phy97.f.c.2015Jan <- subset_samples(phy97.f.c,Year=="2015Jan")
phy97.f.c.2015Jan <- subset_taxa(phy97.f.c.2015Jan, taxa_sums(phy97.f.c) > 0, prune=TRUE)
phy97.f.c.2015May <- subset_samples(phy97.f.c,Year=="2015May")
phy97.f.c.2015May <- subset_taxa(phy97.f.c.2015May, taxa_sums(phy97.f.c) > 0, prune=TRUE)
phy97.f.c.2015July <- subset_samples(phy97.f.c,Year=="2015July")
phy97.f.c.2015July <- subset_taxa(phy97.f.c.2015July, taxa_sums(phy97.f.c) > 0, prune=TRUE)
phy97.f.c.2016March <- subset_samples(phy97.f.c,Year=="2016March")
phy97.f.c.2016March <- subset_taxa(phy97.f.c.2016March, taxa_sums(phy97.f.c) > 0, prune=TRUE)

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.c.2014.p <- transform_sample_counts(phy97.f.c.2014, function(x) x/sum(x))
phy97.f.c.2015Jan.p <- transform_sample_counts(phy97.f.c.2015Jan, function(x) x/sum(x))
phy97.f.c.2015May.p <- transform_sample_counts(phy97.f.c.2015May, function(x) x/sum(x))
phy97.f.c.2015July.p <- transform_sample_counts(phy97.f.c.2015July, function(x) x/sum(x))
phy97.f.c.2016March.p <- transform_sample_counts(phy97.f.c.2016March, function(x) x/sum(x))

# Subset by SampleType within Field Season
phy97.f.c.coral.2014 <- subset_samples(phy97.f.c.coral,Year=="2014")
phy97.f.c.coral.2014 <- subset_taxa(phy97.f.c.coral.2014, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2015Jan <- subset_samples(phy97.f.c.coral,Year=="2015Jan")
phy97.f.c.coral.2015Jan <- subset_taxa(phy97.f.c.coral.2015Jan, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2015May <- subset_samples(phy97.f.c.coral,Year=="2015May")
phy97.f.c.coral.2015May <- subset_taxa(phy97.f.c.coral.2015May, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2015July <- subset_samples(phy97.f.c.coral,Year=="2015July")
phy97.f.c.coral.2015July <- subset_taxa(phy97.f.c.coral.2015July, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)
phy97.f.c.coral.2016March <- subset_samples(phy97.f.c.coral,Year=="2016March")
phy97.f.c.coral.2016March <- subset_taxa(phy97.f.c.coral.2016March, taxa_sums(phy97.f.c.coral) > 0, prune=TRUE)

# No need to do this, as water and sediment were only in this sequencing run from July 2015
# Will need to incorporate first sequencing run in order to look at changes in water and sediment

# phy97.f.c.sediment.2015Jan <- subset_samples(phy97.f.c.sediment,Year=="2015Jan")
# phy97.f.c.sediment.2015Jan <- subset_taxa(phy97.f.c.sediment.2015Jan, taxa_sums(phy97.f.c.sediment) > 0, prune=TRUE)
# phy97.f.c.sediment.2015May <- subset_samples(phy97.f.c.sediment,Year=="2015May")
# phy97.f.c.sediment.2015May <- subset_taxa(phy97.f.c.sediment.2015May, taxa_sums(phy97.f.c.sediment) > 0, prune=TRUE)
# phy97.f.c.sediment.2015July <- subset_samples(phy97.f.c.sediment,Year=="2015July")
# phy97.f.c.sediment.2015July <- subset_taxa(phy97.f.c.sediment.2015July, taxa_sums(phy97.f.c.sediment) > 0, prune=TRUE)
# phy97.f.c.sediment.2016March <- subset_samples(phy97.f.c.sediment,Year=="2016March")
# phy97.f.c.sediment.2016March <- subset_taxa(phy97.f.c.sediment.2016March, taxa_sums(phy97.f.c.sediment) > 0, prune=TRUE)
# 
# phy97.f.c.water.2014 <- subset_samples(phy97.f.c.water,Year=="2014")
# phy97.f.c.water.2014 <- subset_taxa(phy97.f.c.water.2014, taxa_sums(phy97.f.c.water) > 0, prune=TRUE)
# phy97.f.c.water.2015Jan <- subset_samples(phy97.f.c.water,Year=="2015Jan")
# phy97.f.c.water.2015Jan <- subset_taxa(phy97.f.c.water.2015Jan, taxa_sums(phy97.f.c.water) > 0, prune=TRUE)
# phy97.f.c.water.2015May <- subset_samples(phy97.f.c.water,Year=="2015May")
# phy97.f.c.water.2015May <- subset_taxa(phy97.f.c.water.2015May, taxa_sums(phy97.f.c.water) > 0, prune=TRUE)
# phy97.f.c.water.2015July <- subset_samples(phy97.f.c.water,Year=="2015July")
# phy97.f.c.water.2015July <- subset_taxa(phy97.f.c.water.2015July, taxa_sums(phy97.f.c.water) > 0, prune=TRUE)
# phy97.f.c.water.2016March <- subset_samples(phy97.f.c.water,Year=="2016March")
# phy97.f.c.water.2016March <- subset_taxa(phy97.f.c.water.2016March, taxa_sums(phy97.f.c.water) > 0, prune=TRUE)

principal <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.p) > 0.01, prune=TRUE)
background <- subset_taxa(phy97.f.c, taxa_sums(phy97.f.c.p) < 0.01, prune=TRUE)

principal.coral <- subset_taxa(phy97.f.c.coral, taxa_sums(phy97.f.c.coral.p) > 0.01, prune=TRUE)
background.coral <- subset_taxa(phy97.f.c.coral, taxa_sums(phy97.f.c.coral.p) < 0.01, prune=TRUE)

principal.sediment <- subset_taxa(phy97.f.c.sediment, taxa_sums(phy97.f.c.sediment.p) > 0.01, prune=TRUE)
background.sediment <- subset_taxa(phy97.f.c.sediment, taxa_sums(phy97.f.c.sediment.p) < 0.01, prune=TRUE)

principal.water <- subset_taxa(phy97.f.c.water, taxa_sums(phy97.f.c.water.p) > 0.01, prune=TRUE)
background.water <- subset_taxa(phy97.f.c.water, taxa_sums(phy97.f.c.water.p) < 0.01, prune=TRUE)


phy97.f.c.otu.logical <- sign(data.frame(otu_table(phy97.f.c)))
phy97.f.c.otu.samples <- rowSums(phy97.f.c.otu.logical)
phy97.f.c.num.samples <- ncol(phy97.f.c.otu.logical)
phy97.f.c.otu.samples.p <- as.matrix(phy97.f.c.otu.samples)/phy97.f.c.num.samples*100

all.core <- phy97.f.c.otu.samples.p[which(phy97.f.c.otu.samples.p>75),]
all.common <- phy97.f.c.otu.samples.p[which(phy97.f.c.otu.samples.p>25 & phy97.f.c.otu.samples.p<75),]
all.rare <- phy97.f.c.otu.samples.p[which(phy97.f.c.otu.samples.p<25),]

phy97.f.c.coral.otu.logical <- sign(data.frame(otu_table(phy97.f.c.coral)))
phy97.f.c.coral.otu.samples <- rowSums(phy97.f.c.coral.otu.logical)
phy97.f.c.coral.num.samples <- ncol(phy97.f.c.coral.otu.logical)
phy97.f.c.coral.otu.samples.p <- as.matrix(phy97.f.c.coral.otu.samples)/phy97.f.c.coral.num.samples*100

coral.core <- phy97.f.c.coral.otu.samples.p[which(phy97.f.c.coral.otu.samples.p>75),]
coral.common <- phy97.f.c.coral.otu.samples.p[which(phy97.f.c.coral.otu.samples.p>25 & phy97.f.c.coral.otu.samples.p<75),]
coral.rare <- phy97.f.c.coral.otu.samples.p[which(phy97.f.c.coral.otu.samples.p<25),]

phy97.f.c.sediment.otu.logical <- sign(data.frame(otu_table(phy97.f.c.sediment)))
phy97.f.c.sediment.otu.samples <- rowSums(phy97.f.c.sediment.otu.logical)
phy97.f.c.sediment.num.samples <- ncol(phy97.f.c.sediment.otu.logical)
phy97.f.c.sediment.otu.samples.p <- as.matrix(phy97.f.c.sediment.otu.samples)/phy97.f.c.sediment.num.samples*100

sediment.core <- phy97.f.c.sediment.otu.samples.p[which(phy97.f.c.sediment.otu.samples.p>75),]
sediment.common <- phy97.f.c.sediment.otu.samples.p[which(phy97.f.c.sediment.otu.samples.p>25 & phy97.f.c.sediment.otu.samples.p<75),]
sediment.rare <- phy97.f.c.sediment.otu.samples.p[which(phy97.f.c.sediment.otu.samples.p<25),]

phy97.f.c.water.otu.logical <- sign(data.frame(otu_table(phy97.f.c.water)))
phy97.f.c.water.otu.samples <- rowSums(phy97.f.c.water.otu.logical)
phy97.f.c.water.num.samples <- ncol(phy97.f.c.water.otu.logical)
phy97.f.c.water.otu.samples.p <- as.matrix(phy97.f.c.water.otu.samples)/phy97.f.c.water.num.samples*100

water.core <- phy97.f.c.water.otu.samples.p[which(phy97.f.c.water.otu.samples.p>75),]
water.common <- phy97.f.c.water.otu.samples.p[which(phy97.f.c.water.otu.samples.p>25 & phy97.f.c.water.otu.samples.p<75),]
water.rare <- phy97.f.c.water.otu.samples.p[which(phy97.f.c.water.otu.samples.p<25),]

# Save grouped data as RData file
save.image(file = "C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f_grouped.RData")
