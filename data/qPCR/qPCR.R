# Source necessary packages
library(devtools)
library(reshape2)
library(stringr)

# Clear working environment
rm(list=ls())

# Source Ross Cunning's steponeR script to get data from qPCR software into R
devtools::source_url("https://raw.githubusercontent.com/jrcunning/steponeR/master/steponeR.R")

# Import SampleID_key to match FSYM, FQ, FMD ... etc.
SampleID_key <- read.csv("data/Coralphoto__Metadata/SampleID_key_long.csv",check.names = FALSE)

# List files of qPCR output
files = list.files(path="data/qPCR/",pattern="*.txt",full.names = TRUE)

# Run steponeR function
df <- steponeR(files=files, delim="\t", target.ratios = c("C.PaxC","D.PaxC"), 
               fluor.norm=list(C=8.6704,D=6.2979,PaxC=0), # These numbers are calc from standard curves
               copy.number=list(C=10,D=2,PaxC=1), # Copy # for C & D will be determined by counting cells and qPCR, PaxC is expected to be a single copy marker
               ploidy=list(C=1,D=1,PaxC=2), # Ploidy level of Symbiodinium in symbiosis is 1, and coral ploidy level is 2
               extract=list(C=0.813, D=0.813, PaxC=0.982)) # Extraction efficiencies, calculated by Ross previously

# Rename for downstream
platy <- df$result # This is the data we want

## Remove positive and negative controls from qPCR data
platy <- platy[grep("+", platy$Sample.Name, fixed=T, invert = T), ] # Remove positive controls
platy <- platy[grep("NTC", platy$Sample.Name, fixed = T, invert = T), ] # Rem. technical controls
platy <- platy[grep("NEC", platy$Sample.Name, fixed = T, invert = T), ] # Rem. negative controls

# Remove failing samples
platy$fail <- ifelse(platy$PaxC.reps < 2 | platy$PaxC.CT.mean < 16 | platy$PaxC.CT.mean > 21 | platy$PaxC.CT.sd > 3, TRUE, FALSE) # Fail if there are less than 2/2 successful PaxC replicates, or if PaxC CT is <16 or >25 
fails <- platy[platy$fail==TRUE, ]
platy <- platy[which(platy$fail==FALSE),] # Remove all fails

hist(platy$PaxC.CT.mean,breaks = 10) # Look at the CT distribution of the samples

# Choose which samples to try re-running
redo <- platy[which(platy$C.reps==1 | platy$D.reps==1),] # If C or D amplified in one well, but not the other (i.e. 1/2 technical replicates amplified for either)
platy[which(platy$C.reps %in% c(0,1)),"C.PaxC"] <- 0 # If C amplified in one well, but not the other (i.e. 1/2 technical replicates amplified), set ratio C.PaxC to 0
platy[which(platy$D.reps %in% c(0,1)),"D.PaxC"] <- 0 # If D amplified in one well, but not the other (i.e. 1/2 technical replicates amplified ), set ratio D.PaxC to 0

# Manually remove known problematic samples
platy <- platy[which(platy$Sample.Name != "KI15cFMD383"),] # remove, is not Platy (should have been sample 385)
platy <- platy[which(platy$Sample.Name != "KI17aFMD145"),] # remove, FL and FMD didn't match
platy <- platy[which(platy$Sample.Name != "KI17aFMD312"),] # remove, FL and FMD didn't match
platy <- platy[which(platy$Sample.Name != "KI17aFMD314"),] # remove, FL and FMD didn't match
platy <- platy[which(platy$Sample.Name != "KI17aFMD316"),] # remove, FL and FMD didn't match

# Calculate the total symbiont:host ratio by adding ratios of C and D to PaxC
platy$S.H <- platy$C.PaxC + platy$D.PaxC

# Check for zero symbiont amplification
platy[!is.na(platy$PaxC.CT.mean) & is.na(platy$C.CT.mean) & is.na(platy$D.CT.mean),]

# Create a melted key for merging sample names
melt_key <- melt(SampleID_key,id.vars = "Tag")

# Add a Tag column to platy by matching 
platy$Tag <- melt_key[match(platy$Sample.Name,melt_key$value),"Tag"]

# Figure out which sample names are not matching
platy[which(!platy$Sample.Name %in% melt_key$value),] # These are the problem ones that are not matching

# Create field_season column based on Sample.Name
platy$field_season <- str_extract(platy$Sample.Name,"^[^F]*")
# Create Sample.Num column based on Sample.Name
platy$Sample.Num <- as.numeric(str_extract(platy$Sample.Name, "...$"))
# Create Pre.Post column for 2015a based on sample number (1-80 were Pre, 81-end were Post)
platy$Pre.Post <- ifelse(platy$field_season=="KI15a" & platy$Sample.Num <= 80,"Pre",NA)
platy$Pre.Post <- ifelse(platy$field_season=="KI15a" & platy$Sample.Num > 80,"Post",platy$Pre.Post)
# Paste field_season and Pre.Post
platy$field_season <- paste(platy$field_season,platy$Pre.Post,sep="_")
# Use gsub to make field_season match metadata for downstream merging
platy$field_season <- gsub("_NA","",platy$field_season)
platy$field_season <- gsub("KI","KI20",platy$field_season)

# Create ref for downstream merging
platy$ref <- paste(platy$field_season,".tag",platy$Tag, sep="")

# Save RData file
save(platy,file="data/qPCR/qPCR.RData")
