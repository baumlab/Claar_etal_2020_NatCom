# Clear environment
rm(list=ls())

# Load necessary packages
library(dplyr)
library(ggplot2)

# Read in data
frozen <- read.csv("Frozen_platy_amounts.csv") # KT checked - raw sample amounts Frozen at UVIC
sds <- read.csv("KI_samples_in_SDS.csv") # High/Low Biomass samples that Ross has in MIA
frozen_FMD1 <- read.csv("Frozen_samples_totaketoMIA.csv")
frozen_FMD2 <- read.csv("Frozen_samples_totaketoMIA2.csv")

frozen_FMD <- rbind(frozen_FMD1,frozen_FMD2)

# head(frozen)
# head(sds)
# names(sds)

# Create a new column for SampleID
frozen$SampleID <- frozen$sds_SampleID

# Merge by SampleID
samples <- merge(frozen, sds, by="SampleID",all.y = TRUE) 

# Keep the original field season info in field_season_key
samples$field_season_key <- samples$field_season

# But then collapse field seasons into before/during/after
samples$field_season <- gsub("KI2014","before",samples$field_season)
samples$field_season <- gsub("KI2015a_Pre","before",samples$field_season)
samples$field_season <- gsub("KI2015a_Post","before",samples$field_season)
samples$field_season <- gsub("KI2015b","before",samples$field_season)
samples$field_season <- gsub("KI2015c","during",samples$field_season)
samples$field_season <- gsub("KI2016a","after",samples$field_season)
# Set factor levels for field_season
samples$field_season <- factor(samples$field_season, levels=c("before","during","after"))

# Which samples are F-ed? Ones that have high biomass (in SDS) and no frozen sample available
fed <- samples[which(samples$Biomass=="High" & samples$frozen_sample=="no"),]
nrow(fed)

havefrozen <- samples[which(samples$frozen_sample!="no"),]
nrow(havefrozen)
donthavefrozen <- samples[which(samples$frozen_sample=="no"),]
nrow(donthavefrozen)
donthavefrozen_buthavelow <- donthavefrozen[which(donthavefrozen$Biomass=="Low"),]
nrow(donthavefrozen_buthavelow)

# Which samples are ok? Ones that either have low biomass (in SDS) or a frozen sample available
ok <- samples[which(samples$Biomass=="Low" | samples$frozen_sample!="no"),]
nrow(ok)

processing <- data.frame(c(frozen_FMD$ToBeSent,donthavefrozen$SampleID))
nrow(processing)

write.csv(frozen_FMD, file="frozen_FMD.csv")

# Which samples would we need from UVIC?
need <- ok[which(ok$Biomass=="High"),]
# Which samples does Ross already have?
rosshas <- ok[which(ok$Biomass=="Low"),]

# Plot, showing number of samples for each field season
ggplot(aes(x=field_season),data=fed)+geom_bar(stat="count")
ggplot(aes(x=field_season),data=ok)+geom_bar(stat="count")
ggplot(aes(x=field_season),data=samples)+geom_bar(stat="count")

# Count the number of samples by field season
nsamp <- samples %>% count(field_season) %>% as.data.frame()
nfed <- fed %>% count(field_season) %>% as.data.frame()
nok <- ok %>% count(field_season) %>% as.data.frame()
nrosshas <- rosshas %>% count(field_season) %>% as.data.frame()
nneed <- need %>% count(field_season) %>% as.data.frame()

# Merge nsamp, nfed, nok
ns <- merge(nsamp,nfed, by="field_season",all.x = TRUE)
ns <- merge(ns,nok, by="field_season",all.x = TRUE)
colnames(ns) <- c("field_season","nsamp","nfed","nok") # Set column names
ns

write.csv(havefrozen,file="havefrozen.csv")
write.csv(donthavefrozen,file="donthavefrozen.csv")

