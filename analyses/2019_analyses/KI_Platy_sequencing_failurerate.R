# Load necessary libraries
library(phyloseq)

# Load necesssary data
load("analyses/KI_Platy.RData") # This is the data processed for KI_Platy, currently includes Platygyra and F. pentagona samples (n=289 total).

# Use column sums to calculate reads/sample
seqcounts <- colSums(otu_table(phy.f))
# Plot a histogram of sequence counts
hist(seqcounts,breaks = seq(0,44000,2000))

# Subset by pass/fail at different levels of read depth
fail200 <- data.frame(fail200 = seqcounts[seqcounts<200])
pass200 <- data.frame(pass200 = seqcounts[seqcounts>200])

fail500 <- data.frame(fail500 = seqcounts[seqcounts<500])
pass500 <- data.frame(pass500 = seqcounts[seqcounts>500])

fail1000 <- data.frame(fail1000 = seqcounts[seqcounts<1000])
pass1000 <- data.frame(pass1000 = seqcounts[seqcounts>1000])

# Calculate percent failed based on read depth
fail200_perc <- nrow(fail200)/nrow(pass200)*100
fail500_perc <- nrow(fail500)/nrow(pass500)*100
fail1000_perc <- nrow(fail1000)/nrow(pass1000)*100

# Clean up environment
rm(seqcounts,phy.f)

# Save environment
save.image("analyses/KI_Platy_sequencing_failurerate.RData")
