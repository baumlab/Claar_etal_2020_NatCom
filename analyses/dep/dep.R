<!-- Use merge_taxa to collapse OTUs by clade -->
  ```{r, echo=FALSE}
# Find all entries in the taxa_table that contain DI in "hit"
Ds <- grep("^D", data.frame(tax_table(phy97.f))$hit)
# Merge taxa, collapsing all hits with "D1" into 1 row in the taxa table. Eqtaxa says which taxa to collapse together. Archetype is included or it doesn't work, and refers to which name the new collapsed row will take - however I haven't found out how to modify this without it failing yet.
phy97.f.clade <- merge_taxa(phy97.f, eqtaxa=Ds, archetype=1)
# Create tt from the taxa table (to use in renaming below)
tt <- data.frame(tax_table(phy97.f.clade), stringsAsFactors = F)
# Create "isna" which includes the rownames of all taxa where "hit"=NA
isna <- rownames(tt)[is.na(tt$hit)]
# Create info by extracting the row's "hit" string from the original taxa table
info <- "D"
# Rename the NA using the extracted "hit" name from above
tt[isna,"hit"] <- info
# Replace the taxa table in phy97.f.eqtaxa with updated taxa table (tt). Must be converted to matrix first to work.
tax_table(phy97.f.clade) <- as.matrix(tt)

Cs <- grep("^C", data.frame(tax_table(phy97.f.clade))$hit)
phy97.f.clade <- merge_taxa(phy97.f.clade, eqtaxa=Cs, archetype=1)
tt <- data.frame(tax_table(phy97.f.clade), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- "C"
tt[isna,"hit"] <- info
tax_table(phy97.f.clade) <- as.matrix(tt)

Gs <- grep("^G", data.frame(tax_table(phy97.f.clade))$hit)
phy97.f.clade <- merge_taxa(phy97.f.clade, eqtaxa=Gs, archetype=1)
tt <- data.frame(tax_table(phy97.f.clade), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- "G"
tt[isna,"hit"] <- info
tax_table(phy97.f.clade) <- as.matrix(tt)

As <- grep("^A", data.frame(tax_table(phy97.f.clade))$hit)
phy97.f.clade <- merge_taxa(phy97.f.clade, eqtaxa=As, archetype=1)
tt <- data.frame(tax_table(phy97.f.clade), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- "A"
tt[isna,"hit"] <- info
tax_table(phy97.f.clade) <- as.matrix(tt)

Fs <- grep("^F", data.frame(tax_table(phy97.f.clade))$hit)
phy97.f.clade <- merge_taxa(phy97.f.clade, eqtaxa=Fs, archetype=1)
tt <- data.frame(tax_table(phy97.f.clade), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- "F"
tt[isna,"hit"] <- info
tax_table(phy97.f.clade) <- as.matrix(tt)

Is <- grep("^I", data.frame(tax_table(phy97.f.clade))$hit)
phy97.f.clade <- merge_taxa(phy97.f.clade, eqtaxa=Is, archetype=1)
tt <- data.frame(tax_table(phy97.f.clade), stringsAsFactors = F)
isna <- rownames(tt)[is.na(tt$hit)]
info <- "I"
tt[isna,"hit"] <- info
tax_table(phy97.f.clade) <- as.matrix(tt)

# Transform sample counts to proportional abundance for downstream analyses
phy97.f.clade.p <- transform_sample_counts(phy97.f.clade, function(x) x/sum(x))
```
