# <!-- Principal and Background Types - Corals -->

# Define the principal coral types
principal.coral.A <- sum(data.frame(tax_table(principal.coral))$clade=="A")
principal.coral.C <- sum(data.frame(tax_table(principal.coral))$clade=="C")
principal.coral.D <- sum(data.frame(tax_table(principal.coral))$clade=="D")
principal.coral.F <- sum(data.frame(tax_table(principal.coral))$clade=="F")
principal.coral.G <- sum(data.frame(tax_table(principal.coral))$clade=="G")
principal.coral.I <- sum(data.frame(tax_table(principal.coral))$clade=="I")

# Print Principal coral types
print("PRINCIPAL CORAL TYPES")

# Define principal coral clades
principal.coral.clades <- c(A=principal.coral.A,C=principal.coral.C,D=principal.coral.D,F=principal.coral.F,G=principal.coral.G,I=principal.coral.I)

# List and print principal coral types
principal.coral.types <- data.frame(tax_table(principal.coral))$hit
principal.coral.types

# Define Background Types
background.coral.A <- sum(data.frame(tax_table(background.coral))$clade=="A")
background.coral.C <- sum(data.frame(tax_table(background.coral))$clade=="C")
background.coral.D <- sum(data.frame(tax_table(background.coral))$clade=="D")
background.coral.F <- sum(data.frame(tax_table(background.coral))$clade=="F")
background.coral.G <- sum(data.frame(tax_table(background.coral))$clade=="G")
background.coral.I <- sum(data.frame(tax_table(background.coral))$clade=="I")

# Print "Background Coral Types"
print("BACKGROUND CORAL TYPES")

# Define Background Clades
background.coral.clades <- c(A=background.coral.A,C=background.coral.C,D=background.coral.D,F=background.coral.F,G=background.coral.G,I=background.coral.I)

# List and print background types
background.coral.types <- data.frame(tax_table(background.coral))$hit
background.coral.types

# Create data frame of background and principal coral clades
principal.coral.background.coral.clades <- rbind(principal.coral.clades,background.coral.clades)
t.principal.coral.background.coral.clades <- t(principal.coral.background.coral.clades)
colnames(t.principal.coral.background.coral.clades) <- c("Principal.coral Types", "Background.coral Types")

# Plot background and principal coral clades
barplot(t.principal.coral.background.coral.clades,col=rainbow(6),beside=TRUE, ylim=c(0,max(t.principal.coral.background.coral.clades)), main="Coral only")
legend("topleft", legend=rownames(t.principal.coral.background.coral.clades), fill=rainbow(6))

# Make pdf of background and principal coral clades
pdf(file="C:/Users/Dani/Documents/Data_Analysis/KI_Platy/figures/OTUSummary/Background.coral_Pricipal_Clades.pdf", width=10, height=7)
par(xpd=TRUE)
barplot(t.principal.coral.background.coral.clades,col=rainbow(6),beside=TRUE, ylim=c(0,max(t.principal.coral.background.coral.clades)), main="Coral only")
legend("topleft", legend=rownames(t.principal.coral.background.coral.clades), fill=rainbow(6))
dev.off()

# Core vs. Common vs. Rare
# Extract OTU names from Coral core, common, and rare types
names.coral.core <- names(coral.core)
names.coral.common <- names(coral.common)
names.coral.rare <- names(coral.rare)


# Print hits for core, common, and rare in coral
```{r, include=TRUE, echo=FALSE, cache=TRUE}
if (!is.null(names.coral.core)) {
  coral.core.hits <- data.frame()
  for (i in names.coral.core){
    coral.core.hits[i,1] <- paste((data.frame(tax_table(phy97.f.c.coral)))[i,]$hit)
  }
  colnames(coral.core.hits) <- "hits"
  print("coral.core.hits")
  coral.core.hits
} else {
  print("There were no core hits (for coral)")
}

if (!is.null(names.coral.common)) {
  coral.common.hits <- data.frame()
  for (i in names.coral.common){
    coral.common.hits[i,1] <- paste((data.frame(tax_table(phy97.f.c.coral)))[i,]$hit)
  }
  colnames(coral.common.hits) <- "hits"
  print("coral.common.hits")
  coral.common.hits
} else {
  print("There were no common hits (for coral)")
}

if (!is.null(names.coral.rare)) {
  coral.rare.hits <- data.frame()
  for (i in names.coral.rare){
    coral.rare.hits[i,1] <- paste((data.frame(tax_table(phy97.f.c.coral)))[i,]$hit)
  }
  colnames(coral.rare.hits) <- "hits"
  print("coral.rare.hits")
  coral.rare.hits
} else {
  print("There were no rare hits (for coral)")
}

```
