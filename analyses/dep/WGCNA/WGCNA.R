# source("http://bioconductor.org/biocLite.R") 
# biocLite(c("AnnotationDbi", "impute", "GO.db", "preprocessCore")) 
# biocLite("Biostrings")
# install.packages("WGCNA")
library(WGCNA)

options(stringsAsFactors = FALSE); # Make sure to do this!

# Read in 100% OTU table
STJ2012 <- read.table("100_otus.tsv", sep="\t",check.names=FALSE,header=T, row.names=1, skip=1, comment.char="")
disableWGCNAThreads() # Disable WGCNA threads, this allows it to work in R Studio. Ok to enable if running as a script in R

# Subset samples to only look at Diploria strigosa
STJ2012.f <- STJ2012[,c("44","64","54","72","80","7","29","10","36")]

# Remove any OTUs whos total abundance is <10 seqs
STJ2012.f <- STJ2012.f[rowSums(STJ2012.f)>10,]
# Remove any samples whos total sequence count is <10
STJ2012.f <- STJ2012.f[,colSums(STJ2012.f)>10]

# Transpose the data frame for downstream analysis
STJ2012.f <- as.data.frame(t(STJ2012.f))

# Check to see if all OTUs are complete to use in downstream analysis
gsg = goodSamplesGenes(STJ2012.f, verbose = 3);
# Check if all "genes" (OTUs) are good. If the output is "TRUE", good to move on to the next step. If the output is "FALSE" check the WGCNA website for next steps
gsg$allOK

# Create the sampleTree
sampleTree = hclust(dist(STJ2012.f), method = "average");
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# The user should change the dimensions if the window is too large or too small.
sizeGrWindow(12,9)
par(cex = 0.6);
par(mar = c(0,4,2,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
     cex.axis = 1.5, cex.main = 2)
# The goal of this plot is to manually detect and remove outlier samples. We do not currently know enough to decide when and how to make this decision, and thus did not remove any samples at thist step

# Choose a set of soft-thresholding powers
# We chose the range to try from the tutorial, could be changed in the future
powers = c(c(1:10), seq(from = 12, to=20, by=2))
# Call the network topology analysis function
sft = pickSoftThreshold(STJ2012.f, powerVector = powers, verbose = 5)
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2));
cex1 = 0.9;
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.90,col="red")
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

# Create the net object, which contains all information necessary to create dendrogram, etc.
net = blockwiseModules(STJ2012.f, power = 3,
                       TOMType = "unsigned", minModuleSize = 3,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "STJ2012TOM",
                       verbose = 3)

# Check how many OTUs are in each of the modules (each module is a different color)
table(net$colors)

# Open a graphics window
sizeGrWindow(12, 9)
# Convert labels to colors for plotting
mergedColors = labels2colors(net$colors)

# Open a png to write the dendrogram to
png("dendro.png",height=4, width=12,units="in",res=300)

# Plot the dendrogram and the module colors underneath
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)
dev.off() # Close the png

# Write net$colors to "membership.txt" for downstream analysis. This file shows which module each OTU belongs to
write.table(net$colors, file="membership.txt")
