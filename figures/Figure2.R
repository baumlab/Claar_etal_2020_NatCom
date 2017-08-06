#### KI PLATY - FIGURE 2 ####

# Reset graphical parameters
dev.off()

# Clear your environment
rm(list=ls())

# Load necessary packages
library(imager)

# Load in data
load("data/KI_seqs_f_coral_grouped.RData")
load("data/temperature/KI_SB_temp_DHW.RData")


clade.colors <- c(A = "#D55E00", C = "#009E73", D = "#56B4E9", F = "#E69F00", G = "#F0E442", Other = "#A9A9A9")

# Open a tiff image
# tiff(file="/figures/Figure2.tiff",width = 4, height = 6,units="in",res=300)

# Subset samples to only look at coral 99
phycoral99 <- subset_samples(phy97.f.c,coral_tag=="99", prune=TRUE)
phycoral99 <- subset_taxa(phycoral99, taxa_sums(phycoral99) > 0, prune=TRUE)
# Transform sample counts to proportional counts
phycoral99.p <- transform_sample_counts(phycoral99, function(x) x/sum(x))

# Extract the otu table and tax table into data frames
otu_coral99 <- data.frame(otu_table(phycoral99.p))
otu_coral99$otu <- row.names(otu_coral99)
tax_coral99 <- data.frame(tax_table(phycoral99))
# Join together otu table an tax table
otu_tax_coral99 <- join_all(list(otu_coral99, tax_coral99),by = "otu", type="full")
otu_tax_coral99 <- subset(otu_tax_coral99, select=c("KI16aFSYM049","KI15aFSYM103","KI14FSYM099","KI15bFSYM023", "KI15cFSYM018", "otu","hit","clade"))

# Aggregate by clade (i.e. collapse all types down to clade)
coral99_clades <- aggregate(otu_tax_coral99[,1:5], by=list(Clade=otu_tax_coral99$clade), FUN=sum)

# Rearrange
coral99_clades <- coral99_clades[,c(1,4,3,5,6,2)]
# Change from proportion to percent
coral99_clades[,2:6] <- coral99_clades[,2:6]*100
# Transpose
coral99_clades.t <- t(coral99_clades)
# Name columns
colnames(coral99_clades.t) <- coral99_clades.t[1,]
# Format data frame
coral99_clades.t <- data.frame(coral99_clades.t[2:6,])
# Name rows
rownames(coral99_clades.t) <- dates

# Set dates for the field seasons (for plotting)
KI2014 <- as.POSIXct("2014-09-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# Set a start and end date for plotting
startdate <- as.POSIXct("2014-08-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
enddate <- as.POSIXct("2016-11-19 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# Truncate the heat data from startdate to enddate (so we can plot)
KI_heat <- KI_allsites_DHW[which(KI_allsites_DHW$xi3>startdate),]
KI_heat <- KI_heat[which(KI_heat$xi3<enddate),]
# Rename columns
colnames(KI_heat)<- c("time","dhw")
# Set from-to dates for plotting
from <- KI_heat$time-1.75*86400
to <- KI_heat$time+1.75*86400

# Make dataframes for the field_season identifier (dates) and the POSIXct date
dates <- t(data.frame("KI2014","KI2015a","KI2015b","KI2015c","KI2016a"))
Pdates <- t(data.frame(KI2014,KI2015a,KI2015b,KI2015c,KI2016a))
# Transpose
coral99_A <- t(coral99_clades[1,2:6])
# Name columns and rows
colnames(coral99_A) <- c("Proportion")
rownames(coral99_A) <- dates
# Bind together POSIXct dates and data frame
coral99_A<-data.frame(cbind(coral99_A,pdate=as.POSIXct(Pdates)))
# Transpose
coral99_C <- t(coral99_clades[2,2:6])
# Name columns and rows
colnames(coral99_C) <- c("Proportion")
rownames(coral99_C) <- dates
# Bind together POSIXct dates and data frame
coral99_C<-data.frame(cbind(coral99_C,pdate=as.POSIXct(Pdates)))
# Transpose
coral99_D <- t(coral99_clades[3,2:6])
# Name columns and rows
colnames(coral99_D) <- c("Proportion")
rownames(coral99_D) <- dates
# Bind together POSIXct dates and data frame
coral99_D<-data.frame(cbind(coral99_D,pdate=as.POSIXct(Pdates)))

# Start plot with points for percent Clade C
plot(coral99_C$pdate,coral99_C$Proportion,ylim=c(0,100),col=clade.colors["C"],pch=1,xlab=NA,ylab=NA,xaxt='n')
# Add lines connecting points
lines(coral99_C$pdate,coral99_C$Proportion,col=clade.colors["C"])
# Add points for Clade D
points(coral99_D$pdate,coral99_D$Proportion,col=clade.colors["D"],pch=1)
# Add lines for Clade D
lines(coral99_D$pdate,coral99_D$Proportion,col=clade.colors["D"])
# Add a timeline xaxis so that field seasons are spaced appropriately
axis.POSIXct(side=1,as.POSIXct(coral99_C$pdate, origin="1970-01-01"),cex.axis=0.93,tck=0.05,lwd.ticks=2,labels=FALSE)
axis.POSIXct(side=1,at=seq(KI_heat$time[1],KI_heat$time[240],by="month"),KI_heat$time,tck=0.03,cex.axis=0.93,labels=c("","","Oct","","","","","","Apr","","","Jul","","","Oct","","","","","","Apr","","","Jul","","","Oct",""),lwd.ticks=1.5,padj=-1.5)
axis.POSIXct(side=1,as.POSIXct(coral99_C$pdate, origin="1970-01-01"),cex.axis=0.93,tck=0,padj=-1.5)



# ########## All samples ###############
otu_allcoral <- data.frame(otu_table(phy97.f.c.p))
otu_allcoral[,1:262] <- (otu_allcoral[,1:262])*100
otu_allcoral$otu <- row.names(otu_allcoral)
tax_allcoral <- data.frame(tax_table(phy97.f.c.p))

otu_tax_allcoral <- join_all(list(otu_allcoral, tax_allcoral),by = "otu", type="full")
otu_tax_allcoral <- otu_tax_allcoral[,c(1:263,270:271)]

allcoral_clades <- aggregate(otu_tax_allcoral[,1:262], by=list(Clade=otu_tax_allcoral$clade), FUN=sum)
colnames(allcoral_clades)<- gsub("KI14.*","KI2014",colnames(allcoral_clades))
colnames(allcoral_clades)<- gsub("KI15a.*","KI2015a",colnames(allcoral_clades))
colnames(allcoral_clades)<- gsub("KI15b.*","KI2015b",colnames(allcoral_clades))
colnames(allcoral_clades)<- gsub("KI15c.*","KI2015c",colnames(allcoral_clades))
colnames(allcoral_clades)<- gsub("KI16a.*","KI2016a",colnames(allcoral_clades))

rownames(allcoral_clades) <- allcoral_clades$Clade
KI2014_allcoral_clades_temp <- allcoral_clades[which(colnames(allcoral_clades)=="KI2014")]
KI2014_allcoral_clades <- (rowSums(KI2014_allcoral_clades_temp))/ncol(KI2014_allcoral_clades_temp)

KI2015a_allcoral_clades_temp <- allcoral_clades[which(colnames(allcoral_clades)=="KI2015a")]
KI2015a_allcoral_clades <- (rowSums(KI2015a_allcoral_clades_temp))/ncol(KI2015a_allcoral_clades_temp)

KI2015b_allcoral_clades_temp <- allcoral_clades[which(colnames(allcoral_clades)=="KI2015b")]
KI2015b_allcoral_clades <- (rowSums(KI2015b_allcoral_clades_temp))/ncol(KI2015b_allcoral_clades_temp)

KI2015c_allcoral_clades_temp <- allcoral_clades[which(colnames(allcoral_clades)=="KI2015c")]
KI2015c_allcoral_clades <- (rowSums(KI2015c_allcoral_clades_temp))/ncol(KI2015c_allcoral_clades_temp)

KI2016a_allcoral_clades_temp <- allcoral_clades[which(colnames(allcoral_clades)=="KI2016a")]
KI2016a_allcoral_clades <- (rowSums(KI2016a_allcoral_clades_temp))/ncol(KI2016a_allcoral_clades_temp)

allcoral_clades <- t(cbind(KI2014_allcoral_clades,KI2015a_allcoral_clades,KI2015b_allcoral_clades,KI2015c_allcoral_clades,KI2016a_allcoral_clades))

rownames(allcoral_clades) <- dates
# Bind together POSIXct dates and data frame
allcoral_clades<-data.frame(cbind(allcoral_clades,pdate=as.POSIXct(Pdates)))

# Open a tiff image
tiff(file="figures/Figure2.tiff",width = 6, height = 4,units="in",res=300)

par(mar=c(2.5,4,1,1))
# Start plot with points for percent Clade A
plot(allcoral_clades$pdate,allcoral_clades$A,ylim=c(0,100),col=clade.colors["A"],pch=19,xlab=NA,ylab=NA,xaxt='n',yaxt='n',bty='l',yaxs='i')
# Add lines connecting points
lines(allcoral_clades$pdate,allcoral_clades$A,col=clade.colors["A"],lwd=3,lty=3)
# Add points for Clade C
points(allcoral_clades$pdate,allcoral_clades$C,col=clade.colors["C"],pch=19)
# Add lines for Clade C
lines(allcoral_clades$pdate,allcoral_clades$C,col=clade.colors["C"],lwd=3,lty=3)
# Add points for Clade D
points(allcoral_clades$pdate,allcoral_clades$D,col=clade.colors["D"],pch=19)
# Add lines for Clade D
lines(allcoral_clades$pdate,allcoral_clades$D,col=clade.colors["D"],lwd=3,lty=3)
# Add points for Clade G
points(allcoral_clades$pdate,allcoral_clades$G,col=clade.colors["G"],pch=19)
# Add lines for Clade G
lines(allcoral_clades$pdate,allcoral_clades$G,col=clade.colors["G"],lwd=3,lty=3)
# Add a timeline xaxis so that field seasons are spaced appropriately
axis(2,c(10,20,30,40,50,60,70,80,90,100),las=2,outer=FALSE,tck=.02,hadj=0.3)
axis.POSIXct(side=1,as.POSIXct(allcoral_clades$pdate, origin="1970-01-01"),cex.axis=0.93,tck=0.05,lwd.ticks=2,labels=FALSE)
axis.POSIXct(side=1,at=seq(KI_heat$time[1],KI_heat$time[240],by="month"),KI_heat$time,tck=0.03,cex.axis=0.93,labels=c("","","Oct","","","","","","Apr","","","Jul","","","Oct","","","","","","Apr","","","Jul","","","Oct",""),lwd.ticks=1.5,padj=-1.5)
axis.POSIXct(side=1,as.POSIXct(allcoral_clades$pdate, origin="1970-01-01"),cex.axis=0.93,tck=0,padj=-1.5)
legend(as.POSIXct("2015-08-24 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S"),100,c("A","C","D","G"), col=c(clade.colors["A"],clade.colors["C"],clade.colors["D"],clade.colors["G"]),lty=3,bty='n',lwd=3)
mtext(side=1,"Date",line=1.2)
mtext(side=2,"Per cent of sequences",line=1.5)
# KI_heat <- KI_heat[which(KI_heat$time<as.POSIXct(allcoral_clades$pdate[5], origin="1970-01-01")),]
# KI_heat <- KI_heat[which(KI_heat$time>as.POSIXct(allcoral_clades$pdate[1], origin="1970-01-01")),]
# lines(KI_heat$time,((KI_heat$dhw)/(max(KI_heat$dhw)))*100,col="gray")
dev.off()
