# Rank Abundance Curves - Extended Data Figure 2
dev.off()

# Clear your environment
rm(list=ls())

# Load rarefied dataset
load("data/KI_seqs_f_coral_grouped_rare.RData")

clade.colors <- c(A = "#D55E00", C = "#009E73", D = "#56B4E9", G = "#F0E442")

# Open TIFF file
tiff(file="figures/Extended Data/ExData_Figure2.tiff",width = 9, height = 8,units="in",res=300)


par(mar=c(3,2,1,1),xaxs="i",yaxs="i",mfrow=c(3,3),oma=c(1,3,3,0),xpd=TRUE)
# par(mar=c(3,2,1,1),xaxs="i",yaxs="i",mfrow=c(9,3))

# Set the number of bars in the rank abundance barplot
N=5



################## ALL CORAL ######################
# Low Disturbance: before
rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.before.LVL)))
rac.before.temp$otu<-rownames(rac.before.temp)
colnames(rac.before.temp)[1] <- "Abundance"
rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.coral.before.LVL)), by="otu")
rac.before.LVL <- rac.before[order(-rac.before$Abundance),]
max.abund <- max(rac.before.LVL$Abundance)
sum.abund <- sum(rac.before.LVL$Abundance)
rac.before.LVL$Abundance <- (rac.before.LVL$Abundance)/max.abund*100
rac.before.LVL$hit <- gsub("_.*", "", rac.before$hit)
barplot(rac.before.LVL$Abundance[1:N], las=2, names.arg=rac.before.LVL$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,2)], cex.names=1,ylim=c(0,100),mgp=c(0,.5,0),tck=-0.01)
mtext("Before El Niño",side=3,line=1,cex=1.5)
mtext("Low",side=2,line=2.4,cex=1.5)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.after.HVH)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# Low Disturbance: during
rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.during.LVL)))
rac.during.temp$otu<-rownames(rac.during.temp)
colnames(rac.during.temp)[1] <- "Abundance"
rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.coral.during.LVL)), by="otu")
rac.during.LVL <- rac.during[order(-rac.during$Abundance),]
max.abund <- max(rac.during.LVL$Abundance)
sum.abund <- sum(rac.during.LVL$Abundance)
rac.during.LVL$Abundance <- (rac.during.LVL$Abundance)/max.abund*100
rac.during.LVL$hit <- gsub("_.*", "", rac.during$hit)
barplot(rac.during.LVL$Abundance[1:N], las=2, names.arg=rac.during.LVL$hit[1:N],space=0,col=clade.colors[c(2,2,4,2,3)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext("Early El Niño",side=3,line=1,cex=1.5)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.during.LVL)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# Low Disturbance: after
rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.after.LVL)))
rac.after.temp$otu<-rownames(rac.after.temp)
colnames(rac.after.temp)[1] <- "Abundance"
rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.coral.after.LVL)), by="otu")
rac.after.LVL <- rac.after[order(-rac.after$Abundance),]
max.abund <- max(rac.after.LVL$Abundance)
sum.abund <- sum(rac.after.LVL$Abundance)
rac.after.LVL$Abundance <- (rac.after.LVL$Abundance)/max.abund*100
rac.after.LVL$hit <- gsub("_.*", "", rac.after$hit)
barplot(rac.after.LVL$Abundance[1:N], las=2, names.arg=rac.after.LVL$hit[1:N],space=0,col=clade.colors[c(2,2,2,3,4)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext("Late El Niño",side=3,line=1,cex=1.5)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.after.LVL)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# Medium Disturbance: before
rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.before.HM)))
rac.before.temp$otu<-rownames(rac.before.temp)
colnames(rac.before.temp)[1] <- "Abundance"
rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.coral.before.HM)), by="otu")
rac.before.HM <- rac.before[order(-rac.before$Abundance),]
max.abund <- max(rac.before.HM$Abundance)
sum.abund <- sum(rac.before.HM$Abundance)
rac.before.HM$Abundance <- (rac.before.HM$Abundance)/max.abund*100
rac.before.HM$hit <- gsub("_.*", "", rac.before$hit)
barplot(rac.before.HM$Abundance[1:N], las=2, names.arg=rac.before.HM$hit[1:N],space=0,col=clade.colors[c(3,4,3,1,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext("Medium High",side=2,line=2.4,cex=1.5)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.before.HM)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# Medium Disturbance: during
rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.during.HM)))
rac.during.temp$otu<-rownames(rac.during.temp)
colnames(rac.during.temp)[1] <- "Abundance"
rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.coral.during.HM)), by="otu")
rac.during.HM <- rac.during[order(-rac.during$Abundance),]
max.abund <- max(rac.during.HM$Abundance)
sum.abund <- sum(rac.during.HM$Abundance)
rac.during.HM$Abundance <- (rac.during.HM$Abundance)/max.abund*100
rac.during.HM$hit <- gsub("_.*", "", rac.during$hit)
barplot(rac.during.HM$Abundance[1:N], las=2, names.arg=rac.during.HM$hit[1:N],space=0,col=clade.colors[c(2,3,4,3,1)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.during.HM)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# Medium Disturbance: after
rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.after.HM)))
rac.after.temp$otu<-rownames(rac.after.temp)
colnames(rac.after.temp)[1] <- "Abundance"
rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.coral.after.HM)), by="otu")
rac.after.HM <- rac.after[order(-rac.after$Abundance),]
max.abund <- max(rac.after.HM$Abundance)
sum.abund <- sum(rac.after.HM$Abundance)
rac.after.HM$Abundance <- (rac.after.HM$Abundance)/max.abund*100
rac.after.HM$hit <- gsub("_.*", "", rac.after$hit)
barplot(rac.after.HM$Abundance[1:N], las=2, names.arg=rac.after.HM$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,3)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.after.HM)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# High Disturbance: before
rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.before.HVH)))
rac.before.temp$otu<-rownames(rac.before.temp)
colnames(rac.before.temp)[1] <- "Abundance"
rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.coral.before.HVH)), by="otu")
rac.before.HVH <- rac.before[order(-rac.before$Abundance),]
max.abund <- max(rac.before.HVH$Abundance)
sum.abund <- sum(rac.before.HVH$Abundance)
rac.before.HVH$Abundance <- (rac.before.HVH$Abundance)/max.abund*100
rac.before.HVH$hit <- gsub("_.*", "", rac.before$hit)
barplot(rac.before.HVH$Abundance[1:N], las=2, names.arg=rac.before.HVH$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,3)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext("Very High",side=2,line=2.4,cex=1.5)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.before.HVH)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# High Disturbance: during
rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.during.HVH)))
rac.during.temp$otu<-rownames(rac.during.temp)
colnames(rac.during.temp)[1] <- "Abundance"
rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.coral.during.HVH)), by="otu")
rac.during.HVH <- rac.during[order(-rac.during$Abundance),]
max.abund <- max(rac.during.HVH$Abundance)
sum.abund <- sum(rac.during.HVH$Abundance)
rac.during.HVH$Abundance <- (rac.during.HVH$Abundance)/max.abund*100
rac.during.HVH$hit <- gsub("_.*", "", rac.during$hit)
barplot(rac.during.HVH$Abundance[1:N], las=2, names.arg=rac.during.HVH$hit[1:N],space=0,col=clade.colors[c(2,4,3,1,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.during.HVH)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# High Disturbance: after
rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.coral.after.HVH)))
rac.after.temp$otu<-rownames(rac.after.temp)
colnames(rac.after.temp)[1] <- "Abundance"
rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.coral.after.HVH)), by="otu")
rac.after.HVH <- rac.after[order(-rac.after$Abundance),]
max.abund <- max(rac.after.HVH$Abundance)
sum.abund <- sum(rac.after.HVH$Abundance)
rac.after.HVH$Abundance <- (rac.after.HVH$Abundance)/max.abund*100
rac.after.HVH$hit <- gsub("_.*", "", rac.after$hit)
barplot(rac.after.HVH$Abundance[1:N], las=2, names.arg=rac.after.HVH$hit[1:N],space=0,col=clade.colors[c(2,2,4,3,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
mtext(paste("n = ",nsamples(phy97.f.rare.c.coral.after.HVH)," samples 
            (",sum.abund," sequences)",sep=""),side=3,line=-2,adj=0.9,cex=0.75)

# #################### FPENTA ###################
# # Low Disturbance: before
# rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.before.LVL)))
# rac.before.temp$otu<-rownames(rac.before.temp)
# colnames(rac.before.temp)[1] <- "Abundance"
# rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.before.LVL)), by="otu")
# rac.before.LVL <- rac.before[order(-rac.before$Abundance),]
# max.abund <- max(rac.before.LVL$Abundance)
# rac.before.LVL$Abundance <- (rac.before.LVL$Abundance)/max.abund*100
# rac.before.LVL$hit <- gsub("_.*", "", rac.before$hit)
# barplot(rac.before.LVL$Abundance[1:N], las=2, names.arg=rac.before.LVL$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,2)], cex.names=1,ylim=c(0,100),mgp=c(0,.5,0),tck=-0.01)
# 
# # Medium Disturbance: before
# rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.before)))
# rac.before.temp$otu<-rownames(rac.before.temp)
# colnames(rac.before.temp)[1] <- "Abundance"
# rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.before.HM)), by="otu")
# rac.before.HM <- rac.before[order(-rac.before$Abundance),]
# max.abund <- max(rac.before.HM$Abundance)
# rac.before.HM$Abundance <- (rac.before.HM$Abundance)/max.abund*100
# rac.before.HM$hit <- gsub("_.*", "", rac.before$hit)
# barplot(rac.before.HM$Abundance[1:N], las=2, names.arg=rac.before.HM$hit[1:N],space=0,col=clade.colors[c(3,2,2,2,3)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # High Disturbance: before
# rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.before.HVH)))
# rac.before.temp$otu<-rownames(rac.before.temp)
# colnames(rac.before.temp)[1] <- "Abundance"
# rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.before.HVH)), by="otu")
# rac.before.HVH <- rac.before[order(-rac.before$Abundance),]
# max.abund <- max(rac.before.HVH$Abundance)
# rac.before.HVH$Abundance <- (rac.before.HVH$Abundance)/max.abund*100
# rac.before.HVH$hit <- gsub("_.*", "", rac.before$hit)
# barplot(rac.before.HVH$Abundance[1:N], las=2, names.arg=rac.before.HVH$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Low Disturbance: during
# rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.during.LVL)))
# rac.during.temp$otu<-rownames(rac.during.temp)
# colnames(rac.during.temp)[1] <- "Abundance"
# rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.during.LVL)), by="otu")
# rac.during.LVL <- rac.during[order(-rac.during$Abundance),]
# max.abund <- max(rac.during.LVL$Abundance)
# rac.during.LVL$Abundance <- (rac.during.LVL$Abundance)/max.abund*100
# rac.during.LVL$hit <- gsub("_.*", "", rac.during$hit)
# barplot(rac.during.LVL$Abundance[1:N], las=2, names.arg=rac.during.LVL$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,2,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Medium Disturbance: during
# rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.during)))
# rac.during.temp$otu<-rownames(rac.during.temp)
# colnames(rac.during.temp)[1] <- "Abundance"
# rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.during.HM)), by="otu")
# rac.during.HM <- rac.during[order(-rac.during$Abundance),]
# max.abund <- max(rac.during.HM$Abundance)
# rac.during.HM$Abundance <- (rac.during.HM$Abundance)/max.abund*100
# rac.during.HM$hit <- gsub("_.*", "", rac.during$hit)
# barplot(rac.during.HM$Abundance[1:N], las=2, names.arg=rac.during.HM$hit[1:N],space=0,col=clade.colors[c(2,4,3,2,2,2,3)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # High Disturbance: during
# rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.during.HVH)))
# rac.during.temp$otu<-rownames(rac.during.temp)
# colnames(rac.during.temp)[1] <- "Abundance"
# rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.during.HVH)), by="otu")
# rac.during.HVH <- rac.during[order(-rac.during$Abundance),]
# max.abund <- max(rac.during.HVH$Abundance)
# rac.during.HVH$Abundance <- (rac.during.HVH$Abundance)/max.abund*100
# rac.during.HVH$hit <- gsub("_.*", "", rac.during$hit)
# barplot(rac.during.HVH$Abundance[1:N], las=2, names.arg=rac.during.HVH$hit[1:N],space=0,col=clade.colors[c(2,4,3,2,2,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Low Disturbance: after
# rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.after.LVL)))
# rac.after.temp$otu<-rownames(rac.after.temp)
# colnames(rac.after.temp)[1] <- "Abundance"
# rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.after.LVL)), by="otu")
# rac.after.LVL <- rac.after[order(-rac.after$Abundance),]
# max.abund <- max(rac.after.LVL$Abundance)
# rac.after.LVL$Abundance <- (rac.after.LVL$Abundance)/max.abund*100
# rac.after.LVL$hit <- gsub("_.*", "", rac.after$hit)
# barplot(rac.after.LVL$Abundance[1:N], las=2, names.arg=rac.after.LVL$hit[1:N],space=0,col=clade.colors[c(2,2,4,3,2,2,3)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Medium Disturbance: after
# rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.after)))
# rac.after.temp$otu<-rownames(rac.after.temp)
# colnames(rac.after.temp)[1] <- "Abundance"
# rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.after.HM)), by="otu")
# rac.after.HM <- rac.after[order(-rac.after$Abundance),]
# max.abund <- max(rac.after.HM$Abundance)
# rac.after.HM$Abundance <- (rac.after.HM$Abundance)/max.abund*100
# rac.after.HM$hit <- gsub("_.*", "", rac.after$hit)
# barplot(rac.after.HM$Abundance[1:N], las=2, names.arg=rac.after.HM$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,3,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # High Disturbance: after
# rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.fpenta.after.HVH)))
# rac.after.temp$otu<-rownames(rac.after.temp)
# colnames(rac.after.temp)[1] <- "Abundance"
# rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.fpenta.after.HVH)), by="otu")
# rac.after.HVH <- rac.after[order(-rac.after$Abundance),]
# max.abund <- max(rac.after.HVH$Abundance)
# rac.after.HVH$Abundance <- (rac.after.HVH$Abundance)/max.abund*100
# rac.after.HVH$hit <- gsub("_.*", "", rac.after$hit)
# barplot(rac.after.HVH$Abundance[1:N], las=2, names.arg=rac.after.HVH$hit[1:N],space=0,col=clade.colors[c(2,2,4,3,2,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# ######################## PLATY ###################################
# # Low Disturbance: before
# rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.before.LVL)))
# rac.before.temp$otu<-rownames(rac.before.temp)
# colnames(rac.before.temp)[1] <- "Abundance"
# rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.platy.before.LVL)), by="otu")
# rac.before.LVL <- rac.before[order(-rac.before$Abundance),]
# max.abund <- max(rac.before.LVL$Abundance)
# rac.before.LVL$Abundance <- (rac.before.LVL$Abundance)/max.abund*100
# rac.before.LVL$hit <- gsub("_.*", "", rac.before$hit)
# barplot(rac.before.LVL$Abundance[1:N], las=2, names.arg=rac.before.LVL$hit[1:N],space=0,col=clade.colors[c(3,3,1,2,2)], cex.names=1,ylim=c(0,100),mgp=c(0,.5,0),tck=-0.01)
# 
# # Medium Disturbance: before
# rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.before)))
# rac.before.temp$otu<-rownames(rac.before.temp)
# colnames(rac.before.temp)[1] <- "Abundance"
# rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.platy.before.HM)), by="otu")
# rac.before.HM <- rac.before[order(-rac.before$Abundance),]
# max.abund <- max(rac.before.HM$Abundance)
# rac.before.HM$Abundance <- (rac.before.HM$Abundance)/max.abund*100
# rac.before.HM$hit <- gsub("_.*", "", rac.before$hit)
# barplot(rac.before.HM$Abundance[1:N], las=2, names.arg=rac.before.HM$hit[1:N],space=0,col=clade.colors[c(3,4,3,1,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # High Disturbance: before
# rac.before.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.before.HVH)))
# rac.before.temp$otu<-rownames(rac.before.temp)
# colnames(rac.before.temp)[1] <- "Abundance"
# rac.before <- merge(rac.before.temp, data.frame(tax_table(phy97.f.rare.c.platy.before.HVH)), by="otu")
# rac.before.HVH <- rac.before[order(-rac.before$Abundance),]
# max.abund <- max(rac.before.HVH$Abundance)
# rac.before.HVH$Abundance <- (rac.before.HVH$Abundance)/max.abund*100
# rac.before.HVH$hit <- gsub("_.*", "", rac.before$hit)
# barplot(rac.before.HVH$Abundance[1:N], las=2, names.arg=rac.before.HVH$hit[1:N],space=0,col=clade.colors[c(2,2,2,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Low Disturbance: during
# rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.during.LVL)))
# rac.during.temp$otu<-rownames(rac.during.temp)
# colnames(rac.during.temp)[1] <- "Abundance"
# rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.platy.during.LVL)), by="otu")
# rac.during.LVL <- rac.during[order(-rac.during$Abundance),]
# max.abund <- max(rac.during.LVL$Abundance)
# rac.during.LVL$Abundance <- (rac.during.LVL$Abundance)/max.abund*100
# rac.during.LVL$hit <- gsub("_.*", "", rac.during$hit)
# barplot(rac.during.LVL$Abundance[1:N], las=2, names.arg=rac.during.LVL$hit[1:N],space=0,col=clade.colors[c(1,3,1,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Medium Disturbance: during
# rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.during)))
# rac.during.temp$otu<-rownames(rac.during.temp)
# colnames(rac.during.temp)[1] <- "Abundance"
# rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.platy.during.HM)), by="otu")
# rac.during.HM <- rac.during[order(-rac.during$Abundance),]
# max.abund <- max(rac.during.HM$Abundance)
# rac.during.HM$Abundance <- (rac.during.HM$Abundance)/max.abund*100
# rac.during.HM$hit <- gsub("_.*", "", rac.during$hit)
# barplot(rac.during.HM$Abundance[1:N], las=2, names.arg=rac.during.HM$hit[1:N],space=0,col=clade.colors[c(3,4,3,1,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # High Disturbance: during
# rac.during.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.during.HVH)))
# rac.during.temp$otu<-rownames(rac.during.temp)
# colnames(rac.during.temp)[1] <- "Abundance"
# rac.during <- merge(rac.during.temp, data.frame(tax_table(phy97.f.rare.c.platy.during.HVH)), by="otu")
# rac.during.HVH <- rac.during[order(-rac.during$Abundance),]
# max.abund <- max(rac.during.HVH$Abundance)
# rac.during.HVH$Abundance <- (rac.during.HVH$Abundance)/max.abund*100
# rac.during.HVH$hit <- gsub("_.*", "", rac.during$hit)
# barplot(rac.during.HVH$Abundance[1:N], las=2, names.arg=rac.during.HVH$hit[1:N],space=0,col=clade.colors[c(3,1,2,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Low Disturbance: after
# rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.after.LVL)))
# rac.after.temp$otu<-rownames(rac.after.temp)
# colnames(rac.after.temp)[1] <- "Abundance"
# rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.platy.after.LVL)), by="otu")
# rac.after.LVL <- rac.after[order(-rac.after$Abundance),]
# max.abund <- max(rac.after.LVL$Abundance)
# rac.after.LVL$Abundance <- (rac.after.LVL$Abundance)/max.abund*100
# rac.after.LVL$hit <- gsub("_.*", "", rac.after$hit)
# barplot(rac.after.LVL$Abundance[1:N], las=2, names.arg=rac.after.LVL$hit[1:N],space=0,col=clade.colors[c(2,2,3,4,3)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # Medium Disturbance: after
# rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.after)))
# rac.after.temp$otu<-rownames(rac.after.temp)
# colnames(rac.after.temp)[1] <- "Abundance"
# rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.platy.after.HM)), by="otu")
# rac.after.HM <- rac.after[order(-rac.after$Abundance),]
# max.abund <- max(rac.after.HM$Abundance)
# rac.after.HM$Abundance <- (rac.after.HM$Abundance)/max.abund*100
# rac.after.HM$hit <- gsub("_.*", "", rac.after$hit)
# barplot(rac.after.HM$Abundance[1:N], las=2, names.arg=rac.after.HM$hit[1:N],space=0,col=clade.colors[c(4,3,1,2,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)
# 
# # High Disturbance: after
# rac.after.temp <- data.frame(sort(taxa_sums(phy97.f.rare.c.platy.after.HVH)))
# rac.after.temp$otu<-rownames(rac.after.temp)
# colnames(rac.after.temp)[1] <- "Abundance"
# rac.after <- merge(rac.after.temp, data.frame(tax_table(phy97.f.rare.c.platy.after.HVH)), by="otu")
# rac.after.HVH <- rac.after[order(-rac.after$Abundance),]
# max.abund <- max(rac.after.HVH$Abundance)
# rac.after.HVH$Abundance <- (rac.after.HVH$Abundance)/max.abund*100
# rac.after.HVH$hit <- gsub("_.*", "", rac.after$hit)
# barplot(rac.after.HVH$Abundance[1:N], las=2, names.arg=rac.after.HVH$hit[1:N],space=0,col=clade.colors[c(2,1,3,1,2)], cex.names=1,mgp=c(0,.5,0),tck=-0.01)

dev.off()
