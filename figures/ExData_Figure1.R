# Reset graphical parameters
dev.off()

# Clear your environment
rm(list=ls())

# Load necessary packages
library(imager)


tiff(file="../figures/Extended Data/ExData_Figure1.tiff",width = 4, height = 6,units="in",res=300)

# Set both inner and outer margins to 0
par(oma=c(0,0,1.5,0),mar=c(0.1,0.1,0.1,0.1),xpd=TRUE)
# Setup layout for single top panel and 5 bottom panels
layout(matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow = TRUE), heights=c(1,1,1))

# Plot image panels using function created above
img_coral588_site15_July16 <- load.image('../figures/Extended Data/coral588_site15_IMG_17462_July16.jpg')
img_coral588_site15_March16 <- load.image('../figures/Extended Data/coral588_site15_IMG_0659_March16.jpg')
img_coral588_site15_XX <- load.image('')
img_coral850_site34_July15 <- load.image('../figures/Extended Data/coral850_site34_IMG_5320_July15.jpg')
img_coral850_site34_March16 <- load.image('../figures/Extended Data/coral850_site34_IMG_0327_March16.jpg')
img_coral850_site34_XX <- load.image('')
img_coral741_site32_July15 <- load.image('../figures/Extended Data/coral741_site32_IMG_7412_July15.jpg')
img_coral741_site32_March16 <- load.image('../figures/Extended Data/coral741_site32_IMG_0030_March16.jpg')
img_coral741_site32_XX <- load.image('')

plot(img_coral588_site15_July16, axes=F)
mtext("Early El Niño (July 2015)",side=3,col="black",outer=TRUE,adj=0.03)
mtext("i",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral588_site15_March16, axes=F)
mtext("Late El Niño (March 2016)",side=3,col="black",outer=TRUE,adj=0.99)
mtext("ii",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral588_site15_XX, axes=F)
mtext("iii",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral850_site34_July15,axes=F)
mtext("iv",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral850_site34_March16, axes=F)
mtext("v",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral850_site34_XX, axes=F)
mtext("vi",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral741_site32_July15,axes=F)
mtext("vii",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral741_site32_March16, axes=F)
mtext("viii",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)
plot(img_coral741_site32_XX, axes=F)
mtext("ix",adj=0.05,padj=1.6,col="white",font=2) #,side=2, line=-0.5,cex=1,las=2,padj=-2)


dev.off()