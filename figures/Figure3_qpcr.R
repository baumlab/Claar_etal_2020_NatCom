library(ggplot2)
library(gridExtra)


rm(list=ls())

load(file="../Coralphoto__Metadata/KI_Platy_metadataSH.RData")

metadata.SH.AD <- metadata.SH[which(metadata.SH$Status != "UK" & metadata.SH$Status != "gone" & metadata.SH$Status != "dead_or_gone"),]
metadata.SH.noFQ.AD <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone"),]
metadata.SH.noFQ.A <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone" & metadata.SH.noFQ$Status != "dead"),]

C_col <- "#2166ac"
D_col <- "#b2182b"
stress_col <- "#d95f02"
stress_col <- "darkgoldenrod4"
stress_col <- "burlywood4"

scaletitle <- expression(paste("Dominant ", italic("Symbiodinium"), " Clade"))
p1 <- ggplot(aes(y = S.H.log, x = date,group=coral_tag), data = metadata.SH.noFQ.AD)+
  geom_rect(aes(xmin = as.POSIXct("2015-07-01"), xmax = as.POSIXct("2016-05-01"), ymin = -Inf, ymax = Inf),fill = stress_col, alpha = 0.002) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = c(0.75,0.15), 
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18)) +
  geom_point(aes(shape=Status, fill=dom),stroke=0,alpha=0.5, size=1.7) +
  scale_shape_manual(values=c(21,4),guide=FALSE) +
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  geom_smooth(aes(y = S.H.log, x = date, group=Status, color=..y..), 
              span=.67, data = metadata.SH.noFQ.AD,level = 0.9999) + 
  scale_colour_gradient2(low = C_col, high = D_col,mid="gray",midpoint = -5, 
                         name= scaletitle) + 
  ylab("Symbiont:Host Ratio") + xlab("") +
  scale_x_datetime(date_breaks = "4 months",date_labels = "%b-%Y",expand=c(0.01,0.01)) +
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, barwidth=10))+
  annotate("text",x=as.POSIXct("2016-08-05"), y =-10.25,label="C")+
  annotate("text",x=as.POSIXct("2017-05-15"), y =-10.25,label="D")+
  annotate("text",x=as.POSIXct("2015-11-29"), y =-1.5,label="El Nino",size=6)+
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-07-01")),linetype="dashed")+
  geom_vline(xintercept=as.numeric(as.POSIXct("2016-05-01")),linetype="dashed")

p2 <- p1 +  geom_line(linetype="dashed",color="gray")
p2

p3 <- ggplot(aes(y = D.PaxC.log, x = C.PaxC.log, color=dom, shape=Status), 
             data = metadata.SH.noFQ.AD) + 
  geom_point()  +
  scale_color_manual(values=c(C_col,D_col)) + 
  scale_x_continuous(expand=c(0.01,0.01), limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log))) +  
  scale_y_continuous(expand=c(0.01,0.01), limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log))) +
  geom_abline(slope=1,intercept=0) 


grid.arrange(p1,p3,nrow=1,ncol=2)
