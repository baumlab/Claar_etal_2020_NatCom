library(ggplot2)
library(gridExtra)


rm(list=ls())

load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")

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
        panel.background = element_blank(),
        legend.position = c(0.75,0.15), 
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18)) +
  geom_point(aes(shape=Status, fill=dom),stroke=0,alpha=0.5, size=2) +
  scale_shape_manual(values=c(21,7),guide=FALSE) +
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  geom_smooth(aes(y = S.H.log, x = date, group=Status, color=..y..), 
              span=.67, data = metadata.SH.noFQ.AD,level = 0.9999) + 
  scale_colour_gradient2(low = C_col, high = D_col,mid="gray",midpoint = -5, 
                         name= scaletitle) + 
  ylab("Symbiont:Host Ratio") + xlab("") +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b",expand=c(0.01,0.01)) +
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, barwidth=10))+
  annotate("text",x=as.POSIXct("2016-08-05"), y =-10.25,label="C")+
  annotate("text",x=as.POSIXct("2017-05-15"), y =-10.25,label="D")+
  annotate("text",x=as.POSIXct("2015-11-29"), y =-1.5,label="El Niño",size=6)+
  geom_vline(xintercept=as.numeric(as.POSIXct("2015-07-01")),linetype="dashed")+
  geom_vline(xintercept=as.numeric(as.POSIXct("2016-05-01")),linetype="dashed")

p1

p2 <- p1 +  geom_line(linetype="dashed",color="gray")
p2

scaletitle2 <- expression(paste(" Dominant ", italic("Symbiodinium"), " Clade"))
p3 <- ggplot(aes(y = D.PaxC.log, x = C.PaxC.log, color=dom, shape=Status), 
             data = metadata.SH.noFQ.AD) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.81,0.87), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill="white", color="black"),
        legend.key = element_blank(),
        legend.spacing.y = unit(0, "cm"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18)) + 
  geom_point(aes(shape=Status,fill=dom),stroke=0, alpha=0.7, size=2)  +
  scale_color_manual(values=c(C_col,D_col),name=scaletitle2, labels=c("clade C", "clade D")) + 
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  scale_shape_manual(name="       Coral Status Post El Niño  ",
                     values = c(21,7),labels=c("Alive   ","Dead   ")) + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),
                              max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log)),
                     name="Clade C Abundance (log S:H)") +  
  scale_y_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log)),
                     name="Clade D Abundance (log S:H)") +
  geom_abline(slope=1,intercept=0) + 
  guides(colour = guide_legend(title.position = "top",keywidth = 2.75, keyheight = 1.5,override.aes = list(size=8)))+ 
  guides(shape = guide_legend(title.position = "top",keywidth = 3.25, keyheight = 1.5,override.aes = list(size=8)))+
  annotate("text",x=-14.5, y =-13.8,label="Coral is dominated by clade D",angle=40,color=D_col)+
  annotate("text",x=-14, y =-14.5,label="Coral is dominated by clade C",angle=40, color= C_col)


grid.arrange(p1,p3,nrow=1,ncol=2)


# Open a jpg image
jpeg(file="figures/Figure3_qpcr.jpg",width = 14.4, height = 6,units="in",res=300)
grid.arrange(p1,p3,nrow=1,ncol=2)
dev.off()

# Open a jpg image
jpeg(file="figures/Figure3_qpcr2.jpg",width = 14.4, height = 6,units="in",res=300)
grid.arrange(p2,p3,nrow=1,ncol=2)
dev.off()

+
  geom_ribbon(aes(x=seq(-16,-4,0.1), ymin=y, ymax=(-1)), fill="blue") # not working right now
  
x=seq(-16,-4,0.1)
y=seq(-16,-4,0.1)
dat = data.frame(x=x, y=y)
dat$ymax=max(dat$y)

ggplot(dat)+geom_ribbon(aes(x, ymin=y, ymax=ymax), fill="blue",data = dat)

C_col <- "darkgray"
D_col <- "gray40"

p3.2 <- ggplot(aes(y = D.PaxC.log, x = C.PaxC.log, color=Year_Pre_Post, shape=Status), 
             data = metadata.SH.noFQ.AD) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom", #c(0.81,0.87), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill="white", color="black"),
        legend.key = element_blank(),
        legend.spacing.y = unit(0, "cm"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18)) + 
  geom_point(aes(shape=Status,fill=dom),stroke=0, alpha=0.7, size=3)  +
  scale_color_manual(values=c("red","darkorange","green","purple","turquoise"),name=scaletitle2) + 
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  scale_shape_manual(name="       Coral Status (March 2016)  ",
                     values = c(21,7),labels=c("Alive   ","Dead   ")) + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),
                              max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log)),
                     name="Clade C Abundance (log S:H)") +  
  scale_y_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log)),
                     name="Clade D Abundance (log S:H)") +
  geom_abline(slope=1,intercept=0) + 
  guides(colour = guide_legend(title.position = "top",keywidth = 2.75, keyheight = 1.5))+ 
  guides(shape = guide_legend(title.position = "top",keywidth = 3.25, keyheight = 1.5))+
  annotate("text",x=-14.5, y =-13.8,label="clade D",angle=40,color=D_col)+
  annotate("text",x=-14, y =-14.5,label="clade C",angle=40, color= C_col)

str(metadata.SH.noFQ.AD$Year_Pre_Post)
metadata.SH.noFQ.AD.Ordered <- metadata.SH.noFQ.AD[order(metadata.SH.noFQ.AD$Year_Pre_Post),]
p4 <- p3.2 + geom_path(aes(group=coral_tag,color=Year_Pre_Post),arrow=arrow(length=unit(0.30,"cm"), type = "closed"), data = metadata.SH.noFQ.AD.Ordered)
p4

p4.2 <- p4 + facet_wrap(~Year_Pre_Post)
p4.2

p4.3 <- p4 + scale_color_manual(values=c("red","darkorange","lightgray","lightgray","lightgray"),name=scaletitle2)
p4.3
