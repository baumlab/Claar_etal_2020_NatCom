library(ggplot2)
library(gridExtra)
library(grid)

rm(list=ls())

load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")
load("data/temperature/KI_SB_temp_DHW.RData")

metadata.SH.AD <- metadata.SH[which(metadata.SH$Status != "UK" & metadata.SH$Status != "gone" & metadata.SH$Status != "dead_or_gone"),]
metadata.SH.noFQ.AD <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone"),]
metadata.SH.noFQ.A <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone" & metadata.SH.noFQ$Status != "dead"),]


# Set up and format data
# Set a start and end date for plotting
startdate <- as.POSIXct("2014-08-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
enddate <- as.POSIXct("2016-11-19 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
# Truncate the data from startdate to enddate
KI_heat <- KI_allsites_DHW[which(KI_allsites_DHW$xi3>startdate),]
KI_heat <- KI_heat[which(KI_heat$xi3<enddate),]
DHW_positive <- which(KI_heat$DHW > 0)
firstDHW <- KI_heat$xi3[min(DHW_positive)]
lastDHW <- KI_heat$xi3[max(DHW_positive)]


KI2014 <- as.POSIXct("2014-09-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-08 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2017a <- as.POSIXct("2017-07-15 00:00:00", format="%Y-%m-%d %H:%M:%S")

## To look at only corals that were sampled 3+ times (but exclude those with only 3 after timepoints i.e. no before or during)
metadata.SH.noFQ.AD.3plus.temp <- metadata.SH.noFQ.AD[which(sort(table(metadata.SH.noFQ.AD$coral_tag)) > 3),]
t1 <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$coral_tag =="594"),]
t2 <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$coral_tag =="612"),]
t3 <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$coral_tag =="797"),]
t4 <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$coral_tag =="906"),]
t5 <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$coral_tag =="909"),]
t6 <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$coral_tag =="919"),]
t7 <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$coral_tag =="925"),]
dead <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$Status =="dead"),]

metadata.SH.noFQ.AD.3plus <- rbind(metadata.SH.noFQ.AD.3plus.temp, t1, t2, t3, t4, t5, t6, t7, dead)

sort(table(metadata.SH.noFQ.AD.3plus$coral_tag))

################## DANGER!! Do this to see what if you only use those with 4+ timepoints (includes 3+ timepoints if they are not only the last 3 timepoints) ####################
# Always turn this back off (==comment it out) if not actively testing:
metadata.SH.noFQ.AD <- metadata.SH.noFQ.AD.3plus

C_col <- "#2166ac"
D_col <- "#b2182b"
stress_col <- "#d95f02"
stress_col <- "darkgoldenrod4"
stress_col <- "burlywood4"

scaletitle <- expression(paste("Dominant ", italic("Symbiodinium"), " Clade"))
p1 <- ggplot(aes(y = S.H.log10, x = date,group=coral_tag), data = metadata.SH.noFQ.AD)+
  geom_rect(aes(xmin = firstDHW, xmax = lastDHW, ymin = -Inf, ymax = Inf),fill = stress_col, alpha = 0.002) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = c(0.75,0.15), 
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18),
        axis.line = element_line(color="black")) +
  geom_point(aes(shape=Status, fill=dom),stroke=0,alpha=0.5, size=2) +
  scale_shape_manual(values=c(21,7),guide=FALSE) +
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  geom_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..), 
              span=.67, data = metadata.SH.noFQ.AD,level = 0.95) + 
  scale_colour_gradient2(low = C_col, high = D_col,mid="gray",midpoint = -2.25, 
                         name= scaletitle) + 
  ylab("") + xlab("") +
  scale_y_continuous(name="log(Symbiont:Host)", limits=c(-3.5,-1),expand=c(0.01,0.01)) +
  scale_x_datetime(date_breaks = "2 months",date_labels = "%b",expand=c(0.01,0.01)) +
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, barwidth=10))+
  annotate("text",x=as.POSIXct("2016-09-05"), y =-3.175,label="C")+
  annotate("text",x=as.POSIXct("2017-04-15"), y =-3.175,label="D")+
  annotate("text",x=as.POSIXct("2015-11-29"), y =-1.25,label="El Niño",size=6)+
  # annotate("text",x=KI2015b, y =-1,label="iii",size=4)+
  # annotate("text",x=KI2015c, y =-1,label="iv",size=4)+
  # annotate("text",x=KI2016a, y =-1,label="v",size=4)+
  # annotate("text",x=KI2016b, y =-1,label="vi",size=4)+  
  # annotate("text",x=KI2017a, y =-1,label="vii",size=4)+
  geom_vline(xintercept=as.numeric(firstDHW),linetype="dashed")+
  geom_vline(xintercept=as.numeric(lastDHW),linetype="dashed")+ 
  annotate(geom = "text", x = c(as.POSIXct("2015-06-15"),as.POSIXct("2016-01-01"),as.POSIXct("2017-01-01")), y = -10.6, label = unique(format(metadata.SH.noFQ.AD$date,"%Y")), size = 4)


p1

# Open a jpg image
jpeg(file="figures/Figure3a_qpcr_ECRS.jpg",width = 9, height = 6,units="in",res=300)
p1
dev.off()



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
  scale_shape_manual(name="       Coral Status Post El NiÃ±o  ",
                     values = c(21,7),labels=c("Alive   ","Dead   ")) + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),
                              max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log)),
                     name="Clade C Abundance (ln(S:H))") +  
  scale_y_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log),max(metadata.SH.noFQ.AD$C.PaxC.log,metadata.SH.noFQ.AD$D.PaxC.log)),
                     name="Clade D Abundance (ln(S:H))") +
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

# Coral 1024
ggplot(aes(y = C.PaxC.log, x = D.PaxC.log,color=Year_Pre_Post), data = test) + geom_point()
ggplot(aes(y = C.PaxC.log, x = D.PaxC.log,color=Year_Pre_Post), data = test) + geom_point() + scale_x_continuous(limits=c(-17,-2)) + scale_y_continuous(limits=c(-17,-2))