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
logtrans <- as.POSIXct("2017-06-15 00:00:00", format="%Y-%m-%d %H:%M:%S")

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

##################################
summ_means <- metadata.SH.noFQ.AD %>% dplyr::group_by(field_season,Status) %>% dplyr::summarize(mean=mean(S.H),sd=sd(S.H),se=sd(S.H)/(sqrt(n())),S.H.log10.se=sd(S.H.log10)/(sqrt(n())),S.H.log10=mean(S.H.log10))
summ_means_df <- as.data.frame(summ_means)
summ_means_df$date <- as.POSIXct(KI2015b)
summ_means_df$date[summ_means_df$field_season == "KI2015b"] <- as.POSIXct(KI2015b)
summ_means_df$date[summ_means_df$field_season == "KI2015c"] <- as.POSIXct(KI2015c)
summ_means_df$date[summ_means_df$field_season == "KI2016a"] <- as.POSIXct(KI2016a)
summ_means_df$date[summ_means_df$field_season == "KI2016b"] <- as.POSIXct(KI2016b)
summ_means_df$date[summ_means_df$field_season == "KI2017a"] <- as.POSIXct(KI2017a)

summ_means_df$dom <- "D"
summ_means_df$dom[summ_means_df$field_season== "KI2015b" & summ_means_df$Status == "alive"] <- "C"
summ_means_df$dom[summ_means_df$field_season== "KI2015c" & summ_means_df$Status == "alive"] <- "C"


##################################
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
        axis.title = element_text(size=18)) +
  geom_point(aes(shape=Status, fill=dom),stroke=0,alpha=0.5, size=2) +
  scale_shape_manual(values=c(21,25),guide=FALSE) +
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  geom_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..), 
              span=.67, data = metadata.SH.noFQ.AD,level = 0.95) + 
  scale_colour_gradient2(low = C_col, high = D_col,mid="gray",midpoint = -1.57, 
                         name= scaletitle) + 
  ylab("") + xlab("") +
  scale_y_continuous(name="Symbiont:Host Ratio", limits=c(-2.4,-0.7),expand=c(0.01,0.01), breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18)) +
  scale_x_datetime(date_breaks = "2 months",date_labels = "%b",expand=c(0.01,0.01)) +
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, barwidth=10))+
  annotate("text",x=as.POSIXct("2016-08-05"), y =-2.25,label="C")+
  annotate("text",x=as.POSIXct("2017-05-15"), y =-2.25,label="D")+
  annotate("text",x=as.POSIXct("2015-11-29"), y =-0.8,label="El Niño",size=6)+
  annotate("text",x=KI2015b, y =-0.74,label="iii",size=4)+
  annotate("text",x=KI2015c, y =-0.74,label="iv",size=4)+
  annotate("text",x=KI2016a, y =-0.74,label="v",size=4)+
  annotate("text",x=KI2016b, y =-0.74,label="vi",size=4)+  
  annotate("text",x=KI2017a, y =-0.74,label="vii",size=4)+
  geom_vline(xintercept=as.numeric(firstDHW),linetype="dashed")+
  geom_vline(xintercept=as.numeric(lastDHW),linetype="dashed")+
  annotate(geom = "text", x = c(as.POSIXct("2015-06-15"),as.POSIXct("2016-01-01"),as.POSIXct("2017-01-01")), y = -10.6, label = unique(format(metadata.SH.noFQ.AD$date,"%Y")), size = 4)
# geom_hline(yintercept = -2,linetype="longdash", color="gray") + geom_text(aes(logtrans,-2,label = "0.01", vjust=1)) +
# geom_hline(yintercept = -1,linetype="longdash", color="gray") + geom_text(aes(logtrans,-1,label = "0.1", vjust=1)) +
# geom_hline(yintercept = -3,linetype="longdash", color="gray") + geom_text(aes(logtrans,-3,label = "0.001", vjust=1))
p1

# Text1 <- textGrob("2015")
# Text2 <- textGrob("2016")
# Text3 <- textGrob("2017")
# p1 <- p1 + annotation_custom(grob = Text1,  xmin = as.POSIXct("2015-06-15"), xmax = as.POSIXct("2015-06-15"), ymin = -12, ymax = -11) +
#     annotation_custom(grob = Text1,  xmin = as.POSIXct("2016-01-01"), xmax = as.POSIXct("2016-01-01"), ymin = -12, ymax = -11) +
#     annotation_custom(grob = Text1,  xmin = as.POSIXct("2017-01-01"), xmax = as.POSIXct("2017-01-01"), ymin = -12, ymax = -11)
# 
# gg_table <- ggplot_gtable(ggplot_build(p1))
# gg_table$layout$clip[gg_table$layout$name=="panel"] <- "off"
# grid.draw(gg_table)

p1

p2 <- p1 +  geom_line(linetype="dashed",color="gray")
p2

scaletitle2 <- expression(paste(" Dominant ", italic("Symbiodinium"), " Clade"))
p3 <- ggplot(aes(y = D.PaxC.log10, x = C.PaxC.log10, color=dom, shape=Status), 
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
                     values = c(21,25),labels=c("Alive   ","Dead   ")) + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), 
                     labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18),
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log10),
                              max(metadata.SH.noFQ.AD$C.PaxC.log10,metadata.SH.noFQ.AD$D.PaxC.log10)),
                     name="Clade C Abundance (log(S:H))") +  
  scale_y_continuous(expand=c(0.01,0.01), 
                     breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), 
                     labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18),
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log10),max(metadata.SH.noFQ.AD$C.PaxC.log10,metadata.SH.noFQ.AD$D.PaxC.log10)),
                     name="Clade D Abundance (log(S:H))") +
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

# Open a jpg image
jpeg(file="figures/Figure3_qpcr_Aonly.jpg",width = 7.2, height = 6,units="in",res=300)
p1
dev.off()

# Open a jpg image
jpeg(file="figures/Figure3_qpcr2_Aonly.jpg",width = 7.2, height = 6,units="in",res=300)
p2
dev.off()

# Coral 1024
ggplot(aes(y = C.PaxC.log10, x = D.PaxC.log10,color=Year_Pre_Post), data = test) + geom_point()
ggplot(aes(y = C.PaxC.log10, x = D.PaxC.log10,color=Year_Pre_Post), data = test) + geom_point() + scale_x_continuous(limits=c(-17,-2)) + scale_y_continuous(limits=c(-17,-2))

timecols <- c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c")
p4 <- ggplot(aes(y = D.PaxC.log10, x = C.PaxC.log10, color=Status, fill=field_season), 
             data = metadata.SH.noFQ.AD) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.21,0.27), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill="white", color="black"),
        legend.key = element_blank(),
        legend.spacing.y = unit(0, "cm"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18)) + 
  geom_point(stroke=1, alpha=0.7, size=5,shape=21)  +
  scale_color_manual(values=c("lightgray","black"),name="Status") +
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)]),guide=FALSE) +
  # scale_shape_manual(name="       Coral Status Post El Niño  ",
  #                    values = c(21,25),labels=c("Alive   ","Dead   ")) + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log10),
                              max(metadata.SH.noFQ.AD$C.PaxC.log10,metadata.SH.noFQ.AD$D.PaxC.log10)),
                     name="Clade C Abundance (log(S:H))") +  
  scale_y_continuous(expand=c(0.01,0.01), 
                     limits=c(min(metadata.SH.noFQ.AD$C.PaxC.log10),max(metadata.SH.noFQ.AD$C.PaxC.log10,metadata.SH.noFQ.AD$D.PaxC.log10)),
                     name="Clade D Abundance (log(S:H))") +
  geom_abline(slope=1,intercept=0) + 
  guides(colour = guide_legend(title.position = "top",keywidth = 2.75, keyheight = 1.5,override.aes = list(size=8)))+ 
  # guides(shape = guide_legend(title.position = "top",keywidth = 3.25, keyheight = 1.5,override.aes = list(size=8)))+
  annotate("text",x=-14.5, y =-13.8,label="Coral is dominated by clade D",angle=40,color=D_col)+
  annotate("text",x=-14, y =-14.5,label="Coral is dominated by clade C",angle=40, color= C_col)+   geom_hline(yintercept = -2,linetype="longdash", color="gray") + 
  geom_text(aes(-2,-2,label = "0.01", vjust=1)) +
  geom_hline(yintercept = -3,linetype="longdash", color="gray") + 
  geom_text(aes(-3,-3, label = "0.001", vjust=1)) +  
  geom_hline(yintercept = -4,linetype="longdash", color="gray") + 
  geom_text(aes(-4,-4, label = "0.0001", vjust=1)) +
  geom_hline(yintercept = -5,linetype="longdash", color="gray") + 
  geom_text(aes(-5,-5, label = "0.00001", vjust=1)) +
  geom_hline(yintercept = -6,linetype="longdash", color="gray") + 
  geom_text(aes(-6,-6, label = "0.000001", vjust=1)) +
  geom_vline(xintercept = -2,linetype="longdash", color="gray") + 
  geom_vline(xintercept = -3,linetype="longdash", color="gray") +
  geom_vline(xintercept = -4,linetype="longdash", color="gray") +
  geom_vline(xintercept = -5,linetype="longdash", color="gray") +
  geom_vline(xintercept = -6,linetype="longdash", color="gray")
p4

jpeg(file="figures/Figure3_qpcr_b_option2.jpg",width = 7.2, height = 6,units="in",res=300)
p4
dev.off()

##########################
######## Includes 3+ timepoints if they are not only the last 3 timepoints) #########


p1r <- ggplot(aes(y = S.H.log10, x = date,group=coral_tag), data = metadata.SH.noFQ.AD.3plus)+
  geom_rect(aes(xmin = firstDHW, xmax = lastDHW, ymin = -Inf, ymax = Inf),fill = stress_col, alpha = 0.002) +
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
  scale_shape_manual(values=c(21,25),guide=FALSE) +
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  geom_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..), 
              span=.67, data = metadata.SH.noFQ.AD.3plus,level = 0.95) + 
  scale_colour_gradient2(low = C_col, high = D_col,mid="gray",midpoint = -1.57, 
                         name= scaletitle) + 
  ylab("") + xlab("") +
  scale_y_continuous(name="Symbiont:Host Ratio", limits=c(-2.4,-0.7),expand=c(0.01,0.01), breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18)) +
  scale_x_datetime(date_breaks = "2 months",date_labels = "%b",expand=c(0.01,0.01)) +
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, barwidth=10))+
  annotate("text",x=as.POSIXct("2016-08-05"), y =-2.25,label="C")+
  annotate("text",x=as.POSIXct("2017-05-15"), y =-2.25,label="D")+
  annotate("text",x=as.POSIXct("2015-11-29"), y =-0.8,label="El Niño",size=6)+
  annotate("text",x=KI2015b, y =-0.74,label="iii",size=4)+
  annotate("text",x=KI2015c, y =-0.74,label="iv",size=4)+
  annotate("text",x=KI2016a, y =-0.74,label="v",size=4)+
  annotate("text",x=KI2016b, y =-0.74,label="vi",size=4)+  
  annotate("text",x=KI2017a, y =-0.74,label="vii",size=4)+
  geom_vline(xintercept=as.numeric(firstDHW),linetype="dashed")+
  geom_vline(xintercept=as.numeric(lastDHW),linetype="dashed")+
  annotate(geom = "text", x = c(as.POSIXct("2015-06-15"),as.POSIXct("2016-01-01"),as.POSIXct("2017-01-01")), y = -10.6, label = unique(format(metadata.SH.noFQ.AD.3plus$date,"%Y")), size = 4)
p1r

p2r <- p1r +  geom_line(linetype="dashed",color="gray")
p2r

scaletitle2 <- expression(paste(" Dominant ", italic("Symbiodinium"), " Clade"))
p3r <- ggplot(aes(y = D.PaxC.log10, x = C.PaxC.log10, color=dom, shape=Status), 
              data = metadata.SH.noFQ.AD.3plus) +
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
                     values = c(21,25),labels=c("Alive   ","Dead   ")) + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), 
                     labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18),
                     limits=c(min(metadata.SH.noFQ.AD.3plus$C.PaxC.log10),
                              max(metadata.SH.noFQ.AD.3plus$C.PaxC.log10,metadata.SH.noFQ.AD.3plus$D.PaxC.log10)),
                     name="Clade C Abundance (log(S:H))") +  
  scale_y_continuous(expand=c(0.01,0.01), 
                     breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), 
                     labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18),
                     limits=c(min(metadata.SH.noFQ.AD.3plus$C.PaxC.log10),max(metadata.SH.noFQ.AD.3plus$C.PaxC.log10,metadata.SH.noFQ.AD.3plus$D.PaxC.log10)),
                     name="Clade D Abundance (log(S:H))") +
  geom_abline(slope=1,intercept=0) + 
  guides(colour = guide_legend(title.position = "top",keywidth = 2.75, keyheight = 1.5,override.aes = list(size=8)))+ 
  guides(shape = guide_legend(title.position = "top",keywidth = 3.25, keyheight = 1.5,override.aes = list(size=8)))+
  annotate("text",x=-14.5, y =-13.8,label="Coral is dominated by clade D",angle=40,color=D_col)+
  annotate("text",x=-14, y =-14.5,label="Coral is dominated by clade C",angle=40, color= C_col)


grid.arrange(p1r,p3r,nrow=1,ncol=2)

# Open a jpg image
jpeg(file="figures/Figure3_qpcr_repeatcoloniesonly.jpg",width = 14.4, height = 6,units="in",res=300)
grid.arrange(p1r,p3r,nrow=1,ncol=2)
dev.off()

# Open a jpg image
jpeg(file="figures/Figure3_qpcr2_repeatcoloniesonly.jpg",width = 14.4, height = 6,units="in",res=300)
grid.arrange(p2r,p3r,nrow=1,ncol=2)
dev.off()

# Open a jpg image
jpeg(file="figures/Figure3_qpcr_Aonly_repeatcoloniesonly.jpg",width = 7.2, height = 6,units="in",res=300)
p1r
dev.off()

pd <- position_dodge(1000000)


metadata.SH.noFQ.AD$CtoD2 <- metadata.SH.noFQ.AD$C.PaxC / metadata.SH.noFQ.AD$D.PaxC
metadata.SH.noFQ.AD$CtoD2[which(metadata.SH.noFQ.AD$CtoD2>2)] <- 2

metadata.SH.noFQ.A$CtoD2 <- metadata.SH.noFQ.A$C.PaxC / metadata.SH.noFQ.A$D.PaxC
metadata.SH.noFQ.A$CtoD2[which(metadata.SH.noFQ.A$CtoD2>2)] <- 2

metadata.SH.noFQ.D <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$Status=="dead"),]





ggplot() + stat_smooth(aes(y = CtoD2, x = date, group=Status, color=..y..,outfit=fit<<-..y..,outfit=xfit<<-..x..,outfit=sefit<<-..se..),
                       span=.67, data = metadata.SH.noFQ.A,level = 0.95) +   
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = 0.555, name= scaletitle)

ggplot() + stat_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..,outfit=fit2<<-..y..,outfit=xfit2<<-..x..),
                       span=.67, data = metadata.SH.noFQ.A,level = 0.95) +   
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = 0.555, name= scaletitle)
###
ggplot() + stat_smooth(aes(y = CtoD2, x = date, group=Status, color=..y..,outfit=fitD<<-..y..,outfit=xfitD<<-..x..),span=.67, data = metadata.SH.noFQ.D,level = 0.95) +   
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = 0.555, name= scaletitle)

ggplot() + stat_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..,outfit=fit2D<<-..y..,outfit=xfit2D<<-..x..,outfit=sefitD<<-..se..),span=.67, data = metadata.SH.noFQ.D,level = 0.95) +   
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = 0.555, name= scaletitle)

xfit2 <- as.POSIXct(xfit2, origin="1970-01-01")
xfit2D <- as.POSIXct(xfit2D, origin="1970-01-01")

df1 <- cbind(xfit2,as.data.frame(fit2),as.data.frame(fit),as.data.frame(sefit))
df1D <- cbind(xfit2D,as.data.frame(fit2D),as.data.frame(fitD),as.data.frame(sefitD))

ggplot()+
  geom_line(aes(x=xfit2,y=fit2,color=fit),data=df1) + 
  geom_line(aes(x=xfit2D,y=fit2D,color=fitD),data=df1D)+   
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = 0.555, name= scaletitle)


#+ geom_smooth(aes(y = S.H.log10, x = date, group=Status), span=.67, data = metadata.SH.noFQ.A, level = 0.95)+ scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = -1.8, name= scaletitle)
# color=fit$Var1
# span=.67,level = 0.95, data = metadata.SH.noFQ.A)+   
#   scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = 0.555, name= scaletitle)


p5 <- ggplot(aes(y = S.H.log10, x = date, color=CtoD), data = metadata.SH.noFQ.AD)+
  geom_rect(aes(xmin = firstDHW, xmax = lastDHW, ymin = -Inf, ymax = Inf),
            fill = stress_col, alpha = 0.002) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = c(0.75,0.15), 
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18)) +
  # geom_point(aes(shape=Status, fill=dom),stroke=0,alpha=0.5, size=2) +
  scale_shape_manual(values=c(21,25),guide=FALSE) +
  scale_fill_manual(values=c(C_col,D_col),guide=FALSE) +
  geom_smooth(aes(group=Status,color="CtoD"), 
              span=.67,level = 0.95) + 
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",midpoint = 1,
                         name= scaletitle)
+
  # ylab("") + xlab("") +
  # scale_y_continuous(name="Symbiont:Host Ratio", limits=c(-2.4,-0.7),expand=c(0.01,0.01), breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18)) +
  # scale_x_datetime(date_breaks = "2 months",date_labels = "%b",expand=c(0.01,0.01)) +
  # guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, barwidth=10))+
  # annotate("text",x=as.POSIXct("2016-08-05"), y =-2.25,label="C")+
  # annotate("text",x=as.POSIXct("2017-05-15"), y =-2.25,label="D")+
  # annotate("text",x=as.POSIXct("2015-11-29"), y =-0.8,label="El Niño",size=6)+
  # annotate("text",x=KI2015b, y =-0.74,label="iii",size=4)+
  # annotate("text",x=KI2015c, y =-0.74,label="iv",size=4)+
  # annotate("text",x=KI2016a, y =-0.74,label="v",size=4)+
  # annotate("text",x=KI2016b, y =-0.74,label="vi",size=4)+  
# annotate("text",x=KI2017a, y =-0.74,label="vii",size=4)+
# geom_vline(xintercept=as.numeric(firstDHW),linetype="dashed")+
# geom_vline(xintercept=as.numeric(lastDHW),linetype="dashed")+
# annotate(geom = "text", x = c(as.POSIXct("2015-06-15"),as.POSIXct("2016-01-01"),as.POSIXct("2017-01-01")), y = -10.6, label = unique(format(metadata.SH.noFQ.AD$date,"%Y")), size = 4)+
# geom_point(aes(shape=Status, fill=dom, x=as.POSIXct(date, origin="1970-01-01"), group=Status), data=summ_means_df,position=pd)+ 
#   geom_errorbar(aes(x=as.POSIXct(date, origin="1970-01-01"),group=Status,ymin=S.H.log10-S.H.log10.se,ymax=S.H.log10+S.H.log10.se, width=1000000), data=summ_means_df,position = pd)

p5


p6 <- ggplot()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        legend.position = c(0.75,0.15), 
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18)) +
  ylab("") + xlab("") +
  scale_y_continuous(name="Symbiont:Host Ratio", limits=c(-2.4,-0.7),expand=c(0.01,0.01), breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18)) +
  scale_x_datetime(date_breaks = "2 months",date_labels = "%b",expand=c(0.01,0.01)) +
  geom_rect(aes(xmin = firstDHW, xmax = lastDHW, ymin = -2.4, ymax = -0.7),
            fill = stress_col, alpha = 0.002) +
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, barwidth=10))+
  annotate("text",x=as.POSIXct("2016-08-05"), y =-2.25,label="C")+
  annotate("text",x=as.POSIXct("2017-05-15"), y =-2.25,label="D")+
  annotate("text",x=as.POSIXct("2015-11-29"), y =-0.8,label="El Niño",size=6)+
  annotate("text",x=KI2015b, y =-0.74,label="iii",size=4)+
  annotate("text",x=KI2015c, y =-0.74,label="iv",size=4)+
  annotate("text",x=KI2016a, y =-0.74,label="v",size=4)+
  annotate("text",x=KI2016b, y =-0.74,label="vi",size=4)+  
  annotate("text",x=KI2017a, y =-0.74,label="vii",size=4)+
  geom_vline(xintercept=as.numeric(firstDHW),linetype="dashed")+
  geom_vline(xintercept=as.numeric(lastDHW),linetype="dashed")+
  annotate(geom = "text", x = c(as.POSIXct("2015-06-15"),as.POSIXct("2016-01-01"),as.POSIXct("2017-01-01")), y = -10.6, label = unique(format(metadata.SH.noFQ.AD$date,"%Y")), size = 4)+
  geom_ribbon(aes(x=xfit2,ymin=fit2-sefit,ymax=fit2+sefit), data=df1, alpha=0.3)+
  geom_ribbon(aes(x=xfit2D,ymin=fit2D-sefitD,ymax=fit2D+sefitD), data=df1D, alpha=0.3)+
  geom_line(aes(x=xfit2,y=fit2,color=fit),data=df1) + 
  geom_line(aes(x=xfit2D,y=fit2D,color=fitD),data=df1D)+   
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",
                         midpoint = 0.555, name= scaletitle) +
  geom_point(aes(y=S.H.log10, shape=Status, fill=dom, x=as.POSIXct(date, origin="1970-01-01"), 
                 group=Status), data=summ_means_df,position=pd) + 
  geom_errorbar(aes(x=as.POSIXct(date, origin="1970-01-01"),group=Status,ymin=S.H.log10-S.H.log10.se,ymax=S.H.log10+S.H.log10.se, width=1000000), data=summ_means_df,position = pd)
p6

