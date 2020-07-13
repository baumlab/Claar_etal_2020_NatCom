# Load neccessary libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

# Load necessary data
load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")

# Set colors
C_col <- "#2166ac"
D_col <- "#b2182b"
stress_col <- "burlywood4"
timecols <- c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c")

metadata.SH.noFQ$C.PaxC.log10[which(metadata.SH.noFQ$D.PaxC.log10 < -7)] <- -3.2
metadata.SH.noFQ$D.PaxC.log10[which(metadata.SH.noFQ$C.PaxC.log10 < -7)] <- -3.2
scaletitle <- "Field Season"

# Make plot
p1 <- ggplot(aes(y = D.PaxC.log10, x = C.PaxC.log10, color=Year_Pre_Post), 
               data = metadata.SH.noFQ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "black", fill = NA),
        legend.position = "none",
        legend.direction = "vertical",
        legend.background = element_rect(fill="white", color="black"),
        legend.key = element_blank(),
        legend.spacing.y = unit(0, "cm"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18),
        axis.text.x = element_text(angle = 90,vjust=0)) + 
  geom_point(aes(fill=field_season),stroke=0, alpha=0.7, size=3)  +
  scale_color_manual(values=c(timecols[c(1,4,5,3,2)]),name=scaletitle, 
                     labels= c("May 2015", "July 2015", "March 2016", 
                               "November 2016", "July 2017")) + 
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)]),guide=FALSE) +
  scale_shape_manual(name="       Coral Status (March 2016)  ",
                     values = c(21,7),labels=c("Alive   ","Dead   ")) + 
  scale_x_continuous(expand=c(0.01,0.01), 
                     breaks = c(-3,-2,-1.7,-1.398,-1.097,-0.796), 
                     labels = c(0.001,0.010,0.020,0.040,0.080,0.160),
                     limits=c(min(metadata.SH.noFQ$C.PaxC.log10),
                              max(metadata.SH.noFQ$C.PaxC.log10,
                                  metadata.SH.noFQ$D.PaxC.log10)),
                     name="Clade C Abundance (log S:H)") +  
  scale_y_continuous(expand=c(0.01,0.01), 
                     breaks = c(-3,-2,-1.7,-1.398,-1.097,-0.796), 
                     labels = c(0.001,0.010,0.020,0.040,0.080,0.160),
                     limits=c(min(metadata.SH.noFQ$C.PaxC.log10),
                              max(metadata.SH.noFQ$C.PaxC.log10,
                                  metadata.SH.noFQ$D.PaxC.log10)),
                     name="Clade D Abundance (log S:H)") +
  geom_abline(slope=1,intercept=0) + 
  annotate("text",x=-14.5, y =-13.8,label="clade D",angle=40,color=D_col)+
  annotate("text",x=-14, y =-14.5,label="clade C",angle=40, color= C_col)

str(metadata.SH.noFQ$Year_Pre_Post)
metadata.SH.noFQ.Ordered <- metadata.SH.noFQ[order(metadata.SH.noFQ$Year_Pre_Post),]
p4 <- p1 + geom_path(aes(group=coral_tag,color=Year_Pre_Post),
                     arrow=arrow(length=unit(0.30,"cm"), type = "closed"), 
                     data = metadata.SH.noFQ.Ordered)
p4

metadata.SH.noFQ.Ordered.15bto15c <- metadata.SH.noFQ.Ordered[which(metadata.SH.noFQ.Ordered$field_season == "KI2015b" | metadata.SH.noFQ.Ordered$field_season == "KI2015c"),]
metadata.SH.noFQ.Ordered.15cto16a <- metadata.SH.noFQ.Ordered[which(metadata.SH.noFQ.Ordered$field_season == "KI2015c" | metadata.SH.noFQ.Ordered$field_season == "KI2016a"),]
metadata.SH.noFQ.Ordered.16ato16b <- metadata.SH.noFQ.Ordered[which(metadata.SH.noFQ.Ordered$field_season == "KI2016a" | metadata.SH.noFQ.Ordered$field_season == "KI2016b"),]
metadata.SH.noFQ.Ordered.16bto17a <- metadata.SH.noFQ.Ordered[which(metadata.SH.noFQ.Ordered$field_season == "KI2016b" | metadata.SH.noFQ.Ordered$field_season == "KI2017a"),]

p5 <- p1 + 
  geom_path(aes(group=coral_tag,color=Year_Pre_Post),
            arrow=arrow(length=unit(0.30,"cm"), type = "closed"), 
            data = metadata.SH.noFQ.Ordered.15bto15c) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c(timecols[c(1,4)],"white","white","white")) + 
  scale_fill_manual(values=c(timecols[c(1,4)],"white","white","white")) +
  theme(axis.title.x=element_blank())
p5.2 <- p1 + 
  geom_path(aes(group=coral_tag,color=Year_Pre_Post),
            arrow=arrow(length=unit(0.30,"cm"), type = "closed"), 
            data = metadata.SH.noFQ.Ordered.15cto16a) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c("white",timecols[c(4,5)],"white","white")) + 
  scale_fill_manual(values=c("white",timecols[c(4,5)],"white","white")) +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())
p5.3 <- p1 + 
  geom_path(aes(group=coral_tag,color=Year_Pre_Post),
            arrow=arrow(length=unit(0.30,"cm"), type = "closed"), 
            data = metadata.SH.noFQ.Ordered.16ato16b) + 
  theme(legend.position="none") + 
  scale_color_manual(values=c("white","white",timecols[c(5,3)],"white")) + 
  scale_fill_manual(values=c("white","white",timecols[c(5,3)],"white"))
p5.4 <- p1 + 
  geom_path(aes(group=coral_tag,color=Year_Pre_Post),
            arrow=arrow(length=unit(0.30,"cm"), type = "closed"), 
            data = metadata.SH.noFQ.Ordered.16bto17a) + 
  theme(legend.position="none") +
  scale_color_manual(values=c("white","white","white",timecols[c(3,2)])) + 
  scale_fill_manual(values=c("white","white","white",timecols[c(3,2)])) +
  theme(axis.title.y=element_blank())

# Make figure
jpeg(file="figures/Extended_Data/Figure_S7.jpg",
     width=16, height=8,units="in", res=300)
grid.arrange(p5,p5.2,p4,p5.3,p5.4,nrow=2,ncol=3, 
             widths=c(1,1,2), layout_matrix = rbind(c(1,2,3),c(4,5,3)))
dev.off()

tiff(file="figures/Extended_Data/Figure_S7.tiff",
     width=16, height=8,units="in", res=300)
grid.arrange(p5,p5.2,p4,p5.3,p5.4,nrow=2,ncol=3, widths=c(1,1,2), 
             layout_matrix = rbind(c(1,2,3),c(4,5,3)))
dev.off()

pdf(file = "figures/Extended_Data/Figure_S7.pdf",
    width=16, height=8,useDingbats = FALSE)
grid.arrange(p5,p5.2,p4,p5.3,p5.4,nrow=2,ncol=3, widths=c(1,1,2),
             layout_matrix = rbind(c(1,2,3),c(4,5,3)))
dev.off()

# Count samples
metadata.SH.noFQ %>% group_by(field_season) %>% summarize(n=n())
