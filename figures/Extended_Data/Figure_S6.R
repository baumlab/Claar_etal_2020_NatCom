library(ggplot2)
library(gridExtra)

rm(list=ls())

load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")
theme_set(theme_bw())

timecols <- c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c")


# metadata.SH.noFQ$field_season <- (as.data.frame(metadata.SH.noFQ$field_season))
# levels(metadata.SH.noFQ$field_season) <- c("May 2015", "July 2015", "March 2016", "November 2016", "July 2017")
# # metadata.SH.noFQ$field_season[which(metadata.SH.noFQ$field_season == "KI2015b")] <- "May 2015"
# # metadata.SH.noFQ$field_season[which(metadata.SH.noFQ$field_season == "KI2015c")] <- "July 2015"
# # metadata.SH.noFQ$field_season[which(metadata.SH.noFQ$field_season == "KI2016a")] <- "March 2016"
# # metadata.SH.noFQ$field_season[which(metadata.SH.noFQ$field_season == "KI2016b")] <- "November 2016"
# # metadata.SH.noFQ$field_season[which(metadata.SH.noFQ$field_season == "KI2017a")] <- "July 2017"

fsnames <- c("KI2015b" = "May 2015",
             "KI2015c" = "July 2015",
             "KI2016a" = "March 2016",
             "KI2016b" = "November 2016",
             "KI2017a" = "July 2017")



p1 <- ggplot(aes(x = S.H.log, fill=field_season), data = metadata.SH.noFQ) + 
  theme(legend.position = c(0.2,0.7),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_histogram(bins=30) + 
  # facet_grid(. ~ field_season) +
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)]),name ="Field Season",labels=c("May 2015","July 2015","March 2016","November 2016","July 2017")) +
  ylab("Count") +
  xlab("log(Symbiont:Host Ratio)") +
  xlim(-11,-1)
p1  


p2 <- ggplot(aes(x = S.H.log, fill=field_season), data = metadata.SH.noFQ) + 
  # facet_grid(. ~ field_season) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.08,0.75)) +
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)]),labels=c("May 2015","July 2015","March 2016","November 2016","July 2017")) +
  geom_density(aes(x = S.H.log), alpha=0.5) +  
  ylab("Density") +
  xlab("Symbiont:Host Ratio") +
  labs(fill="Field Season") +
  scale_x_continuous(limits = c(-11,0), breaks = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1), labels = c(expression(10^-10),expression(10^-9),expression(10^-8),expression(10^-7),expression(10^-6),expression(10^-5),expression(10^-4),expression(10^-3),expression(10^-2),expression(10^-1)),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,0.85))
p2
# xlim(0,0.1)

p3 <- ggplot(aes(x = S.H.log, fill=field_season), data = metadata.SH.noFQ) + 
  facet_grid(. ~ field_season, labeller = as_labeller(fsnames)) +
  theme(axis.text.x = element_text(size=8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color="black",bins=c(25),alpha=0.7) + 
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)], "lightgray")) +
  guides(fill=FALSE) + 
  # geom_density(aes(x = S.H.log, fill="lightgray"), alpha=0.5) + 
  # ylab("Density
       # Count") +
  ylab("Count") +
  xlab("Symbiont:Host Ratio") +
  scale_x_continuous(limits = c(-11,-1), breaks = c(-8,-6,-4,-2), labels = c(expression(10^-8),expression(10^-6),expression(10^-4),expression(10^-2)),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,10.5))

p3

jpeg(file="figures/Extended_Data/Figure_S6.jpg",width=10, height=8,units="in", res=300)
# grid.arrange(p1,p2,p3)
grid.arrange(p2,p3)
dev.off()

tiff(file="figures/Extended_Data/Figure_S6.tiff",width=10, height=8,units="in", res=300)
# grid.arrange(p1,p2,p3)
grid.arrange(p2,p3)
dev.off()

pdf(file="figures/Extended_Data/Figure_S6.pdf",width=10, height=8)
# grid.arrange(p1,p2,p3)
grid.arrange(p2,p3)
dev.off()

# p4 <- ggplot(aes(x = S.H, y=C.PaxC, fill=bleaching_proportion, col=dom), data = metadata.SH.noFQ) + 
#   theme(legend.position = c(0.9,0.7)) +
#   # facet_grid(. ~ bleaching_proportion) +
#   geom_point(aes(x = S.H, y=C.PaxC)) +
#   scale_fill_manual(values=c("orange","red","blue")) +
#   geom_density_2d(aes(x = S.H, y=C.PaxC), alpha=0.5) +  
#   # guides(fill=FALSE) +
#   xlab("log(Symbiont:Host Ratio)")
# # xlim(0,0.1)
# p4
# 
# p5 <- ggplot(aes(x = S.H, fill=field_season), data = metadata.SH.noFQ) + 
#   facet_grid(. ~ bleaching_proportion) +
#   scale_fill_manual(values=c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c")) +
#   # geom_density(aes(x = S.H), alpha=0.5) + 
#   geom_histogram(position="dodge") +
#   # guides(fill=FALSE) +
#   xlab("log(Symbiont:Host Ratio)") +
#   xlim(0,0.08)
# p5
