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
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)]),name ="Field Season") +
  ylab("Count") +
  xlab("log(Symbiont:Host Ratio)") +
  xlim(-11,-1)
p1  


p2 <- ggplot(aes(x = S.H.log, fill=field_season), data = metadata.SH.noFQ) + 
  # facet_grid(. ~ field_season) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)])) +
  geom_density(aes(x = S.H.log), alpha=0.5) +  
  guides(fill=FALSE) +
  ylab("Density") +
  xlab("log(Symbiont:Host Ratio)") +
  xlim(-11,-1)

# xlim(0,0.1)

p3 <- ggplot(aes(x = S.H.log, fill=field_season), data = metadata.SH.noFQ) + 
  facet_grid(. ~ field_season, labeller = as_labeller(fsnames)) +
  theme(axis.text.x = element_text(size=8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color="black",bins=c(25)) + 
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)], "lightgray")) +
  guides(fill=FALSE) + 
  # geom_density(aes(x = S.H.log, fill="lightgray"), alpha=0.5) + 
  # ylab("Density
       # Count") +
  ylab("Count") +
  xlab("log(Symbiont:Host Ratio)") +
  xlim(-11,-1)


jpeg(file="figures/Extended Data/ExData_Figure6.jpg",width=10, height=8,units="in", res=300)
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
