# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Load necessary data
load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")

# Set ggplot theme
theme_set(theme_bw())

# Set colors for field season
timecols <- c("#2b83ba","#abdda4","#e6f598","#fdae61","#d7191c")

# Set field season names
fsnames <- c("KI2015b" = "May 2015",
             "KI2015c" = "July 2015",
             "KI2016a" = "March 2016",
             "KI2016b" = "November 2016",
             "KI2017a" = "July 2017")

# Make plot
p2 <- ggplot(aes(x = S.H.log, fill=field_season), data = metadata.SH.noFQ) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.08,0.75)) +
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)]),
                    labels=c("May 2015","July 2015","March 2016",
                             "November 2016","July 2017")) +
  geom_density(aes(x = S.H.log), alpha=0.5) +  
  ylab("Density") +
  xlab("Symbiont:Host Ratio") +
  labs(fill="Field Season") +
  scale_x_continuous(limits = c(-11,0), 
                     breaks = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1), 
                     labels = c(expression(10^-10),expression(10^-9),
                                expression(10^-8),expression(10^-7),
                                expression(10^-6),expression(10^-5),
                                expression(10^-4),expression(10^-3),
                                expression(10^-2),expression(10^-1)),
                     expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,0.85))
p2

p3 <- ggplot(aes(x = S.H.log, fill=field_season), data = metadata.SH.noFQ) + 
  facet_grid(. ~ field_season, labeller = as_labeller(fsnames)) +
  theme(axis.text.x = element_text(size=8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_histogram(color="black",bins=c(25),alpha=0.7) + 
  scale_fill_manual(values=c(timecols[c(1,4,5,3,2)], "lightgray")) +
  guides(fill=FALSE) + 
  ylab("Count") +
  xlab("Symbiont:Host Ratio") +
  scale_x_continuous(limits = c(-11,-1), breaks = c(-8,-6,-4,-2), 
                     labels = c(expression(10^-8),expression(10^-6),
                                expression(10^-4),expression(10^-2)),
                     expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,10.5))

p3

jpeg(file="figures/Extended_Data/Extended_Data_Figure_6/Extended_Data_Figure_6.jpg",width=10, height=8,units="in", res=300)
# grid.arrange(p1,p2,p3)
grid.arrange(p2,p3)
dev.off()

tiff(file="figures/Extended_Data/Extended_Data_Figure_6/Extended_Data_Figure_6.tiff",width=10, height=8,units="in", res=300)
grid.arrange(p2,p3)
dev.off()

pdf(file="figures/Extended_Data/Extended_Data_Figure_6/Extended_Data_Figure_6.pdf",width=10, height=8)
grid.arrange(p2,p3)
dev.off()

# Count number of samples and colonies
metadata.SH.noFQ %>% group_by(field_season) %>% summarize(n=n())
unique(metadata.SH.noFQ$coral_tag)
