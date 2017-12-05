##################################################################################
### El Ni?o Global Map ###

# Load in and format world map showing El Ni?o
# Use imager to load the image
img_20152016ElNino <- load.image('figures/global_elnino_map/figure_1.jpg')
# # Check that the data is in the correct format for the next step
# as.data.frame(img_20152016ElNino,wide="c") %>% head
# Mutate image data to be able to plot it using GridExtra/ggplot2
df <- as.data.frame(img_20152016ElNino,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
# # Double check that it worked
# head(df,3)

# Turn image into a ggplot object for incorporating in multi-panel figure below
world_20152016ElNino <- ggplot(df,aes(x,y)) + 
  geom_raster(aes(fill=rgb.val)) + 
  scale_fill_identity()+scale_y_reverse() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),                                         axis.title.x=element_blank(),                                                                   axis.title.y=element_blank(),legend.position="none",                                            panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = unit(c(0,0,0,0),c("in","in","in","in")))

# Open a jpg image
jpeg(file="figures/Extended Data/worldcoralstress.jpg",width = 7.2, height = 2,units="in",res=300)
world_20152016ElNino
dev.off()
