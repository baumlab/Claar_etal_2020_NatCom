# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(lme4)
library(emmeans)

# Load necessary data
load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")
load("data/temperature/KI_SB_temp_DHW.RData")
load("data/temperature/KI_SB_temp_DHW_allsites.RData")

##################################
# Filter out colonies with unknown statuses
metadata.SH.AD <- metadata.SH[which(metadata.SH$Status != "UK" & metadata.SH$Status != "gone" & metadata.SH$Status != "dead_or_gone"),]
metadata.SH.noFQ.AD <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone"),]
metadata.SH.noFQ.A <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone" & metadata.SH.noFQ$Status != "dead"),]
metadata.SH.noFQ.D <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$Status=="dead"),]

mid <- 1.3

metadata.SH.noFQ.AD$CtoD2 <- metadata.SH.noFQ.AD$C.PaxC / metadata.SH.noFQ.AD$D.PaxC
metadata.SH.noFQ.AD$CtoD2[which(metadata.SH.noFQ.AD$CtoD2>mid)] <- mid

metadata.SH.noFQ.A$CtoD2 <- metadata.SH.noFQ.A$C.PaxC / metadata.SH.noFQ.A$D.PaxC
metadata.SH.noFQ.A$CtoD2[which(metadata.SH.noFQ.A$CtoD2>mid)] <- mid

metadata.SH.noFQ.D$CtoD2 <- metadata.SH.noFQ.D$C.PaxC / metadata.SH.noFQ.D$D.PaxC
metadata.SH.noFQ.D$CtoD2[which(metadata.SH.noFQ.D$CtoD2>mid)] <- mid

##################################
# Set up and format data
# Set a start and end date for plotting
startdate <- as.POSIXct("2014-08-01 00:00:00",tz="Pacific/Kiritimati", 
                        format="%Y-%m-%d %H:%M:%S")
enddate <- as.POSIXct("2016-11-19 00:00:00",tz="Pacific/Kiritimati", 
                      format="%Y-%m-%d %H:%M:%S")
# Truncate the data from startdate to enddate
KI_heat <- KI_allsites_DHW[which(KI_allsites_DHW$xi3>startdate),]
KI_heat <- KI_heat[which(KI_heat$xi3<enddate),]
# Determine when DHW > 4
DHW_positive <- which(KI_heat$DHW > 4)
# Determine when DHW first exceeded 4 and last exceeded 4
firstDHW <- KI_heat$xi3[min(DHW_positive)]
lastDHW <- KI_heat$xi3[max(DHW_positive)]
# Set field season dates
KI2014 <- as.POSIXct("2014-09-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015a <- as.POSIXct("2015-01-20 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015b <- as.POSIXct("2015-05-10 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2015c <- as.POSIXct("2015-07-25 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2016a <- as.POSIXct("2016-03-25 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2016b <- as.POSIXct("2016-11-08 00:00:00", format="%Y-%m-%d %H:%M:%S")
KI2017a <- as.POSIXct("2017-07-15 00:00:00", format="%Y-%m-%d %H:%M:%S")
logtrans <- as.POSIXct("2017-06-15 00:00:00", format="%Y-%m-%d %H:%M:%S")

##################################
# Set colors for Cladocopium- and Durusdinium-dominated samples
C_col <- "#2166ac"
D_col <- "#b2182b"
# Set color for shaded area demonstrating heat stress
stress_col <- "#ffcc66"

##################################
# Summarize mean and error of each time point and survival outcome
summ_means <- metadata.SH.noFQ.AD %>% 
  dplyr::group_by(field_season,Status) %>% 
  dplyr::summarize(mean=mean(S.H),
                   sd=sd(S.H),
                   se=sd(S.H)/(sqrt(n())),
                   S.H.log10.se=sd(S.H.log10)/(sqrt(n())),
                   S.H.log10=mean(S.H.log10),
                   C.PaxC=mean(C.PaxC),
                   D.PaxC=mean(D.PaxC), 
                   CtoD=mean(CtoD))
summ_means_df <- as.data.frame(summ_means)
# Create newe column for date
summ_means_df$date <- as.POSIXct(KI2015b)
summ_means_df$date[summ_means_df$field_season == "KI2015b"] <- as.POSIXct(KI2015b)
summ_means_df$date[summ_means_df$field_season == "KI2015c"] <- as.POSIXct(KI2015c)
summ_means_df$date[summ_means_df$field_season == "KI2016a"] <- as.POSIXct(KI2016a)
summ_means_df$date[summ_means_df$field_season == "KI2016b"] <- as.POSIXct(KI2016b)
summ_means_df$date[summ_means_df$field_season == "KI2017a"] <- as.POSIXct(KI2017a)
# Create new column for dominant symbiont
summ_means_df$dom <- "D"
summ_means_df$dom[summ_means_df$field_season== "KI2015b" & summ_means_df$Status == "alive"] <- "C"
summ_means_df$dom[summ_means_df$field_season== "KI2015c" & summ_means_df$Status == "alive"] <- "C"
summ_means_df$Status <- as.factor(summ_means_df$Status)

##################################
# Insert a title for the scale
scaletitle <- expression(paste("Dominant ", "Symbiodiniaceae", " Genus"))
pd <- position_dodge(2000000)

# Extract fitted values for plotting below
ggplot() + stat_smooth(aes(y = CtoD2, x = date, group=Status, color=..y..,
                           outfit=fit<<-..y..,outfit=xfit<<-..x..,
                           outfit=sefit<<-..se..),
            span=.67, data = metadata.SH.noFQ.A,level = 0.95)  

ggplot() + stat_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..,
                           outfit=fit2<<-..y..,outfit=xfit2<<-..x..),
                       span=.67, data = metadata.SH.noFQ.A,level = 0.95) 
###
ggplot() + stat_smooth(aes(y = CtoD2, x = date, group=Status, color=..y..,
                           outfit=fitD<<-..y..,outfit=xfitD<<-..x..),
                       span=.67, data = metadata.SH.noFQ.D,level = 0.95) 

ggplot() + stat_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..,
                           outfit=fit2D<<-..y..,outfit=xfit2D<<-..x..,
                           outfit=sefitD<<-..se..),
                       span=.67, data = metadata.SH.noFQ.D,level = 0.95) 

xfit2 <- as.POSIXct(xfit2, origin="1970-01-01")
xfit2D <- as.POSIXct(xfit2D, origin="1970-01-01")

# Create data frames with fitted data
df1 <- cbind(xfit2,
             as.data.frame(fit2),
             as.data.frame(fit),
             as.data.frame(sefit))
df1D <- cbind(xfit2D,
              as.data.frame(fit2D),
              as.data.frame(fitD),
              as.data.frame(sefitD))

# Make plot
p6 <- ggplot()+
  geom_rect(aes(xmin = firstDHW, xmax = lastDHW, ymin = -Inf, ymax = Inf),
            fill = stress_col, alpha = 0.3) + # Show heat stress extent
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color="#404040"),
        plot.margin = unit(c(0.1,0.5,0.1,0.1),"cm"),
        legend.position = c(0.725,0.2), 
        legend.direction = "horizontal",
        legend.title.align = 0.5,
        legend.key = element_blank(),
        legend.box.just = "center",
        legend.margin = margin(-0.15,0,0,0, unit="cm"),
        legend.background = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18,color="#404040")) +
  ylab("") + xlab("2015                  2016                               2017         ") +
  scale_y_continuous(name="Symbiont:Host Ratio", limits=c(-2.4,-1),
                     expand=c(0.01,0.01), 
                     breaks = c(log10(0.01),log10(0.02),log10(0.04),
                                log10(0.06),log10(0.08),log10(0.1),
                                log10(0.12),log10(0.14),log10(0.16),
                                log10(0.18)), 
                     labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",
                                0.12,0.14,0.16,0.18)) +
  scale_x_datetime(date_breaks = "2 months",date_labels = "%b",
                   expand=c(0.01,0.01)) +
  annotate("text",x=as.POSIXct("2016-05-10"), y =-2.28,label="100% 
  Durusdinium")+
  annotate("text",x=as.POSIXct("2017-06-01"), y =-2.28,label="100% 
  Cladocopium")+
  annotate("text",x=as.POSIXct("2015-11-6"), y =-1.05,label="El Niño",size=6)+
  annotate("text",x=KI2015b, y =-1,label="iii",size=4)+
  annotate("text",x=KI2015c, y =-1,label="iv",size=4)+
  annotate("text",x=KI2016a, y =-1,label="v",size=4)+
  annotate("text",x=KI2016b, y =-1,label="vi",size=4)+  
  annotate("text",x=KI2017a, y =-1,label="vii",size=4)+
  geom_vline(xintercept=as.numeric(firstDHW),linetype="dashed")+
  geom_vline(xintercept=as.numeric(lastDHW),linetype="dashed")+
  annotate(geom = "text", x = c(as.POSIXct("2015-06-15"),
                                as.POSIXct("2016-01-01"),
                                as.POSIXct("2017-01-01")), 
           y = -10.6, label = unique(format(metadata.SH.noFQ.AD$date,"%Y")),
           size = 4)+
  geom_ribbon(aes(x=xfit2,ymin=fit2-sefit,ymax=fit2+sefit), 
              data=df1, alpha=0.2)+ # add in fitted data
  geom_ribbon(aes(x=xfit2D,ymin=fit2D-sefitD,ymax=fit2D+sefitD), 
              data=df1D, alpha=0.2)+ # add in fitted data
  geom_line(aes(x=xfit2,y=fit2,color=fit),
            data=df1,size=1.5) + # add in fitted data
  geom_line(aes(x=xfit2D,y=fit2D,color=fitD),
            data=df1D,size=1.5)+   # add in fitted data
  scale_colour_gradient2(low = D_col, high = C_col,mid="gray",
                         midpoint = 0.555, name= scaletitle) +
  geom_point(aes(y=S.H.log10, shape=Status, # add data points
                 x=as.POSIXct(date, origin="1970-01-01"), 
                 group=Status,fill=dom), data=summ_means_df,
             position=pd,cex=4) +
  geom_errorbar(aes(x=as.POSIXct(date, origin="1970-01-01"), # add error bars
                    group=Status,
                    ymin=S.H.log10-S.H.log10.se,ymax=S.H.log10+S.H.log10.se, 
                    width=1000000),
                data=summ_means_df,position = pd) +
  scale_shape_manual(name= "Coral Fate",values=c("alive"=21,"dead"=24),
                     labels=c("Survived","Died"))+
  scale_fill_manual(values=c(C_col,D_col),guide=F) +
  guides(colour=guide_colourbar(title.position="top", title.hjust=0.5, 
                                barwidth=10,label = F),
         shape=guide_legend(title="Coral Fate",title.position = "top",
                            order = 1, override.aes = list(size=4)))

p6 # show plot

# Open a jpg image
jpeg(file="figures/Figure_3/Figure3a.jpg",width = 7.2, height = 4,units="in",res=300)
p6
dev.off()

tiff(file="figures/Figure_3/Figure3a.tiff",width = 7.2, height = 4,units="in",res=300)
p6
dev.off()

pdf(file="figures/Figure_3/Figure3a.pdf",width = 7.2, height = 4,useDingbats = FALSE)
p6
dev.off()

#https://stackoverflow.com/questions/25378184/need-to-extract-data-from-the-ggplot-geom-histogram
p6b <- ggplot_build(p6)
p6b$data[[17]]

# Extract mean S:H in 'real' values (i.e. covert from log to regular)
KI2015b_dead_SH_mean <- 10^(p6b$data[[17]]$y[1])
KI2015b_alive_SH_mean <- 10^(p6b$data[[17]]$y[2])
KI2015c_dead_SH_mean <- 10^(p6b$data[[17]]$y[3])
KI2015c_alive_SH_mean <- 10^(p6b$data[[17]]$y[4])
KI2016a_alive_SH_mean <- 10^(p6b$data[[17]]$y[5])
KI2016b_alive_SH_mean <- 10^(p6b$data[[17]]$y[6])
KI2017a_alive_SH_mean <- 10^(p6b$data[[17]]$y[7])
# Extract standard error of S:H in 'real' values (i.e. covert from log to regular)
KI2015b_dead_SH_se <- ((10^(p6b$data[[18]]$ymax[1])-10^(p6b$data[[18]]$ymin[1]))/2)
KI2015b_alive_SH_se <- ((10^(p6b$data[[18]]$ymax[2])-10^(p6b$data[[18]]$ymin[2]))/2)
KI2015c_dead_SH_se <- ((10^(p6b$data[[18]]$ymax[3])-10^(p6b$data[[18]]$ymin[3]))/2)
KI2015c_alive_SH_se <- ((10^(p6b$data[[18]]$ymax[4])-10^(p6b$data[[18]]$ymin[4]))/2)
KI2016a_alive_SH_se <- ((10^(p6b$data[[18]]$ymax[5])-10^(p6b$data[[18]]$ymin[5]))/2)
KI2016b_alive_SH_se <- ((10^(p6b$data[[18]]$ymax[6])-10^(p6b$data[[18]]$ymin[6]))/2)
KI2017a_alive_SH_se <- ((10^(p6b$data[[18]]$ymax[7])-10^(p6b$data[[18]]$ymin[7]))/2)

# Total number of samples
nrow(metadata.SH.noFQ.AD)
# Total number of colonies
unique(metadata.SH.noFQ.AD$coral_tag)

# Test for differences among field season and status
fit <- lmer(S.H.log10 ~ field_season + Status + (1|coral_tag), data=metadata.SH.noFQ.AD)
anova(fit)
# We have to provide the model (here called fit and the factors we want to contrast
emmeans(fit, list(pairwise ~ field_season), adjust = "tukey")

# Tally the number of corals from 2016a onwards that were dominated by C, 
# but had >0 D
metadata.SH.noFQ.AD %>% filter(Year_Pre_Post != "KI2016a", 
                               Year_Pre_Post != "KI2016b", 
                               Year_Pre_Post != "KI2017a") %>% 
  group_by(coral_tag) %>% 
  filter(dom=="C") %>% 
  tally(D.PaxC > 0) 

# Tally total number from 2016a onwards that were dominated by C
metadata.SH.noFQ.AD %>% filter(Year_Pre_Post != "KI2016a", 
                               Year_Pre_Post != "KI2016b", 
                               Year_Pre_Post != "KI2017a") %>% 
  group_by(coral_tag) %>% 
  filter(dom=="C") %>% 
  tally() 

# Tally number per field season dominated by C with NO D
metadata.SH.noFQ.AD %>% 
  group_by(Year_Pre_Post) %>% 
  filter(dom=="C") %>% 
  tally(D.PaxC == 0)

# Tally number per field season with no D
metadata.SH.noFQ.AD %>% 
  group_by(Year_Pre_Post) %>% 
  tally(D.PaxC == 0)

# Tally total number of colonies
metadata.SH.noFQ.AD %>% tally() # total # colonies

# Count number of unique colonies that were sampled from 2016a onwards
metadata.SH.noFQ.AD %>% 
  filter(field_season %in% c("KI2016a","KI2016b","KI2017a")) %>% 
  group_by(coral_tag) %>% tally() %>% nrow()

