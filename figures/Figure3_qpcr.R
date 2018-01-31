
library(ggplot2)
library(gridExtra)
library(grid)

rm(list=ls())

load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")
load("data/temperature/KI_SB_temp_DHW.RData")

##################################
metadata.SH.AD <- metadata.SH[which(metadata.SH$Status != "UK" & metadata.SH$Status != "gone" & metadata.SH$Status != "dead_or_gone"),]
metadata.SH.noFQ.AD <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone"),]
metadata.SH.noFQ.A <- metadata.SH.noFQ[which(metadata.SH.noFQ$Status != "UK" & metadata.SH.noFQ$Status != "gone" & metadata.SH.noFQ$Status != "dead_or_gone" & metadata.SH.noFQ$Status != "dead"),]
metadata.SH.noFQ.D <- metadata.SH.noFQ.AD[which(metadata.SH.noFQ.AD$Status=="dead"),]

metadata.SH.noFQ.AD$CtoD2 <- metadata.SH.noFQ.AD$C.PaxC / metadata.SH.noFQ.AD$D.PaxC
metadata.SH.noFQ.AD$CtoD2[which(metadata.SH.noFQ.AD$CtoD2>2)] <- 2

metadata.SH.noFQ.A$CtoD2 <- metadata.SH.noFQ.A$C.PaxC / metadata.SH.noFQ.A$D.PaxC
metadata.SH.noFQ.A$CtoD2[which(metadata.SH.noFQ.A$CtoD2>2)] <- 2

metadata.SH.noFQ.D$CtoD2 <- metadata.SH.noFQ.D$C.PaxC / metadata.SH.noFQ.D$D.PaxC
metadata.SH.noFQ.D$CtoD2[which(metadata.SH.noFQ.D$CtoD2>2)] <- 2

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

##################################
C_col <- "#2166ac"
D_col <- "#b2182b"
stress_col <- "#d95f02"
stress_col <- "darkgoldenrod4"
stress_col <- "burlywood4"

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
scaletitle <- expression(paste("Dominant ", italic("Symbiodinium"), " Clade"))
pd <- position_dodge(1000000)

ggplot() + stat_smooth(aes(y = CtoD2, x = date, group=Status, color=..y..,
                           outfit=fit<<-..y..,outfit=xfit<<-..x..,outfit=sefit<<-..se..),
            span=.67, data = metadata.SH.noFQ.A,level = 0.95)  

ggplot() + stat_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..,
                           outfit=fit2<<-..y..,outfit=xfit2<<-..x..),
                       span=.67, data = metadata.SH.noFQ.A,level = 0.95) 
###
ggplot() + stat_smooth(aes(y = CtoD2, x = date, group=Status, color=..y..,
                           outfit=fitD<<-..y..,outfit=xfitD<<-..x..),
                       span=.67, data = metadata.SH.noFQ.D,level = 0.95) 

ggplot() + stat_smooth(aes(y = S.H.log10, x = date, group=Status, color=..y..,
                           outfit=fit2D<<-..y..,outfit=xfit2D<<-..x..,outfit=sefitD<<-..se..),
                       span=.67, data = metadata.SH.noFQ.D,level = 0.95) 

xfit2 <- as.POSIXct(xfit2, origin="1970-01-01")
xfit2D <- as.POSIXct(xfit2D, origin="1970-01-01")


df1 <- cbind(xfit2,
             as.data.frame(fit2),
             as.data.frame(fit),
             as.data.frame(sefit))
df1D <- cbind(xfit2D,
              as.data.frame(fit2D),
              as.data.frame(fitD),
              as.data.frame(sefitD))

p6 <- ggplot()+
  geom_rect(aes(xmin = firstDHW, xmax = lastDHW, ymin = -2.4, ymax = -0.7),
            fill = stress_col, alpha = 0.3) +
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
  scale_y_continuous(name="Symbiont:Host Ratio", limits=c(-2.4,-0.7),expand=c(0.01,0.01), 
                     breaks = c(-2,-1.7,-1.398,-1.22,-1.097,-1,-0.921,-0.854,-0.796,-0.745), 
                     labels = c(0.01,0.02,0.04,0.06,0.08,"0.10",0.12,0.14,0.16,0.18)) +
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
  annotate(geom = "text", x = c(as.POSIXct("2015-06-15"),as.POSIXct("2016-01-01"),
                                as.POSIXct("2017-01-01")), 
           y = -10.6, label = unique(format(metadata.SH.noFQ.AD$date,"%Y")), size = 4)+
  geom_ribbon(aes(x=xfit2,ymin=fit2-sefit,ymax=fit2+sefit), data=df1, alpha=0.3)+
  geom_ribbon(aes(x=xfit2D,ymin=fit2D-sefitD,ymax=fit2D+sefitD), data=df1D, alpha=0.3)+
  geom_line(aes(x=xfit2,y=fit2,color=fit),data=df1,size=1.5) + 
  geom_line(aes(x=xfit2D,y=fit2D,color=fitD),data=df1D,size=1.5)+   
  scale_colour_gradient2(low = C_col, high = D_col,mid="gray",
                         midpoint = 0.555, name= scaletitle) +
  geom_point(aes(y=S.H.log10, shape=Status, fill=dom, x=as.POSIXct(date, origin="1970-01-01"), 
                 group=Status), data=summ_means_df,position=pd) + 
  geom_errorbar(aes(x=as.POSIXct(date, origin="1970-01-01"),group=Status,
                    ymin=S.H.log10-S.H.log10.se,ymax=S.H.log10+S.H.log10.se, width=1000000),
                data=summ_means_df,position = pd)
p6

# Open a jpg image
jpeg(file="figures/Figure3.jpg",width = 7.2, height = 6,units="in",res=300)
p6
dev.off()