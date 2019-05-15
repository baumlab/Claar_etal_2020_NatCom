rm(list=ls())
load("data/temperature/KI_SB_temp_DHW.RData")

allsites <- c("site8_DHW$DHW","site34_DHW$DHW","site35_DHW$DHW",
              "site27_DHW$DHW","site30_DHW$DHW","site9_DHW$DHW",
              "site32_DHW$DHW","site33_DHW$DHW","site40_DHW$DHW",
              "site25_DHW$DHW","site3_DHW$DHW","site15_DHW$DHW",
              "site19_DHW$DHW", "site5_DHW$DHW")
xi3 <- site8_DHW$xi3

indivlogger <- paste(allsites,collapse=",")
evalstr <- paste("DHW <- cbind(",indivlogger,")")
eval(parse(text=evalstr))
DHW <- rowMeans(DHW,na.rm=TRUE)
KI_allsites_DHW<-cbind.data.frame(xi3,DHW)

plot(KI_allsites_DHW)

save(KI_allsites_DHW,file = "data/temperature/KI_SB_temp_DHW_allsites.RData")
