library(tidyr)

# Import data
cn <- read.table("data/qPCR/copyno/KIPlaty_copynumber_20180122_data.txt", sep="\t", 
                 skip=8, header=T, stringsAsFactors = FALSE)
cn <- cn[,c(1,2,3,4,7,10)]  # Subset columns
cn <- cn[!grepl("^$|NTC", cn$Sample.Name), ]  # Remove blank wells and NTC
names(cn)[5] <- "CT"
cn$CT <- as.numeric(cn$CT)

# Plot standard curves
cnstd <- subset(cn, Task=="STANDARD")
cnstd$logquant <- log(cnstd$Quantity, 10)

plot(logquant ~ CT, data=cnstd, col=c("blue", "red")[factor(Target.Name)])

# Fit C standard curve
Cstd <- subset(cnstd, Target.Name=="C")
Cmod <- lm(logquant ~ CT, data=Cstd)
#Cout <- names(which(cooks.distance(Cmod) > 4/nrow(Cstd)))
#Cmod <- update(Cmod, data=Cstd[!rownames(Cstd) %in% Cout, ])
#Cmod <- update(Cmod, data=subset(Cstd, Quantity!=128000))
abline(Cmod, col="blue")

# Fit D standard curve
Dstd <- subset(cnstd, Target.Name=="D")
Dmod <- lm(logquant ~ CT, data=Dstd)#Dstd[-(c(1,2)),])
#Dout <- names(which(cooks.distance(Dmod) > 4/nrow(Dstd)))
#Dmod <- update(Dmod, data=Dstd[!rownames(Dstd) %in% Dout, ])
#Dmod <- update(Dmod, data=subset(Dstd, Quantity!=128000))
abline(Dmod, col="red")

Cquant <- subset(cn[,-6], Task=="UNKNOWN" & Target.Name=="C")
Cquant$logquant <- predict(Cmod, data.frame(CT=Cquant$CT))
Cquant$Quantity <- 10^(Cquant$logquant)
Cquant$Quantity <- Cquant$Quantity / 0.955
Cquant$copyno <- Cquant$Quantity / 2000

Dquant <- subset(cn[,-6], Task=="UNKNOWN" & Target.Name=="D")
Dquant$logquant <- predict(Dmod, data.frame(CT=Dquant$CT))
Dquant$Quantity <- 10^(Dquant$logquant)
Dquant$Quantity <- Dquant$Quantity / 0.955
Dquant$copyno <- Dquant$Quantity / 2000
Dquant[Dquant$Sample.Name=="1.3", "copyno"] <- Dquant[Dquant$Sample.Name=="1.3", "Quantity"] / 1474.4

# Results
plot(copyno ~ factor(Sample.Name), data=Cquant)  # 4.1 is an outlier
Cquant <- Cquant[Cquant$Sample.Name!="4.1", ]

plot(copyno ~ factor(Sample.Name), data=Dquant)

copyno <- rbind(Cquant, Dquant) %>%
  group_by(Target.Name) %>%
  summarise(mean=mean(copyno),
            sd=sd(copyno))

write.table(copyno, file="data/qPCR/copyno/copyno_output.txt", row.names=F, quote=F)
