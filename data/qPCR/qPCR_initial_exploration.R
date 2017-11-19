library(ggplot2)

rm(list=ls())

load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")

metadata.SH$S.H.log <- log(metadata.SH$S.H)

metadata.SH$field_season[which(metadata.SH$field_season=="KI2015a_Pre")] <- "KI2015a"
# But then collapse field seasons into before/during/after
metadata.SH$before.after <- metadata.SH$field_season
metadata.SH$before.after <- gsub("KI2014","before",metadata.SH$before.after)
metadata.SH$before.after <- gsub("KI2015a","before",metadata.SH$before.after)
metadata.SH$before.after <- gsub("KI2015b","before",metadata.SH$before.after)
metadata.SH$before.after <- gsub("KI2015c","during",metadata.SH$before.after)
metadata.SH$before.after <- gsub("KI2016a","after",metadata.SH$before.after)
metadata.SH$before.after <- gsub("KI2016b","recovery1",metadata.SH$before.after)
metadata.SH$before.after <- gsub("KI2017a","recovery2",metadata.SH$before.after)

metadata.SH.FQ <- metadata.SH[grepl("FQ", metadata.SH$Sample.Name),]
metadata.SH.noFQ <- metadata.SH[!grepl("FQ", metadata.SH$Sample.Name),]


ggplot(aes(y = S.H.log, x = field_season), data = metadata.SH.noFQ) + geom_boxplot()
ggplot(aes(y = S.H.log, x = before.after), data = metadata.SH.noFQ) + geom_boxplot()

ggplot(aes(y = S.H.log, x = before.after,color=bleaching_proportion), data = metadata.SH.noFQ) + geom_point()

hist(metadata.SH$S.H,breaks = 100)

names(metadata.SH)
metadata.SH$bleaching_proportion <- gsub(" ","",metadata.SH$bleaching_proportion)

metadata.SH$bleaching2Plus <- metadata.SH$bleaching_proportion
metadata.SH$bleaching2Plus <- gsub("NONE","FALSE",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub("1","FALSE",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub("2","2",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub("3","3",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub(" ","",metadata.SH$bleaching2Plus)

ggplot(aes(y = S.H.log, x = bleaching_proportion,color=before.after), data = metadata.SH.noFQ) + geom_point()

ggplot(aes(y = S.H.log, x = bleaching_proportion,color=before.after), data = metadata.SH.noFQ) + geom_point()

ggplot(aes(y = S.H.log, x = field_season,color=Status), data = metadata.SH.noFQ) + geom_point() + facet_wrap(~Status)

names(metadata.SH)
