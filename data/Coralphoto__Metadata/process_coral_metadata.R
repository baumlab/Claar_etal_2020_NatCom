rm(list=ls())

library(plyr)

load("data/qPCR/qPCR.RData")

meta <- read.csv("data/Coralphoto__Metadata/KI_Coralphoto_Metadata_through2017_ALLDONE.csv",header=T)
meta$before.after <- gsub("before","_Pre",meta$before.after)
meta$before.after <- gsub("after","_Post",meta$before.after)
meta$before.after <- gsub("during but not affectd","_Pre",meta$before.after)
meta$before.after[which(meta$before.after=="1")] <- ""
meta$Year_Pre_Post <- paste(meta$field_season,meta$before.after, sep="")

meta$coral_tag[which(meta$coral_tag==248)] <- "248_696"
# meta$coral_tag[which(meta$coral_tag==338)] <- "338_1168"
meta$coral_tag[which(meta$coral_tag==410)] <- "410_893"
meta$coral_tag[which(meta$coral_tag=="754_899")] <- "754_899_1060"
meta$coral_tag[which(meta$coral_tag==1060)] <- "754_899_1060"


meta$ref <- paste(meta$Year_Pre_Post,".tag",meta$coral_tag, sep="")
meta.forcat <- meta[,c(1:23,25:26)]
count(duplicated(meta.forcat$ref))
meta.forcat <- meta.forcat[which(!duplicated(meta.forcat$ref)),] # Remove duplicates
# meta.forcat[which(meta.forcat$ref=="KI2014.tag394"),] 


map1 <- read.table("data/mapping_file.txt",stringsAsFactors = FALSE)
map2 <- read.table("data/mapping_file2.txt",stringsAsFactors = FALSE,sep="\t")
map <- rbind(map1,map2)
colnames(map) <- c("SampleID", "InputFileName", "coral_tag","SampleType", "Year", "TubeNumber", "Coral_Species","Site","Status","Year_Pre_Post")

fs2014 <- which(map$Year==2014)
map$Year[fs2014] <- "KI2014"
fs2015aa <- which(map$Year_Pre_Post=="2015Jan_Pre")
map$Year[fs2015aa] <- "KI2015a_Pre"
fs2015ab <- which(map$Year_Pre_Post=="2015Jan_Post")
map$Year[fs2015ab] <- "KI2015a_Post"
fs2015b <- which(map$Year=="2015May")
map$Year[fs2015b] <- "KI2015b"
fs2015c <- which(map$Year=="2015July")
map$Year[fs2015c] <- "KI2015c"
fs2016a <- which(map$Year=="2016March")
map$Year[fs2016a] <- "KI2016a"
fs2016b <- which(map$Year=="2016Nov")
map$Year[fs2016b] <- "KI2016b"
fs2017a <- which(map$Year=="2017Jul")
map$Year[fs2017a] <- "KI2017a"

# map$ref <- paste(map$V5,"FSYM",map$V3,sep="")
map.platy <- map

names(map.platy)[names(map.platy)=="Year"] <- "field_season"
names(map.platy)[names(map.platy)=="Site"] <- "site"
# Sampled the same coral colony twice during one field season
fix <- which(map.platy$SampleID=="KI15cFSYM509")
map.platy$coral_tag[fix] <- "341.2"

# Standardize multi-tag names
map.platy$coral_tag[which(map.platy$coral_tag==248)] <- "248_696"
map.platy$coral_tag[which(map.platy$coral_tag==338)] <- "338_1168"
map.platy$coral_tag[which(map.platy$coral_tag==410)] <- "410_893"
map.platy$coral_tag[which(map.platy$coral_tag=="754_899")] <- "754_899_1060"
map.platy$coral_tag[which(map.platy$coral_tag==1060)] <- "754_899_1060"
map.platy$coral_tag[which(map.platy$coral_tag==754)] <- "754_899_1060"

map.platy$ref <- paste(map.platy$field_season,".tag",map.platy$coral_tag, sep="")
# duplicated(map.platy)
# map.platy

map.platy.forcat <- map.platy[,c(1,3:5,7:9,11)]


metadata <- join_all(list(map.platy.forcat,meta.forcat),by='ref',match='all')
names(metadata)
# metadata <- metadata[which(!(metadata$SampleID=="KI16aFSYM101" & metadata$annotator == "HD")),]
# metadata <- metadata[which(!(metadata$SampleID=="KI15cFSYM104" & metadata$date_annotated == "27-01-2015")),]
rownames(metadata) <- metadata[,1] # Make rownames from SampleID
# Current problem : non-unique values when setting 'row.names': ‘KI15cFSYM104’, ‘KI16aFSYM101’ 
# metadata<-subset(metadata,select=-c(SampleID)) # Remove SampleID column


# platy[which(is.na(platy$Tag)),]
platy$Tag[which(platy$Sample.Name == "KI16bFMD068")] <- "820"
platy$ref[which(platy$Sample.Name == "KI16bFMD068")] <- "KI2016b.tag820"

metadata <- join_all(list(metadata,platy),by="ref",match="all")

# S.H <- data.frame(ref = platy$ref, S.H = platy$S.H)
# 
# metadata <- join_all(list(metadata,S.H),by="ref",match="all") 

metadata <- metadata[which(metadata$Coral_Species=="Platygyra_sp"),]

count(!is.na(metadata$S.H))

metadata.SH <- metadata[which(!is.na(metadata$S.H)),]

metadata.SH[which(duplicated(metadata.SH$SampleID)),]
# "248_696" %in% metadata.SH$coral_tag

# Clean up columns in this metadata
metadata.SH <- metadata.SH[,c(1:8,10:48,50:51)]
metadata.SH <- metadata.SH[,c(1:8,11:46,48:49)]
names(metadata.SH)

# dups <- metadata.SH[which(duplicated(metadata.SH$SampleID) | duplicated(metadata.SH$SampleID, fromLast = TRUE)),]

# Remove duplicates, keeping later runs and discarding earlier runs
# This is clunky and error-prone...try to change later.
metadata.SH <- metadata.SH[which(!duplicated(metadata.SH$SampleID, fromLast = TRUE)),]

platy[which(!platy$ref %in% metadata.SH$ref),]
platy[which(!metadata.SH$ref %in% platy$ref),]


metadata.SH$dom <- ifelse(metadata.SH$C.PaxC > metadata.SH$D.PaxC,"C","D")

metadata.SH$S.H.log <- log(metadata.SH$S.H)
metadata.SH$C.PaxC.log <- log(metadata.SH$C.PaxC+6.107637e-08)
metadata.SH$D.PaxC.log <- log(metadata.SH$D.PaxC+6.107637e-08)
metadata.SH$CtoD <- metadata.SH$C.PaxC.log / metadata.SH$D.PaxC.log
sort(metadata.SH$D.PaxC)

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
metadata.SH$bleaching_proportion <- gsub("NONE","FALSE",metadata.SH$bleaching_proportion)
metadata.SH$bleaching_proportion <- gsub("1","FALSE",metadata.SH$bleaching_proportion)
metadata.SH$bleaching_proportion <- gsub("2","2",metadata.SH$bleaching_proportion)
metadata.SH$bleaching_proportion <- gsub("3","3",metadata.SH$bleaching_proportion)
metadata.SH$bleaching_proportion <- gsub(" ","",metadata.SH$bleaching_proportion)

metadata.SH.FQ <- metadata.SH[grepl("FQ", metadata.SH$Sample.Name),]
metadata.SH.noFQ <- metadata.SH[!grepl("FQ", metadata.SH$Sample.Name),]

metadata.SH$bleaching_proportion <- gsub(" ","",metadata.SH$bleaching_proportion)

metadata.SH$bleaching2Plus <- metadata.SH$bleaching_proportion
metadata.SH$bleaching2Plus <- gsub("NONE","FALSE",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub("1","FALSE",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub("2","2",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub("3","3",metadata.SH$bleaching2Plus)
metadata.SH$bleaching2Plus <- gsub(" ","",metadata.SH$bleaching2Plus)

metadata.SH.noFQ$date <- as.POSIXct("2010-01-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
metadata.SH.noFQ$date[which(metadata.SH.noFQ$field_season=="KI2014")] <- as.POSIXct("2014-09-01 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
metadata.SH.noFQ$date[which(metadata.SH.noFQ$field_season=="KI2015a")] <- as.POSIXct("2015-01-20 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
metadata.SH.noFQ$date[which(metadata.SH.noFQ$field_season=="KI2015b")] <- as.POSIXct("2015-05-10 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
metadata.SH.noFQ$date[which(metadata.SH.noFQ$field_season=="KI2015c")] <- as.POSIXct("2015-07-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
metadata.SH.noFQ$date[which(metadata.SH.noFQ$field_season=="KI2016a")] <- as.POSIXct("2016-03-25 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
metadata.SH.noFQ$date[which(metadata.SH.noFQ$field_season=="KI2016b")] <- as.POSIXct("2016-11-08 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")
metadata.SH.noFQ$date[which(metadata.SH.noFQ$field_season=="KI2017a")] <- as.POSIXct("2017-07-15 00:00:00",tz="Pacific/Kiritimati", format="%Y-%m-%d %H:%M:%S")


write.csv(metadata, file="data/Coralphoto__Metadata/KI_Platy_metadata.csv")
write.table(metadata, file="data/Coralphoto__Metadata/KI_Platy_metadata.tsv", quote=FALSE, sep="\t", col.names = NA)

write.csv(metadata.SH, file="data/Coralphoto__Metadata/KI_Platy_metadataSH.csv")
write.table(metadata.SH, file="data/Coralphoto__Metadata/KI_Platy_metadataSH.tsv", quote=FALSE, sep="\t", col.names = NA)

save(metadata.SH,file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")
