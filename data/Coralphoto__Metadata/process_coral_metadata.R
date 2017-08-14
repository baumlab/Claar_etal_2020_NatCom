library(plyr)

meta <- read.csv("data/Coralphoto__Metadata/KI_Coralphoto_Metadata_Jan_to_Apr_2017_23March.csv",header=T)
meta$Year_Pre_Post <- paste(meta$field_season,meta$before.after, sep="")
meta$ref <- paste(meta$Year_Pre_Post,".tag",meta$coral_tag, sep="")
meta.forcat <- meta[,c(1:25)]

map <- read.table("data/mapping_file.txt",stringsAsFactors = FALSE)
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
# map$ref <- paste(map$V5,"FSYM",map$V3,sep="")
map.platy <- map

names(map.platy)[names(map.platy)=="Year"] <- "field_season"
names(map.platy)[names(map.platy)=="Site"] <- "site"
# Sampled the same coral colony twice during one field season
fix <- which(map.platy$SampleID=="KI15cFSYM509")
map.platy$coral_tag[fix] <- "341.2"

map.platy$ref <- paste(map.platy$field_season,".tag",map.platy$coral_tag, sep="")
duplicated(map.platy)
map.platy

map.platy.forcat <- map.platy[,c(1,3:5,7:9,11)]


metadata <- join_all(list(map.platy.forcat,meta.forcat),by='ref',match='first')
names(metadata)
rownames(metadata) <- metadata[,1] # Make rownames from SampleID
# metadata<-subset(metadata,select=-c(SampleID)) # Remove SampleID column

write.csv(metadata, file="data/Coralphoto__Metadata/KI_Platy_metadata.csv")
write.table(metadata, file="data/Coralphoto__Metadata/KI_Platy_metadata.tsv", quote=FALSE, sep="\t", col.names = NA)
