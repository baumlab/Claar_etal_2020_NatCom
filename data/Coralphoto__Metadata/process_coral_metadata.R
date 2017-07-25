meta <- read.csv("/Users/Dani/Documents/Data_Analysis/KI_seqs/data/KI_Coralphoto_Metadata_Jan_to_Apr_2017_17March.csv",header=T)
meta$ref <- paste(meta$Year_Pre_Post,".tag.",meta$coral_tag, sep="")
meta.forcat <- meta[,c(7,10:25,27)]

map <- read.table("/Users/Dani/Documents/Data_Analysis/KI_Platy/data/mapping_file.txt",stringsAsFactors = FALSE)
colnames(map) <- c("SampleID", "InputFileName", "coral_tag","SampleType", "Year", "TubeNumber", "Coral_Species","Site","Status","Year_Pre_Post")

fs2014 <- which(map$Year==2014)
map$Year[fs2014] <- "KI2014"
fs2015a <- which(map$Year=="2015Jan")
map$Year[fs2015a] <- "KI2015a"
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
map.platy$ref <- paste(map.platy$Year_Pre_Post,".tag",map.platy$coral_tag, sep="")
duplicated(map.platy)
map.platy.forcat <- map.platy[,c(1,3:5,7:9,11)]


metadata <- join_all(list(map.platy.forcat,meta.forcat),by='ref',match='all')
names(metadata)
rownames(metadata) <- metadata[,1]
metadata<-subset(metadata,select=-c(SampleID))

write.csv(metadata, file="/Users/Dani/Documents/Data_Analysis/KI_Platy/data/Coralphoto__Metadata/KI_Platy_metadata.csv")
write.table(metadata, file="/Users/Dani/Documents/Data_Analysis/KI_Platy/data/Coralphoto__Metadata/KI_Platy_metadata.tsv", quote=FALSE, sep="\t", col.names = NA)
