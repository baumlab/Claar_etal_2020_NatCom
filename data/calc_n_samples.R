load("data/KI_seqs_f_coral_grouped_all.RData")


test <- metadata.SH[,c(1:2,4,6:8)]
names(test)

test$info <- ifelse(as.numeric(test$coral_tag) > 896,"after_only",test$coral_tag)

test$info <- ifelse(as.numeric(test$coral_tag) < 858,"sampled_before",test$info)

# test$info <- ifelse(as.numeric(test$info) > 851,"sampled_during",test$info)

test$info <- ifelse(is.na(test$info),test$coral_tag,test$info)

test$info <- ifelse(test$info=="410_893","sampled_before",test$info)

test$info <- ifelse(test$info=="754_899_1060","sampled_before",test$info)

test$info <- ifelse(test$info=="338_1168","sampled_before",test$info)


test$info
head(test)

test_agg <- aggregate(x=test$coral_tag, 
          by=list(info = test$info,
                  Status = test$Status),
          FUN=length)



seq <- sample_data(phy97.f.c.platy)

names(seq)
seq <- seq[,c(1,3:7)]
head(seq)

seq$info <- ifelse(as.numeric(as.character(seq$coral_tag)) > 896,"after_only",seq$coral_tag)

seq$info <- ifelse(as.numeric(as.character(seq$coral_tag)) < 858,"sampled_before",seq$info)

# seq$info <- ifelse(as.numeric(as.character(seq$coral_tag))> 851,"sampled_during",seq$info)

seq$info <- ifelse(is.na(seq$info),seq$coral_tag,seq$info)

seq$info <- ifelse(seq$coral_tag=="410_893","sampled_before",seq$info)

seq$info <- ifelse(seq$coral_tag=="754_899_1060","sampled_before",seq$info)

seq$info <- ifelse(seq$coral_tag=="338_1168","sampled_before",seq$info)
seq$info

seq$ref <- gsub(pattern="2015May*",replacement="KI2015b",seq$ref)
seq$ref <- gsub(pattern="2016March*",replacement="KI2016a",seq$ref)
seq$ref <- gsub(pattern="2015Jan_Post*",replacement="KI2015a",seq$ref)
seq$ref <- gsub(pattern="2015Jan_Pre*",replacement="KI2015a",seq$ref)
seq$ref <- gsub(pattern="2015July*",replacement="KI2015c",seq$ref)
seq$ref <- gsub(pattern="2014",replacement="KI2014",seq$ref)

seq$ref %in% test$ref
seq <- as(seq, "data.frame")
str(seq)

qpcr_seq <- merge(seq,test,by="ref",all = TRUE)
names(qpcr_seq)
qpcr_seq$coral_tag.x==qpcr_seq$coral_tag.y
qpcr_seq$field_season.x==qpcr_seq$field_season.y
qpcr_seq$site.x==qpcr_seq$site.y
qpcr_seq$Status.x==qpcr_seq$Status.y # Some don't match!! Potential data inconsisency... CHECK!
qpcr_seq$info.x==qpcr_seq$info.y

qpcr_seq$coral_tag <- ifelse(is.na(qpcr_seq$coral_tag.x),as.character(qpcr_seq$coral_tag.y),as.character(qpcr_seq$coral_tag.x))
qpcr_seq$info <- ifelse(is.na(qpcr_seq$info.y),qpcr_seq$info.x,qpcr_seq$info.y)
qpcr_seq$Status <- ifelse(!is.na(qpcr_seq$Status.y),as.character(qpcr_seq$Status.y),as.character(qpcr_seq$Status.x))

qpcr_seq_agg <- aggregate(x=qpcr_seq$coral_tag, 
                      by=list(info = qpcr_seq$info,
                              Status = qpcr_seq$Status),
                      FUN=length)

tail(qpcr_seq)

seq_only <- qpcr_seq[which(is.na(qpcr_seq$coral_tag.y)),]
qpcr_only <- qpcr_seq[which(is.na(qpcr_seq$coral_tag.x)),]
both <- qpcr_seq[which((!is.na(qpcr_seq$info.x)) & !is.na(qpcr_seq$info.y)),]

qpcr_only$ref %in% seq_only$ref
seq_only$ref %in% qpcr_only$ref
both$ref %in% qpcr_only$ref
both$ref %in% seq_only$ref
qpcr_only$ref %in% both$ref
seq_only$ref %in% both$ref

qpcr_onlyN <- nrow(qpcr_only) 
seq_onlyN <- nrow(seq_only) 
bothN <- nrow(both)

totalN <- nrow(qpcr_only) + nrow(seq_only) + nrow(both)
totalN

qpcr_onlyagg <- aggregate(x=qpcr_only$coral_tag, 
                          by=list(info = qpcr_only$info,
                                  Status = qpcr_only$Status),
                          FUN=length)

seq_onlyagg <- aggregate(x=seq_only$coral_tag, 
                          by=list(info = seq_only$info,
                                  Status = seq_only$Status),
                          FUN=length)

bothagg <- aggregate(x=both$coral_tag, 
                         by=list(info = both$info,
                                 Status = both$Status),
                         FUN=length)

t1 <- table(seq_only$Status)
t2 <- table(qpcr_only$Status)
t3 <- table(both$Status)

t4 <- table(seq_only$Status,seq_only$info.x)
t5 <- table(qpcr_only$Status,qpcr_only$info.y)
t6 <- table(both$Status,both$info.y)

t1["alive"]
t1["dead"]
t1[c("dead_or_gone","gone","UK")]

t2["alive"]
t2["dead"]
t2[c("dead_or_gone","gone","UK")]

t3["alive"]
t3["dead"]
t3[c("dead_or_gone","gone","UK")]

N_alive_sampledbefore <- t4["alive","sampled_before"] + t5["alive","sampled_before"] + t6["alive","sampled_before"]
N_dead_sampledbefore <- t4["dead","sampled_before"] + t6["dead","sampled_before"]

N_alive_afteronly <- t4["alive","after_only"] + t5["alive","after_only"] + t6["alive","after_only"]

N_deadorgone <- t4["dead_or_gone","sampled_before"] + t6["dead_or_gone","sampled_before"]
N_gone <- t4["gone","sampled_before"] + t5["gone","sampled_before"] + t6["gone","sampled_before"]
N_UK <- t4["UK","sampled_before"] + t5["UK","sampled_before"] + t6["UK","sampled_before"]

sum(N_deadorgone,N_gone,N_UK)

seq_onlyagg <- seq_onlyagg[,c(1,3)]

m1 <- merge(seq_onlyagg, bothagg, by="info")
m2 <- merge(m1,qpcr_only,by=c("info","Status"))
m2

nrow(m2)
table(m2$Status)
