rm(list=ls())

load("data/KI_seqs_f_coral_grouped.RData")
load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")


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
          FUN=function(x) length(unique(x)))



seq <- sample_data(phy97.f.c.platy)

names(seq)
seq <- seq[,c(1,3:7)]
head(seq)

seq$info <- ifelse(as.numeric(as.character(seq$coral_tag)) > 896,"after_only",seq$coral_tag)

seq$info <- ifelse(as.numeric(as.character(seq$coral_tag)) < 858,"sampled_before",seq$info)

# seq$info <- ifelse(as.numeric(as.character(seq$coral_tag))> 851,"sampled_during",seq$info)

seq$info <- ifelse(is.na(seq$info),as.character(seq$coral_tag),seq$info)

seq$info <- ifelse(seq$coral_tag=="410_893","sampled_before",seq$info)

seq$info <- ifelse(seq$coral_tag=="754_899","sampled_before",seq$info)

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

######### Try #2 ################
head(test)
rownames(test) <- test$SampleID
test <- test[,c(2:7)]
test$orig <- "test"
head(seq)
seq <- seq[,c(1:2,4:7)]
seq$orig <- "seq"
bla <- rbind(seq,test)
head(bla)

unique(bla$coral_tag)
bla$coral_tag[which(as.character(bla$coral_tag)=="754_899")] <- "754_899_1060"
bla$coral_tag[which(as.character(bla$coral_tag)=="410")] <- "410_893"

bla_before <- bla[which(bla$info=="sampled_before"),]
bla_after <- bla[which(bla$info=="after_only"),]

bla_simple <- bla_before[,c("site","coral_tag","Status")]
bla_simple2 <- bla_after[,c("site","coral_tag","Status")]

table(bla_simple)
bla_simple$Status[which(bla_simple$Status=="dead_or_gone")] <- "gone"
  

bla_simple$S <- paste(bla_simple$site, bla_simple$Status,sep="_")

table(bla_simple$site,bla_simple$Status)

unique(bla_simple$coral_tag[which(bla_simple$site==5)])


r <- bla_simple[(unique(bla_simple$coral_tag)),]
unique(r$coral_tag)

duplicated(bla_simple$coral_tag)

fuck <- aggregate(x=bla_simple$coral_tag, 
                          by=list(site = bla_simple$site,
                                  Status = bla_simple$Status),
                          FUN=function(x) length(unique(x)))
sum(fuck$x)

fuck2 <- aggregate(x=bla_simple2$coral_tag, 
                  by=list(site = bla_simple2$site,
                          Status = bla_simple2$Status),
                  FUN=function(x) length(unique(x)))
sum(fuck2$x)


sum(fuck2$x, fuck$x) 

bla_simple3 <- bla_simple
# bla_simple3$Status[which(bla_simple3$Status == "gone")] <- "UK"
fuck3 <- aggregate(x=bla_simple3$coral_tag, 
                   by=list(site = bla_simple3$site,
                           Status = bla_simple3$Status),
                   FUN=function(x) length(unique(x)))

fuck3_1 <- fuck3[which(fuck3$Status=="alive"),]
fuck3_2 <- fuck3[which(fuck3$Status=="dead"),]
fuck3_3 <- fuck3[which(fuck3$Status=="UK"),]
fuck3_4 <- fuck3[which(fuck3$Status=="gone"),]

bla_simple4 <- bla_simple3
bla_simple4$Status[which(bla_simple4$Status == "gone")] <- "dead"
bla_simple4$Status[which(bla_simple4$Status == "UK")] <- "dead"

fuck5 <- aggregate(x=bla_simple4$coral_tag, 
                   by=list(site = bla_simple4$site,
                           Status = bla_simple4$Status),
                   FUN=function(x) length(unique(x)))

fuck5_1 <- fuck5[which(fuck5$Status=="alive"),]
fuck5_2 <- fuck5[which(fuck5$Status=="dead"),]

alive3 <- sum(fuck5_1$x)
dead3 <- sum(fuck5_2$x)
total = alive3+dead3
total
alive3/total
dead3/total

alive <- sum(fuck3_1$x)
dead <- sum(fuck3_2$x)
UK <- sum(fuck3_3$x)
gone <- sum(fuck3_4$x)

known <- alive+dead
alive_pct <- alive/known
dead_pct <- dead/known

known_maybe <- alive+dead+gone
alive_pct_maybe <- alive/known_maybe
dead_pct_maybe <- (dead+gone)/known_maybe

ggplot(data=fuck) + geom_col(aes(x=site, y=x, fill = Status, color=site),position="dodge") + scale_fill_manual(values = c("blue","red","orange", "yellow")) + ylab("Number of Samples")

ggplot(data=fuck2) + geom_col(aes(x=site, y=x, fill = Status, color=site),position="dodge") + scale_fill_manual(values = c("blue","red","orange", "yellow")) + ylab("Number of Samples")

ggplot(data=fuck3) + geom_col(aes(x=site, y=x, fill = Status, color=site),position="dodge") + scale_fill_manual(values = c("blue","red","orange", "yellow")) + ylab("Number of Samples")

ggplot(data=fuck5) + geom_col(aes(x=site, y=x, fill = Status, color=site),position="dodge") + scale_fill_manual(values = c("blue","red","orange", "yellow")) + ylab("Number of Samples") + scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9))

###################################



qpcr_seq <- merge(seq,test,by="ref",all = TRUE)
names(qpcr_seq)
qpcr_seq$coral_tag.x==qpcr_seq$coral_tag.y
qpcr_seq$field_season.x==qpcr_seq$field_season.y
qpcr_seq$site.x==qpcr_seq$site.y
qpcr_seq$Status.x==qpcr_seq$Status.y # Some don't match!! Potential data inconsisency... CHECK!
qpcr_seq$info.x==qpcr_seq$info.y
qpcr$ref <- gsub(pattern="KI2015_Post*",replacement="KI2015a",seq$ref)
qpcr$ref <- gsub(pattern="KI2015_Pre*",replacement="KI2015a",seq$ref)


qpcr_seq$coral_tag <- ifelse(is.na(qpcr_seq$coral_tag.x),as.character(qpcr_seq$coral_tag.y),as.character(qpcr_seq$coral_tag.x))
qpcr_seq$info <- ifelse(is.na(qpcr_seq$info.y),qpcr_seq$info.x,qpcr_seq$info.y)
qpcr_seq$Status <- ifelse(!is.na(qpcr_seq$Status.y),as.character(qpcr_seq$Status.y),as.character(qpcr_seq$Status.x))

qpcr_seq_agg <- aggregate(x=qpcr_seq$coral_tag, 
                      by=list(info = qpcr_seq$info,
                              Status = qpcr_seq$Status),
                      FUN=function(x) length(unique(x)))

sum(qpcr_seq_agg$x)
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
                          FUN=function(x) length(unique(x)))

seq_onlyagg <- aggregate(x=seq_only$coral_tag, 
                          by=list(info = seq_only$info,
                                  Status = seq_only$Status),
                         FUN=function(x) length(unique(x)))

bothagg <- aggregate(x=both$coral_tag, 
                         by=list(info = both$info,
                                 Status = both$Status),
                     FUN=function(x) length(unique(x)))

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

a1 <- qpcr_only$coral_tag.y[which(qpcr_only$site.y==5)]
a2 <- seq_only$coral_tag.x[which(qpcr_only$site.y==5)]
a3 <- both$coral_tag.x[which(both$site.y==5)]

a1 %in% a2
a2 %in% a3

qpcr_onlyagg_site <- aggregate(x=qpcr_only$coral_tag, 
                          by=list(info = qpcr_only$info,
                                  Status = qpcr_only$Status,
                                  site = qpcr_only$site.y),
                          FUN=function(x) length(unique(x)))

seq_onlyagg_site <- aggregate(x=seq_only$coral_tag, 
                         by=list(info = seq_only$info,
                                 Status = seq_only$Status,
                                 site = seq_only$site.x),
                         FUN=function(x) length(unique(x)))


bothagg_site <- aggregate(x=both$coral_tag, 
                     by=list(info = both$info,
                             Status = both$Status,
                             site = both$site.y),
                     FUN=function(x) length(unique(x)))

m3 <- merge(bothagg_site,seq_onlyagg_site,by=c("info","Status","site"),all = TRUE)
m4 <- merge(m3,qpcr_onlyagg_site,by=c("info","Status","site"),all = TRUE)
colnames(m4) <- c("info","Status","site","both","seq_only","qpcr_only")
m4$total <- rowSums(m4[4:6],na.rm = TRUE)
m4

m5 <- m4[order(m4$site),]
m5

m5total <- m5[,c(1,3,2,7)]
m5total$Status[which(m5total$Status=="dead_or_gone")] <- "gone"

write.csv(m5total, "data/calc_n_samples.csv")

m5total_before <- m5total[which(m5total$info=="sampled_before"),]
ggplot(data=m5total) + geom_col(aes(x=site, y=total, fill = Status, alpha=info, color=info),position="dodge") + scale_fill_manual(values = c("blue","red","orange", "yellow")) + ylab("Number of Samples")
 