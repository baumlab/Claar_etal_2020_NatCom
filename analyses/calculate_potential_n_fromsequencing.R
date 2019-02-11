rm(list=ls())

data <- read.csv("~/../Desktop/Sequencing_Runs_1-6_PlatyOptions.csv")

colnames(data) <- c("Coral_Tag","Coral_Tag_All","SampleType","Year","Tube_label","Coral_Species","Site","Run")

data <- data[,c(1:7)]

data_w <- reshape(data,
                    idvar = c("Coral_Tag","Coral_Tag_All","SampleType","Coral_Species","Site"),
                    timevar = c("Year"),
                    direction = "wide")

n_colonies_total <- nrow(data_w) # 365 coral colonies

unique(data_w$Coral_Species)
colnames(data_w) <- c("Coral_Tag", "Coral_Tag_All", "SampleType", 
                      "Coral_Species","Site","Tube_label.2014",
                      "Tube_label.2015Jan","Tube_label.2015May",
                      "Tube_label.2015July", "Tube_label.2016March",
                      "Tube_label.2016Nov", "Tube_label.2017")  
# Condense/Remove duplicated rows
data_w$Tube_label.2017 <- as.character(data_w$Tube_label.2017)
data_w$Tube_label.2016Nov <- as.character(data_w$Tube_label.2016Nov)
data_w$Tube_label.2015July <- as.character(data_w$Tube_label.2015July)


data_w[data_w$Coral_Tag==62,]
data_w$Tube_label.2017[data_w$Coral_Tag==62 & data_w$Coral_Tag_All==62] <- paste(data_w$Tube_label.2017[data_w$Coral_Tag==62 & data_w$Coral_Tag_All==62],data_w$Tube_label.2017[data_w$Coral_Tag==62 & data_w$Coral_Tag_All=="62_1260"], sep="_")
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("62_1260")), ]

data_w[data_w$Coral_Tag==99,]
data_w$Tube_label.2017[data_w$Coral_Tag==99 & data_w$Coral_Tag_All==99] <- paste(data_w$Tube_label.2017[data_w$Coral_Tag==99 & data_w$Coral_Tag_All==99],data_w$Tube_label.2017[data_w$Coral_Tag==99 & data_w$Coral_Tag_All=="99_1312"], sep="_")
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("99_1312")), ]

data_w[data_w$Coral_Tag==128,]
data_w$Tube_label.2017[data_w$Coral_Tag==128 & data_w$Coral_Tag_All==128] <- paste(data_w$Tube_label.2017[data_w$Coral_Tag==128 & data_w$Coral_Tag_All==128],data_w$Tube_label.2017[data_w$Coral_Tag==128 & data_w$Coral_Tag_All=="128_1261"], sep="_")
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("128_1261")), ]

data_w[data_w$Coral_Tag==146,]
data_w$Tube_label.2016Nov[data_w$Coral_Tag==146 & data_w$Coral_Tag_All==146] <- data_w$Tube_label.2016Nov[data_w$Coral_Tag==146 & data_w$Coral_Tag_All=="146_1081"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("146_1081")), ]

data_w[data_w$Coral_Tag==205,]
data_w$Tube_label.2017[data_w$Coral_Tag==205 & data_w$Coral_Tag_All==205] <- data_w$Tube_label.2017[data_w$Coral_Tag==205 & data_w$Coral_Tag_All=="205_1258"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("205_1258")), ]

data_w[data_w$Coral_Tag==248,]
data_w$Tube_label.2015May[data_w$Coral_Tag==248 & data_w$Coral_Tag_All==248] <- data_w$Tube_label.2015May[data_w$Coral_Tag==248 & data_w$Coral_Tag_All=="248_696"]
data_w$Tube_label.2015July[data_w$Coral_Tag==248 & data_w$Coral_Tag_All==248] <- data_w$Tube_label.2015July[data_w$Coral_Tag==248 & data_w$Coral_Tag_All=="248_696"]
data_w$Tube_label.2016March[data_w$Coral_Tag==248 & data_w$Coral_Tag_All==248] <- data_w$Tube_label.2016March[data_w$Coral_Tag==248 & data_w$Coral_Tag_All=="248_696"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("248_696")), ]


data_w[data_w$Coral_Tag==370,]
data_w$Tube_label.2015July[data_w$Coral_Tag==370 & data_w$Coral_Tag_All==370] <- data_w$Tube_label.2015July[data_w$Coral_Tag==370 & data_w$Coral_Tag_All=="370_1265"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("370_1265")), ]

data_w[data_w$Coral_Tag==388,]
data_w$Tube_label.2016Nov[data_w$Coral_Tag==388 & data_w$Coral_Tag_All==388] <- data_w$Tube_label.2016Nov[data_w$Coral_Tag==388 & data_w$Coral_Tag_All=="388_1168"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("388_1168")), ]

data_w[data_w$Coral_Tag==400,]
data_w$Tube_label.2017[data_w$Coral_Tag==400 & data_w$Coral_Tag_All==400] <- data_w$Tube_label.2017[data_w$Coral_Tag==400 & data_w$Coral_Tag_All=="400_1102"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("400_1102")), ]

data_w[data_w$Coral_Tag==410,]
data_w$Tube_label.2015July[data_w$Coral_Tag==410 & data_w$Coral_Tag_All==410] <- data_w$Tube_label.2015July[data_w$Coral_Tag==410 & data_w$Coral_Tag_All=="410_893"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("410_893")), ]

data_w[data_w$Coral_Tag==412,]
data_w$Tube_label.2015July[data_w$Coral_Tag==412 & data_w$Coral_Tag_All==412] <- data_w$Tube_label.2015July[data_w$Coral_Tag==412 & data_w$Coral_Tag_All=="412_892"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("412_892")), ]

data_w[data_w$Coral_Tag==482,]
data_w$Tube_label.2016Nov[data_w$Coral_Tag==482 & data_w$Coral_Tag_All==482] <- data_w$Tube_label.2016Nov[data_w$Coral_Tag==482 & data_w$Coral_Tag_All=="482_897" & data_w$Coral_Species=="Favites pentagona"]
data_w <- data_w[ !(data_w$Coral_Species %in% c("Favites pentagona") & data_w$Coral_Tag_All %in% c("482_897")), ]

data_w[data_w$Coral_Tag==482,]
data_w$Tube_label.2017[data_w$Coral_Tag==482 & data_w$Coral_Tag_All==482] <- data_w$Tube_label.2017[data_w$Coral_Tag==482 & data_w$Coral_Tag_All=="482_897"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("482_897")), ]

data_w[data_w$Coral_Tag==588,]
data_w$Tube_label.2016Nov[data_w$Coral_Tag==588 & data_w$Coral_Species=="Favites pentagona"] <- data_w$Tube_label.2016Nov[data_w$Coral_Tag==588 & data_w$Coral_Tag_All=="588_1227" & data_w$Coral_Species=="Favites pentagona"]
data_w <- data_w[ !(data_w$Coral_Species %in% c("Favites pentagona") & data_w$Coral_Tag_All %in% c("588_1227")), ]

data_w[data_w$Coral_Tag==597,]
data_w$Tube_label.2016Nov[data_w$Coral_Tag==597 & data_w$Coral_Tag_All==597] <- data_w$Tube_label.2016Nov[data_w$Coral_Tag==597 & data_w$Coral_Tag_All=="597_1228" & data_w$Coral_Species=="Favites pentagona"]
data_w <- data_w[ !(data_w$Coral_Species %in% c("Favites pentagona") & data_w$Coral_Tag_All %in% c("597_1228")), ]

data_w[data_w$Coral_Tag==656,]
data_w$Tube_label.2016Nov[data_w$Coral_Tag==656 & data_w$Coral_Tag_All==656] <- data_w$Tube_label.2016Nov[data_w$Coral_Tag==656 & data_w$Coral_Tag_All=="656_1075"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("656_1075")), ]

data_w[data_w$Coral_Tag==754,]
data_w$Tube_label.2016March[data_w$Coral_Tag==754 & data_w$Coral_Tag_All==754] <- data_w$Tube_label.2016March[data_w$Coral_Tag==754 & data_w$Coral_Tag_All=="754_899"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("754_899")), ]

data_w[data_w$Coral_Tag==754,]
data_w$Tube_label.2016Nov[data_w$Coral_Tag==754 & data_w$Coral_Tag_All==754] <- data_w$Tube_label.2016Nov[data_w$Coral_Tag==754 & data_w$Coral_Tag_All=="754_899_1060"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("754_899_1060")), ]

data_w[data_w$Coral_Tag==907,]
data_w$Tube_label.2017[data_w$Coral_Tag==907 & data_w$Coral_Tag_All==907] <- data_w$Tube_label.2017[data_w$Coral_Tag==907 & data_w$Coral_Tag_All=="907_1284"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("907_1284")), ]

data_w[data_w$Coral_Tag==911,]
data_w$Tube_label.2017[data_w$Coral_Tag==911 & data_w$Coral_Tag_All==911] <- data_w$Tube_label.2017[data_w$Coral_Tag==911 & data_w$Coral_Tag_All=="911_1321"]
data_w <- data_w[ !(data_w$Coral_Tag_All %in% c("911_1321")), ]


##############
#Add in info about alive/dead manually from sequencing runs spreadsheet - 8Feb2019 - DC
# Alive means that it was alive in 2016a - it could have died after that, but we are considering that the endpoint for this study
head(data_w)

data_w$status <- NA

data_w$Coral_Tag[1:10]

data_w$status[data_w$Coral_Tag==4] <- "gone_after" #2016a
data_w$status[data_w$Coral_Tag==11] <- "dead" #2016a
data_w$status[data_w$Coral_Tag==17] <- "dead"#2016a
data_w$status[data_w$Coral_Tag==19] <- "UK" #last sampled 2014
data_w$status[data_w$Coral_Tag==24] <- "alive" #barely
data_w$status[data_w$Coral_Tag==29] <- "alive"
data_w$status[data_w$Coral_Tag==34] <- "UK" #Last sampled 2015c
data_w$status[data_w$Coral_Tag==51] <- "dead" #2016a
data_w$status[data_w$Coral_Tag==53] <- "gone_before" #2015a
data_w$status[data_w$Coral_Tag==56] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==61] <- "alive"
data_w$status[data_w$Coral_Tag==62] <- "alive"
data_w$status[data_w$Coral_Tag==64] <- "dead" #2016a,last sampled 2014
data_w$status[data_w$Coral_Tag==72] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==88] <- "dead"#2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==89] <- "alive"
data_w$status[data_w$Coral_Tag==91] <- "gone_before" # 2015a_Post, last sampled 2015a_Pre
data_w$status[data_w$Coral_Tag==93] <- "UK" #last sampled 2015a
data_w$status[data_w$Coral_Tag==95] <- "alive" #barely
data_w$status[data_w$Coral_Tag==96] <- "dead"#2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==99] <- "alive" 
data_w$status[data_w$Coral_Tag==100] <- "alive"
data_w$status[data_w$Coral_Tag==101] <- "gone_before" # 2015a_Pre, last sampled 2014
data_w$status[data_w$Coral_Tag==103] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==108] <- "gone_before" # 2015a_Pre, last sampled 2014
data_w$status[data_w$Coral_Tag==111] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==113] <- "dead" #2016a
data_w$status[data_w$Coral_Tag==115] <- "alive" #barely
data_w$status[data_w$Coral_Tag==118] <- "alive" #barely
data_w$status[data_w$Coral_Tag==127] <- "alive"
data_w$status[data_w$Coral_Tag==128] <- "alive" 
data_w$status[data_w$Coral_Tag==130] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==138] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==141] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==146] <- "alive"
data_w$status[data_w$Coral_Tag==148] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==153] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==157] <- "alive"
data_w$status[data_w$Coral_Tag==158] <- "UK" #Last sampled 2015c
data_w$status[data_w$Coral_Tag==161] <- "UK" #Last sampled 2015c
data_w$status[data_w$Coral_Tag==164] <- "alive"
data_w$status[data_w$Coral_Tag==173] <- "alive"
data_w$status[data_w$Coral_Tag==176] <- "UK"#Last sampled 2014
data_w$status[data_w$Coral_Tag==181] <- "gone_after" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==187] <- "alive"
data_w$status[data_w$Coral_Tag==190] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==191] <- "alive"
data_w$status[data_w$Coral_Tag==192] <- "dead"#2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==195] <- "dead"#2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==199] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==202] <- "UK" #Last sampled 2015c
data_w$status[data_w$Coral_Tag==205] <- "alive" 
data_w$status[data_w$Coral_Tag==206] <- "alive"
data_w$status[data_w$Coral_Tag==207] <- "UK" #Last sampled 2015c
data_w$status[data_w$Coral_Tag==212] <- "dead" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==213] <- "alive"
data_w$status[data_w$Coral_Tag==214] <- "UK" # last sampled 2015c
data_w$status[data_w$Coral_Tag==223] <- "alive"
data_w$status[data_w$Coral_Tag==224] <- "alive"
data_w$status[data_w$Coral_Tag==226] <- "dead" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==227] <- "alive"
data_w$status[data_w$Coral_Tag==231] <- "alive"
data_w$status[data_w$Coral_Tag==232] <- "dead" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==234] <- "dead"#2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==237] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==240] <- "UK" #Last sampled 2015c
data_w$status[data_w$Coral_Tag==242] <- "UK" # last sampled 2014
data_w$status[data_w$Coral_Tag==243] <- "UK" # last sampled 2014
data_w$status[data_w$Coral_Tag==244] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==245] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==247] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==248] <- "alive"
data_w$status[data_w$Coral_Tag==249] <- "gone_after" #2016b, last sampled 2015b
data_w$status[data_w$Coral_Tag==253] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==255] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==258] <- "UK"# Last sampled 2014
data_w$status[data_w$Coral_Tag==260] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==263] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==266] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==267] <- "UK"# Last sampled 2014
data_w$status[data_w$Coral_Tag==268] <- "UK"# Last sampled 2014
data_w$status[data_w$Coral_Tag==271] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==272] <- "gone_after" # 2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==273] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==274] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==278] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==281] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==284] <- "dead" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==285] <- "alive"
data_w$status[data_w$Coral_Tag==289] <- "alive"
data_w$status[data_w$Coral_Tag==290] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==292] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==297] <- "dead" #2016a,last sampled 2014
data_w$status[data_w$Coral_Tag==298] <- "dead" #2016a,last sampled 2014
data_w$status[data_w$Coral_Tag==299] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==302] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==306] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==308] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==311] <- "gone_UK" #2016b, last sampled 2014
data_w$status[data_w$Coral_Tag==314] <- "alive"
data_w$status[data_w$Coral_Tag==315] <- "dead" # 2016b, last sampled 2014
data_w$status[data_w$Coral_Tag==320] <- "dead" # 2016b, last sampled 2014
data_w$status[data_w$Coral_Tag==323] <- "alive" #barely
data_w$status[data_w$Coral_Tag==328] <- "alive"
data_w$status[data_w$Coral_Tag==330] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==333] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==334] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==338] <- "alive" #2017, last sampled 2014
data_w$status[data_w$Coral_Tag==339] <- "alive"
data_w$status[data_w$Coral_Tag==340] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==341] <- "gone_after" # 2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==344] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==346] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==348] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==349] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==351] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==352] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==353] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==367] <- "UK" # Last sampled 2014
data_w$status[data_w$Coral_Tag==370] <- "alive"
data_w$status[data_w$Coral_Tag==371] <- "gone_UK" #2017, last sampled 2014
data_w$status[data_w$Coral_Tag==373] <- "UK" #Last sampled 2015c
data_w$status[data_w$Coral_Tag==376] <- "dead" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==379] <- "alive"
data_w$status[data_w$Coral_Tag==381] <- "alive" #barely
data_w$status[data_w$Coral_Tag==382] <- "dead" # 2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==386] <- "alive"
data_w$status[data_w$Coral_Tag==388] <- "alive" #barely
data_w$status[data_w$Coral_Tag==389] <- "UK" #Last sampled 2015b
data_w$status[data_w$Coral_Tag==395] <- "gone_after" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==398] <- "dead" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==400] <- "alive"
data_w$status[data_w$Coral_Tag==402] <- "gone_after" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==404] <- "dead" # 2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==406] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==408] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==410] <- "UK" #last sampled 2015c
data_w$status[data_w$Coral_Tag==411] <- "dead" #2017, last sampled 2015c
data_w$status[data_w$Coral_Tag==412] <- "UK" # recorded as "Barely alive?" in 2018, but cannot be confirmed. Last sampled in 2015c
data_w$status[data_w$Coral_Tag==415] <- "alive"
data_w$status[data_w$Coral_Tag==429] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==431] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==433] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==435] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==439] <- "alive"
data_w$status[data_w$Coral_Tag==442] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==444] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==447] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==449] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==454] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==455] <- "UK" #Last sampled 2015a
data_w$status[data_w$Coral_Tag==457] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==461] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==465] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==466] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==470] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==473] <- "gone_before" #2015a, last samples 2014
data_w$status[data_w$Coral_Tag==478] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==479] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==482] <- "alive"
data_w$status[data_w$Coral_Tag==483] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==486] <- "UK" #Last samples 2015b
data_w$status[data_w$Coral_Tag==487] <- "dead" #2016b, last samples 2015c
data_w$status[data_w$Coral_Tag==492] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==493] <- "UK" #Last sampled 2014
data_w$status[data_w$Coral_Tag==497] <- "alive" 
data_w$status[data_w$Coral_Tag==500] <- "alive" #inconsistent species 
data_w$status[data_w$Coral_Tag==504] <- "gone_UK" #2016a, last sampled 2014
data_w$status[data_w$Coral_Tag==575] <- "gone_before" # 2015a_post, last sampled 2015a_pre
data_w$status[data_w$Coral_Tag==577] <- "alive" #barely
data_w$status[data_w$Coral_Tag==581] <- "alive" #barely
data_w$status[data_w$Coral_Tag==583] <- "alive" #barely
data_w$status[data_w$Coral_Tag==587] <- "alive"
data_w$status[data_w$Coral_Tag==588] <- "alive"
data_w$status[data_w$Coral_Tag==590] <- "alive"
data_w$status[data_w$Coral_Tag==592] <- "alive" 
data_w$status[data_w$Coral_Tag==594] <- "alive"
data_w$status[data_w$Coral_Tag==595] <- "alive" #barely
data_w$status[data_w$Coral_Tag==597] <- "alive"
data_w$status[data_w$Coral_Tag==605] <- "alive"
data_w$status[data_w$Coral_Tag==607] <- "alive" #barely in 2016a, died in 2016b
data_w$status[data_w$Coral_Tag==612] <- "alive"
data_w$status[data_w$Coral_Tag==618] <- "alive"
data_w$status[data_w$Coral_Tag==619] <- "alive"
data_w$status[data_w$Coral_Tag==620] <- "alive"
data_w$status[data_w$Coral_Tag==623] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==628] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==630] <- "alive"
data_w$status[data_w$Coral_Tag==631] <- "alive" #inconsistent species
data_w$status[data_w$Coral_Tag==637] <- "alive"
data_w$status[data_w$Coral_Tag==656] <- "alive"
data_w$status[data_w$Coral_Tag==658] <- "gone_after" #2016b, last sampled 2015c
data_w$status[data_w$Coral_Tag==664] <- "UK" #last sampled 2015c
data_w$status[data_w$Coral_Tag==668] <- "UK" #last sampled 2015c
data_w$status[data_w$Coral_Tag==675] <- "UK" #last sampled 2015c
data_w$status[data_w$Coral_Tag==681] <- "UK" #last sampled 2015c
data_w$status[data_w$Coral_Tag==685] <- "UK" #last sampled 2015b
data_w$status[data_w$Coral_Tag==689] <- "alive"
data_w$status[data_w$Coral_Tag==691] <- "alive" #barely, dead 2016b
data_w$status[data_w$Coral_Tag==695] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==699] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==719] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==722] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==726] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==727] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==729] <- "alive"
data_w$status[data_w$Coral_Tag==735] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==736] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==741] <- "alive"
data_w$status[data_w$Coral_Tag==742] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==743] <- "alive" #but died in 2016b
data_w$status[data_w$Coral_Tag==754] <- "alive"
data_w$status[data_w$Coral_Tag==762] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==766] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==767] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==768] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==770] <- "alive"
data_w$status[data_w$Coral_Tag==772] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==776] <- "alive"
data_w$status[data_w$Coral_Tag==777] <- "alive" #barely
data_w$status[data_w$Coral_Tag==779] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==781] <- "alive" #barely, dead 2016b
data_w$status[data_w$Coral_Tag==783] <- "alive"
data_w$status[data_w$Coral_Tag==786] <- "alive"
data_w$status[data_w$Coral_Tag==787] <- "alive"
data_w$status[data_w$Coral_Tag==788] <- "alive"
data_w$status[data_w$Coral_Tag==792] <- "alive"
data_w$status[data_w$Coral_Tag==793] <- "alive" #barely, dead 2016b
data_w$status[data_w$Coral_Tag==797] <- "alive"
data_w$status[data_w$Coral_Tag==800] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==804] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==806] <- "alive" #but dead in 2017
data_w$status[data_w$Coral_Tag==807] <- "alive"
data_w$status[data_w$Coral_Tag==810] <- "dead" 
data_w$status[data_w$Coral_Tag==813] <- "alive"
data_w$status[data_w$Coral_Tag==817] <- "alive"
data_w$status[data_w$Coral_Tag==820] <- "alive"
data_w$status[data_w$Coral_Tag==824] <- "alive"
data_w$status[data_w$Coral_Tag==826] <- "alive" #barely, dead in 2016b
data_w$status[data_w$Coral_Tag==829] <- "alive" #barely, dead in 2016b
data_w$status[data_w$Coral_Tag==834] <- "alive"
data_w$status[data_w$Coral_Tag==844] <- "alive"
data_w$status[data_w$Coral_Tag==845] <- "gone_after" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==846] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==850] <- "alive"
data_w$status[data_w$Coral_Tag==851] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==854] <- "alive"
data_w$status[data_w$Coral_Tag==856] <- "dead" #2016a, last sampled 2015c
data_w$status[data_w$Coral_Tag==857] <- "alive"
data_w$status[data_w$Coral_Tag==900] <- "alive"
data_w$status[data_w$Coral_Tag==902] <- "alive"
data_w$status[data_w$Coral_Tag==903] <- "alive"
data_w$status[data_w$Coral_Tag==904] <- "alive"
data_w$status[data_w$Coral_Tag==906] <- "alive"
data_w$status[data_w$Coral_Tag==907] <- "alive"
data_w$status[data_w$Coral_Tag==909] <- "alive"
data_w$status[data_w$Coral_Tag==910] <- "alive"
data_w$status[data_w$Coral_Tag==911] <- "alive"
data_w$status[data_w$Coral_Tag==914] <- "alive"
data_w$status[data_w$Coral_Tag==916] <- "alive"
data_w$status[data_w$Coral_Tag==919] <- "alive"
data_w$status[data_w$Coral_Tag==920] <- "alive"
data_w$status[data_w$Coral_Tag==921] <- "alive"
data_w$status[data_w$Coral_Tag==925] <- "alive"
data_w$status[data_w$Coral_Tag==926] <- "alive"
data_w$status[data_w$Coral_Tag==927] <- "alive"
data_w$status[data_w$Coral_Tag==928] <- "alive"
data_w$status[data_w$Coral_Tag==929] <- "alive"
data_w$status[data_w$Coral_Tag==930] <- "alive"
data_w$status[data_w$Coral_Tag==932] <- "alive"
data_w$status[data_w$Coral_Tag==933] <- "alive"
data_w$status[data_w$Coral_Tag==935] <- "alive"
data_w$status[data_w$Coral_Tag==936] <- "alive"
data_w$status[data_w$Coral_Tag==937] <- "alive"
data_w$status[data_w$Coral_Tag==939] <- "alive"
data_w$status[data_w$Coral_Tag==940] <- "alive"
data_w$status[data_w$Coral_Tag==941] <- "alive"
data_w$status[data_w$Coral_Tag==942] <- "alive"
data_w$status[data_w$Coral_Tag==1005] <- "alive"
data_w$status[data_w$Coral_Tag==1008] <- "alive"
data_w$status[data_w$Coral_Tag==1009] <- "alive"
data_w$status[data_w$Coral_Tag==1011] <- "alive"
data_w$status[data_w$Coral_Tag==1013] <- "alive"
data_w$status[data_w$Coral_Tag==1014] <- "alive"
data_w$status[data_w$Coral_Tag==1017] <- "alive"
data_w$status[data_w$Coral_Tag==1020] <- "alive"
data_w$status[data_w$Coral_Tag==1022] <- "alive"
data_w$status[data_w$Coral_Tag==1023] <- "alive"
data_w$status[data_w$Coral_Tag==1024] <- "alive"
data_w$status[data_w$Coral_Tag==1026] <- "alive"
data_w$status[data_w$Coral_Tag==1028] <- "alive"
data_w$status[data_w$Coral_Tag==1030] <- "alive"
data_w$status[data_w$Coral_Tag==1031] <- "alive"
data_w$status[data_w$Coral_Tag==1032] <- "alive" 
data_w$status[data_w$Coral_Tag==1033] <- "alive" 
data_w$status[data_w$Coral_Tag==1034] <- "alive" 
data_w$status[data_w$Coral_Tag==1035] <- "alive"
data_w$status[data_w$Coral_Tag==1036] <- "alive"
data_w$status[data_w$Coral_Tag==1037] <- "alive"
data_w$status[data_w$Coral_Tag==1040] <- "alive"
data_w$status[data_w$Coral_Tag==1049] <- "alive"
data_w$status[data_w$Coral_Tag==1050] <- "alive"
data_w$status[data_w$Coral_Tag==1052] <- "alive"
data_w$status[data_w$Coral_Tag==1055] <- "alive"
data_w$status[data_w$Coral_Tag==1056] <- "alive"
data_w$status[data_w$Coral_Tag==1057] <- "alive"
data_w$status[data_w$Coral_Tag=="583b"] <- "alive"
data_w$status[data_w$Coral_Tag=="93b"] <- "UK" #Last sampled 2015a_post
data_w$status[data_w$Coral_Tag=="631b"] <- "alive"
data_w$status[data_w$Coral_Tag=="810b"] <- "alive"


data_w$Coral_Tag[duplicated(data_w$Coral_Tag)]
data_w[is.na(data_w$status),]

####
n_fpenta <- nrow(data_w[data_w$Coral_Species=="Favites pentagona",])
n_fmatt <- nrow(data_w[data_w$Coral_Species=="Favia matthaii",])
n_platy <- nrow(data_w[data_w$Coral_Species=="Platygyra sp",])
n_favhali <- nrow(data_w[data_w$Coral_Species=="Favites halicora",])
n_hydno <- nrow(data_w[data_w$Coral_Species=="Hydnophora microconos",])
n_faviasp <- nrow(data_w[data_w$Coral_Species=="Favia sp",])
n_favitessp <- nrow(data_w[data_w$Coral_Species=="Favites sp",])

n_all_favites <- n_fpenta+n_favhali+n_favitessp
n_all_favia <- n_fmatt+n_faviasp

head(data_w)

# Before is if it was sampled in any one (or more) of 2014, 2015 January, 2015 May
data_w$before <- ifelse(is.na(data_w$Tube_label.2014)==F | is.na(data_w$Tube_label.2015Jan)==F | is.na(data_w$Tube_label.2015May)==F,"Yes","No")
# During is in July 2015
data_w$during <- ifelse(is.na(data_w$Tube_label.2015July)==F,"Yes","No")
# After is in March 2016
data_w$after <- ifelse(is.na(data_w$Tube_label.2016March)==F,"Yes","No")
# Recovery is November 2016 and July 2017
data_w$recovery <- ifelse(is.na(data_w$Tube_label.2016Nov)==F | is.na(data_w$Tube_label.2017)==F,"Yes","No")

n_colonies_total_wbefore <- nrow(data_w[data_w$before=="Yes",]) # 246 coral colonies
n_fpenta_wbefore <- nrow(data_w[data_w$Coral_Species=="Favites pentagona" & data_w$before=="Yes",])
n_fmatt_wbefore <- nrow(data_w[data_w$Coral_Species=="Favia matthaii" & data_w$before=="Yes",])
n_platy_wbefore <- nrow(data_w[data_w$Coral_Species=="Platygyra sp" & data_w$before=="Yes",])
n_favhali_wbefore <- nrow(data_w[data_w$Coral_Species=="Favites halicora" & data_w$before=="Yes",])
n_hydno_wbefore <- nrow(data_w[data_w$Coral_Species=="Hydnophora microconos" & data_w$before=="Yes",])
n_faviasp_wbefore <- nrow(data_w[data_w$Coral_Species=="Favia sp" & data_w$before=="Yes",])
n_favitessp_wbefore <- nrow(data_w[data_w$Coral_Species=="Favites sp" & data_w$before=="Yes",])

n_all_favites_wbefore <- n_fpenta_wbefore+n_favhali_wbefore+n_favitessp_wbefore
n_all_favia_wbefore <- n_fmatt_wbefore+n_faviasp_wbefore

n_colonies_alive <- nrow(data_w[data_w$status=="alive",]) # 151 coral colonies
n_colonies_dead <- nrow(data_w[data_w$status=="dead",]) # 44 coral colonies
n_colonies_UK <- nrow(data_w[data_w$status=="UK",]) # 60 coral colonies
n_colonies_gone_before <- nrow(data_w[data_w$status=="gone_before",]) # 8 coral colonies
n_colonies_gone_after <- nrow(data_w[data_w$status=="gone_after",]) # 37 coral colonies
n_colonies_gone_UK <- nrow(data_w[data_w$status=="gone_UK",]) # 15 coral colonies

n_colonies_alive+n_colonies_dead+n_colonies_UK+n_colonies_gone_before+n_colonies_gone_after+n_colonies_gone_UK

nrow(data_w)
