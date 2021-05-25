### run from /PIMMVC/papers/competition/
rm(list=ls())
library(stringr)
path <- getwd()

path <- strsplit(path, "/papers/competition")[[1]]

traders <- read.csv(paste(path,"data/public/traders.csv", sep="/"), stringsAsFactors = TRUE)
farmers <- read.csv(paste(path,"data/public/farmers.csv", sep="/"), stringsAsFactors = TRUE)

## store data for table 1: dimensions are (variable , statistic, trader/farmer(panel A/B) )
df_averages <- array(NA,dim=c(10,4,2))
### relational contract proxies (trader level)
#credit q102n
df_averages[1,1,1] <- mean(traders$hh.maize.q30.f)
df_averages[1,2,1] <- as.numeric(median(traders$hh.maize.q30.f))
df_averages[1,3,1] <- sd(traders$hh.maize.q30.f)
df_averages[1,4,1] <- sum(!is.na(traders$hh.maize.q30.f))
#ag inputs  q102r
df_averages[2,1,1] <- mean(traders$hh.maize.q30.a)
df_averages[2,2,1] <- as.numeric(median(traders$hh.maize.q30.a))
df_averages[2,3,1] <- sd(traders$hh.maize.q30.a)
df_averages[2,4,1] <- sum(!is.na(traders$hh.maize.q30.a))

#training ()q102q
df_averages[3,1,1] <- mean(traders$hh.maize.q30.e)
df_averages[3,2,1] <- as.numeric(median(traders$hh.maize.q30.e))
df_averages[3,3,1] <- sd(traders$hh.maize.q30.e)
df_averages[3,4,1] <- sum(!is.na(traders$hh.maize.q30.e))

#storage inputs b,c or d -q102p
df_averages[4,1,1] <- mean(traders$hh.maize.q30.b | traders$hh.maize.q30.c | traders$hh.maize.q30.d)
df_averages[4,2,1] <- as.numeric(median(traders$hh.maize.q30.b | traders$hh.maize.q30.c | traders$hh.maize.q30.d))
df_averages[4,3,1] <- sd(traders$hh.maize.q30.b | traders$hh.maize.q30.c | traders$hh.maize.q30.d)
df_averages[4,4,1] <- sum(!is.na(traders$hh.maize.q30.b | traders$hh.maize.q30.c | traders$hh.maize.q30.d))

#buys before harvest
df_averages[5,1,1] <- mean(traders$hh.maize.q38=="Yes")
df_averages[5,2,1] <- as.numeric(median(traders$hh.maize.q38=="Yes"))
df_averages[5,3,1] <- sd(traders$hh.maize.q38=="Yes")
df_averages[5,4,1] <- sum(!is.na(traders$hh.maize.q38=="Yes"))

### relational contract proxies (farmer level)
farmers$hh.maize.trader1.q102n <- as.numeric(as.character(farmers$hh.maize.trader1.q102n))
#assumption there is not relational contract if farmer does not know
farmers$hh.maize.trader1.q102n[farmers$hh.maize.trader1.q102n == 98] <- 3

#share of farmers that sold to trader and report that trader provides credit
df_averages[1,1,2] <- mean(((farmers$hh.maize.trader1.q102n == 1 | farmers$hh.maize.trader1.q102n == 2) ), na.rm=T)
df_averages[1,2,2] <- as.numeric(median(((farmers$hh.maize.trader1.q102n == 1 | farmers$hh.maize.trader1.q102n == 2) ), na.rm=TRUE))
df_averages[1,3,2] <- sd(((farmers$hh.maize.trader1.q102n == 1 | farmers$hh.maize.trader1.q102n == 2) ), na.rm=T)
df_averages[1,4,2] <- sum(!is.na(farmers$hh.maize.trader1.q102n))

#Does this trader provide you with farming inputs such as maize seed, fertilizer and chemicals?
#assumption there is not relational contract if farmer does not know
farmers$hh.maize.trader1.q102r[farmers$hh.maize.trader1.q102r == "98"] <- "No"
farmers$hh.maize.trader1.q102r[farmers$hh.maize.trader1.q102r == "n/a"] <- NA

df_averages[2,1,2] <- mean(((farmers$hh.maize.trader1.q102r =="Yes") ), na.rm=T)
df_averages[2,2,2] <- as.numeric(median(((farmers$hh.maize.trader1.q102r =="Yes")  ), na.rm=TRUE))
df_averages[2,3,2] <- sd(((farmers$hh.maize.trader1.q102r =="Yes") ), na.rm=T)
df_averages[2,4,2] <- sum(!is.na(farmers$hh.maize.trader1.q102r))

#assumption there is not relational contract if farmer does not know
#Does this trader provide you with training on maize farming or maize storage and handling?
farmers$hh.maize.trader1.q102q[farmers$hh.maize.trader1.q102q == "98"] <- "No"
farmers$hh.maize.trader1.q102q[farmers$hh.maize.trader1.q102q == "n/a"] <- NA

df_averages[3,1,2] <- mean(((farmers$hh.maize.trader1.q102q =="Yes") ), na.rm=T)
df_averages[3,2,2] <- as.numeric(median(((farmers$hh.maize.trader1.q102q =="Yes")  ), na.rm=TRUE))
df_averages[3,3,2] <- sd(((farmers$hh.maize.trader1.q102q =="Yes") ), na.rm=T)
df_averages[3,4,2] <- sum(!is.na(farmers$hh.maize.trader1.q102q))

#assumption there is not relational contract if farmer does not know
#Does this trader provides you with storage and handling related inputs such as bags, tarpaulins?
farmers$hh.maize.trader1.q102p[farmers$hh.maize.trader1.q102p == "98"] <- "No"
farmers$hh.maize.trader1.q102p[farmers$hh.maize.trader1.q102p == "n/a"] <- NA

df_averages[4,1,2] <- mean(((farmers$hh.maize.trader1.q102p =="Yes") ), na.rm=T)
df_averages[4,2,2] <- as.numeric(median(((farmers$hh.maize.trader1.q102p =="Yes")  ), na.rm=TRUE))
df_averages[4,3,2] <- sd(((farmers$hh.maize.trader1.q102p =="Yes") ), na.rm=T)
df_averages[4,4,2] <- sum(!is.na(farmers$hh.maize.trader1.q102p))



#competition indicators: what can we use as measures of competition? 
#at the level of the farmers, it is probably easiest to use:
#integer	q105	105. Please estimate how many of these maize traders or middlemen are buying maize in your village or neighborhood.
# for input dealers and input shops, if we decided to also run this analysis, we may want to do something similar to 
#farmer level

farmers$hh.maize.q105[farmers$hh.maize.q105==999] <- NA



### some quick regressions
summary(lm((farmers$hh.maize.trader1.q102n == 1 | farmers$hh.maize.trader1.q102n == 2)~hh.maize.q105,data=farmers))


#now merge
merged <- merge(farmers, traders, by.x="id.trader1", by.y="id.trader")

merged$hh.maize.q105[merged$hh.maize.q105==999] <- NA
merged$hh.maize.q105[merged$hh.maize.q105==0] <- NA
#test if households report more competition in traders say they collide
summary(lm(hh.maize.q105~(hh.maize.q40.y==4), data=merged))



#### Dummies --- FARMERS #####
##############################

farmers$gender <- 0
farmers$gender [farmers$hh.maize.q25=="Female"] <- 1 #female farmers
farmers$educ <- 0
farmers$educ[farmers$hh.maize.q27=="b" | farmers$hh.maize.q27=="c" | farmers$hh.maize.q27=="d" | farmers$hh.maize.q27=="e" | 
               farmers$hh.maize.q27=="f" | farmers$hh.maize.q27=="g" ] <- 1 #educated farmers
farmers$married <- ifelse(farmers$hh.maize.q26 == 'a', 1, 0)  #married farmers 
farmers$member <- ifelse(farmers$hh.maize.q37 == 'Yes', 1, 0)  #member of farmer group or cooperatives 

farmers$hh.maize.q24[farmers$hh.maize.q24==999] <- NA #age 
farmers$hh.maize.q36[farmers$hh.maize.q36==999] <- NA #Land for crop production (Acres) 

#maize sold in bags 
farmers$hh.maize.q101a[farmers$hh.maize.q101a==999] <- NA
table(farmers$hh.maize.q101a)
farmers$maizesold <- as.numeric(as.character(farmers$hh.maize.q101a))
mean(farmers$maizesold, na.rm=T)

#number of separate transactions 
farmers$notrans <- as.numeric(as.character(farmers$hh.maize.q101b))
mean(farmers$notrans, na.rm=T)


#### Dummies --- TRADERS #####
##############################

traders$gender <- 0
traders$gender [traders$hh.maize.q7=="Female"] <- 1 #female traders
traders$educ <- 0
traders$educ[traders$hh.maize.q9=="b" |traders$hh.maize.q9=="c" | traders$hh.maize.q9=="d" | traders$hh.maize.q9=="e" | 
               traders$hh.maize.q9=="f"] <- 1 #educated traders
traders$married <- ifelse(traders$hh.maize.q8 == 'a', 1, 0)  #married traders

#percentage of maize traded
traders$maizetrade<-as.numeric(as.character(traders$hh.maize.q14)) 
mean(traders$maizetrade, na.rm=T)

##ACTION radius
#a	Entire country; b	Various districts ;c	One district
#d	Various sub-counties ;e	One subcounty; f	various parishes; g	One parish; h	different villages; i	One village
#j	Part of village; k	Only a few households
traders$radius<-as.character(traders$hh.maize.radius)
table(traders$radius)

traders$rada <- ifelse(traders$radius == 'a', 1, 0) 
traders$radb <- ifelse(traders$radius == 'b', 1, 0) 
traders$radc <- ifelse(traders$radius == 'c', 1, 0) 
traders$radd <- ifelse(traders$radius == 'd', 1, 0) 
traders$rade <- ifelse(traders$radius == 'e', 1, 0) 
traders$radf <- ifelse(traders$radius == 'f', 1, 0) 
traders$radg <- ifelse(traders$radius == 'g', 1, 0) 
traders$radh <- ifelse(traders$radius == 'h', 1, 0) 
traders$radi <- ifelse(traders$radius == 'i', 1, 0)
traders$radj <- ifelse(traders$radius == 'j', 1, 0) 
traders$radk <- ifelse(traders$radius == 'k', 1, 0) 

#number of other traders operating in the area 
traders$hh.maize.q21[traders$hh.maize.q21==999] <- NA
table(traders$hh.maize.q21)
traders$other <- as.numeric(as.character(traders$hh.maize.q21))
mean(traders$other, na.rm=T)

#storage capacity in kgs
traders$hh.maize.q28[traders$hh.maize.q28==999] <- NA
table(traders$hh.maize.q28)
traders$storage <- as.numeric(as.character(traders$hh.maize.q28))
mean(traders$storage, na.rm=T)

traders$lowprice <- as.numeric(as.character(traders$hh.maize.q22e)) #lowest selling price 
mean(traders$lowprice, na.rm=T)
traders$highprice <- as.numeric(as.character(traders$hh.maize.q23e)) #highest selling price 
mean(traders$highprice, na.rm=T)

#Highest buying price
traders$hh.maize.q23c[traders$hh.maize.q23c==999] <- NA

#Maize collected per day (kgs, during planting/growing)
traders$hh.maize.q23b[traders$hh.maize.q23b==999] <- NA

### services to farmers
#a	Inputs (seed, fertilizer); b	Tarpaulins for drying of maize; c	PICS bags; d	Gunny bags
#e	Technical assistance or training in post-harvest handling (drying and storage)
#f	Credit; g	Provide nothing

traders$certscales <- ifelse(traders$hh.maize.q32 == 'Yes', 1, 0) #certified scales dummy 


