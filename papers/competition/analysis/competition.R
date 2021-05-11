### run from /PIMMVC/papers/competition/
rm(list=ls())
path <- getwd()

path <- strsplit(path, "/papers/competition")[[1]]

traders <- read.csv(paste(path,"data/public/traders.csv", sep="/"), stringsAsFactors = TRUE)
farmers <- read.csv(paste(path,"data/public/farmers.csv", sep="/"), stringsAsFactors = TRUE)
#These are the variables that 


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
