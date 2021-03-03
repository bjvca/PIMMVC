path <- getwd()
library(ggplot2)
library(ggsignif) 
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

#############INPUT DEALERS#####################

##Prepping data
trans <- c("hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.agro1","hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")],"Yes")
names(stack1) <- c("farmerID","id.agro","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")
stack2 <- cbind(farmers[c("ID","id.agro2","hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l","hh.maize.agro2.q110")])
names(stack2) <- c("farmerID","id.agro","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")
stack3 <- cbind(farmers[c("ID","id.agro3","hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l","hh.maize.agro3.q112")])
names(stack3) <- c("farmerID","id.agro","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")

ratings <-rbind(stack1,stack2,stack3)
ratings[c("id.agro","bought")] <- lapply(ratings[c("id.agro","bought")], function(x) as.factor(as.character(x)) )

##Subsetting 
ratings <- subset(ratings, !is.na(rating_reputation) )
ratings <- subset(ratings[!apply(ratings == "", 1, any),])

### simple average (Index for overall rating)
ratings$rating_overall <- rowSums(ratings[c("rating_location","rating_price","rating_quality","rating_stock","rating_reputation")])/5
summary(ratings$rating_overall)

#new data with farmerID, agro ID and ratings 
rat <- subset(ratings[c(1,2,9)])

#shows duplicate rows
which(duplicated(ratings))
which(duplicated(rat)) 
#viewing duplicate rows 
ratings %>% slice(which(duplicated(ratings)))
rat %>% slice(which(duplicated(rat)))

##duplicates based on farmer ID and Agro ID
ratings[c(duplicated(ratings[,1:2])),]
rat[c(duplicated(rat[,1:2])),]

rat[c(rat$farmerID=="F_527"), ]

##creating crossed designs 

library(dplyr)
library(tidyr)
#by farmerID
ratpivot <- rat %>%
  group_by(id.agro, farmerID) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
ratpivot[c(duplicated(ratpivot[,1])),] #duplicates based on id.agro
#by agro ID
ratp <- rat %>%
  group_by(farmerID, id.agro) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = id.agro, values_from = rating_overall) %>%
  select(-row)
which(duplicated(ratp)) #there are duplicates 
ratp[c(duplicated(ratp[,1])),] #duplicates based on farmerID

table(ratings$id.agro)
table(ratings$farmerID)

##RATINGS ---- PERCENTAGE OF FARMERS AND AGRO-INPUT DEALERS WHO RATE

##### OVERALL RATINGS #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_overall>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#23.81786
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_overall>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
# 15.18987
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_overall>4]))/sum(table(ratings$farmerID))*100
#21.94787
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$dealer_rating_overall>4]))/sum(table(dealers$id.agro))*100
#55.12821

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_overall<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#2.101576
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_overall<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#11.39241
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_overall<=2]))/sum(table(ratings$farmerID))*100
#4.115226
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$dealer_rating_overall<=2]))/sum(table(dealers$id.agro))*100
#0
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$dealer_rating_overall<=3]))/sum(table(dealers$id.agro))*100
#2.564103

##### RATING LOCATION #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_location>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#31.17338
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_location>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
# 34.17722
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_location>4]))/sum(table(ratings$farmerID))*100
#31.82442
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q79>4]))/sum(table(dealers$id.agro))*100
#47.4359

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_location<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#19.61471
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_location<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#18.98734
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_location<=2]))/sum(table(ratings$farmerID))*100
#19.47874
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q79<=2]))/sum(table(dealers$id.agro))*100
#3.846154
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q79<=3]))/sum(table(dealers$id.agro))*100
#21.79487

##### RATING PRICE #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_price>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#9.982487
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_price>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#5.063291
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_price>4]))/sum(table(ratings$farmerID))*100
#8.916324
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q80>4]))/sum(table(dealers$id.agro))*100
#34.61538

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_price<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#27.84588
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_price<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#36.70886
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_price<=2]))/sum(table(ratings$farmerID))*100
#29.7668
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q80<=2]))/sum(table(dealers$id.agro))*100
#1.282051
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q80<=3]))/sum(table(dealers$id.agro))*100
#28.20513

##### RATING QUALITY #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_quality>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
# 23.64273
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_quality>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
# 11.39241
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_quality>4]))/sum(table(ratings$farmerID))*100
#20.98765
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q81>4]))/sum(table(dealers$id.agro))*100
#64.10256

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_quality<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#7.880911
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_quality<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#22.1519
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_quality<=2]))/sum(table(ratings$farmerID))*100
#10.97394
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q81<=2]))/sum(table(dealers$id.agro))*100
#0
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q81<=3]))/sum(table(dealers$id.agro))*100
#6.410256

##### RATING STOCK #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_stock>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#33.62522
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_stock>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#18.98734
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_stock>4]))/sum(table(ratings$farmerID))*100
#30.45267
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q82>4]))/sum(table(dealers$id.agro))*100
#16.66667

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_stock<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#8.231173
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_stock<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#22.1519
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_stock<=2]))/sum(table(ratings$farmerID))*100
#11.24829
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q82<=2]))/sum(table(dealers$id.agro))*100
# 12.82051
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q82<=3]))/sum(table(dealers$id.agro))*100
# 57.69231

##### RATING REPUTATION #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_reputation>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#26.44483
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_reputation>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
# 17.72152
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_reputation>4]))/sum(table(ratings$farmerID))*100
#24.55418
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q83>4]))/sum(table(dealers$id.agro))*100
#55.12821

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_reputation<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#7.005254
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_reputation<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
# 14.55696
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_reputation<=2]))/sum(table(ratings$farmerID))*100
#8.641975
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q83<=2]))/sum(table(dealers$id.agro))*100
# 3.846154
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q83<=3]))/sum(table(dealers$id.agro))*100
# 8.974359

####### NUMBER OF RATINGS RECEIVED #######
##number of agro input dealers who have been rated a certain number of times 
table(table(ratings$id.agro)>10)
table(table(ratings$id.agro)>20)
table(table(ratings$id.agro)>30)
table(table(ratings$id.agro)>40)

##frequency of the ratings for each agro input dealer
subset(data.frame(table(ratings$id.agro)), Freq > 20)
subset(data.frame(table(ratings$id.agro)), Freq > 30)
subset(data.frame(table(ratings$id.agro)), Freq > 40)
##    Var1 Freq
##12 AS011   50
##14 AS013   42

##Another way
library(dplyr)
ratings %>% 
  count(id.agro)  %>%
  filter(n > 20)

##subsetting based on the number of ratings received for each agro input dealer
gt20 <- ratings[ ratings$id.agro %in%  names(table(ratings$id.agro))[table(ratings$id.agro) >20] , ] #294 obs               
gt30 <- ratings[ ratings$id.agro %in%  names(table(ratings$id.agro))[table(ratings$id.agro) >30] , ] #128 obs
gt40 <- ratings[ ratings$id.agro %in%  names(table(ratings$id.agro))[table(ratings$id.agro) >40] , ] #92 obs

tapply(ratings$rating_overall,ratings$bought, mean )
wilcox.test(ratings$rating_quality~ratings$bought)

## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))
##Index for overall rating by dealers for themselves 
dealers$dealer_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$dealer_rating_overall)
wiltest<-wilcox.test(ratings$rating_overall,dealers$dealer_rating_overall)
##Null: Distributions are same and have same median
##Looking at p-value, we may conclude that the medians of the 2 distributions differ (<0.05)
##W indicates the number of times the rating from a farmer is different to rating by the dealers


#OVERALL RATING
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_overall, na.rm=T) #3.577778
#overall mean score with interaction
mean(ratings$rating_overall[ratings$bought=="Yes"], na.rm=T) #3.663047
#overall mean score without interaction
mean(ratings$rating_overall[ratings$bought=="No"], na.rm=T) #3.26962
 
#overall rating provided by dealer to themselves 
mean(dealers$dealer_rating_overall, na.rm=T)
#4.130769 (higher than farmers)

##input dealers who provide extension/training to clients (dealers$hh.maize.q67) and seed on credit (dealers$hh.maize.q68)
#1 = No, 2 = To some, 3 = to everyone who wants 
#rating received from farmers 
mean(ratings$rating_overall[dealers$hh.maize.q67 %in% c(1) & dealers$hh.maize.q68 %in% c(1)], na.rm=T) #3.41
mean(ratings$rating_overall[dealers$hh.maize.q67 %in% c(2,3) & dealers$hh.maize.q68 %in% c(2,3)], na.rm=T) #3.618925
#ratings by input dealers 
mean(dealers$dealer_rating_overall[dealers$hh.maize.q67 %in% c(1) & dealers$hh.maize.q68 %in% c(1)], na.rm=T) #3.95
mean(dealers$dealer_rating_overall[dealers$hh.maize.q67 %in% c(2,3) & dealers$hh.maize.q68 %in% c(2,3)], na.rm=T) #4.164
##ratings by input dealers are higher

##RATING LOCATION
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_location, na.rm=T) #3.644719
#overall mean score with interaction
mean(ratings$rating_location[ratings$bought=="Yes"], na.rm=T) #3.660245
#overall mean score without interaction
mean(ratings$rating_location[ratings$bought=="No"], na.rm=T) #3.588608

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q79, na.rm=T)
#4.217949 (higher than farmers)

##RATING PRICE
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_price, na.rm=T) #2.990398
#overall mean score with interaction
mean(ratings$rating_price[ratings$bought=="Yes"], na.rm=T) #3.052539
#overall mean score without interaction
mean(ratings$rating_price[ratings$bought=="No"], na.rm=T) #2.765823

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q80, na.rm=T)
# 4.051282 (higher than farmers)

##RATING QUALITY
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_quality, na.rm=T) #3.613169
#overall mean score with interaction
mean(ratings$rating_quality[ratings$bought=="Yes"], na.rm=T) #3.756567
#overall mean score without interaction
mean(ratings$rating_quality[ratings$bought=="No"], na.rm=T) #3.094937

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q81, na.rm=T)
# 4.576923 (higher than farmers)

##RATING STOCK
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_stock, na.rm=T) #3.824417
#overall mean score with interaction
mean(ratings$rating_stock[ratings$bought=="Yes"], na.rm=T) #3.950963
#overall mean score without interaction
mean(ratings$rating_stock[ratings$bought=="No"], na.rm=T) #3.367089

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q82, na.rm=T)
#3.410256 (lower than farmers)

##RATING REPUTATION
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_reputation, na.rm=T) #3.816187
#overall mean score with interaction
mean(ratings$rating_reputation[ratings$bought=="Yes"], na.rm=T) #3.894921
#overall mean score without interaction
mean(ratings$rating_reputation[ratings$bought=="No"], na.rm=T) #3.531646

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q83, na.rm=T)
# 4.397436 (higher than farmers)

###CREATE GRAPHS
## first graph is a simple bar chart of means - OVERALL RATING of customers, non-customers and dealers

df <- data.frame(c(mean(dealers$dealer_rating_overall),tapply(ratings$rating_overall,ratings$bought, mean )[2:3]))
names(df) <- "score"
rownames(df) <- NULL
df$levels <- c("dealer","non-customer","customer")
df <- df[order(df$score,decreasing = TRUE),]
df$levels <- factor(df$levels,levels= c("dealer","customer","non-customer"))

png(paste(path, "figures/fig_dealer_baroverallrate.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("dealer", "customer")), annotations="***", y_position = 4.3, tip_length = 0.03) +
  geom_signif(comparisons = list(c("dealer", "non-customer")), annotations="***", y_position = 4.5, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.1, tip_length = 0.03)
dev.off()

###Tests in the graph
### OVERALL RATING 
wilcox.test(ratings$rating_overall[ratings$bought=="No"],dealers$dealer_rating_overall)
wilcox.test(ratings$rating_overall[ratings$bought=="Yes"],dealers$dealer_rating_overall)
wilcox.test(ratings$rating_overall[ratings$bought=="Yes"],ratings$rating_overall[ratings$bought=="No"])
#for all 3, we reject the null that the distributions are same

## second graph is a simple bar chart of means - LOCATION RATING of customers, non-customers and dealers

df_location <- data.frame(c(mean(dealers$hh.maize.q79),tapply(ratings$rating_location,ratings$bought, mean )[2:3]))
names(df_location) <- "score"
rownames(df_location) <- NULL
df_location$levels <- c("dealer","non-customer","customer")
df_location <- df_location[order(df_location$score,decreasing = TRUE),]
df_location$levels <- factor(df_location$levels,levels= c("dealer","customer","non-customer"))

png(paste(path, "figures/fig_dealer_bar_rateloc.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_location, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("dealer", "customer")), annotations="***", y_position = 4.3, tip_length = 0.03) +
  geom_signif(comparisons = list(c("dealer", "non-customer")), annotations="***", y_position = 4.5, tip_length = 0.03) 
dev.off()

#Tests in the graph
### LOCATION RATING 
wilcox.test(ratings$rating_location[ratings$bought=="No"],dealers$hh.maize.q79)
#reject null, distributions are different
wilcox.test(ratings$rating_location[ratings$bought=="Yes"],dealers$hh.maize.q79)
#reject null, distributions are different
wilcox.test(ratings$rating_location[ratings$bought=="Yes"],ratings$rating_location[ratings$bought=="No"])
#cannot reject null, distributions are same

## third graph is a simple bar chart of means - PRICE RATING of customers, non-customers and dealers

df_price <- data.frame(c(mean(dealers$hh.maize.q80),tapply(ratings$rating_price,ratings$bought, mean )[2:3]))
names(df_price) <- "score"
rownames(df_price) <- NULL
df_price$levels <- c("dealer","non-customer","customer")
df_price <- df_price[order(df_price$score,decreasing = TRUE),]
df_price$levels <- factor(df_price$levels,levels= c("dealer","customer","non-customer"))

png(paste(path, "figures/fig_dealer_bar_rateprice.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_price, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("dealer", "customer")), annotations="***", y_position = 4.3, tip_length = 0.03) +
  geom_signif(comparisons = list(c("dealer", "non-customer")), annotations="***", y_position = 4.5, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.1, tip_length = 0.03)
dev.off()

#Tests in the graph
### PRICE RATING
wilcox.test(ratings$rating_price[ratings$bought=="No"],dealers$hh.maize.q80)
#reject null, distributions are different
wilcox.test(ratings$rating_price[ratings$bought=="Yes"],dealers$hh.maize.q80)
#reject null, distributions are different
wilcox.test(ratings$rating_price[ratings$bought=="Yes"],ratings$rating_price[ratings$bought=="No"])
#reject null, distributions are different

## fourth graph is a simple bar chart of means - QUALITY RATING of customers, non-customers and dealers

df_quality <- data.frame(c(mean(dealers$hh.maize.q81),tapply(ratings$rating_quality,ratings$bought, mean )[2:3]))
names(df_quality) <- "score"
rownames(df_quality) <- NULL
df_quality$levels <- c("dealer","non-customer","customer")
df_quality <- df_quality[order(df_quality$score,decreasing = TRUE),]
df_quality$levels <- factor(df_quality$levels,levels= c("dealer","customer","non-customer"))

png(paste(path, "figures/fig_dealer_bar_ratequal.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_quality, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("dealer", "customer")), annotations="***", y_position = 4.62, tip_length = 0.03) +
  geom_signif(comparisons = list(c("dealer", "non-customer")), annotations="***", y_position = 4.8, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.3, tip_length = 0.03)
dev.off()

#Tests in the graph
### QUALITY RATING
wilcox.test(ratings$rating_quality[ratings$bought=="No"],dealers$hh.maize.q81)
#reject null, distributions are different
wilcox.test(ratings$rating_quality[ratings$bought=="Yes"],dealers$hh.maize.q81)
#reject null, distributions are different
wilcox.test(ratings$rating_quality[ratings$bought=="Yes"],ratings$rating_quality[ratings$bought=="No"])
#reject null, distributions are different

## fifth graph is a simple bar chart of means - STOCK RATING of customers, non-customers and dealers

df_stock <- data.frame(c(mean(dealers$hh.maize.q82),tapply(ratings$rating_stock,ratings$bought, mean )[2:3]))
names(df_stock) <- "score"
rownames(df_stock) <- NULL
df_stock$levels <- c("dealer","non-customer","customer")
df_stock <- df_stock[order(df_stock$score,decreasing = TRUE),]
df_stock$levels <- factor(df_stock$levels,levels= c("dealer","customer","non-customer"))

png(paste(path, "figures/fig_dealer_bar_ratestock.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_stock, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("dealer", "customer")), annotations="***", y_position = 4.3, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.1, tip_length = 0.03)
dev.off()

#Tests in the graph
### STOCK RATING
wilcox.test(ratings$rating_stock[ratings$bought=="No"],dealers$hh.maize.q82)
#cannot reject null, distributions are same
wilcox.test(ratings$rating_stock[ratings$bought=="Yes"],dealers$hh.maize.q82)
#reject null, distributions are different
wilcox.test(ratings$rating_stock[ratings$bought=="Yes"],ratings$rating_stock[ratings$bought=="No"])
#reject null, distributions are different

## sixth graph is a simple bar chart of means - REPUTATION RATING of customers, non-customers and dealers

df_repu <- data.frame(c(mean(dealers$hh.maize.q83),tapply(ratings$rating_reputation,ratings$bought, mean )[2:3]))
names(df_repu) <- "score"
rownames(df_repu) <- NULL
df_repu$levels <- c("dealer","non-customer","customer")
df_repu <- df_repu[order(df_repu$score,decreasing = TRUE),]
df_repu$levels <- factor(df_repu$levels,levels= c("dealer","customer","non-customer"))

png(paste(path, "figures/fig_dealer_bar_raterepu.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_repu, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("dealer", "customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("dealer", "non-customer")), annotations="***", y_position = 4.85, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.3, tip_length = 0.03)
dev.off()

#Tests in the graph
### REPUTATION RATING
wilcox.test(ratings$rating_reputation[ratings$bought=="No"],dealers$hh.maize.q83)
#reject null, distributions are different
wilcox.test(ratings$rating_reputation[ratings$bought=="Yes"],dealers$hh.maize.q83)
#reject null, distributions are different
wilcox.test(ratings$rating_reputation[ratings$bought=="Yes"],ratings$rating_reputation[ratings$bought=="No"])
#reject null, distributions are different

##Likert scales bar charts for the different components of the scores, again for the three categories

##DEALERS
plot_non_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$bought=="No"),2)[,1],
prop.table(table(ratings$rating_price, ratings$bought=="No"),2)[,1],
prop.table(table(ratings$rating_quality, ratings$bought=="No"),2)[,1],
prop.table(table(ratings$rating_stock, ratings$bought=="No"),2)[,1],
prop.table(table(ratings$rating_reputation, ratings$bought=="No"),2)[,1]))
names(plot_non_customer) <- c("location","price","quality","stock","reputation")

plot_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_price, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_quality, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_stock, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_reputation, ratings$bought=="Yes"),2)[,2]))
names(plot_customer) <- c("location","price","quality","stock","reputation")

plot_dealer <- data.frame(cbind(c(0,prop.table(table(dealers$hh.maize.q79))),
c(0,prop.table(table(dealers$hh.maize.q80))),
c(0,0,prop.table(table(dealers$hh.maize.q81))),
c(prop.table(table(dealers$hh.maize.q82))),
c(prop.table(table(dealers$hh.maize.q83)))))
names(plot_dealer) <- c("location","price","quality","stock","reputation")
png(paste(path, "figures/fig_dealer_likert.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
par(mfrow=c(1,3), xpd=NA, mar = c(10, 5,5, 1)) 
colfunc<-colorRampPalette(c("red", "green"))
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="non-customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_dealer), col=colfunc(5), main="dealer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()

###########################################
##SUBSETTING AND PIVOTING TRIAL 
subset(data.frame(table(ratings$farmerID)), Freq > 1)
subset(data.frame(table(ratings$farmerID)), Freq > 2)
#dataset where farmers have rated more than twice 
farmer_gt2 <- ratings[ ratings$farmerID %in%  names(table(ratings$farmerID))[table(ratings$farmerID) >2] , ]
subset(data.frame(table(farmer_gt2$id.agro)), Freq > 2)
subset(data.frame(table(farmer_gt2$id.agro)), Freq > 1)
#dataset where agro input dealers have been rated more than once 
agro_gt1 <- farmer_gt2[ farmer_gt2$id.agro %in%  names(table(farmer_gt2$id.agro))[table(farmer_gt2$id.agro) >1] , ]
#dataset with farmer ID, agro ID and ratings overall
agro <- subset(agro_gt1[c(1,2,9)])

#creating crossed design with small group of dealers and farmers 
library(tidyr)
library(dplyr)
agropivot <- agro %>%
  group_by(id.agro, farmerID) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
agropivot[c(duplicated(agropivot[,1])),] #duplicates exist

agrop <- agro %>%
   group_by(farmerID, id.agro) %>%
   mutate(row = row_number()) %>%
   tidyr::pivot_wider(names_from = id.agro, values_from = rating_overall) %>%
   select(-row)
agrop[c(duplicated(agrop[,1])),] #duplicates exist

agropivot <- agropivot[ -c(3:5), ]
agropivot <- agropivot[ ,-c(1,4) ]

kappam.fleiss(agropivot) 
#kappa = -0.333 (poor agreement)
agree(agropivot, tolerance=0.5)
icc(agropivot, model = c("twoway"),
     type = c( "agreement"),
     unit = c("average"), conf.level = 0.95)
#ICC = 0.381
#an ICC of .381 means that approximately 38% of the variance is attributed to the "true score," while approximately rest% is attributed to error.
#does not meet the minimum reliability criteria 


#### TRY RATER BIAS
agrop <- agrop[ -c(3:6), ]
agrop <- agrop[ ,-c(1,4) ]
rater.bias(agrop)
rater.bias(agropivot)
###DOES NOT WORK, subjects should not be 16.2

############################################





################### TRADERS ########################

### Prepping data for ratings 
trans <- c("hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j","hh.maize.trader1.q102k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j","hh.maize.trader2.q103k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j","hh.maize.trader3.q104k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.trader1","hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j","hh.maize.trader1.q102k")],"Yes")
names(stack1) <- c("farmerID","id.trader","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")
stack2 <- cbind(farmers[c("ID","id.trader2","hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j","hh.maize.trader2.q103k","hh.maize.trader2.q103l")])
names(stack2) <- c("farmerID","id.trader","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")
stack3 <- cbind(farmers[c("ID","id.trader3","hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j","hh.maize.trader3.q104k","hh.maize.trader3.q104l")])
names(stack3) <- c("farmerID","id.trader","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")

ratings_trader <-rbind(stack1,stack2,stack3)
ratings_trader[c("id.trader","sold")] <- lapply(ratings_trader[c("id.trader","sold")], function(x) as.factor(as.character(x)) )

##subsetting and cleaning missing values 
ratings_trader <- subset(ratings_trader,!is.na(rating_reputation))
ratings_trader <- subset(ratings_trader[!apply(ratings_trader == "", 1, any),])
ratings_trader <- subset(ratings_trader[!apply(ratings_trader == ".", 1, any),])
table(ratings_trader=="")

### simple average of the ratings (index) --- from farmers
ratings_trader$rating_overall <- rowSums(ratings_trader[c("rating_location","rating_price","rating_quality","rating_honesty","rating_reputation")])/5
summary(ratings_trader$rating_overall)

###Subsetting to only include farmerID, trader ID and overall ratings 
rat_trader <- subset(ratings_trader[c(1,2,9)])

#shows duplicate rows
which(duplicated(ratings_trader))
which(duplicated(rat_trader)) 
#viewing duplicate rows 
ratings_trader %>% slice(which(duplicated(ratings_trader)))
rat_trader %>% slice(which(duplicated(rat_trader)))

##duplicates based on farmer ID and trader ID
ratings_trader[c(duplicated(ratings_trader[,1:2])),]
rat_trader[c(duplicated(rat_trader[,1:2])),]

ratings_trader[c(ratings_trader$farmerID=="F_330"), ]

##creating crossed designs 

library(dplyr)
library(tidyr)
#by farmerID
ratpivot_trader <- rat_trader %>%
  #could also group_by(farmerID)
  group_by(id.trader, farmerID) %>% 
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
ratpivot_trader[c(duplicated(ratpivot_trader[,1])),] #duplicates based on id.trader
#by trader ID
ratp_trader <- rat_trader %>%
  #could also group_by(id.trader)
  group_by(farmerID, id.trader) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = id.trader, values_from = rating_overall) %>%
  select(-row)
ratp_trader[c(duplicated(ratp_trader[,1])),] #duplicates based on farmerID

table(ratings_trader$id.trader)
table(ratings_trader$farmerID)

###Getting traders' data 
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

###Index for overall rating from traders
traders$trader_rating_overall <- rowSums(traders[c("hh.maize.q40a","hh.maize.q40b","hh.maize.q40c","hh.maize.q40d","hh.maize.q40e")])/5
summary(traders$trader_rating_overall)


##RATINGS ---- PERCENTAGE OF FARMERS AND TRADERS WHO RATE

##### OVERALL RATINGS #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#32.47059
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#19.92188
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall>4]))/sum(table(ratings_trader$farmerID))*100
# 30.37231
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$trader_rating_overall>4]))/sum(table(traders$trader_rating_overall))*100
#65.9824

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#3.294118
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#10.15625
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall<=2]))/sum(table(ratings_trader$farmerID))*100
#4.441541
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$trader_rating_overall<=2]))/sum(table(traders$id.trader))*100
#0
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$trader_rating_overall<=3]))/sum(table(traders$id.trader))*100
#2.346041

##### RATING LOCATION #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_location>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#44.47059
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_location>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#39.0625
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_location>4]))/sum(table(ratings_trader$farmerID))*100
# 43.5663
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$hh.maize.q40a>4]))/sum(table(traders$trader_rating_overall))*100
# 37.82991

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_location<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#7.137255
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_location<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
# 13.28125
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_location<=2]))/sum(table(ratings_trader$farmerID))*100
#8.164598
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$hh.maize.q40a<=2]))/sum(table(traders$id.trader))*100
#3.225806
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40a<=3]))/sum(table(traders$id.trader))*100
#23.46041

##### RATING PRICE #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_price>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#9.333333
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_price>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#10.54688
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_price>4]))/sum(table(ratings_trader$farmerID))*100
# 9.536251
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$hh.maize.q40b>4]))/sum(table(traders$trader_rating_overall))*100
# 25.5132

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_price<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 25.41176
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_price<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
# 33.20312
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_price<=2]))/sum(table(ratings_trader$farmerID))*100
#26.71457
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$hh.maize.q40b<=2]))/sum(table(traders$id.trader))*100
#3.225806
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40b<=3]))/sum(table(traders$id.trader))*100
#31.08504

##### RATING QUALITY #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_quality>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#18.19608
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_quality>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#11.32812
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_quality>4]))/sum(table(ratings_trader$farmerID))*100
#  17.04768
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$hh.maize.q40c>4]))/sum(table(traders$trader_rating_overall))*100
# 48.68035

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_quality<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 11.76471
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_quality<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#19.53125
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_quality<=2]))/sum(table(ratings_trader$farmerID))*100
#13.06336
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$hh.maize.q40c<=2]))/sum(table(traders$id.trader))*100
#2.639296
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40c<=3]))/sum(table(traders$id.trader))*100
#12.31672

##### RATING HONESTY #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#32.39216
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
# 18.75
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty>4]))/sum(table(ratings_trader$farmerID))*100
#30.11104
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$hh.maize.q40d>4]))/sum(table(traders$trader_rating_overall))*100
# 73.02053

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 12.94118
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#21.875
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty<=2]))/sum(table(ratings_trader$farmerID))*100
# 14.43501
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$hh.maize.q40d<=2]))/sum(table(traders$id.trader))*100
#0.8797654
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40d<=3]))/sum(table(traders$id.trader))*100
# 5.571848

##### RATING REPUTATION #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 30.27451
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
# 19.92188
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation>4]))/sum(table(ratings_trader$farmerID))*100
#28.54344
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$hh.maize.q40e>4]))/sum(table(traders$trader_rating_overall))*100
# 59.23754

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 7.921569
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#  17.1875
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation<=2]))/sum(table(ratings_trader$farmerID))*100
#  9.470934
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$hh.maize.q40e<=2]))/sum(table(traders$id.trader))*100
#2.346041
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40e<=3]))/sum(table(traders$id.trader))*100
# 12.31672

##NUMBER OF RATINGS RECEIVED 
##number of traders who have these ratings 
table(table(ratings_trader$id.trader)>10)
table(table(ratings_trader$id.trader)>15)
table(table(ratings_trader$id.trader)>20)

##frequency of the ratings for each trader
subset(data.frame(table(ratings_trader$id.trader)), Freq > 10)
subset(data.frame(table(ratings_trader$id.trader)), Freq > 15)
subset(data.frame(table(ratings_trader$id.trader)), Freq > 20)
##      Var1 Freq
##85  T0098   23
##155 T0176   21
##245 T0276   29

##Another way
library(dplyr)
ratings_trader %>% 
  count(id.trader)  %>%
  filter(n > 20)

##subsetting based on the number of ratings for each trader
gt10_trad <- ratings_trader[ ratings_trader$id.trader %in%  names(table(ratings_trader$id.trader))[table(ratings_trader$id.trader) >10] , ]  #663 obs              
gt15_trad <- ratings_trader[ ratings_trader$id.trader %in%  names(table(ratings_trader$id.trader))[table(ratings_trader$id.trader) >15] , ]  #317 obs             
gt20_trad <- ratings_trader[ ratings_trader$id.trader %in%  names(table(ratings_trader$id.trader))[table(ratings_trader$id.trader) >20] , ]   #73 obs             

gt20_tradsub <- subset(gt20_trad[c(1,2,9)]) #KEEPING farmerID, trader ID and overall ratings 

#shows duplicate rows
which(duplicated(gt20_tradsub)) #0
which(duplicated(gt20_trad)) #0

##duplicates based on farmer ID and trader ID
gt20_tradsub[c(duplicated(gt20_tradsub[,1:2])),] #0

##creating crossed designs 
library(dplyr)
library(tidyr)
#by farmerID
gt20_tradsub_f <- gt20_tradsub %>%
  #could also group_by(farmerID)
  group_by(id.trader, farmerID) %>% 
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
gt20_tradsub_f[c(duplicated(gt20_tradsub_f[,1])),] #no duplicates
#by trader ID
gt20_tradsub_t <- gt20_tradsub %>%
  #could also group_by(id.trader)
  group_by(farmerID, id.trader) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = id.trader, values_from = rating_overall) %>%
  select(-row)
gt20_tradsub_t[c(duplicated(gt20_tradsub_t[,1])),] #no duplicates

####CHANGE COLUMN NAMES 
##colnames(DATASET) <- paste("Rater",0:74, sep="")
##names(DATASET)[1]<-paste("Subjects")

### Wilcox test
tapply(ratings_trader$rating_overall,ratings_trader$sold, mean )
wilcox.test(ratings_trader$rating_quality~ratings_trader$sold)
##Null: Distributions are same and have same median
##Looking at p-value, we may conclude that the medians of the 2 distributions differ
##W indicates the number of times the rating from a farmer is different to rating by the traders

wilcox.test(ratings_trader$rating_overall,traders$trader_rating_overall)
#reject null, distributions are different 

#OVERALL RATING
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_overall, na.rm=T) #3.644024
#overall mean score with interaction
mean(ratings_trader$rating_overall[ratings_trader$sold=="Yes"], na.rm=T) #3.700078
#overall mean score without interaction
mean(ratings_trader$rating_overall[ratings_trader$sold=="No"], na.rm=T) #3.364844

#overall rating provided by traders to themselves 
mean(traders$trader_rating_overall, na.rm=T)
#4.291496 (higher than farmers)

##RATING LOCATION
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_location, na.rm=T) #4.081646
#overall mean score with interaction
mean(ratings_trader$rating_location[ratings_trader$sold=="Yes"], na.rm=T) #4.119216
#overall mean score without interaction
mean(ratings_trader$rating_location[ratings_trader$sold=="No"], na.rm=T) #3.894531

#overall rating provided by dealer to themselves 
mean(traders$hh.maize.q40a, na.rm=T)
#4.108504 (higher than farmers)

##RATING PRICE
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_price, na.rm=T) #3.058132
#overall mean score with interaction
mean(ratings_trader$rating_price[ratings_trader$sold=="Yes"], na.rm=T) #3.095686
#overall mean score without interaction
mean(ratings_trader$rating_price[ratings_trader$sold=="No"], na.rm=T) #2.871094

#overall rating provided by traders to themselves 
mean(traders$hh.maize.q40b, na.rm=T)
# 3.906158

##RATING QUALITY
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_quality, na.rm=T) #3.516003
#overall mean score with interaction
mean(ratings_trader$rating_quality[ratings_trader$sold=="Yes"], na.rm=T) #3.574118
#overall mean score without interaction
mean(ratings_trader$rating_quality[ratings_trader$sold=="No"], na.rm=T) #3.226562

#overall rating provided by traders to themselves 
mean(traders$hh.maize.q40c, na.rm=T)
# 4.334311 (higher than farmers)

##RATING HONESTY
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_honesty, na.rm=T) #3.744611
#overall mean score with interaction
mean(ratings_trader$rating_honesty[ratings_trader$sold=="Yes"], na.rm=T) #3.824314
#overall mean score without interaction
mean(ratings_trader$rating_honesty[ratings_trader$sold=="No"], na.rm=T) #3.347656

#overall rating provided by traders to themselves 
mean(traders$hh.maize.q40d, na.rm=T)
#4.662757 (higher than farmers)

##RATING REPUTATION
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_reputation, na.rm=T) #3.819726
#overall mean score with interaction
mean(ratings_trader$rating_reputation[ratings_trader$sold=="Yes"], na.rm=T) #3.887059
#overall mean score without interaction
mean(ratings_trader$rating_reputation[ratings_trader$sold=="No"], na.rm=T) #3.484375

#overall rating provided by traders to themselves 
mean(traders$hh.maize.q40e, na.rm=T)
# 4.445748 (higher than farmers)

###  GRAPHS  ####
## First graph is a simple bar chart of means - OVERALL RATING of customers, non-customers and dealers

df_trad <- data.frame(c(mean(traders$trader_rating_overall),tapply(ratings_trader$rating_overall,ratings_trader$sold, mean )[2:3]))
names(df_trad) <- "score"
rownames(df_trad) <- NULL
df_trad$levels <- c("trader","non-customer","customer")
df_trad <- df_trad[order(df_trad$score,decreasing = TRUE),]
df_trad$levels <- factor(df_trad$levels,levels= c("trader","customer","non-customer"))

png(paste(path, "figures/fig_trader_rateoverall.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_trad, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("trader", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("trader", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#OVERALL RATING 
wilcox.test(ratings_trader$rating_overall[ratings_trader$sold=="No"],traders$traders_rating_overall)
wilcox.test(ratings_trader$rating_overall[ratings_trader$sold=="Yes"],traders$trader_rating_overall)
wilcox.test(ratings_trader$rating_overall[ratings_trader$sold=="Yes"],ratings_trader$rating_overall[ratings_trader$sold=="No"])
#for all 3, reject null, distributions are different 


## Second graph is a simple bar chart of means - LOCATION RATING of customers, non-customers and dealers

df_trad_loc <- data.frame(c(mean(traders$hh.maize.q40a),tapply(ratings_trader$rating_location,ratings_trader$sold, mean )[2:3]))
names(df_trad_loc) <- "score"
rownames(df_trad_loc) <- NULL
df_trad_loc$levels <- c("trader","non-customer","customer")
df_trad_loc <- df_trad_loc[order(df_trad_loc$score,decreasing = TRUE),]
df_trad_loc$levels <- factor(df_trad_loc$levels,levels= c("trader","customer","non-customer"))

png(paste(path, "figures/fig_trader_ratelocation.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_trad_loc, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="**", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#LOCATION RATING 
wilcox.test(ratings_trader$rating_location[ratings_trader$sold=="No"],traders$hh.maize.q40a)
#cannot reject the null, distributions are the same 
wilcox.test(ratings_trader$rating_location[ratings_trader$sold=="Yes"],traders$hh.maize.q40a)
#cannot reject the null, distributions are the same 
wilcox.test(ratings_trader$rating_location[ratings_trader$sold=="Yes"],ratings_trader$rating_location[ratings_trader$sold=="No"])
#reject the null at 5% level

## Third graph is a simple bar chart of means - PRICE RATING of customers, non-customers and dealers

df_trad_price <- data.frame(c(mean(traders$hh.maize.q40b),tapply(ratings_trader$rating_price,ratings_trader$sold, mean )[2:3]))
names(df_trad_price) <- "score"
rownames(df_trad_price) <- NULL
df_trad_price$levels <- c("trader","non-customer","customer")
df_trad_price <- df_trad_price[order(df_trad_price$score,decreasing = TRUE),]
df_trad_price$levels <- factor(df_trad_price$levels,levels= c("trader","customer","non-customer"))

png(paste(path, "figures/fig_trader_rateprice.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_trad_price, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("trader", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("trader", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#PRICE RATING 
wilcox.test(ratings_trader$rating_price[ratings_trader$sold=="No"],traders$hh.maize.q40b)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_price[ratings_trader$sold=="Yes"],traders$hh.maize.q40b)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_price[ratings_trader$sold=="Yes"],ratings_trader$rating_price[ratings_trader$sold=="No"])
#reject the null, distributions are different 

## Fourth graph is a simple bar chart of means - QUALITY RATING of customers, non-customers and dealers

df_trad_qual <- data.frame(c(mean(traders$hh.maize.q40c),tapply(ratings_trader$rating_quality,ratings_trader$sold, mean )[2:3]))
names(df_trad_qual) <- "score"
rownames(df_trad_qual) <- NULL
df_trad_qual$levels <- c("trader","non-customer","customer")
df_trad_qual <- df_trad_qual[order(df_trad_qual$score,decreasing = TRUE),]
df_trad_qual$levels <- factor(df_trad_qual$levels,levels= c("trader","customer","non-customer"))

png(paste(path, "figures/fig_trader_ratequal.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_trad_qual, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("trader", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("trader", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#QUALITY RATING 
wilcox.test(ratings_trader$rating_quality[ratings_trader$sold=="No"],traders$hh.maize.q40c)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_quality[ratings_trader$sold=="Yes"],traders$hh.maize.q40c)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_quality[ratings_trader$sold=="Yes"],ratings_trader$rating_quality[ratings_trader$sold=="No"])
#reject the null, distributions are different 

## Fifth graph is a simple bar chart of means - HONESTY RATING of customers, non-customers and dealers

df_trad_hon <- data.frame(c(mean(traders$hh.maize.q40d),tapply(ratings_trader$rating_honesty,ratings_trader$sold, mean )[2:3]))
names(df_trad_hon) <- "score"
rownames(df_trad_hon) <- NULL
df_trad_hon$levels <- c("trader","non-customer","customer")
df_trad_hon <- df_trad_hon[order(df_trad_hon$score,decreasing = TRUE),]
df_trad_hon$levels <- factor(df_trad_hon$levels,levels= c("trader","customer","non-customer"))

png(paste(path, "figures/fig_trader_ratehonesty.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_trad_hon, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("trader", "customer")), annotations="***", y_position = 4.75, tip_length = 0.03) +
  geom_signif(comparisons = list(c("trader", "non-customer")), annotations="***", y_position = 4.95, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.4, tip_length = 0.03)
dev.off()

### Tests in the graph
#HONESTY RATING 
wilcox.test(ratings_trader$rating_honesty[ratings_trader$sold=="No"],traders$hh.maize.q40d)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_honesty[ratings_trader$sold=="Yes"],traders$hh.maize.q40d)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_honesty[ratings_trader$sold=="Yes"],ratings_trader$rating_honesty[ratings_trader$sold=="No"])
#reject the null, distributions are different 

## Sixth graph is a simple bar chart of means - REPUTATION RATING of customers, non-customers and dealers

df_trad_repu <- data.frame(c(mean(traders$hh.maize.q40e),tapply(ratings_trader$rating_reputation,ratings_trader$sold, mean )[2:3]))
names(df_trad_repu) <- "score"
rownames(df_trad_repu) <- NULL
df_trad_repu$levels <- c("trader","non-customer","customer")
df_trad_repu <- df_trad_repu[order(df_trad_repu$score,decreasing = TRUE),]
df_trad_repu$levels <- factor(df_trad_repu$levels,levels= c("trader","customer","non-customer"))

png(paste(path, "figures/fig_trader_raterepu.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_trad_repu, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("trader", "customer")), annotations="***", y_position = 4.5, tip_length = 0.03) +
  geom_signif(comparisons = list(c("trader", "non-customer")), annotations="***", y_position = 4.8, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#REPUTATION RATING 
wilcox.test(ratings_trader$rating_reputation[ratings_trader$sold=="No"],traders$hh.maize.q40e)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_reputation[ratings_trader$sold=="Yes"],traders$hh.maize.q40e)
#reject the null, distributions are different 
wilcox.test(ratings_trader$rating_reputation[ratings_trader$sold=="Yes"],ratings_trader$rating_reputation[ratings_trader$sold=="No"])
#reject the null, distributions are different 

#Likert scales bar charts for the different components of the scores, again for the three categories ----- TRADERS

plot_non_customer <- data.frame(cbind(prop.table(table(ratings_trader$rating_location, ratings_trader$sold=="No"),2)[,1],
prop.table(table(ratings_trader$rating_price, ratings_trader$sold=="No"),2)[,1],
prop.table(table(ratings_trader$rating_quality, ratings_trader$sold=="No"),2)[,1],
prop.table(table(ratings_trader$rating_honesty, ratings_trader$sold=="No"),2)[,1],
prop.table(table(ratings_trader$rating_reputation, ratings_trader$sold=="No"),2)[,1]))
names(plot_non_customer) <- c("location","price","quality","honesty","reputation")

plot_customer <- data.frame(cbind(prop.table(table(ratings_trader$rating_location, ratings_trader$sold=="Yes"),2)[,2],
prop.table(table(ratings_trader$rating_price, ratings_trader$sold=="Yes"),2)[,2],
prop.table(table(ratings_trader$rating_quality, ratings_trader$sold=="Yes"),2)[,2],
prop.table(table(ratings_trader$rating_honesty, ratings_trader$sold=="Yes"),2)[,2],
prop.table(table(ratings_trader$rating_reputation, ratings_trader$sold=="Yes"),2)[,2]))
names(plot_customer) <- c("location","price","quality","honesty","reputation")

plot_trader <- data.frame(cbind(c(prop.table(table(traders$hh.maize.q40a))),
c(prop.table(table(traders$hh.maize.q40b))),
c(prop.table(table(traders$hh.maize.q40c))),
c(prop.table(table(traders$hh.maize.q40d))),
c(0,prop.table(table(traders$hh.maize.q40e)))))
names(plot_trader) <- c("location","price","quality","honesty","reputation")
png(paste(path, "figures/fig_trader_likert.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
par(mfrow=c(1,3), xpd=NA, mar = c(10, 5,5, 1)) 
colfunc<-colorRampPalette(c("red", "green"))
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="non-customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_trader), col=colfunc(5), main="trader", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()

###########################################
##SUBSETTING AND PIVOTING TRIAL 
subset(data.frame(table(ratings_trader$farmerID)), Freq > 1)
subset(data.frame(table(ratings_trader$farmerID)), Freq > 2)
#dataset where farmers have rated more than twice 
farmer_gt2_trad <- ratings_trader[ ratings_trader$farmerID %in%  names(table(ratings_trader$farmerID))[table(ratings_trader$farmerID) >2] , ]
subset(data.frame(table(farmer_gt2_trad$id.trader)), Freq > 2)
subset(data.frame(table(farmer_gt2_trad$id.trader)), Freq > 3)
#dataset where traders have been rated more than thrice
trad_gt3 <- farmer_gt2_trad[ farmer_gt2_trad$id.trader %in%  names(table(farmer_gt2_trad$id.trader))[table(farmer_gt2_trad$id.trader) >3] , ]
#dataset with farmer ID, trader ID and ratings overall
trad <- subset(trad_gt3[c(1,2,9)])

#creating crossed design with small group of traders and farmers 
library(tidyr)
library(dplyr)
tradpivot <- trad %>%
  group_by(id.trader, farmerID) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
tradpivot[c(duplicated(tradpivot[,1])),] #duplicate exists

tradp <- trad %>%
  group_by(farmerID, id.trader) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = id.trader, values_from = rating_overall) %>%
  select(-row)
tradp[c(duplicated(tradp[,1])),] #duplicate exists

tradpivot_s <- tradpivot[-c(9),] #removing duplicate row 
tradpivot_s <- tradpivot[,-c(1)] #removing first row with trader IDs

##INCREASING THE FREQUENCY
subset(data.frame(table(farmer_gt2_trad$id.trader)), Freq > 5)
#dataset where traders have been rated more than FIVE TIMES
trad_gt5 <- farmer_gt2_trad[ farmer_gt2_trad$id.trader %in%  names(table(farmer_gt2_trad$id.trader))[table(farmer_gt2_trad$id.trader) >5] , ]
#dataset with farmer ID, trader ID and ratings overall
trad5 <- subset(trad_gt5[c(1,2,9)])

#creating crossed design with small group of traders and farmers 
library(tidyr)
library(dplyr)
trad55 <- trad5 %>%
  group_by(id.trader, farmerID) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
trad55[c(duplicated(trad55[,1])),] #duplicate exists

trad55 <- trad55[-c(2),] #removing duplicate row 
trad55 <- trad55[,-c(1)] #removing first row with trader IDs
library(irr)
kappam.fleiss(trad55)
#Kappa = -0.25 (poor agreement)

############################################


################### MILLERS ########################

###Prepping data for ratings 
trans <- c("hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.miller1","hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k")],"Yes")
names(stack1) <- c("farmerID","id.miller","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")
stack2 <- cbind(farmers[c("ID", "id.miller2","hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k","hh.maize.miller2.q99l")])
names(stack2) <- c("farmerID","id.miller","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")
stack3 <- cbind(farmers[c("ID","id.miller3","hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k","hh.maize.miller3.q100l")])
names(stack3) <- c("farmerID","id.miller","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")

ratings_mill <-rbind(stack1,stack2,stack3)
ratings_mill[c("id.miller","used")] <- lapply(ratings_mill[c("id.miller","used")], function(x) as.factor(as.character(x)) )

##subsetting and cleaning missing values 
ratings_mill <- subset(ratings_mill, !is.na(rating_reputation) )
ratings_mill <- subset(ratings_mill[!apply(ratings_mill == "", 1, any),])
ratings_mill <- subset(ratings_mill[!apply(ratings_mill == ".", 1, any),])
table(ratings_mill=="")

### simple average of the ratings (index) --- from farmers
ratings_mill$rating_overall <- rowSums(ratings_mill[c("rating_location","rating_price","rating_quality","rating_service","rating_reputation")])/5
summary(ratings_mill$rating_overall)
###Subsetting to only include farmerID, miller ID and overall ratings 
rat_mill <- subset(ratings_mill[c(1,2,9)])

#shows duplicate rows
which(duplicated(ratings_mill))
which(duplicated(rat_mill)) 

##duplicates based on farmer ID and miller ID
ratings_mill[c(duplicated(ratings_mill[,1:2])),]
rat_mill[c(duplicated(rat_mill[,1:2])),]

##creating crossed designs 
library(dplyr)
library(tidyr)
#by farmerID
ratpivot_mill <- rat_mill %>%
  #could also group_by(farmerID)
  group_by(id.miller, farmerID) %>% 
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
ratpivot_mill[c(duplicated(ratpivot_mill[,1])),] #duplicates based on id.miller
#by miller ID
ratp_mill <- rat_mill %>%
  #could also group_by(id.miller)
  group_by(farmerID, id.miller) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = id.miller, values_from = rating_overall) %>%
  select(-row)
ratp_mill[c(duplicated(ratp_mill[,1])),] #duplicates based on farmerID

table(ratings_mill$id.miller)
table(ratings_mill$farmerID)

###Getting millers' data 
millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"))

###Index for overall rating from millers
millers$miller_rating_overall <- rowSums(millers[c("hh.maize.q36","hh.maize.q37","hh.maize.q38","hh.maize.q39","hh.maize.q40")])/5
summary(millers$miller_rating_overall)


##RATINGS ---- PERCENTAGE OF FARMERS AND MILLERS WHO RATE

##### OVERALL RATINGS #####
#Customers: rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_overall>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#23.29099
#NON-Customers : rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_overall>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
#10.52632
#All: rating greater than 4
sum(table(ratings_mill$farmerID[ratings_mill$rating_overall>4]))/sum(table(ratings_mill$farmerID))*100
# 22.58433
#rating greater than 4 by millers
sum(table(millers$id.miller[millers$miller_rating_overall>4]))/sum(table(millers$miller_rating_overall))*100
#57.47126

#Customers: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_overall<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#3.38778
#NON-Customers : rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_overall<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 9.473684
#All: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_overall<=2]))/sum(table(ratings_mill$farmerID))*100
#3.716409
#rating less than equal to 2 by millers
sum(table(millers$id.miller[millers$miller_rating_overall<=2]))/sum(table(millers$miller_rating_overall))*100
#0
#rating less than equal to 3 by millers
sum(table(millers$id.miller[millers$miller_rating_overall<=3]))/sum(table(millers$miller_rating_overall))*100
# 2.873563

##### RATING LOCATION #####
#Customers: rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_location>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#37.50756
#NON-Customers : rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_location>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 22.10526
#All: rating greater than 4
sum(table(ratings_mill$farmerID[ratings_mill$rating_location>4]))/sum(table(ratings_mill$farmerID))*100
# 36.64951
#rating greater than 4 by millers
sum(table(millers$id.miller[millers$hh.maize.q36>4]))/sum(table(millers$miller_rating_overall))*100
#36.2069

#Customers: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_location<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#15.12402
#NON-Customers : rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_location<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 18.94737
#All: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_location<=2]))/sum(table(ratings_mill$farmerID))*100
#15.32304
#rating less than equal to 2 by millers
sum(table(millers$id.miller[millers$hh.maize.q36<=2]))/sum(table(millers$miller_rating_overall))*100
#7.471264
#rating less than equal to 3 by millers
sum(table(millers$id.miller[millers$hh.maize.q36<=3]))/sum(table(millers$miller_rating_overall))*100
# 28.73563

##### RATING PRICE #####
#Customers: rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_price>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#10.8288
#NON-Customers : rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_price>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 6.315789
#All: rating greater than 4
sum(table(ratings_mill$farmerID[ratings_mill$rating_price>4]))/sum(table(ratings_mill$farmerID))*100
#10.57747
#rating greater than 4 by millers
sum(table(millers$id.miller[millers$hh.maize.q37>4]))/sum(table(millers$miller_rating_overall))*100
#29.31034

#Customers: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_price<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#32.48639
#NON-Customers : rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_price<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 25.26316
#All: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_price<=2]))/sum(table(ratings_mill$farmerID))*100
# 32.07547
#rating less than equal to 2 by millers
sum(table(millers$id.miller[millers$hh.maize.q37<=2]))/sum(table(millers$miller_rating_overall))*100
# 6.321839
#rating less than equal to 3 by millers
sum(table(millers$id.miller[millers$hh.maize.q37<=3]))/sum(table(millers$miller_rating_overall))*100
#  37.35632

##### RATING QUALITY #####
#Customers: rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_quality>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#20.75015
#NON-Customers : rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_quality>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 11.57895
#All: rating greater than 4
sum(table(ratings_mill$farmerID[ratings_mill$rating_quality>4]))/sum(table(ratings_mill$farmerID))*100
# 20.24014
#rating greater than 4 by millers
sum(table(millers$id.miller[millers$hh.maize.q38>4]))/sum(table(millers$miller_rating_overall))*100
# 40.8046

#Customers: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_quality<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#22.62553
#NON-Customers : rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_quality<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 34.73684
#All: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_quality<=2]))/sum(table(ratings_mill$farmerID))*100
# 23.32762
#rating less than equal to 2 by millers
sum(table(millers$id.miller[millers$hh.maize.q38<=2]))/sum(table(millers$miller_rating_overall))*100
#  4.022989
#rating less than equal to 3 by millers
sum(table(millers$id.miller[millers$hh.maize.q38<=3]))/sum(table(millers$miller_rating_overall))*100
#20.68966

##### RATING SERVICE #####
#Customers: rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_service>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#21.5971
#NON-Customers : rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_service>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
#  10.52632
#All: rating greater than 4
sum(table(ratings_mill$farmerID[ratings_mill$rating_service>4]))/sum(table(ratings_mill$farmerID))*100
# 21.04059
#rating greater than 4 by millers
sum(table(millers$id.miller[millers$hh.maize.q39>4]))/sum(table(millers$miller_rating_overall))*100
#55.17241

#Customers: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_service<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
# 10.64731
#NON-Customers : rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_service<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 27.36842
#All: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_service<=2]))/sum(table(ratings_mill$farmerID))*100
# 11.54946
#rating less than equal to 2 by millers
sum(table(millers$id.miller[millers$hh.maize.q39<=2]))/sum(table(millers$miller_rating_overall))*100
#  2.873563
#rating less than equal to 3 by millers
sum(table(millers$id.miller[millers$hh.maize.q39<=3]))/sum(table(millers$miller_rating_overall))*100
# 12.64368

##### RATING REPUTATION #####
#Customers: rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#29.2196
#NON-Customers : rating greater than 4 
sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
# 15.78947
#All: rating greater than 4
sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation>4]))/sum(table(ratings_mill$farmerID))*100
# 28.47341
#rating greater than 4 by millers
sum(table(millers$id.miller[millers$hh.maize.q40>4]))/sum(table(millers$miller_rating_overall))*100
#59.77011

#Customers: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100
#  9.497883
#NON-Customers : rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100
#16.84211
#All: rating less than equal to 2 
sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation<=2]))/sum(table(ratings_mill$farmerID))*100
# 9.891366
#rating less than equal to 2 by millers
sum(table(millers$id.miller[millers$hh.maize.q40<=2]))/sum(table(millers$miller_rating_overall))*100
#1.149425
#rating less than equal to 3 by millers
sum(table(millers$id.miller[millers$hh.maize.q40<=3]))/sum(table(millers$miller_rating_overall))*100
# 8.62069

##NUMBER OF RATINGS RECEIVED 
##number of millers who have these ratings 
table(table(ratings_mill$id.miller)>10)
table(table(ratings_mill$id.miller)>15)
table(table(ratings_mill$id.miller)>20)

##frequency of the ratings for each miller
subset(data.frame(table(ratings_mill$id.miller)), Freq > 10)
subset(data.frame(table(ratings_mill$id.miller)), Freq > 15)
subset(data.frame(table(ratings_mill$id.miller)), Freq > 20)

##subsetting based on the number of ratings for each miller
gt10_mill <- ratings_mill[ ratings_mill$id.miller %in%  names(table(ratings_mill$id.miller))[table(ratings_mill$id.miller) >10] , ]   #1391 obs
gt15_mill <- ratings_mill[ ratings_mill$id.miller %in%  names(table(ratings_mill$id.miller))[table(ratings_mill$id.miller) >15] , ]   #1192 obs
gt20_mill <- ratings_mill[ ratings_mill$id.miller %in%  names(table(ratings_mill$id.miller))[table(ratings_mill$id.miller) >20] , ]   #944 obs

gt20_millsub <- subset(gt20_mill[c(1,2,9)]) #KEEPING farmerID, miller ID and overall ratings 

#shows duplicate rows
which(duplicated(gt20_millsub))
which(duplicated(gt20_mill)) 

##duplicates based on farmer ID and miller ID
gt20_millsub[c(duplicated(gt20_millsub[,1:2])),] 

##creating crossed designs 
library(dplyr)
library(tidyr)
#by farmerID
gt20_millsub_f<- gt20_millsub %>%
  group_by(id.miller, farmerID) %>% 
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
gt20_millsub_f[c(duplicated(gt20_millsub_f[,1])),]

### Wilcox test
tapply(ratings_mill$rating_overall,ratings_mill$used, mean )
wilcox.test(ratings_mill$rating_overall,millers$miller_rating_overall)
##Null: Distributions are same and have same median
##Looking at p-value, we may conclude that the medians of the 2 distributions differ
##W indicates the number of times the rating from a farmer is different to rating by the millers


#OVERALL RATING
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_overall, na.rm=T) #3.527844
#overall mean score with interaction
mean(ratings_mill$rating_overall[ratings_mill$used=="Yes"], na.rm=T) # 3.550272
#overall mean score without interaction
mean(ratings_mill$rating_overall[ratings_mill$used=="No"], na.rm=T) #3.138947

#overall rating provided by millers to themselves 
mean(millers$miller_rating_overall, na.rm=T)
#4.177011 (higher than farmers)

##RATING LOCATION
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_location, na.rm=T) #3.813608
#overall mean score with interaction
mean(ratings_mill$rating_location[ratings_mill$used=="Yes"], na.rm=T) # 3.832426
#overall mean score without interaction
mean(ratings_mill$rating_location[ratings_mill$used=="No"], na.rm=T) # 3.494737

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q36, na.rm=T)
#3.988506 (higher than farmers)

##RATING PRICE
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_price, na.rm=T) #3.008576
#overall mean score with interaction
mean(ratings_mill$rating_price[ratings_mill$used=="Yes"], na.rm=T) # 3.010889
#overall mean score without interaction
mean(ratings_mill$rating_price[ratings_mill$used=="No"], na.rm=T) # 2.968421

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q37, na.rm=T)
#3.844828 (higher than farmers)

##RATING QUALITY
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_quality, na.rm=T) #3.389937
#overall mean score with interaction
mean(ratings_mill$rating_quality[ratings_mill$used=="Yes"], na.rm=T) # 3.421658
#overall mean score without interaction
mean(ratings_mill$rating_quality[ratings_mill$used=="No"], na.rm=T) # 2.852632

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q38, na.rm=T)
#4.16092 (higher than farmers)

##RATING SERVICE
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_service, na.rm=T) #3.606632
#overall mean score with interaction
mean(ratings_mill$rating_service[ratings_mill$used=="Yes"], na.rm=T) #3.640048
#overall mean score without interaction
mean(ratings_mill$rating_service[ratings_mill$used=="No"], na.rm=T) #  3.010526

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q39, na.rm=T)
#4.390805 (higher than farmers)

##RATING REPUTATION
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_reputation, na.rm=T) #3.820469
#overall mean score with interaction
mean(ratings_mill$rating_reputation[ratings_mill$used=="Yes"], na.rm=T) #3.84634
#overall mean score without interaction
mean(ratings_mill$rating_reputation[ratings_mill$used=="No"], na.rm=T) # 3.368421

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q40, na.rm=T)
#4.5 (higher than farmers)


###  GRAPHS  ####
## First graph is a simple bar chart of means - OVERALL RATING of customers, non-customers and dealers

df_mill <- data.frame(c(mean(millers$miller_rating_overall),tapply(ratings_mill$rating_overall,ratings_mill$used, mean )[2:3]))
names(df_mill) <- "score"
rownames(df_mill) <- NULL
df_mill$levels <- c("miller","non-customer","customer")
df_mill <- df_mill[order(df_mill$score,decreasing = TRUE),]
df_mill$levels <- factor(df_mill$levels,levels= c("miller","customer","non-customer"))

png(paste(path, "figures/fig_miller_rateoverall.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_mill, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("miller", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("miller", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#OVERALL RATING 
wilcox.test(ratings_mill$rating_overall[ratings_mill$used=="No"],millers$miller_rating_overall)
wilcox.test(ratings_mill$rating_overall[ratings_mill$used=="Yes"],millers$miller_rating_overall)
wilcox.test(ratings_mill$rating_overall[ratings_mill$used=="Yes"],ratings_mill$rating_overall[ratings_mill$used=="No"])
#for all 3, reject null, distributions are different 

## Second graph is a simple bar chart of means - LOCATION RATING of customers, non-customers and dealers

df_mill_loc <- data.frame(c(mean(millers$hh.maize.q36),tapply(ratings_mill$rating_location,ratings_mill$used, mean )[2:3]))
names(df_mill_loc) <- "score"
rownames(df_mill_loc) <- NULL
df_mill_loc$levels <- c("miller","non-customer","customer")
df_mill_loc <- df_mill_loc[order(df_mill_loc$score,decreasing = TRUE),]
df_mill_loc$levels <- factor(df_mill_loc$levels,levels= c("miller","customer","non-customer"))

png(paste(path, "figures/fig_miller_rateloc.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_mill_loc, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank())  +
  geom_signif(comparisons = list(c("miller", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#LOCATION RATING 
wilcox.test(ratings_mill$rating_location[ratings_mill$used=="No"],millers$hh.maize.q36)
#reject null, distributions are different 
wilcox.test(ratings_mill$rating_location[ratings_mill$used=="Yes"],millers$hh.maize.q36)
#cannot reject null, distributions are same 
wilcox.test(ratings_mill$rating_location[ratings_mill$used=="Yes"],ratings_mill$rating_location[ratings_mill$used=="No"])
#reject null, distributions are different 

## Third graph is a simple bar chart of means - PRICE RATING of customers, non-customers and dealers

df_mill_p <- data.frame(c(mean(millers$hh.maize.q37),tapply(ratings_mill$rating_price,ratings_mill$used, mean )[2:3]))
names(df_mill_p) <- "score"
rownames(df_mill_p) <- NULL
df_mill_p$levels <- c("miller","non-customer","customer")
df_mill_p <- df_mill_p[order(df_mill_p$score,decreasing = TRUE),]
df_mill_p$levels <- factor(df_mill_p$levels,levels= c("miller","customer","non-customer"))

png(paste(path, "figures/fig_miller_rateprice.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_mill_p, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("miller", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("miller", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) 
dev.off()

### Tests in the graph
#PRICE RATING 
wilcox.test(ratings_mill$rating_price[ratings_mill$used=="No"],millers$hh.maize.q37)
#reject null, distributions are different 
wilcox.test(ratings_mill$rating_price[ratings_mill$used=="Yes"],millers$hh.maize.q37)
# reject null, distributions are different 
wilcox.test(ratings_mill$rating_price[ratings_mill$used=="Yes"],ratings_mill$rating_price[ratings_mill$used=="No"])
#cannot reject null, distributions are same  

## Fourth graph is a simple bar chart of means - QUALITY RATING of customers, non-customers and dealers

df_mill_q <- data.frame(c(mean(millers$hh.maize.q38),tapply(ratings_mill$rating_quality,ratings_mill$used, mean )[2:3]))
names(df_mill_q) <- "score"
rownames(df_mill_q) <- NULL
df_mill_q$levels <- c("miller","non-customer","customer")
df_mill_q <- df_mill_q[order(df_mill_q$score,decreasing = TRUE),]
df_mill_q$levels <- factor(df_mill_q$levels,levels= c("miller","customer","non-customer"))

png(paste(path, "figures/fig_miller_ratequal.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_mill_q, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("miller", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("miller", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#QUALITY RATING 
wilcox.test(ratings_mill$rating_quality[ratings_mill$used=="No"],millers$hh.maize.q38)
#reject null, distributions are different 
wilcox.test(ratings_mill$rating_quality[ratings_mill$used=="Yes"],millers$hh.maize.q38)
#reject null, distributions are different 
wilcox.test(ratings_mill$rating_quality[ratings_mill$used=="Yes"],ratings_mill$rating_quality[ratings_mill$used=="No"])
#reject null, distributions are different 


## Fifth graph is a simple bar chart of means - SERVICE RATING of customers, non-customers and dealers

df_mill_s <- data.frame(c(mean(millers$hh.maize.q39),tapply(ratings_mill$rating_service,ratings_mill$used, mean )[2:3]))
names(df_mill_s) <- "score"
rownames(df_mill_s) <- NULL
df_mill_s$levels <- c("miller","non-customer","customer")
df_mill_s <- df_mill_s[order(df_mill_s$score,decreasing = TRUE),]
df_mill_s$levels <- factor(df_mill_s$levels,levels= c("miller","customer","non-customer"))

png(paste(path, "figures/fig_miller_rateservice.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_mill_s, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("miller", "customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("miller", "non-customer")), annotations="***", y_position = 4.8, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#SERVICE RATING 
wilcox.test(ratings_mill$rating_service[ratings_mill$used=="No"],millers$hh.maize.q39)
#reject null, distributions are different 
wilcox.test(ratings_mill$rating_service[ratings_mill$used=="Yes"],millers$hh.maize.q39)
# reject null, distributions are different 
wilcox.test(ratings_mill$rating_service[ratings_mill$used=="Yes"],ratings_mill$rating_service[ratings_mill$used=="No"])
#reject null, distributions are different 

## Sixth graph is a simple bar chart of means - REPUTATION RATING of customers, non-customers and dealers

df_mill_r <- data.frame(c(mean(millers$hh.maize.q40),tapply(ratings_mill$rating_reputation,ratings_mill$used, mean )[2:3]))
names(df_mill_r) <- "score"
rownames(df_mill_r) <- NULL
df_mill_r$levels <- c("miller","non-customer","customer")
df_mill_r <- df_mill_r[order(df_mill_r$score,decreasing = TRUE),]
df_mill_r$levels <- factor(df_mill_r$levels,levels= c("miller","customer","non-customer"))

png(paste(path, "figures/fig_miller_raterepu.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df_mill_r, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("miller", "customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("miller", "non-customer")), annotations="***", y_position = 4.9, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

### Tests in the graph
#REPUTATION RATING 
wilcox.test(ratings_mill$rating_reputation[ratings_mill$used=="No"],millers$hh.maize.q40)
#reject null, distributions are different 
wilcox.test(ratings_mill$rating_reputation[ratings_mill$used=="Yes"],millers$hh.maize.q40)
#reject null, distributions are different 
wilcox.test(ratings_mill$rating_reputation[ratings_mill$used=="Yes"],ratings_mill$rating_reputation[ratings_mill$used=="No"])
#reject null, distributions are different 

#Likert scales bar charts for the different components of the scores, again for the three categories ----- MILLERS

plot_non_customer <- data.frame(cbind(prop.table(table(ratings_mill$rating_location, ratings_mill$used=="No"),2)[,1],
prop.table(table(ratings_mill$rating_price, ratings_mill$used=="No"),2)[,1],
prop.table(table(ratings_mill$rating_quality, ratings_mill$used=="No"),2)[,1],
prop.table(table(ratings_mill$rating_service, ratings_mill$used=="No"),2)[,1],
prop.table(table(ratings_mill$rating_reputation, ratings_mill$used=="No"),2)[,1]))
names(plot_non_customer) <- c("location","price","quality","service","reputation")

plot_customer <- data.frame(cbind(prop.table(table(ratings_mill$rating_location, ratings_mill$used=="Yes"),2)[,2],
prop.table(table(ratings_mill$rating_price, ratings_mill$used=="Yes"),2)[,2],
prop.table(table(ratings_mill$rating_quality, ratings_mill$used=="Yes"),2)[,2],
prop.table(table(ratings_mill$rating_service, ratings_mill$used=="Yes"),2)[,2],
prop.table(table(ratings_mill$rating_reputation, ratings_mill$used=="Yes"),2)[,2]))
names(plot_customer) <- c("location","price","quality","service","reputation")

plot_miller <- data.frame(cbind(c(prop.table(table(millers$hh.maize.q36))),
c(prop.table(table(millers$hh.maize.q37))),
c(0,prop.table(table(millers$hh.maize.q38))),
c(prop.table(table(millers$hh.maize.q39))),
c(0,prop.table(table(millers$hh.maize.q40)))))
names(plot_miller) <- c("location","price","quality","service","reputation")
png(paste(path, "figures/fig_miller_likert.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
par(mfrow=c(1,3), xpd=NA, mar = c(10, 5,5, 1)) 
colfunc<-colorRampPalette(c("red", "green"))
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="non-customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_miller), col=colfunc(5), main="miller", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()


###########################################
##SUBSETTING AND PIVOTING TRIAL 
subset(data.frame(table(ratings_mill$farmerID)), Freq > 1)
subset(data.frame(table(ratings_mill$farmerID)), Freq > 2)
#dataset where farmers have rated more than twice 
farmer_gt2_mill <- ratings_mill[ ratings_mill$farmerID %in%  names(table(ratings_mill$farmerID))[table(ratings_mill$farmerID) >2] , ]
subset(data.frame(table(farmer_gt2_mill$id.miller)), Freq > 2)
subset(data.frame(table(farmer_gt2_mill$id.miller)), Freq > 3)
subset(data.frame(table(farmer_gt2_mill$id.miller)), Freq > 7)
subset(data.frame(table(farmer_gt2_mill$id.miller)), Freq > 8)
#dataset where millers have been rated more than 8 times 
mill_gt8 <- farmer_gt2_mill[ farmer_gt2_mill$id.miller %in%  names(table(farmer_gt2_mill$id.miller))[table(farmer_gt2_mill$id.miller) >8] , ]
#dataset with farmer ID, miller ID and ratings overall
mill <- subset(mill_gt8[c(1,2,9)])

#creating crossed design with small group of traders and farmers 
library(tidyr)
library(dplyr)
millpivot <- mill %>%
  group_by(id.miller, farmerID) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
millpivot[c(duplicated(millpivot[,1])),] #duplicate exists

##INCREASING THE FREQUENCY
subset(data.frame(table(farmer_gt2_mill$id.miller)), Freq > 12)
#dataset where millers have been rated more than 12 times 
mill_gt12 <- farmer_gt2_mill[ farmer_gt2_mill$id.miller %in%  names(table(farmer_gt2_mill$id.miller))[table(farmer_gt2_mill$id.miller) >12] , ]
#dataset with farmer ID, miller ID and ratings overall
mill_12 <- subset(mill_gt12[c(1,2,9)])

#creating crossed design with small group of traders and farmers 
library(tidyr)
library(dplyr)
millpivot_12 <- mill_12 %>%
  group_by(id.miller, farmerID) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = farmerID, values_from = rating_overall) %>%
  select(-row)
millpivot_12[c(duplicated(millpivot_12[,1])),] #duplicate exists

millpivot_12<- millpivot_12[-c(2),-c(1)] #removing duplicate row 

library(irr)
kappam.fleiss(millpivot_12)
#Kappa = -0.0833  (poor agreement)
agree(millpivot_12, tolerance = 0.5) #0 percent 

############################################################################

###### REGRESSIONS ######
#Also regressions run on subsets where the value chain actors have received ratings more than 20 times from the farmers 

#AGRO INPUT DEALERS 

library(fBasics)
library(sjPlot)
###############################################
##checking for missing data
library(mice)
md.pattern(ratings)
#No need for mice. This data set is completely observed.

###############################################
###RATING LOCATION
fe_interceptloc <- lm(rating_location ~ 1, data=ratings)
summary(fe_interceptloc)
#3.64472 

fe_interceptloc20 <- lm(rating_location ~ 1, data=gt20)
summary(fe_interceptloc20)
#3.78231 

fe_modlocation <- lm(rating_location ~ id.agro, data = ratings)
summary(fe_modlocation)
##reject null, individual means diff from overall mean
#F-statistic: 3.615 on 72 and 656 DF, 1.318 (F-table value)<F-stat (reject null). 5% level

fe_modlocation20 <- lm(rating_location ~ id.agro, data = gt20)
summary(fe_modlocation20)
#reject null, 1.8799<F-stat; reject null at 5%

tab_model(fe_interceptloc, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocation, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptloc, fe_modlocation, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocation, fe_modlocation20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modlocation$resid)
#Asymptotic p Value: 2.258e-07 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution
jarqueberaTest(fe_modlocation20$resid) #not a normal distribution

########################################
##RATING PRICE 
fe_interceptprice <- lm(rating_price ~ 1, data=ratings)
summary(fe_interceptprice)
# 2.9904 

fe_interceptprice20 <- lm(rating_price ~ 1, data=gt20)
summary(fe_interceptprice20)
#2.97959

fe_modprice <- lm(rating_price ~ id.agro, data = ratings)
summary(fe_modprice)
## p-value: 0.2174, cannot reject null, individual means same as overall mean

fe_modprice20 <- lm(rating_price ~ id.agro, data = gt20)
summary(fe_modprice20) #cannot reject null at 5% level

tab_model(fe_interceptprice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modprice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptprice, fe_modprice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modprice, fe_modprice20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modprice$resid)
#Asymptotic p Value: 0.07378 
#we reject the null only at 10% level that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution only at 10% level
#normal distribution at 5% and 1% levels 
jarqueberaTest(fe_modprice20$resid)
#cannot reject the null, is a normal distribution 

########################################
##RATING QUALITY
fe_interceptqual <- lm(rating_quality ~ 1, data=ratings)
summary(fe_interceptqual)
#3.61317 

fe_interceptqual20 <- lm(rating_quality ~ 1, data=gt20)
summary(fe_interceptqual20)
#3.59864

fe_modqual <- lm(rating_quality ~ id.agro, data = ratings)
summary(fe_modqual)
## p-value: 0.06005, reject null at 10% level, individual means diff from overall mean
#F-statistic: 1.292 on 72 and 656 DF, 1.318 (F-table value); F-table value>F-stat, cannot reject null (5% level)
#cannot reject null at 5% and 1% levels, individual means same as overall mean
fe_modqual20 <- lm(rating_quality ~ id.agro, data = gt20)
summary(fe_modqual20)
#cannot reject null

tab_model(fe_interceptqual, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqual, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptqual, fe_modqual, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqual, fe_modqual20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modqual$resid)
#Asymptotic p Value: 0.0001118 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modqual20$resid)
#not normal distribution 

###############################################################
##RATING STOCK
fe_interceptstock <- lm(rating_stock ~ 1, data=ratings)
summary(fe_interceptstock)
#3.82442 

fe_interceptstock20 <- lm(rating_stock ~ 1, data=gt20)
summary(fe_interceptstock20)
# 3.94558 

fe_modstock <- lm(rating_stock ~ id.agro, data = ratings)
summary(fe_modstock)
## p-value: 0.1032,cannot reject null, individual means same as overall mean
fe_modstock20 <- lm(rating_stock ~ id.agro, data = gt20)
summary(fe_modstock20)
#cannot reject null

tab_model(fe_interceptstock, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modstock, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptstock, fe_modstock, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modstock, fe_modstock20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modstock$resid)
#Asymptotic p Value: 4.441e-15 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modstock20$resid)
#not normal distribution

###############################################################
##RATING REPUTATION
fe_interceptrep <- lm(rating_reputation ~ 1, data=ratings)
summary(fe_interceptrep)
#3.81619 

fe_interceptrep20 <- lm(rating_reputation ~ 1, data=gt20)
summary(fe_interceptrep20)
# 3.8435

fe_modrep <- lm(rating_reputation ~ id.agro, data = ratings)
summary(fe_modrep)
## p-value:  0.0519, reject null at 10% level
#F-statistic: 1.307 on 72 and 656 DF, F-table value 1.318>F-stat, cannot reject null, 5% level
#cannot reject null at 1 or 5% levels, individual means same as overall mean
fe_modrep20 <- lm(rating_reputation ~ id.agro, data = gt20)
summary(fe_modrep20)
#cannot reject null

tab_model(fe_interceptrep, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrep, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptrep, fe_modrep, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrep, fe_modrep20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modrep$resid)
#Asymptotic p Value: < 2.2e-16  
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modrep20$resid) #not normal

###############################################################
##RATING OVERALL
fe_interceptoverall <- lm(rating_overall ~ 1, data=ratings)
summary(fe_interceptoverall)
#3.57778

fe_interceptoverall20 <- lm(rating_overall ~ 1, data=gt20)
summary(fe_interceptoverall20) #3.62993 

fe_modoverall <- lm(rating_overall ~ id.agro, data = ratings)
summary(fe_modoverall)
## p-value: 0.0003263, reject null, individual means diff from overall mean
#F-statistic: 1.732 on 72 and 656 DF, F-table value 1.318<F-stat, reject null (5% level)
fe_modoverall20 <- lm(rating_overall ~ id.agro, data = gt20)
summary(fe_modoverall20) #cannot reject null

tab_model(fe_interceptoverall, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverall, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptoverall, fe_modoverall, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverall, fe_modoverall20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modoverall$resid)
#Asymptotic p Value: 8.327e-15 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modoverall20$resid) #not normal

#################################################################################################################


############## TRADERS ################

library(fBasics)
library(sjPlot)
###############################################
##checking for missing data
library(mice)
md.pattern(ratings_trader)
#No need for mice. This data set is completely observed.

###############################################
###RATING LOCATION
fe_interceptloct <- lm(rating_location ~ 1, data=ratings_trader)
summary(fe_interceptloct)
#4.08165 
fe_interceptloct20 <- lm(rating_location ~ 1, data=gt20_trad)
summary(fe_interceptloct20)
#4.2466 

fe_modlocationt <- lm(rating_location ~ id.trader, data = ratings_trader)
summary(fe_modlocationt)
##reject null, individual means diff from overall mean
#F-statistic: 1.433 on 309 and 1221 DF, 1 (F-table value)<F-stat (reject null), 5% level
fe_modlocationt20 <- lm(rating_location ~ id.trader, data = gt20_trad)
summary(fe_modlocationt20)
#reject null, F-stat > 3.1504, reject null

tab_model(fe_interceptloct, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocationt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptloct, fe_modlocationt, show.se = TRUE, show.stat = TRUE)
tab_model( fe_modlocationt, fe_modlocationt20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modlocationt$resid)
#Asymptotic p Value: < 2.2e-16 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution
jarqueberaTest(fe_modlocationt20$resid) #not normal at 5%

########################################
##RATING PRICE 
fe_interceptpricet <- lm(rating_price ~ 1, data=ratings_trader)
summary(fe_interceptpricet)
#3.05813

fe_interceptpricet20 <- lm(rating_price ~ 1, data=gt20_trad)
summary(fe_interceptpricet20) #3.1507 

fe_modpricet <- lm(rating_price ~ id.trader, data = ratings_trader)
summary(fe_modpricet)
## p-value: 0.002471, reject null, individual means diff from overall mean
#F-statistic: 1.279 on 309 and 1221 DF > 1 (F-table value), reject null
fe_modpricet20 <- lm(rating_price ~ id.trader, data = gt20_trad)
summary(fe_modpricet20)
#cannot reject null

tab_model(fe_interceptpricet, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricet, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptpricet, fe_modpricet, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricet, fe_modpricet20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modpricet$resid)
#Asymptotic p Value: 0.6908 
#we cannot reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, a normal distribution 
jarqueberaTest(fe_modpricet20$resid) #normal

########################################
##RATING QUALITY
fe_interceptqualt <- lm(rating_quality ~ 1, data=ratings_trader)
summary(fe_interceptqualt)
#3.51600 
fe_interceptqualt20 <- lm(rating_quality ~ 1, data=gt20_trad)
summary(fe_interceptqualt20) #  3.7123

fe_modqualt <- lm(rating_quality ~ id.trader, data = ratings_trader)
summary(fe_modqualt)
## p-value: 0.001379, reject null, individual means diff from overall mean
#F-statistic: 1.299 on 309 and 1221 DF, 1 (F-table value); F-table value<F-stat, reject null (5% level)
fe_modqualt20 <- lm(rating_quality ~ id.trader, data = gt20_trad)
summary(fe_modqualt20) #can reject at 10% level, F-stat<3.1504 (cannot reject at 5%)

tab_model(fe_interceptqualt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptqualt, fe_modqualt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualt, fe_modqualt20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modqualt$resid)
#Asymptotic p Value: 3.114e-09 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modqualt20$resid) #cannot reject at 5%, normal

###############################################################
##RATING HONESTY
fe_intercepthonesty <- lm(rating_honesty ~ 1, data=ratings_trader)
summary(fe_intercepthonesty)
#3.74461
fe_intercepthonesty20 <- lm(rating_honesty ~ 1, data=gt20_trad)
summary(fe_intercepthonesty20) #3.8904  

fe_modhonesty <- lm(rating_honesty ~ id.trader, data = ratings_trader)
summary(fe_modhonesty)
## p-value: 0.0007262, reject null, individual means diff from overall mean
#F-statistic:  1.32 on 309 and 1221 DF > F-table value (1), reject null
fe_modhonesty20 <- lm(rating_honesty ~ id.trader, data = gt20_trad)
summary(fe_modhonesty20) #can reject at 5%, F-stat>3.15, reject at 5%

tab_model(fe_intercepthonesty, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modhonesty, show.se = TRUE, show.stat = TRUE)
tab_model(fe_intercepthonesty, fe_modhonesty, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modhonesty, fe_modhonesty20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modhonesty$resid)
#Asymptotic p Value:2.22e-16 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modhonesty20$resid) #can reject at 5%, not normal

###############################################################
##RATING REPUTATION
fe_interceptrept <- lm(rating_reputation ~ 1, data=ratings_trader)
summary(fe_interceptrept)
#3.81973  
fe_interceptrept20 <- lm(rating_reputation ~ 1, data=gt20_trad)
summary(fe_interceptrept20) #3.9452 

fe_modrept <- lm(rating_reputation ~ id.trader, data = ratings_trader)
summary(fe_modrept)
## p-value:   0.001123, reject null, individual means diff from overall means 
#F-statistic: 1.306 on 309 and 1221 DF, F-table value 1<F-stat, reject null, 5% level
fe_modrept20 <- lm(rating_reputation ~ id.trader, data = gt20_trad)
summary(fe_modrept20) #reject null, F stat>3.15, reject

tab_model(fe_interceptrept, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrept, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptrept, fe_modrept, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrept, fe_modrept20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modrept$resid)
#Asymptotic p Value: < 2.2e-16  
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modrept20$resid) #can reject at 5%, not normal

###############################################################
##RATING OVERALL
fe_interceptoverallt <- lm(rating_overall ~ 1, data=ratings_trader)
summary(fe_interceptoverallt)
#3.64402 
fe_interceptoverallt20 <- lm(rating_overall ~ 1, data=gt20_trad)
summary(fe_interceptoverallt20) # 3.78904

fe_modoverallt <- lm(rating_overall ~ id.trader, data = ratings_trader)
summary(fe_modoverallt)
## p-value: 9.433e-06, reject null, individual means diff from overall mean
#F-statistic: 1.448 on 309 and 1221 DF, F-table value 1 < F-stat, reject null (5% level)
fe_modoverallt20 <- lm(rating_overall ~ id.trader, data = gt20_trad)
summary(fe_modoverallt20) #reject at 5%, F-stat greater, reject 

tab_model(fe_interceptoverallt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptoverallt, fe_modoverallt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallt, fe_modoverallt20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modoverallt$resid)
#Asymptotic p Value: 2.176e-14 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modoverallt20$resid) #reject at 5%, not normal

############################################################################################

############## MILLERS  ################

library(fBasics)
library(sjPlot)
###############################################
##checking for missing data
library(mice)
md.pattern(ratings_mill)
#No need for mice. This data set is completely observed.

###############################################
###RATING LOCATION
fe_interceptlocm <- lm(rating_location ~ 1, data=ratings_mill)
summary(fe_interceptlocm)
#3.81361  
fe_interceptlocm20 <- lm(rating_location ~ 1, data=gt20_mill)
summary(fe_interceptlocm20)  # 3.85805 

fe_modlocationm <- lm(rating_location ~ id.miller, data = ratings_mill)
summary(fe_modlocationm)
##reject null, individual means diff from overall mean
#F-statistic: 3.052 on 147 and 1601 DF, 1.2214 (F-table value)<F-stat (reject null), 5% level
fe_modlocationm20 <- lm(rating_location ~ id.miller, data = gt20_mill)
summary(fe_modlocationm20) #reject null, F-stat>1.4591, reject

tab_model(fe_interceptlocm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocationm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptlocm, fe_modlocationm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocationm, fe_modlocationm20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modlocationm$resid)
#Asymptotic p Value: < 2.2e-16 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution
jarqueberaTest(fe_modlocationm20$resid) #not normal

########################################
##RATING PRICE 
fe_interceptpricem <- lm(rating_price ~ 1, data=ratings_mill)
summary(fe_interceptpricem)
# 3.0086 
fe_interceptpricem20 <- lm(rating_price ~ 1, data=gt20_mill)
summary(fe_interceptpricem20) #  3.0064  

fe_modpricem <- lm(rating_price ~ id.miller, data = ratings_mill)
summary(fe_modpricem)
## p-value:  2.441e-07, reject null, individual means diff from overall mean
#F-statistic: 1.756 on 147 and 1601 DF, 1.2214 (F-table value)<F-stat (reject null), 5% level
fe_modpricem20 <- lm(rating_price ~ id.miller, data = gt20_mill)
summary(fe_modpricem20) #reject null, F-stat larger, reject

tab_model(fe_interceptpricem, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricem, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptpricem, fe_modpricem, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricem, fe_modpricem20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modpricem$resid)
#Asymptotic p Value: 0.0003833 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modpricem20$resid) #not normal

########################################
##RATING QUALITY
fe_interceptqualm <- lm(rating_quality ~ 1, data=ratings_mill)
summary(fe_interceptqualm)
#3.38994
fe_interceptqualm20 <- lm(rating_quality ~ 1, data=gt20_mill)
summary(fe_interceptqualm20)   #3.48623  

fe_modqualm <- lm(rating_quality ~ id.miller, data = ratings_mill)
summary(fe_modqualm)
## p-value: < 2.2e-16, reject null, individual means diff from overall mean
#F-statistic:4.531 on 147 and 1601 DF, 1.2214 (F-table value)<F-stat (reject null), 5% level
fe_modqualm20 <- lm(rating_quality ~ id.miller, data = gt20_mill)
summary(fe_modqualm20) #reject, F-stat larger, reject

tab_model(fe_interceptqualm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptqualm, fe_modqualm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualm, fe_modqualm20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modqualm$resid)
#Asymptotic p Value:  0.0007949 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modqualm20$resid) #not normal

###############################################################
##RATING SERVICE
fe_interceptservice <- lm(rating_service ~ 1, data=ratings_mill)
summary(fe_interceptservice)
# 3.60663 
fe_interceptservice20 <- lm(rating_service ~ 1, data=gt20_mill)
summary(fe_interceptservice20)   # 3.57839 

fe_modservice <- lm(rating_service ~ id.miller, data = ratings_mill)
summary(fe_modservice)
## p-value: 3.295e-07, reject null, individual means diff from overall mean
#F-statistic:  1.32 on 309 and 1221 DF, 1.2214 (F-table value)<F-stat (reject null), 5% level
fe_modservice20 <- lm(rating_service ~ id.miller, data = gt20_mill)
summary(fe_modservice20) #reject null, Fstat larger, reject

tab_model(fe_interceptservice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modservice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptservice, fe_modservice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modservice, fe_modservice20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modservice$resid)
#Asymptotic p Value:1.989e-06 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modservice20$resid) #not normal

###############################################################
##RATING REPUTATION
fe_interceptrepm <- lm(rating_reputation ~ 1, data=ratings_mill)
summary(fe_interceptrepm)
#3.82047  
fe_interceptrepm20 <- lm(rating_reputation ~ 1, data=gt20_mill)
summary(fe_interceptrepm20)   # 3.76165 

fe_modrepm <- lm(rating_reputation ~ id.miller, data = ratings_mill)
summary(fe_modrepm)
## p-value:   0.002989, reject null, individual means diff from overall means 
#F-statistic:  1.373 on 147 and 1601 DF, 1.2214 (F-table value)<F-stat (reject null), 5% level
fe_modrepm20 <- lm(rating_reputation ~ id.miller, data = gt20_mill)
summary(fe_modrepm20) #reject null, F-stat larger, reject 

tab_model(fe_interceptrepm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrepm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptrepm, fe_modrepm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrepm, fe_modrepm20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modrepm$resid)
#Asymptotic p Value: < 2.2e-16  
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modrepm20$resid) #not normal

###############################################################
##RATING OVERALL
fe_interceptoverallm <- lm(rating_overall ~ 1, data=ratings_mill)
summary(fe_interceptoverallm)
# 3.52784  
fe_interceptoverallm20 <- lm(rating_overall ~ 1, data=gt20_mill)
summary(fe_interceptoverallm20) #3.53814

fe_modoverallm <- lm(rating_overall ~ id.miller, data = ratings_mill)
summary(fe_modoverallm)
## p-value:2.849e-15, reject null, individual means diff from overall mean
#F-statistic:2.324 on 147 and 1601 DF, 1.2214 (F-table value)<F-stat (reject null), 5% level
fe_modoverallm20 <- lm(rating_overall ~ id.miller, data = gt20_mill)
summary(fe_modoverallm20)   #reject, F-stat larger, reject null

tab_model(fe_interceptoverallm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptoverallm, fe_modoverallm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallm, fe_modoverallm20, show.se = TRUE, show.stat = TRUE)

##testing for normality
jarqueberaTest(fe_modoverallm$resid)
#Asymptotic p Value: 0.0003261  
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution 
jarqueberaTest(fe_modoverallm20$resid) #not normal



######################################
#inter-rater agreements 
library(agreement)
devtools::install_github("jmgirard/agreement")

newdata <- ratings[ , c("id.agro", "farmerID", "rating_overall")]   

rat <- newdata[!apply(newdata == "", 1, any), ] ##check if it works without this

library(dplyr)
ratnew <- rat %>% rename (Object = id.agro, Rater = farmerID, Score = rating_overall)

##Calculate chance-adjusted indexes of categorical agreement for unordered categories
results1 <- cat_adjusted(ratnew)
summary(results1, ci = TRUE, type = "perc")
tidy(results1, type = "perc")
plot(results1)

##Calculate chance-adjusted indexes of categorical agreement for ordered categories
results2 <- cat_adjusted(ratnew, weighting = "linear")
summary(results2, ci = TRUE, type = "perc")
tidy(results2, type = "perc")
plot(results2)

##Calculate category-specific agreement
results3 <- cat_specific(ratnew)
summary(results3, ci = TRUE, type = "bca")
tidy(results3, type = "bca")
plot(results3)

##Calculate intraclass correlation coefficient for dimensional data with 1 trial
##each rater rates a different group of objects
results4 <- dim_icc(ratnew, model = "1A", type = "agreement", unit = "average",
                    object = Object, rater = Rater, score = Score, warnings = FALSE)
summary(results4)
tidy(results4)
plot(results4, intra = FALSE, inter = TRUE)

res <- dim_icc(highscore, model = "2", type = "agreement", unit = "average",
               object = Object, rater = Rater, score = Score, warnings = FALSE)

##model 2A:both raters and objects are random, excludes interaction, can be used with single or multiple trials per rater
results_ms <- dim_icc(ms, model = "2A", type = "agreement", unit = "average",
                      object = Object, rater = Rater, score = Score, warnings = FALSE)

reshigh <- dim_icc(highscore, model = "2A", type = "consistency", unit = "average",
                   object = Object, rater = Rater, score = Score, warnings = FALSE)

rhigh <- dim_icc(highs, model = "2A", type = "agreement", unit = "average",
                 object = Object, rater = Rater, score = Score, warnings = FALSE)

##each rater rates a different group of objects

rate1b <- dim_icc(ratnew, model = "1B", type = "agreement", unit = "average",
                  object = Object, rater = Rater, score = Score, bootstrap = 20, warnings = TRUE)

rate <- dim_icc(highs, model = "1B", type = "agreement", unit = "single",
                object = Object, rater = Rater, score = Score, warnings = FALSE)

rate1 <- dim_icc(highs, model = "1B", type = "agreement", unit = "average",
                 object = Object, rater = Rater, score = Score, warnings = FALSE)

highscore <- subset(ratnew, Score>=3)

highs <- subset(ratnew, Score>=4)

##Calculate chance-adjusted indexes of categorical agreement for ordered categories
results_high <- cat_adjusted(highs, weighting = "linear")
summary(results_high, ci = TRUE, type = "perc")
tidy(results_high, type = "perc")
plot(results_high)

r1 <- cat_specific(highs)
summary(r1, ci = TRUE, type = "bca")
tidy(r1, type = "bca")
plot(r1)

#########################################################################
##Joint Hypothesis testing using F stat
#Can we reject the hypothesis that the coefficient on id.agro is 0?
#We have to do joint hypothesis test. A joint hypothesis imposes restrictions on multiple regression coefficients.
#library(car)
#linearHypothesis(fe_modstock, c("factor(id.agro)=0"))
#we can reject the hypothesis that the coefficients are 0 

##heteroskedasticity robust f-test
#linearHypothesis(fe_modqual, c("factor(id.agro)=0"), white.adjust = "hc1")
##same conclusion

