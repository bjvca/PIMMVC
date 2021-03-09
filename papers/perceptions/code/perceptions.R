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

stack1 <- cbind(farmers[c("ID","id.agro1","hh.maize.q25","hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")],"Yes")
names(stack1) <- c("farmerID","id.agro", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")
stack2 <- cbind(farmers[c("ID","id.agro2","hh.maize.q25","hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l","hh.maize.agro2.q110")])
names(stack2) <- c("farmerID","id.agro", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")
stack3 <- cbind(farmers[c("ID","id.agro3","hh.maize.q25","hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l","hh.maize.agro3.q112")])
names(stack3) <- c("farmerID","id.agro", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")

ratings <-rbind(stack1,stack2,stack3)
ratings[c("id.agro","bought")] <- lapply(ratings[c("id.agro","bought")], function(x) as.factor(as.character(x)) )

##Subsetting 
ratings <- subset(ratings, !is.na(rating_reputation) )
ratings <- subset(ratings[!apply(ratings == "", 1, any),])
ratings <- ratings[c(!duplicated(ratings[,1:2])),] #removing duplicates 

which(duplicated(ratings)) #no duplicates

### simple average (Index for overall rating)
ratings$rating_overall <- rowSums(ratings[c("rating_location","rating_price","rating_quality","rating_stock","rating_reputation")])/5
summary(ratings$rating_overall)

## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))
##Index for overall rating by dealers for themselves 
dealers$dealer_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$dealer_rating_overall)

#subset for merging 
#input dealers who provide extension/training to clients (dealers$hh.maize.q67) and seed on credit (dealers$hh.maize.q68)
#1 = No, 2 = To some, 3 = to everyone who wants 
#Including Total quantity sold of the top 3 most popular hybrid maize seeds over the first season of 2018 - q22; Total quantity sold of the top 3 most popular opv seeds over the first season of 2018 - q38
#hh.maize.q7 - gender of input dealers 
dealers_m <- subset(dealers, select = c('id.agro' , 'hh.maize.q7','hh.maize.q67','hh.maize.q68','hh.maize.q79','hh.maize.q80','hh.maize.q81','hh.maize.q82','hh.maize.q83','dealer_rating_overall', 'hh.maize.seed.1..q22', 'hh.maize.seed.2..q22', 'hh.maize.seed.3..q22', 'hh.maize.opv.1..q38', 'hh.maize.opv.2..q38', 'hh.maize.opv.3..q38'))

#Merging the datasets
merged_dealer <- merge(ratings,dealers_m, by="id.agro")
merged_dealer[merged_dealer=="n/a"] <- 0

head(as.numeric(merged_dealer$hh.maize.seed.1..q22)) 
head(as.numeric(merged_dealer$hh.maize.seed.2..q22)) 
head(as.numeric(merged_dealer$hh.maize.seed.3..q22)) 

merged_dealer$hh.maize.seed.1..q22 <- as.numeric(merged_dealer$hh.maize.seed.1..q22)
merged_dealer$hh.maize.seed.2..q22 <- as.numeric(merged_dealer$hh.maize.seed.2..q22)
merged_dealer$hh.maize.seed.3..q22 <- as.numeric(merged_dealer$hh.maize.seed.3..q22)

merged_dealer$hh.maize.seed.1..q22 <- as.numeric(as.character(merged_dealer$hh.maize.seed.1..q22))
merged_dealer$hh.maize.seed.2..q22 <- as.numeric(as.character(merged_dealer$hh.maize.seed.2..q22))
merged_dealer$hh.maize.seed.3..q22 <- as.numeric(as.character(merged_dealer$hh.maize.seed.3..q22))

merged_dealer$seed_sale <- merged_dealer$hh.maize.seed.1..q22 + merged_dealer$hh.maize.seed.2..q22 + merged_dealer$hh.maize.seed.3..q22 

head(as.numeric(merged_dealer$hh.maize.opv.1..q38 )) 
head(as.numeric(merged_dealer$hh.maize.opv.2..q38 )) 
head(as.numeric(merged_dealer$hh.maize.opv.3..q38 )) 

merged_dealer$hh.maize.opv.1..q38  <- as.numeric(merged_dealer$hh.maize.opv.1..q38 )
merged_dealer$hh.maize.opv.2..q38  <- as.numeric(merged_dealer$hh.maize.opv.2..q38 )
merged_dealer$hh.maize.opv.3..q38 <- as.numeric(merged_dealer$hh.maize.opv.3..q38 )

merged_dealer$hh.maize.opv.1..q38  <- as.numeric(as.character(merged_dealer$hh.maize.opv.1..q38 ))
merged_dealer$hh.maize.opv.2..q38 <- as.numeric(as.character(merged_dealer$hh.maize.opv.2..q38 ))
merged_dealer$hh.maize.opv.3..q38 <- as.numeric(as.character(merged_dealer$hh.maize.opv.3..q38 ))

merged_dealer$opv_sale <- merged_dealer$hh.maize.opv.1..q38 + merged_dealer$hh.maize.opv.2..q38 + merged_dealer$hh.maize.opv.3..q38 

#Correlations 
cor(merged_dealer$rating_overall, merged_dealer$hh.maize.q67) #extension or training to clients 
#-0.004892853 (very weak negative relationship)
cor(merged_dealer$rating_overall, merged_dealer$hh.maize.q68) #seed on credit 
#0.001917009 (very weak positive relationship)
cor(merged_dealer$rating_overall, merged_dealer$dealer_rating_overall)
#0.0520961 ( positive relationship)
cor(merged_dealer$rating_location, merged_dealer$hh.maize.q79) 
#-0.1232023 (very weak negative relationship)
cor(merged_dealer$rating_price, merged_dealer$hh.maize.q80) 
# -0.01468438 (very weak negative relationship)
cor(merged_dealer$rating_quality, merged_dealer$hh.maize.q81) 
#0.03858979 (very weak positive relationship)
cor(merged_dealer$rating_stock, merged_dealer$hh.maize.q82) 
#0.0237727 (very weak positive relationship)
cor(merged_dealer$rating_reputation, merged_dealer$hh.maize.q83) 
#0.04947769 (very weak positive relationship)


##RATINGS ---- PERCENTAGE OF FARMERS AND AGRO-INPUT DEALERS WHO RATE

##### OVERALL RATINGS #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_overall>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#23.83513
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_overall>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
# 15.54054
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_overall>4]))/sum(table(ratings$farmerID))*100
#22.09632
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$dealer_rating_overall>4]))/sum(table(dealers$id.agro))*100
#55.12821

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_overall<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#2.150538
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_overall<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#10.13514
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_overall<=2]))/sum(table(ratings$farmerID))*100
#3.824363
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$dealer_rating_overall<=2]))/sum(table(dealers$id.agro))*100
#0
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$dealer_rating_overall<=3]))/sum(table(dealers$id.agro))*100
#2.564103

##### RATING LOCATION #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_location>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
# 30.64516
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_location>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#35.13514
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_location>4]))/sum(table(ratings$farmerID))*100
#31.5864
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q79>4]))/sum(table(dealers$id.agro))*100
#47.4359

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_location<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#19.71326
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_location<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#17.56757
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_location<=2]))/sum(table(ratings$farmerID))*100
# 19.26346
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q79<=2]))/sum(table(dealers$id.agro))*100
#3.846154
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q79<=3]))/sum(table(dealers$id.agro))*100
#21.79487

##### RATING PRICE #####
#Customers: rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_price>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#9.677419
#NON-Customers : rating greater than 4 
sum(table(ratings$farmerID[ratings$rating_price>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#4.72973
#All: rating greater than 4
sum(table(ratings$farmerID[ratings$rating_price>4]))/sum(table(ratings$farmerID))*100
#8.640227
#rating greater than 4 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q80>4]))/sum(table(dealers$id.agro))*100
#34.61538

#Customers: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_price<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100
#28.1362
#NON-Customers : rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_price<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100
#36.48649
#All: rating less than equal to 2 
sum(table(ratings$farmerID[ratings$rating_price<=2]))/sum(table(ratings$farmerID))*100
#29.88669
#rating less than equal to 2 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q80<=2]))/sum(table(dealers$id.agro))*100
#1.282051
#rating less than equal to 3 by input dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q80<=3]))/sum(table(dealers$id.agro))*100
#28.20513

##### RATING QUALITY #####
#Customers: rating greater than 4 
round(sum(table(ratings$farmerID[ratings$rating_quality>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100)
# 23
#NON-Customers : rating greater than 4 
round(sum(table(ratings$farmerID[ratings$rating_quality>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100)
# 11
#All: rating greater than 4
round(sum(table(ratings$farmerID[ratings$rating_quality>4]))/sum(table(ratings$farmerID))*100)
#21
#rating greater than 4 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q81>4]))/sum(table(dealers$id.agro))*100)
#64

#Customers: rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_quality<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100)
#8
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_quality<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100)
#21
#All: rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_quality<=2]))/sum(table(ratings$farmerID))*100)
#11
#rating less than equal to 2 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q81<=2]))/sum(table(dealers$id.agro))*100)
#0
#rating less than equal to 3 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q81<=3]))/sum(table(dealers$id.agro))*100)
#6

##### RATING STOCK #####
#Customers: rating greater than 4 
round(sum(table(ratings$farmerID[ratings$rating_stock>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100)
#34
#NON-Customers : rating greater than 4 
round(sum(table(ratings$farmerID[ratings$rating_stock>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100)
#20
#All: rating greater than 4
round(sum(table(ratings$farmerID[ratings$rating_stock>4]))/sum(table(ratings$farmerID))*100)
#31
#rating greater than 4 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q82>4]))/sum(table(dealers$id.agro))*100)
#17

#Customers: rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_stock<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100)
#8
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_stock<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100)
#21
#All: rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_stock<=2]))/sum(table(ratings$farmerID))*100)
#11
#rating less than equal to 2 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q82<=2]))/sum(table(dealers$id.agro))*100)
# 13
#rating less than equal to 3 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q82<=3]))/sum(table(dealers$id.agro))*100)
# 58

##### RATING REPUTATION #####
#Customers: rating greater than 4 
round(sum(table(ratings$farmerID[ratings$rating_reputation>4 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100)
#27
#NON-Customers : rating greater than 4 
round(sum(table(ratings$farmerID[ratings$rating_reputation>4 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100)
# 18
#All: rating greater than 4
round(sum(table(ratings$farmerID[ratings$rating_reputation>4]))/sum(table(ratings$farmerID))*100)
#25
#rating greater than 4 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q83>4]))/sum(table(dealers$id.agro))*100)
#55

#Customers: rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_reputation<=2 & ratings$bought=="Yes"]))/sum(table(ratings$farmerID[ratings$bought=="Yes"]))*100)
#7
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_reputation<=2 & ratings$bought=="No"]))/sum(table(ratings$farmerID[ratings$bought=="No"]))*100)
# 13
#All: rating less than equal to 2 
round(sum(table(ratings$farmerID[ratings$rating_reputation<=2]))/sum(table(ratings$farmerID))*100)
#8
#rating less than equal to 2 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q83<=2]))/sum(table(dealers$id.agro))*100)
#4
#rating less than equal to 3 by input dealers 
round(sum(table(dealers$id.agro[dealers$hh.maize.q83<=3]))/sum(table(dealers$id.agro))*100)
# 9

####### NUMBER OF RATINGS RECEIVED #######
##number of agro input dealers who have been rated a certain number of times 
table(table(ratings$id.agro)>10) #23
table(table(ratings$id.agro)>20) #10
table(table(ratings$id.agro)>30) #3
table(table(ratings$id.agro)>40) #1

##frequency of the ratings for each agro input dealer
subset(data.frame(table(ratings$id.agro)), Freq > 20)
subset(data.frame(table(ratings$id.agro)), Freq > 30)
subset(data.frame(table(ratings$id.agro)), Freq > 40)

##subsetting based on the number of ratings received for each agro input dealer
gt20 <- ratings[ ratings$id.agro %in%  names(table(ratings$id.agro))[table(ratings$id.agro) >20] , ] #284 obs               
gt30 <- ratings[ ratings$id.agro %in%  names(table(ratings$id.agro))[table(ratings$id.agro) >30] , ] #122 obs
gt40 <- ratings[ ratings$id.agro %in%  names(table(ratings$id.agro))[table(ratings$id.agro) >40] , ] #49 obs

tapply(ratings$rating_overall,ratings$bought, mean )
wilcox.test(ratings$rating_quality~ratings$bought) #distributions differ

wiltest<-wilcox.test(ratings$rating_overall,dealers$dealer_rating_overall)
##Null: Distributions are same and have same median
##Looking at p-value, we may conclude that the medians of the 2 distributions differ (<0.05)
##W indicates the number of times the rating from a farmer is different to rating by the dealers


#OVERALL RATING
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_overall, na.rm=T) #3.586402
#overall mean score with interaction
mean(ratings$rating_overall[ratings$bought=="Yes"], na.rm=T) #3.66129
#overall mean score without interaction
mean(ratings$rating_overall[ratings$bought=="No"], na.rm=T) #3.304054
 
#overall rating provided by dealer to themselves 
mean(dealers$dealer_rating_overall, na.rm=T)
#4.130769 (higher than farmers)

##input dealers who provide extension/training to clients (dealers$hh.maize.q67) and seed on credit (dealers$hh.maize.q68)
#1 = No, 2 = To some, 3 = to everyone who wants 
#rating received from farmers 
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67 %in% c(1) & merged_dealer$hh.maize.q68 %in% c(1)], na.rm=T) #3.673913
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67 %in% c(3) & merged_dealer$hh.maize.q68 %in% c(3)], na.rm=T)   #3.472
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67 %in% c(2,3) & merged_dealer$hh.maize.q68 %in% c(2,3)], na.rm=T)  #3.610185
#farmers rate the input dealers who provide extension or training to clients and seed on credit lower on average
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67 %in% c(1)], na.rm=T)
# 3.559292
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67 %in% c(3)], na.rm=T)
# 3.575492
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67 %in% c(2,3)], na.rm=T) #3.591568
#farmers rate higher if the input dealer provides extension/credit 
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q68 %in% c(1)], na.rm=T)
#3.571014
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q68 %in% c(3)], na.rm=T)
# 3.521951
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q68 %in% c(2,3)], na.rm=T) # 3.592786
#farmers rate higher if the input dealer provides seed on credit 

#ratings by input dealers 
mean(dealers$dealer_rating_overall[dealers$hh.maize.q67 %in% c(1) & dealers$hh.maize.q68 %in% c(1)], na.rm=T) #3.95
mean(dealers$dealer_rating_overall[dealers$hh.maize.q67 %in% c(2,3) & dealers$hh.maize.q68 %in% c(2,3)], na.rm=T) #4.164
##ratings by input dealers are higher and they rate themselves higher if they provide training and seed on credit 

##RATING LOCATION
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_location, na.rm=T) #3.648725
#overall mean score with interaction
mean(ratings$rating_location[ratings$bought=="Yes"], na.rm=T) #3.650538
#overall mean score without interaction
mean(ratings$rating_location[ratings$bought=="No"], na.rm=T) #3.641892

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q79, na.rm=T)
#4.217949 (higher than farmers)

##RATING PRICE
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_price, na.rm=T) # 2.990085
#overall mean score with interaction
mean(ratings$rating_price[ratings$bought=="Yes"], na.rm=T) #3.046595
#overall mean score without interaction
mean(ratings$rating_price[ratings$bought=="No"], na.rm=T) #2.777027

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q80, na.rm=T)
# 4.051282 (higher than farmers)

##RATING QUALITY
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_quality, na.rm=T) #3.620397
#overall mean score with interaction
mean(ratings$rating_quality[ratings$bought=="Yes"], na.rm=T) #3.752688
#overall mean score without interaction
mean(ratings$rating_quality[ratings$bought=="No"], na.rm=T) #3.121622

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q81, na.rm=T)
# 4.576923 (higher than farmers)

##RATING STOCK
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_stock, na.rm=T) #3.84136
#overall mean score with interaction
mean(ratings$rating_stock[ratings$bought=="Yes"], na.rm=T) #3.953405
#overall mean score without interaction
mean(ratings$rating_stock[ratings$bought=="No"], na.rm=T) #3.418919

#overall rating provided by dealer to themselves 
mean(dealers$hh.maize.q82, na.rm=T)
#3.410256 (lower than farmers)

##RATING REPUTATION
#Overall mean score received by input dealers from farmers 
mean(ratings$rating_reputation, na.rm=T) # 3.831445
#overall mean score with interaction
mean(ratings$rating_reputation[ratings$bought=="Yes"], na.rm=T) #3.903226
#overall mean score without interaction
mean(ratings$rating_reputation[ratings$bought=="No"], na.rm=T) #3.560811

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
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="**", y_position = 4.1, tip_length = 0.03)
dev.off()

#Tests in the graph
### PRICE RATING
wilcox.test(ratings$rating_price[ratings$bought=="No"],dealers$hh.maize.q80)
#reject null, distributions are different
wilcox.test(ratings$rating_price[ratings$bought=="Yes"],dealers$hh.maize.q80)
#reject null, distributions are different
wilcox.test(ratings$rating_price[ratings$bought=="Yes"],ratings$rating_price[ratings$bought=="No"])
#reject null, distributions are different at 5%

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
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="Non-Customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="Customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_dealer), col=colfunc(5), main="Dealer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()


############################################





################### TRADERS ########################

### Prepping data for ratings 
trans <- c("hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j","hh.maize.trader1.q102k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j","hh.maize.trader2.q103k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j","hh.maize.trader3.q104k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.trader1","hh.maize.q25","hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j","hh.maize.trader1.q102k")],"Yes")
names(stack1) <- c("farmerID","id.trader","farmer_gender","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")
stack2 <- cbind(farmers[c("ID","id.trader2","hh.maize.q25","hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j","hh.maize.trader2.q103k","hh.maize.trader2.q103l")])
names(stack2) <- c("farmerID","id.trader","farmer_gender","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")
stack3 <- cbind(farmers[c("ID","id.trader3","hh.maize.q25","hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j","hh.maize.trader3.q104k","hh.maize.trader3.q104l")])
names(stack3) <- c("farmerID","id.trader","farmer_gender","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")

ratings_trader <-rbind(stack1,stack2,stack3)
ratings_trader[c("id.trader","sold")] <- lapply(ratings_trader[c("id.trader","sold")], function(x) as.factor(as.character(x)) )

##subsetting and cleaning missing values 
ratings_trader <- subset(ratings_trader,!is.na(rating_reputation))
ratings_trader <- subset(ratings_trader[!apply(ratings_trader == "", 1, any),])
ratings_trader <- subset(ratings_trader[!apply(ratings_trader == ".", 1, any),])
table(ratings_trader=="")
ratings_trader <- ratings_trader[c(!duplicated(ratings_trader[,1:2])),] #removing duplicates 

### simple average of the ratings (index) --- from farmers
ratings_trader$rating_overall <- rowSums(ratings_trader[c("rating_location","rating_price","rating_quality","rating_honesty","rating_reputation")])/5
summary(ratings_trader$rating_overall)

###Getting traders' data 
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

###Index for overall rating from traders
traders$trader_rating_overall <- rowSums(traders[c("hh.maize.q40a","hh.maize.q40b","hh.maize.q40c","hh.maize.q40d","hh.maize.q40e")])/5
summary(traders$trader_rating_overall)

#subset for merging 
traders_m <- subset(traders, select = c('id.trader' , 'hh.maize.q7','hh.maize.q40a','hh.maize.q40b','hh.maize.q40c','hh.maize.q40d','hh.maize.q40e','trader_rating_overall'))

#Merging the datasets
merged_trader <- merge(ratings_trader,traders_m, by="id.trader")


##RATINGS ---- PERCENTAGE OF FARMERS AND TRADERS WHO RATE

##### OVERALL RATINGS #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 32.53397
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#20.08032
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall>4]))/sum(table(ratings_trader$farmerID))*100
#30.46667
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$trader_rating_overall>4]))/sum(table(traders$trader_rating_overall))*100
#65.9824

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#3.277378
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#10.04016
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_overall<=2]))/sum(table(ratings_trader$farmerID))*100
#4.4
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$trader_rating_overall<=2]))/sum(table(traders$id.trader))*100
#0
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$trader_rating_overall<=3]))/sum(table(traders$id.trader))*100
#2.346041

##### RATING LOCATION #####
#Customers: rating greater than 4 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_location>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100)
#45
#NON-Customers : rating greater than 4 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_location>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100)
#39
#All: rating greater than 4
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_location>4]))/sum(table(ratings_trader$farmerID))*100)
# 44
#rating greater than 4 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40a>4]))/sum(table(traders$trader_rating_overall))*100)
# 38

#Customers: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_location<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100)
#7
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_location<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100)
# 13
#All: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_location<=2]))/sum(table(ratings_trader$farmerID))*100)
#8
#rating less than equal to 2 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40a<=2]))/sum(table(traders$id.trader))*100)
#3
#rating less than equal to 3 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40a<=3]))/sum(table(traders$id.trader))*100)
#23

##### RATING PRICE #####
#Customers: rating greater than 4 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_price>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100)
#9
#NON-Customers : rating greater than 4 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_price>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100)
#11
#All: rating greater than 4
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_price>4]))/sum(table(ratings_trader$farmerID))*100)
# 10
#rating greater than 4 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40b>4]))/sum(table(traders$trader_rating_overall))*100)
# 26

#Customers: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_price<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100)
# 25
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_price<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100)
# 33
#All: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_price<=2]))/sum(table(ratings_trader$farmerID))*100)
#27
#rating less than equal to 2 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40b<=2]))/sum(table(traders$id.trader))*100)
#3
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40b<=3]))/sum(table(traders$id.trader))*100
#31.08504

##### RATING QUALITY #####
#Customers: rating greater than 4 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_quality>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100)
#18
#NON-Customers : rating greater than 4 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_quality>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100)
#12
#All: rating greater than 4
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_quality>4]))/sum(table(ratings_trader$farmerID))*100)
#  17
#rating greater than 4 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40c>4]))/sum(table(traders$trader_rating_overall))*100)
# 49

#Customers: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_quality<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100)
# 12
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_quality<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100)
#20
#All: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_quality<=2]))/sum(table(ratings_trader$farmerID))*100)
#13
#rating less than equal to 2 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40c<=2]))/sum(table(traders$id.trader))*100)
#3
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40c<=3]))/sum(table(traders$id.trader))*100
#12.31672

##### RATING HONESTY #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
#32.45404
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
# 18.4739
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty>4]))/sum(table(ratings_trader$farmerID))*100
#30.13333
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$hh.maize.q40d>4]))/sum(table(traders$trader_rating_overall))*100
# 73.02053

#Customers: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 13.02958
#NON-Customers : rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
#21.68675
#All: rating less than equal to 2 
sum(table(ratings_trader$farmerID[ratings_trader$rating_honesty<=2]))/sum(table(ratings_trader$farmerID))*100
# 14.46667
#rating less than equal to 2 by traders
sum(table(traders$id.trader[traders$hh.maize.q40d<=2]))/sum(table(traders$id.trader))*100
#0.8797654
#rating less than equal to 3 by traders
sum(table(traders$id.trader[traders$hh.maize.q40d<=3]))/sum(table(traders$id.trader))*100
# 5.571848

##### RATING REPUTATION #####
#Customers: rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation>4 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100
# 30.3757
#NON-Customers : rating greater than 4 
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation>4 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100
# 19.67871
#All: rating greater than 4
sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation>4]))/sum(table(ratings_trader$farmerID))*100
#28.6
#rating greater than 4 by traders
sum(table(traders$id.trader[traders$hh.maize.q40e>4]))/sum(table(traders$trader_rating_overall))*100
# 59.23754

#Customers: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation<=2 & ratings_trader$sold=="Yes"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="Yes"]))*100)
#8
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation<=2 & ratings_trader$sold=="No"]))/sum(table(ratings_trader$farmerID[ratings_trader$sold=="No"]))*100)
#  17
#All: rating less than equal to 2 
round(sum(table(ratings_trader$farmerID[ratings_trader$rating_reputation<=2]))/sum(table(ratings_trader$farmerID))*100)
# 10
#rating less than equal to 2 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40e<=2]))/sum(table(traders$id.trader))*100)
#2
#rating less than equal to 3 by traders
round(sum(table(traders$id.trader[traders$hh.maize.q40e<=3]))/sum(table(traders$id.trader))*100)
# 12

##NUMBER OF RATINGS RECEIVED 
##number of traders who have these ratings 
table(table(ratings_trader$id.trader)>10) #43
table(table(ratings_trader$id.trader)>15) #14
table(table(ratings_trader$id.trader)>20) #3

##frequency of the ratings for each trader
subset(data.frame(table(ratings_trader$id.trader)), Freq > 10)
subset(data.frame(table(ratings_trader$id.trader)), Freq > 15)
subset(data.frame(table(ratings_trader$id.trader)), Freq > 20)


##subsetting based on the number of ratings for each trader
gt10_trad <- ratings_trader[ ratings_trader$id.trader %in%  names(table(ratings_trader$id.trader))[table(ratings_trader$id.trader) >10] , ]  #641 obs              
gt15_trad <- ratings_trader[ ratings_trader$id.trader %in%  names(table(ratings_trader$id.trader))[table(ratings_trader$id.trader) >15] , ]  #267 obs             
gt20_trad <- ratings_trader[ ratings_trader$id.trader %in%  names(table(ratings_trader$id.trader))[table(ratings_trader$id.trader) >20] , ]   #73 obs             

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
mean(ratings_trader$rating_overall, na.rm=T) #3.646667
#overall mean score with interaction
mean(ratings_trader$rating_overall[ratings_trader$sold=="Yes"], na.rm=T) #3.701519
#overall mean score without interaction
mean(ratings_trader$rating_overall[ratings_trader$sold=="No"], na.rm=T) #3.371084

#overall rating provided by traders to themselves 
mean(traders$trader_rating_overall, na.rm=T)
#4.291496 (higher than farmers)

##RATING LOCATION
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_location, na.rm=T) #4.087333
#overall mean score with interaction
mean(ratings_trader$rating_location[ratings_trader$sold=="Yes"], na.rm=T) #4.1255
#overall mean score without interaction
mean(ratings_trader$rating_location[ratings_trader$sold=="No"], na.rm=T) #3.895582

#overall rating provided by dealer to themselves 
mean(traders$hh.maize.q40a, na.rm=T)
#4.108504 (higher than farmers)

##RATING PRICE
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_price, na.rm=T) #3.061333
#overall mean score with interaction
mean(ratings_trader$rating_price[ratings_trader$sold=="Yes"], na.rm=T) #3.096723
#overall mean score without interaction
mean(ratings_trader$rating_price[ratings_trader$sold=="No"], na.rm=T) #2.883534

#overall rating provided by traders to themselves 
mean(traders$hh.maize.q40b, na.rm=T)
# 3.906158

##RATING QUALITY
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_quality, na.rm=T) #3.520667
#overall mean score with interaction
mean(ratings_trader$rating_quality[ratings_trader$sold=="Yes"], na.rm=T) #3.577138
#overall mean score without interaction
mean(ratings_trader$rating_quality[ratings_trader$sold=="No"], na.rm=T) #3.236948

#overall rating provided by traders to themselves 
mean(traders$hh.maize.q40c, na.rm=T)
# 4.334311 (higher than farmers)

##RATING HONESTY
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_honesty, na.rm=T) #3.744
#overall mean score with interaction
mean(ratings_trader$rating_honesty[ratings_trader$sold=="Yes"], na.rm=T) #3.821743
#overall mean score without interaction
mean(ratings_trader$rating_honesty[ratings_trader$sold=="No"], na.rm=T) #3.353414

#overall rating provided by traders to themselves 
mean(traders$hh.maize.q40d, na.rm=T)
#4.662757 (higher than farmers)

##RATING REPUTATION
#Overall mean score received by traders from farmers 
mean(ratings_trader$rating_reputation, na.rm=T) #3.82
#overall mean score with interaction
mean(ratings_trader$rating_reputation[ratings_trader$sold=="Yes"], na.rm=T) #3.886491
#overall mean score without interaction
mean(ratings_trader$rating_reputation[ratings_trader$sold=="No"], na.rm=T) #3.485944

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
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="Non-Customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="Customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_trader), col=colfunc(5), main="Trader", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()

###############################################################################


################### MILLERS ########################

###Prepping data for ratings 
trans <- c("hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.miller1","hh.maize.q25","hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k")],"Yes")
names(stack1) <- c("farmerID","id.miller","farmer_gender","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")
stack2 <- cbind(farmers[c("ID", "id.miller2","hh.maize.q25","hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k","hh.maize.miller2.q99l")])
names(stack2) <- c("farmerID","id.miller","farmer_gender","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")
stack3 <- cbind(farmers[c("ID","id.miller3","hh.maize.q25","hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k","hh.maize.miller3.q100l")])
names(stack3) <- c("farmerID","id.miller","farmer_gender","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")

ratings_mill <-rbind(stack1,stack2,stack3)
ratings_mill[c("id.miller","used")] <- lapply(ratings_mill[c("id.miller","used")], function(x) as.factor(as.character(x)) )

##subsetting and cleaning missing values 
ratings_mill <- subset(ratings_mill, !is.na(rating_reputation) )
ratings_mill <- subset(ratings_mill[!apply(ratings_mill == "", 1, any),])
ratings_mill <- subset(ratings_mill[!apply(ratings_mill == ".", 1, any),])
table(ratings_mill=="")
ratings_mill <- ratings_mill[c(!duplicated(ratings_mill[,1:2])),] #removing duplicates 

### simple average of the ratings (index) --- from farmers
ratings_mill$rating_overall <- rowSums(ratings_mill[c("rating_location","rating_price","rating_quality","rating_service","rating_reputation")])/5
summary(ratings_mill$rating_overall)

###Getting millers' data 
millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"))

###Index for overall rating from millers
millers$miller_rating_overall <- rowSums(millers[c("hh.maize.q36","hh.maize.q37","hh.maize.q38","hh.maize.q39","hh.maize.q40")])/5
summary(millers$miller_rating_overall)

#subset for merging 
millers_m <- subset(millers, select = c('id.miller' , 'hh.maize.q7','hh.maize.q36','hh.maize.q37','hh.maize.q38','hh.maize.q39','hh.maize.q40','miller_rating_overall','hh.maize.q24', 'hh.maize.q26', 'hh.maize.q27', 'hh.maize.q23', 'hh.maize.q23a'))

#Merging the datasets
merged_miller <- merge(ratings_mill,millers_m, by="id.miller")

##RATINGS ---- PERCENTAGE OF FARMERS AND MILLERS WHO RATE

##### OVERALL RATINGS #####
#Customers: rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_overall>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#24
#NON-Customers : rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_overall>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
#10
#All: rating greater than 4
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_overall>4]))/sum(table(ratings_mill$farmerID))*100)
# 23
#rating greater than 4 by millers
round(sum(table(millers$id.miller[millers$miller_rating_overall>4]))/sum(table(millers$miller_rating_overall))*100)
#57

#Customers: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_overall<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#3
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_overall<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
#10
#All: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_overall<=2]))/sum(table(ratings_mill$farmerID))*100)
#4
#rating less than equal to 2 by millers
round(sum(table(millers$id.miller[millers$miller_rating_overall<=2]))/sum(table(millers$miller_rating_overall))*100)
#0
#rating less than equal to 3 by millers
round(sum(table(millers$id.miller[millers$miller_rating_overall<=3]))/sum(table(millers$miller_rating_overall))*100)
#3

##### RATING LOCATION #####
#Customers: rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_location>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#38
#NON-Customers : rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_location>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 21
#All: rating greater than 4
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_location>4]))/sum(table(ratings_mill$farmerID))*100)
# 37
#rating greater than 4 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q36>4]))/sum(table(millers$miller_rating_overall))*100)
#36

#Customers: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_location<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#15
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_location<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 20
#All: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_location<=2]))/sum(table(ratings_mill$farmerID))*100)
#15
#rating less than equal to 2 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q36<=2]))/sum(table(millers$miller_rating_overall))*100)
#7
#rating less than equal to 3 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q36<=3]))/sum(table(millers$miller_rating_overall))*100)
# 29

##### RATING PRICE #####
#Customers: rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_price>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#11
#NON-Customers : rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_price>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 7
#All: rating greater than 4
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_price>4]))/sum(table(ratings_mill$farmerID))*100)
#11
#rating greater than 4 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q37>4]))/sum(table(millers$miller_rating_overall))*100)
#29

#Customers: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_price<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#33
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_price<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 25
#All: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_price<=2]))/sum(table(ratings_mill$farmerID))*100)
# 32
#rating less than equal to 2 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q37<=2]))/sum(table(millers$miller_rating_overall))*100)
# 6
#rating less than equal to 3 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q37<=3]))/sum(table(millers$miller_rating_overall))*100)
#  37

##### RATING QUALITY #####
#Customers: rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_quality>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#21
#NON-Customers : rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_quality>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 11
#All: rating greater than 4
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_quality>4]))/sum(table(ratings_mill$farmerID))*100)
# 20
#rating greater than 4 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q38>4]))/sum(table(millers$miller_rating_overall))*100)
# 41

#Customers: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_quality<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#23
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_quality<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 36
#All: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_quality<=2]))/sum(table(ratings_mill$farmerID))*100)
# 23
#rating less than equal to 2 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q38<=2]))/sum(table(millers$miller_rating_overall))*100)
#  4
#rating less than equal to 3 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q38<=3]))/sum(table(millers$miller_rating_overall))*100)
#21

##### RATING SERVICE #####
#Customers: rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_service>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#22
#NON-Customers : rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_service>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
#  11
#All: rating greater than 4
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_service>4]))/sum(table(ratings_mill$farmerID))*100)
# 21
#rating greater than 4 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q39>4]))/sum(table(millers$miller_rating_overall))*100)
#55

#Customers: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_service<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
# 11
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_service<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 29
#All: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_service<=2]))/sum(table(ratings_mill$farmerID))*100)
# 12
#rating less than equal to 2 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q39<=2]))/sum(table(millers$miller_rating_overall))*100)
#3
#rating less than equal to 3 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q39<=3]))/sum(table(millers$miller_rating_overall))*100)
# 13

##### RATING REPUTATION #####
#Customers: rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation>4 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#29
#NON-Customers : rating greater than 4 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation>4 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
# 15
#All: rating greater than 4
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation>4]))/sum(table(ratings_mill$farmerID))*100)
# 29
#rating greater than 4 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q40>4]))/sum(table(millers$miller_rating_overall))*100)
#60

#Customers: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation<=2 & ratings_mill$used=="Yes"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="Yes"]))*100)
#10
#NON-Customers : rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation<=2 & ratings_mill$used=="No"]))/sum(table(ratings_mill$farmerID[ratings_mill$used=="No"]))*100)
#18
#All: rating less than equal to 2 
round(sum(table(ratings_mill$farmerID[ratings_mill$rating_reputation<=2]))/sum(table(ratings_mill$farmerID))*100)
# 10
#rating less than equal to 2 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q40<=2]))/sum(table(millers$miller_rating_overall))*100)
#1
#rating less than equal to 3 by millers
round(sum(table(millers$id.miller[millers$hh.maize.q40<=3]))/sum(table(millers$miller_rating_overall))*100)
#9

##NUMBER OF RATINGS RECEIVED 
##number of millers who have these ratings 
table(table(ratings_mill$id.miller)>10) #57
table(table(ratings_mill$id.miller)>15) #43
table(table(ratings_mill$id.miller)>20) #28

##frequency of the ratings for each miller
subset(data.frame(table(ratings_mill$id.miller)), Freq > 10)
subset(data.frame(table(ratings_mill$id.miller)), Freq > 15)
subset(data.frame(table(ratings_mill$id.miller)), Freq > 20)

##subsetting based on the number of ratings for each miller
gt10_mill <- ratings_mill[ ratings_mill$id.miller %in%  names(table(ratings_mill$id.miller))[table(ratings_mill$id.miller) >10] , ]   #1346 obs
gt15_mill <- ratings_mill[ ratings_mill$id.miller %in%  names(table(ratings_mill$id.miller))[table(ratings_mill$id.miller) >15] , ]   #1163 obs
gt20_mill <- ratings_mill[ ratings_mill$id.miller %in%  names(table(ratings_mill$id.miller))[table(ratings_mill$id.miller) >20] , ]   #900 obs

### Wilcox test
tapply(ratings_mill$rating_overall,ratings_mill$used, mean )
wilcox.test(ratings_mill$rating_overall,millers$miller_rating_overall)
##Null: Distributions are same and have same median
##Looking at p-value, we may conclude that the medians of the 2 distributions differ
##W indicates the number of times the rating from a farmer is different to rating by the millers


#OVERALL RATING
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_overall, na.rm=T) #3.531656
#overall mean score with interaction
mean(ratings_mill$rating_overall[ratings_mill$used=="Yes"], na.rm=T) # 3.554855
#overall mean score without interaction
mean(ratings_mill$rating_overall[ratings_mill$used=="No"], na.rm=T) #3.120879

#overall rating provided by millers to themselves 
mean(millers$miller_rating_overall, na.rm=T)
#4.177011 (higher than farmers)

##RATING LOCATION
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_location, na.rm=T) #3.819778
#overall mean score with interaction
mean(ratings_mill$rating_location[ratings_mill$used=="Yes"], na.rm=T) # 3.839827
#overall mean score without interaction
mean(ratings_mill$rating_location[ratings_mill$used=="No"], na.rm=T) # 3.472527

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q36, na.rm=T)
#3.988506 (higher than farmers)

##RATING PRICE
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_price, na.rm=T) #3.011703
#overall mean score with interaction
mean(ratings_mill$rating_price[ratings_mill$used=="Yes"], na.rm=T) # 3.014224
#overall mean score without interaction
mean(ratings_mill$rating_price[ratings_mill$used=="No"], na.rm=T) # 2.967033

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q37, na.rm=T)
#3.844828 (higher than farmers)

##RATING QUALITY
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_quality, na.rm=T) #3.392627
#overall mean score with interaction
mean(ratings_mill$rating_quality[ratings_mill$used=="Yes"], na.rm=T) # 3.425479
#overall mean score without interaction
mean(ratings_mill$rating_quality[ratings_mill$used=="No"], na.rm=T) # 2.824176

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q38, na.rm=T)
#4.16092 (higher than farmers)

##RATING SERVICE
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_service, na.rm=T) #3.613224
#overall mean score with interaction
mean(ratings_mill$rating_service[ratings_mill$used=="Yes"], na.rm=T) #3.647495
#overall mean score without interaction
mean(ratings_mill$rating_service[ratings_mill$used=="No"], na.rm=T) # 2.989011

#overall rating provided by millers to themselves 
mean(millers$hh.maize.q39, na.rm=T)
#4.390805 (higher than farmers)

##RATING REPUTATION
#Overall mean score received by millers from farmers 
mean(ratings_mill$rating_reputation, na.rm=T) #3.820948
#overall mean score with interaction
mean(ratings_mill$rating_reputation[ratings_mill$used=="Yes"], na.rm=T) #3.847248
#overall mean score without interaction
mean(ratings_mill$rating_reputation[ratings_mill$used=="No"], na.rm=T) # 3.351648

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
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="Non-Customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="Customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_miller), col=colfunc(5), main="Miller", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()

##########################################################################################################################

######################################################################################################################################

##### ANALYSIS OF RATINGS BASED ON GENDER

# DEALERS

#  Percentage of females in the datasets
sum(table(ratings$farmerID[ratings$farmer_gender=="Female"]))/sum(table(ratings$farmerID))*100
#35.55241 percent are female farmers 
sum(table(dealers$id.agro[dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro))*100
#29.48718 percent are female dealers 
sum(table(dealers$id.agro[dealers$hh.maize.q7=="Female"])) #23 female dealers 
sum(table(dealers$id.agro)) #total 78 dealers 

########## OVERALL RATING ###########

##### ratings from dealers

mean(dealers$dealer_rating_overall[dealers$hh.maize.q7=="Male"])   #4.170909
mean(dealers$dealer_rating_overall[dealers$hh.maize.q7=="Female"]) #4.034783
#mean overall rating higher for males

sum(table(dealers$id.agro[dealers$dealer_rating_overall>4 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#56.36364, Male 
sum(table(dealers$id.agro[dealers$dealer_rating_overall>4 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#52.17391, Female 
#female dealers less likely to rate themselves more than 4 
sum(table(dealers$id.agro[dealers$dealer_rating_overall<=3 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#1.818182, Male 
sum(table(dealers$id.agro[dealers$dealer_rating_overall<=3 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
# 4.347826, Female 
#female dealers more likely to rate themselves less than equal to 3 

#####ratings from farmers for dealers 

mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Male"]) #3.588716
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Female"]) #3.580208
#ratings for male dealers little bit higher

mean(merged_dealer$rating_overall[merged_dealer$farmer_gender=="Male"]) #3.577143
mean(merged_dealer$rating_overall[merged_dealer$farmer_gender=="Female"]) #3.603187
#ratings by female farmers are higher

#customers
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall<=2 & merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Male"]))*100
# 2.917772, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall<=2 & merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Female"]))*100
#  0.5524862, female
#Females who are customers less likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Male"]))*100
# 22.28117, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="Yes" & merged_dealer$farmer_gender=="Female"]))*100
#27.07182, female
#Females who are customers more likely to rate more than 4 

#non customers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall<=2 & merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Male"]))*100
# 10.25641, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall<=2 & merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Female"]))*100
#10, female
#Females who are non-customers less likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Male"]))*100
# 11.53846, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$bought=="No" & merged_dealer$farmer_gender=="Female"]))*100
#20, female
#Females who are non-customers more likely to rate more than 4 

#customers and non-customers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall<=2 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
#  4.175824, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall<=2 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
#3.187251, female
#Female farmers less likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
# 20.43956, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
#25.0996, female
#Female farmers more likely to rate more than 4 

#Gender of rater and subject
#Overall female farmers give higher scores compared to male farmers 
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female"])
# 3.609231
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female"])
#3.601075
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male"])
# 3.581707
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male"])
#3.565354
#mean of the overall ratings higher when the gender of the rater and the dealer is the same 

#CUSTOMERS
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female" & merged_dealer$bought=="Yes"])
# 3.640816
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female" & merged_dealer$bought=="Yes"])
#3.721212
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male" & merged_dealer$bought=="Yes"])
# 3.644118
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male" & merged_dealer$bought=="Yes"])
#3.64
##Male dealers get higher score irrespective of the gender of the farmer 

#NON-CUSTOMERS 
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female" & merged_dealer$bought=="No"])
# 3.5125
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female" & merged_dealer$bought=="No"])
#3.307407
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male" & merged_dealer$bought=="No"])
# 3.278571
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male" & merged_dealer$bought=="No"])
#3.209091
##Male dealers get higher score, but female farmers rate female dealers better

### Rating >4
#female farmers rating female dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))*100
#18.46154
#male farmers rating male dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))*100
#20.73171
#female farmers rating male dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))*100
# 27.41935
#male farmers rating female dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_overall>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))*100
#19.68504
###Overall male dealers are rated more often more than 4 
##### More percentage of male raters give >4 rating when the dealer is male 
##### More percentage of female raters give >4 rating when the dealer is male 
#Maybe male dealers are performing well

################################################################################################

########## LOCATION RATING ###########

##### ratings from dealers

mean(dealers$hh.maize.q79[dealers$hh.maize.q7=="Male"])   #4.327273
mean(dealers$hh.maize.q79[dealers$hh.maize.q7=="Female"]) #3.956522
#rating higher for males 

sum(table(dealers$id.agro[dealers$hh.maize.q79>4 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
# 50.90909, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q79>4 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#39.13043, Female 
#female dealers less likely to rate themselves more than 4 
sum(table(dealers$id.agro[dealers$hh.maize.q79<=2 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#1.818182, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q79<=2 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#8.695652, Female 
#female dealers more likely to rate themselves less than equal to 2 

#####ratings from farmers for dealers 

mean(merged_dealer$rating_location[merged_dealer$hh.maize.q7=="Male"]) #3.723735
mean(merged_dealer$rating_location[merged_dealer$hh.maize.q7=="Female"]) #3.447917
#ratings for male dealers higher

mean(merged_dealer$rating_location[merged_dealer$farmer_gender=="Male"]) # 3.534066
mean(merged_dealer$rating_location[merged_dealer$farmer_gender=="Female"]) #3.856574
#ratings by female farmers are higher

#customers and non-customers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_location<=2 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
#   22.85714, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_location<=2 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
#12.749, female
#Female farmers less likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_location>4 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
# 29.01099, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_location>4 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 36.25498, female
#Female farmers more likely to rate more than 4 

#Gender of rater and subject
#Overall female farmers give higher scores compared to male farmers 
mean(merged_dealer$rating_location[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female"])
#3.676923
mean(merged_dealer$rating_location[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female"])
#3.919355
mean(merged_dealer$rating_location[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male"])
#3.612805
mean(merged_dealer$rating_location[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male"])
#3.330709
#male dealers get better scores from both male and female farmers 

### Rating >4
#female farmers rating female dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_location>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))*100
#27.69231
#male farmers rating male dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_location>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))*100
#32.62195
#female farmers rating male dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_location>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))*100
#39.24731
#male farmers rating female dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_location>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))*100
#19.68504
###Overall male dealers are rated more often more than 4 
##### More percentage of male raters give >4 rating when the dealer is male 
##### More percentage of female raters give >4 rating when the dealer is male 

#######################################################################################################

########## PRICE RATING ###########

##### ratings from dealers

mean(dealers$hh.maize.q80[dealers$hh.maize.q7=="Male"])   # 4.036364
mean(dealers$hh.maize.q80[dealers$hh.maize.q7=="Female"]) # 4.086957
#rating higher for females 

sum(table(dealers$id.agro[dealers$hh.maize.q80>4 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#  34.54545, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q80>4 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#34.78261, Female 
#female dealers more likely to rate themselves more than 4 
sum(table(dealers$id.agro[dealers$hh.maize.q80<=3 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#29.09091, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q80<=3 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
# 26.08696, Female 
#female dealers less likely to rate themselves less than equal to 3

#####ratings from farmers for dealers 

mean(merged_dealer$rating_price[merged_dealer$hh.maize.q7=="Male"]) #2.994163
mean(merged_dealer$rating_price[merged_dealer$hh.maize.q7=="Female"]) #2.979167
#ratings for male dealers higher

mean(merged_dealer$rating_price[merged_dealer$farmer_gender=="Male"]) #2.945055
mean(merged_dealer$rating_price[merged_dealer$farmer_gender=="Female"]) #3.071713
#ratings by female farmers are higher

#customers and non-customers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_price<=2 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
# 31.20879, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_price<=2 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 27.49004, female
#Female farmers less likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_price>4 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
# 8.791209, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_price>4 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 8.366534, female
#Female farmers less likely to rate more than 4 

#Gender of rater and subject
#Overall female farmers give higher scores compared to male farmers 
mean(merged_dealer$rating_price[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female"])
#3.107692
mean(merged_dealer$rating_price[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female"])
#3.05914
mean(merged_dealer$rating_price[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male"])
#2.957317
mean(merged_dealer$rating_price[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male"])
#2.913386
#higher scores if gender of rater and subject same 

### Rating >4
#female farmers rating female dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_price>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))*100
# 6.153846
#male farmers rating male dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_price>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))*100
#  9.146341
#female farmers rating male dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_price>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))*100
#    9.139785
#male farmers rating female dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_price>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))*100
#   7.874016
###Overall male dealers are rated more often more than 4 by both genders 

####################################################################################################################

########## QUALITY RATING ###########

##### ratings from dealers

mean(dealers$hh.maize.q81[dealers$hh.maize.q7=="Male"])   # 4.563636
mean(dealers$hh.maize.q81[dealers$hh.maize.q7=="Female"]) # 4.608696
#rating higher for females 

sum(table(dealers$id.agro[dealers$hh.maize.q81>4 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
# 63.63636, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q81>4 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#  65.21739, Female 
#female dealers more likely to rate themselves more than 4 
sum(table(dealers$id.agro[dealers$hh.maize.q81<=3 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#7.272727, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q81<=3 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#  4.347826, Female 
#female dealers less likely to rate themselves less than equal to 3

#####ratings from farmers for dealers 

mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q7=="Male"]) #3.61284
mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q7=="Female"]) #3.640625
#ratings for female dealers higher

mean(merged_dealer$rating_quality[merged_dealer$farmer_gender=="Male"]) #3.676923
mean(merged_dealer$rating_quality[merged_dealer$farmer_gender=="Female"]) #3.517928
#ratings by male farmers are higher

#customers and non-customers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality<=2 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
# 9.89011, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality<=2 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 12.3506, female
#Female farmers more likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality>4 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
#  23.51648, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality>4 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 16.33466, female
#Female farmers less likely to rate more than 4 

#Gender of rater and subject
mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female"])
#3.646154
mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female"])
#  3.473118
mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male"])
#3.692073
mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male"])
# 3.637795
#higher scores if gender of rater and subject same 

### Rating >4
#female farmers rating female dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))*100
#16.92308
#male farmers rating male dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))*100
#25.30488
#female farmers rating male dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))*100
#16.12903
#male farmers rating female dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_quality>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))*100
# 18.89764
### gender of rater and subject same, more likely to be rated >4 

######################################################################################

########## STOCK RATING ###########

##### ratings from dealers

mean(dealers$hh.maize.q82[dealers$hh.maize.q7=="Male"])   #  3.490909
mean(dealers$hh.maize.q82[dealers$hh.maize.q7=="Female"]) # 3.217391
#rating higher for males 

sum(table(dealers$id.agro[dealers$hh.maize.q82>4 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
# 20, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q82>4 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#   8.695652, Female 
#female dealers less likely to rate themselves more than 4 
sum(table(dealers$id.agro[dealers$hh.maize.q82<=3 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#  58.18182, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q82<=3 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#  56.52174, Female 
#female dealers less likely to rate themselves less than equal to 3

#####ratings from farmers for dealers 

mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q7=="Male"]) #3.811284
mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q7=="Female"]) #3.921875
#ratings for female dealers higher

mean(merged_dealer$rating_stock[merged_dealer$farmer_gender=="Male"]) # 3.876923
mean(merged_dealer$rating_stock[merged_dealer$farmer_gender=="Female"]) #3.776892
#ratings by male farmers are higher

#customers and non-customers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock<=2 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
# 10.98901, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock<=2 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 10.75697, female
#Female farmers less likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock>4 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
#  32.74725, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock>4 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 27.88845, female
#Female farmers less likely to rate more than 4 

#Gender of rater and subject
mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female"])
#3.784615
mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female"])
# 3.774194
mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male"])
#3.832317
mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male"])
#3.992126
#higher scores for female dealers from both genders 

### Rating >4
#female farmers rating female dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))*100
#   24.61538
#male farmers rating male dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))*100
#   32.92683
#female farmers rating male dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))*100
# 29.03226
#male farmers rating female dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_stock>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))*100
#32.28346
### male dealers get more often >4 from both genders 

##############################################################################

########## REPUTATION RATING ###########

##### ratings from dealers

mean(dealers$hh.maize.q83[dealers$hh.maize.q7=="Male"])   # 4.436364
mean(dealers$hh.maize.q83[dealers$hh.maize.q7=="Female"]) #4.304348
#rating higher for males 

sum(table(dealers$id.agro[dealers$hh.maize.q83>4 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#56.36364, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q83>4 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
#52.17391, Female 
#female dealers less likely to rate themselves more than 4 
sum(table(dealers$id.agro[dealers$hh.maize.q83<=3 & dealers$hh.maize.q7=="Male"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Male"]))*100
#7.272727, Male 
sum(table(dealers$id.agro[dealers$hh.maize.q83<=3 & dealers$hh.maize.q7=="Female"]))/sum(table(dealers$id.agro [dealers$hh.maize.q7=="Female"]))*100
# 13.04348, Female 
#female dealers more likely to rate themselves less than equal to 3

#####ratings from farmers for dealers 

mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q7=="Male"]) #  3.801556
mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q7=="Female"]) #3.911458
#ratings for female dealers higher
mean(merged_dealer$rating_reputation[merged_dealer$farmer_gender=="Male"]) # 3.852747
mean(merged_dealer$rating_reputation[merged_dealer$farmer_gender=="Female"]) #  3.792829
#ratings by male farmers are higher

#customers and non-customers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation<=2 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
# 7.252747, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation<=2 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 9.561753, female
#Female farmers more likely to rate lower than equal 2
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation>4 & merged_dealer$farmer_gender=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male"]))*100
#  26.37363, male 
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation>4 & merged_dealer$farmer_gender=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female"]))*100
# 21.91235, female
#Female farmers less likely to rate more than 4 

#Gender of rater and subject
mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Female"])
#3.830769
mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Female"])
#  3.77957
mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q7=="Male" & merged_dealer$farmer_gender=="Male"])
#3.814024
mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q7=="Female" & merged_dealer$farmer_gender=="Male"])
#3.952756
#higher scores for female dealers from both genders 

### Rating >4
#female farmers rating female dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Female"]))*100
#16.92308
#male farmers rating male dealer
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Male"]))*100
#25.30488
#female farmers rating male dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation>4 & merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Female" & merged_dealer$hh.maize.q7=="Male"]))*100
#  23.65591
#male farmers rating female dealers 
sum(table(merged_dealer$farmerID[merged_dealer$rating_reputation>4 & merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))/sum(table(merged_dealer$farmerID[merged_dealer$farmer_gender=="Male" & merged_dealer$hh.maize.q7=="Female"]))*100
#  29.13386
### female dealers get more >4 from male farmers 
### male dealers fer more more >4 from female farmers 

################################################################################################


# TRADERS 

#  Percentage of females in the datasets
sum(table(ratings_trader$farmerID[ratings_trader$farmer_gender=="Female"]))/sum(table(ratings_trader$farmerID))*100
#43.8 percent are female farmers 
sum(table(traders$id.trader[traders$hh.maize.q7=="Female"]))/sum(table(traders$id.trader))*100
# 2.052786 percent are female traders
sum(table(traders$id.trader[traders$hh.maize.q7=="Female"])) #7 female traders 
sum(table(traders$id.trader))  #total 341 traders 

########## OVERALL RATING ###########

##### ratings from traders

mean(traders$trader_rating_overall[traders$hh.maize.q7=="Male"])   # 4.286826
mean(traders$trader_rating_overall[traders$hh.maize.q7=="Female"])  # 4.514286
#mean overall rating higher for females 

sum(table(traders$id.trader[traders$trader_rating_overall>4 & traders$hh.maize.q7=="Male"]))/sum(table(traders$id.trader [traders$hh.maize.q7=="Male"]))*100
# 65.86826, Male 
sum(table(traders$id.trader[traders$trader_rating_overall>4 & traders$hh.maize.q7=="Female"]))/sum(table(traders$id.trader [traders$hh.maize.q7=="Female"]))*100
# 71.42857, Female 
#female traders more likely to rate themselves more than 4 
sum(table(traders$id.trader[traders$trader_rating_overall<=3 & traders$hh.maize.q7=="Male"]))/sum(table(traders$id.trader [traders$hh.maize.q7=="Male"]))*100
# 2.39521, Male 
sum(table(traders$id.trader[traders$trader_rating_overall<=3 & traders$hh.maize.q7=="Female"]))/sum(table(traders$id.trader [traders$hh.maize.q7=="Female"]))*100
# 0, female 
#female traders not at all likely to rate themselves less than equal to 3 

#####ratings from farmers for traders 

mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Male"]) #3.637286
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Female"]) #3.951351
#ratings for female traders --- higher

mean(merged_trader$rating_overall[merged_trader$farmer_gender=="Male"]) #3.624524
mean(merged_trader$rating_overall[merged_trader$farmer_gender=="Female"]) #3.671341
#ratings by female farmers are higher

#customers  
sum(table(merged_trader$farmerID[merged_trader$rating_overall<=2 & merged_trader$farmer_gender=="Male" & merged_trader$sold=="Yes"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male" & merged_trader$sold=="Yes"]))*100
#  3.606103, male 
sum(table(merged_trader$farmerID[merged_trader$rating_overall<=2 & merged_trader$farmer_gender=="Female" & merged_trader$sold=="Yes"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female" & merged_trader$sold=="Yes"]))*100
# 2.851711, female
#Female customers less likely to rate lower than equal 2
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Male" & merged_trader$sold=="Yes"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male" & merged_trader$sold=="Yes"]))*100
# 32.45492, male 
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Female" & merged_trader$sold=="Yes"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female" & merged_trader$sold=="Yes"]))*100
#   32.31939, female
#male customers more likely to rate more than 4 

#non-customers  
sum(table(merged_trader$farmerID[merged_trader$rating_overall<=2 & merged_trader$farmer_gender=="Male" & merged_trader$sold=="No"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male" & merged_trader$sold=="No"]))*100
# 12.60504, male 
sum(table(merged_trader$farmerID[merged_trader$rating_overall<=2 & merged_trader$farmer_gender=="Female" & merged_trader$sold=="No"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female" & merged_trader$sold=="No"]))*100
# 7.692308, female
#Female non-customers less likely to rate lower than equal 2
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Male" & merged_trader$sold=="No"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male" & merged_trader$sold=="No"]))*100
# 18.48739, male 
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Female" & merged_trader$sold=="No"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female" & merged_trader$sold=="No"]))*100
# 21.53846, female
#female non-customers more likely to rate more than 4 

#customers and non-customers 
sum(table(merged_trader$farmerID[merged_trader$rating_overall<=2 & merged_trader$farmer_gender=="Male"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male"]))*100
#  4.880952, male 
sum(table(merged_trader$farmerID[merged_trader$rating_overall<=2 & merged_trader$farmer_gender=="Female"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female"]))*100
#   3.810976, female
#Female farmers less likely to rate lower than equal 2
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Male"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male"]))*100
# 30.47619, male 
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Female"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female"]))*100
#   30.18293, female
#male farmers more likely to rate more than 4 

#Gender of the rater and the subject 
#Customers and non-customers 
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Female" & merged_trader$farmer_gender=="Female"])
#  4.117647
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Male" & merged_trader$farmer_gender=="Female"])
#  3.659468
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Male" & merged_trader$farmer_gender=="Male"])
#  3.62
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Female" & merged_trader$farmer_gender=="Male"])
#  3.81
#Female traders get higher scores 
#female and male farmers give higher scores to female traders 

#Customers 
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Female" & merged_trader$farmer_gender=="Female" & merged_trader$sold=="Yes"])
# 4.353846
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Male" & merged_trader$farmer_gender=="Female" & merged_trader$sold=="Yes"])
# 3.706823
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Male" & merged_trader$farmer_gender=="Male" & merged_trader$sold=="Yes"])
#  3.678865
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Female" & merged_trader$farmer_gender=="Male" & merged_trader$sold=="Yes"])
#  3.8625
#Female traders get higher scores 
#female and male farmers give higher scores to female traders 

#Non-Customers 
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Female" & merged_trader$farmer_gender=="Female" & merged_trader$sold=="No"])
# 3.35
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Male" & merged_trader$farmer_gender=="Female" & merged_trader$sold=="No"])
# 3.466667
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Male" & merged_trader$farmer_gender=="Male" & merged_trader$sold=="No"])
#  3.25913
mean(merged_trader$rating_overall[merged_trader$hh.maize.q7=="Female" & merged_trader$farmer_gender=="Male" & merged_trader$sold=="No"])
#  3.6
#Female traders get higher scores from male farmers 
#Male traders get higher scores from female farmers 

### Rating >4
#female farmers rating female traders
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Female" & merged_trader$hh.maize.q7=="Female"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female" & merged_trader$hh.maize.q7=="Female"]))*100
#58.82353
#male farmers rating male traders
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Male" & merged_trader$hh.maize.q7=="Male"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male" & merged_trader$hh.maize.q7=="Male"]))*100
#30.4878
#female farmers rating male traders
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Female" & merged_trader$hh.maize.q7=="Male"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Female" & merged_trader$hh.maize.q7=="Male"]))*100
#29.42097
#male farmers rating female traders
sum(table(merged_trader$farmerID[merged_trader$rating_overall>4 & merged_trader$farmer_gender=="Male" & merged_trader$hh.maize.q7=="Female"]))/sum(table(merged_trader$farmerID[merged_trader$farmer_gender=="Male" & merged_trader$hh.maize.q7=="Female"]))*100
#30
###when gender of rater and trader same, they are rater more than 4 more often 
##female farmers rating female traders (more percentage >4)

###################################################################################################

#   MILLERS

#  Percentage of females in the datasets
sum(table(ratings_mill$farmerID[ratings_mill$farmer_gender=="Female"]))/sum(table(ratings_mill$farmerID))*100
#44.06085 percent are female farmers 
sum(table(millers$id.miller[millers$hh.maize.q7=="Female"]))/sum(table(millers$id.miller))*100
#6.896552 percent are female millers 
sum(table(millers$id.miller[millers$hh.maize.q7=="Female"])) #12 female millers
sum(table(millers$id.miller)) #total 174 millers

########## OVERALL RATING ###########

##### ratings from millers 

mean(millers$miller_rating_overall[millers$hh.maize.q7=="Male"])   #4.171605
mean(millers$miller_rating_overall[millers$hh.maize.q7=="Female"]) #4.25
#mean overall rating higher for females 

sum(table(millers$id.miller[millers$miller_rating_overall>4 & millers$hh.maize.q7=="Male"]))/sum(table(millers$id.miller [millers$hh.maize.q7=="Male"]))*100
#56.79012, Male 
sum(table(millers$id.miller[millers$miller_rating_overall>4 & millers$hh.maize.q7=="Female"]))/sum(table(millers$id.miller [millers$hh.maize.q7=="Female"]))*100
#66.66667, female 
#female millers more likely to rate themselves more than 4 
sum(table(millers$id.miller[millers$miller_rating_overall<=3 & millers$hh.maize.q7=="Male"]))/sum(table(millers$id.miller [millers$hh.maize.q7=="Male"]))*100
#3.08642, Male 
sum(table(millers$id.miller[millers$miller_rating_overall<=3 & millers$hh.maize.q7=="Female"]))/sum(table(millers$id.miller [millers$hh.maize.q7=="Female"]))*100
#0, female 
#male millers more likely to rate themselves less than equal to 3 

#####ratings from farmers for millers 

mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Male"]) #3.530056
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Female"]) #3.552475
#ratings for female millers little bit higher
mean(merged_miller$rating_overall[merged_miller$farmer_gender=="Male"]) # 3.51623
mean(merged_miller$rating_overall[merged_miller$farmer_gender=="Female"])  #3.550598
#ratings by female farmers are higher

#customers
sum(table(merged_miller$farmerID[merged_miller$rating_overall<=2 & merged_miller$used=="Yes" & merged_miller$farmer_gender=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$used=="Yes" & merged_miller$farmer_gender=="Male"]))*100
# 3.543743, male 
sum(table(merged_miller$farmerID[merged_miller$rating_overall<=2 & merged_miller$used=="Yes" & merged_miller$farmer_gender=="Female"]))/sum(table(merged_miller$farmerID[merged_miller$used=="Yes" & merged_miller$farmer_gender=="Female"]))*100
# 3.366059, female 
#Females who are customers more likely to rate lower than equal 2
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$used=="Yes" & merged_miller$farmer_gender=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$used=="Yes" & merged_miller$farmer_gender=="Male"]))*100
# 22.03765, male 
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$used=="Yes" & merged_miller$farmer_gender=="Female"]))/sum(table(merged_miller$farmerID[merged_miller$used=="Yes" & merged_miller$farmer_gender=="Female"]))*100
#25.6662, female 
#Females who are customers more likely to rate more than 4 

#non customers 
sum(table(merged_miller$farmerID[merged_miller$rating_overall<=2 & merged_miller$used=="No" & merged_miller$farmer_gender=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$used=="No" & merged_miller$farmer_gender=="Male"]))*100
#  7.843137, male 
sum(table(merged_miller$farmerID[merged_miller$rating_overall<=2 & merged_miller$used=="No" & merged_miller$farmer_gender=="Female"]))/sum(table(merged_miller$farmerID[merged_miller$used=="No" & merged_miller$farmer_gender=="Female"]))*100
# 12.5, female 
#Female non-customers more likely to rate lower than equal 2
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$used=="No" & merged_miller$farmer_gender=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$used=="No" & merged_miller$farmer_gender=="Male"]))*100
# 11.76471, male 
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$used=="No" & merged_miller$farmer_gender=="Female"]))/sum(table(merged_miller$farmerID[merged_miller$used=="No" & merged_miller$farmer_gender=="Female"]))*100
#7.5, female 
#Female non-customers less likely to rate more than 4 

#customers and non-customers 
sum(table(merged_miller$farmerID[merged_miller$rating_overall<=2 & merged_miller$farmer_gender=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$farmer_gender=="Male"]))*100
#   3.769634, male 
sum(table(merged_miller$farmerID[merged_miller$rating_overall<=2 & merged_miller$farmer_gender=="Female"]))/sum(table(merged_miller$farmerID[ merged_miller$farmer_gender=="Female"]))*100
# 3.851262, female 
#Female farmers more likely to rate lower than equal 2
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$farmer_gender=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$farmer_gender=="Male"]))*100
#  21.46597, male 
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$farmer_gender=="Female"]))/sum(table(merged_miller$farmerID[merged_miller$farmer_gender=="Female"]))*100
#24.7012, female 
#Female farmers more likely to rate more than 4 

#Gender of rater and the subject
#customers and non-customers 
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Female" & merged_miller$farmer_gender=="Female"]) # 3.504545
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Male" & merged_miller$farmer_gender=="Female"]) #3.553456
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Male" & merged_miller$farmer_gender=="Male"])    #3.511581
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Female" & merged_miller$farmer_gender=="Male"]) #3.589474
#female farmers give higher scores to female millers, male farmers give higher scores to female millers 
#female farmers give higher rating to male millers compared to male farmers rating male millers 

#CUSTOMERS
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Female" & merged_miller$farmer_gender=="Female" & merged_miller$used=="Yes"]) #3.556098
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Male" & merged_miller$farmer_gender=="Female" & merged_miller$used=="Yes"]) #3.579762
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Male" & merged_miller$farmer_gender=="Male" & merged_miller$used=="Yes"]) # 3.531294
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Female" & merged_miller$farmer_gender=="Male" & merged_miller$used=="Yes"]) #3.607547
##female customers give higher rating to male millers, male customers give higher rating to female millers 

#NON-CUSTOMERS 
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Female" & merged_miller$farmer_gender=="Female" & merged_miller$used=="No"]) #2.8
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Male" & merged_miller$farmer_gender=="Female" & merged_miller$used=="No"]) #3.075676
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Male" & merged_miller$farmer_gender=="Male" & merged_miller$used=="No"]) #3.157447
mean(merged_miller$rating_overall[merged_miller$hh.maize.q7=="Female" & merged_miller$farmer_gender=="Male" & merged_miller$used=="No"]) #3.35
##female non-customers give higher rating to male millers, male non-customers give higher rating to female millers 

### Rating >4
#female farmers rating female miller
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$farmer_gender=="Female" & merged_miller$hh.maize.q7=="Female"]))/sum(table(merged_miller$farmerID[merged_miller$farmer_gender=="Female" & merged_miller$hh.maize.q7=="Female"]))*100
#15.90909
#male farmers rating male miller
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$farmer_gender=="Male" & merged_miller$hh.maize.q7=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$farmer_gender=="Male" & merged_miller$hh.maize.q7=="Male"]))*100
# 20.93541
#female farmers rating male millers
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$farmer_gender=="Female" & merged_miller$hh.maize.q7=="Male"]))/sum(table(merged_miller$farmerID[merged_miller$farmer_gender=="Female" & merged_miller$hh.maize.q7=="Male"]))*100
#  25.24683
#male farmers rating female millers 
sum(table(merged_miller$farmerID[merged_miller$rating_overall>4 & merged_miller$farmer_gender=="Male" & merged_miller$hh.maize.q7=="Female"]))/sum(table(merged_miller$farmerID[merged_miller$farmer_gender=="Male" & merged_miller$hh.maize.q7=="Female"]))*100
# 29.82456
#gender of rater and miller different, more chances of getting >4

##############################################################################################################################################



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
#3.64873

fe_interceptloc20 <- lm(rating_location ~ 1, data=gt20)
summary(fe_interceptloc20)
#3.79577

fe_modlocation <- lm(rating_location ~ id.agro, data = ratings)
summary(fe_modlocation)
##reject null, individual means diff from overall mean
#F-statistic: 3.291 on 72 and 633 DF, 1.32 (F-table value)<F-stat (reject null) at 5% level

fe_modlocation20 <- lm(rating_location ~ id.agro, data = gt20)
summary(fe_modlocation20)
#reject null, 1.88<F-stat; reject null at 5%

tab_model(fe_interceptloc, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocation, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptloc, fe_modlocation, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocation, fe_modlocation20, show.se = TRUE, show.stat = TRUE)

########################################
##RATING PRICE 
fe_interceptprice <- lm(rating_price ~ 1, data=ratings)
summary(fe_interceptprice)
# 2.99008

fe_interceptprice20 <- lm(rating_price ~ 1, data=gt20)
summary(fe_interceptprice20)
#2.98944

fe_modprice <- lm(rating_price ~ id.agro, data = ratings)
summary(fe_modprice)
## p-value: 0.4169, cannot reject null, individual means same as overall mean

fe_modprice20 <- lm(rating_price ~ id.agro, data = gt20)
summary(fe_modprice20) #cannot reject null

tab_model(fe_interceptprice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modprice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptprice, fe_modprice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modprice, fe_modprice20, show.se = TRUE, show.stat = TRUE)

########################################
##RATING QUALITY
fe_interceptqual <- lm(rating_quality ~ 1, data=ratings)
summary(fe_interceptqual)
#3.62040

fe_interceptqual20 <- lm(rating_quality ~ 1, data=gt20)
summary(fe_interceptqual20)
#3.61268

fe_modqual <- lm(rating_quality ~ id.agro, data = ratings)
summary(fe_modqual)
## p-value: 0.0913, reject null at 10% level, individual means diff from overall mean, cannot reject at 5%

fe_modqual20 <- lm(rating_quality ~ id.agro, data = gt20)
summary(fe_modqual20)
#cannot reject null

tab_model(fe_interceptqual, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqual, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptqual, fe_modqual, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqual, fe_modqual20, show.se = TRUE, show.stat = TRUE)


###############################################################
##RATING STOCK
fe_interceptstock <- lm(rating_stock ~ 1, data=ratings)
summary(fe_interceptstock)
#3.84136

fe_interceptstock20 <- lm(rating_stock ~ 1, data=gt20)
summary(fe_interceptstock20)
# 3.96831

fe_modstock <- lm(rating_stock ~ id.agro, data = ratings)
summary(fe_modstock)
## p-value: 0.08097,cannot reject null at 5%
fe_modstock20 <- lm(rating_stock ~ id.agro, data = gt20)
summary(fe_modstock20)
#cannot reject null

tab_model(fe_interceptstock, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modstock, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptstock, fe_modstock, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modstock, fe_modstock20, show.se = TRUE, show.stat = TRUE)

###############################################################
##RATING REPUTATION
fe_interceptrep <- lm(rating_reputation ~ 1, data=ratings)
summary(fe_interceptrep)
#3.83144 

fe_interceptrep20 <- lm(rating_reputation ~ 1, data=gt20)
summary(fe_interceptrep20)
# 3.85211

fe_modrep <- lm(rating_reputation ~ id.agro, data = ratings)
summary(fe_modrep)
## p-value:  0.1156, cannot reject null
fe_modrep20 <- lm(rating_reputation ~ id.agro, data = gt20)
summary(fe_modrep20)
#cannot reject null

tab_model(fe_interceptrep, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrep, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptrep, fe_modrep, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrep, fe_modrep20, show.se = TRUE, show.stat = TRUE)

###############################################################
##RATING OVERALL
fe_interceptoverall <- lm(rating_overall ~ 1, data=ratings)
summary(fe_interceptoverall)
#3.58640

fe_interceptoverall20 <- lm(rating_overall ~ 1, data=gt20)
summary(fe_interceptoverall20) #3.64366

fe_modoverall <- lm(rating_overall ~ id.agro, data = ratings)
summary(fe_modoverall)
## p-value: 0.0003263, reject null, individual means diff from overall mean
#F-statistic: 1.575 on 72 and 633 DF, F-table value 1.32<F-stat, reject null (5% level)
fe_modoverall20 <- lm(rating_overall ~ id.agro, data = gt20)
summary(fe_modoverall20) #cannot reject null

tab_model(fe_interceptoverall, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverall, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptoverall, fe_modoverall, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverall, fe_modoverall20, show.se = TRUE, show.stat = TRUE)

#########################

#differences in ratings 
merged_dealer$ratingoverall_diff <- merged_dealer$rating_overall - merged_dealer$dealer_rating_overall #difference in overall ratings 
merged_dealer$ratingloc_diff <- merged_dealer$rating_location - merged_dealer$hh.maize.q79 ##diff in location ratings 
merged_dealer$ratingprice_diff <- merged_dealer$rating_price - merged_dealer$hh.maize.q80 ##diff in price ratings 
merged_dealer$ratingqual_diff <- merged_dealer$rating_quality - merged_dealer$hh.maize.q81 ##diff in quality ratings 
merged_dealer$ratingstock_diff <- merged_dealer$rating_stock - merged_dealer$hh.maize.q82 ##diff in stock ratings 
merged_dealer$ratingrepu_diff <- merged_dealer$rating_reputation - merged_dealer$hh.maize.q83 ##diff in reputation ratings 

##### REGRESSIONS WITH SALES OF HYBRID MAIZE SEEDS ########
lm_overalld_dealer <- lm(ratingoverall_diff ~ id.agro + seed_sale, data=merged_dealer)
summary(lm(ratingoverall_diff ~ id.agro + seed_sale, data=merged_dealer)) #seed_sale coeff = NA
alias(lm_overalld_dealer)
##high collinearity

#LOCATION
summary(lm(ratingloc_diff ~ id.agro + seed_sale, data=merged_dealer)) #seed_sale=NA

#### looking at ratings from farmers #####
mean(merged_dealer$rating_overall[merged_dealer$seed_sale>1000])  #3.673504
mean(merged_dealer$rating_overall[merged_dealer$seed_sale<300])  #3.563765

mean(merged_dealer$rating_location[merged_dealer$seed_sale>1000]) #3.923077
mean(merged_dealer$rating_location[merged_dealer$seed_sale<300])  # 3.677647

mean(merged_dealer$rating_quality[merged_dealer$seed_sale>1000])  #  3.555556
mean(merged_dealer$rating_quality[merged_dealer$seed_sale<300]) #3.592941

mean(merged_dealer$rating_price[merged_dealer$seed_sale>1000])  #3.034188
mean(merged_dealer$rating_price[merged_dealer$seed_sale<300]) # 2.983529

mean(merged_dealer$rating_stock[merged_dealer$seed_sale>1000]) #4
mean(merged_dealer$rating_stock[merged_dealer$seed_sale<300]) # 3.781176

mean(merged_dealer$rating_reputation[merged_dealer$seed_sale>1000]) #3.854701
mean(merged_dealer$rating_reputation[merged_dealer$seed_sale<300]) #3.783529

##### REGRESSIONS WITH SALES OF OPV SEEDS ########
lm_dealeroverall_opv <- lm(ratingoverall_diff ~ id.agro + opv_sale, data=merged_dealer)
summary(lm_dealeroverall_opv) #opv_sale = NA
alias(lm_dealeroverall_opv)

#### looking at ratings from farmers #####
mean(merged_dealer$rating_overall[merged_dealer$opv_sale>1000])  # 3.650909
mean(merged_dealer$rating_overall[merged_dealer$opv_sale<300])  # 3.572792

mean(merged_dealer$rating_location[merged_dealer$opv_sale>1000]) #3.563636
mean(merged_dealer$rating_location[merged_dealer$opv_sale<300])  #3.642005

mean(merged_dealer$rating_quality[merged_dealer$opv_sale>1000])  # 3.745455
mean(merged_dealer$rating_quality[merged_dealer$opv_sale<300]) #3.603819

mean(merged_dealer$rating_price[merged_dealer$opv_sale>1000])  #2.872727
mean(merged_dealer$rating_price[merged_dealer$opv_sale<300]) #3.011933

mean(merged_dealer$rating_stock[merged_dealer$opv_sale>1000]) # 4.072727
mean(merged_dealer$rating_stock[merged_dealer$opv_sale<300]) #3.785203

mean(merged_dealer$rating_reputation[merged_dealer$opv_sale>1000]) #4
mean(merged_dealer$rating_reputation[merged_dealer$opv_sale<300]) #3.821002

##### REGRESSIONS --- input dealers providing extension/training to clients ########
merged_dealer$extension_training <- ifelse(merged_dealer$hh.maize.q67 == '3', 1, 0)
summary(lm(ratingoverall_diff ~ id.agro + extension_training, data=merged_dealer)) #NA
lm_ext_train <- lm(ratingoverall_diff ~ id.agro + extension_training, data=merged_dealer)
alias(lm_ext_train)

#### looking at ratings from farmers #####
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67=="3"])  #3.575492
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q67=="1"])  # 3.559292

mean(merged_dealer$rating_location[merged_dealer$hh.maize.q67=="3"]) #3.673961
mean(merged_dealer$rating_location[merged_dealer$hh.maize.q67=="1"])  #3.672566

mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q67=="3"])  # 3.599562
mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q67=="1"]) #3.646018

mean(merged_dealer$rating_price[merged_dealer$hh.maize.q67=="3"])  # 2.956236
mean(merged_dealer$rating_price[merged_dealer$hh.maize.q67=="1"]) # 2.99115

mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q67=="3"]) # 3.835886
mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q67=="1"]) # 3.716814

mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q67=="3"]) # 3.811816
mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q67=="1"]) #3.769912


##### REGRESSIONS --- input dealers providing seed on credit ########
merged_dealer$seed_credit <- ifelse(merged_dealer$hh.maize.q68 == '3', 1, 0)
summary(lm(ratingoverall_diff ~ id.agro + seed_credit, data=merged_dealer)) #NA
lm_seedcredit <- lm(ratingoverall_diff ~ id.agro + seed_credit, data=merged_dealer)
alias(lm_seedcredit)

#### looking at ratings from farmers #####
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q68=="3"])  #3.521951
mean(merged_dealer$rating_overall[merged_dealer$hh.maize.q68=="1"])  #3.571014

mean(merged_dealer$rating_location[merged_dealer$hh.maize.q68=="3"]) #3.634146
mean(merged_dealer$rating_location[merged_dealer$hh.maize.q68=="1"])  #3.681159

mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q68=="3"])  #3.609756
mean(merged_dealer$rating_quality[merged_dealer$hh.maize.q68=="1"]) #3.584541

mean(merged_dealer$rating_price[merged_dealer$hh.maize.q68=="3"])  #  2.926829
mean(merged_dealer$rating_price[merged_dealer$hh.maize.q68=="1"]) # 3

mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q68=="3"]) #3.707317
mean(merged_dealer$rating_stock[merged_dealer$hh.maize.q68=="1"]) #3.78744

mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q68=="3"]) #  3.731707
mean(merged_dealer$rating_reputation[merged_dealer$hh.maize.q68=="1"]) # 3.801932


##### REGRESSIONS WITH GENDER ########
merged_dealer$farmer_fem <- ifelse(merged_dealer$farmer_gender == 'Female', 1, 0)
merged_dealer$dealer_fem <- ifelse(merged_dealer$hh.maize.q7 == 'Female', 1, 0)
merged_dealer$farmer_male <- ifelse(merged_dealer$farmer_gender == 'Male', 1, 0)
merged_dealer$dealer_male <- ifelse(merged_dealer$hh.maize.q7 == 'Male', 1, 0)

###################OVERALL RATING
summary(lm(ratingoverall_diff ~ id.agro + farmer_fem, data=merged_dealer)) #-0.003227   
#if the rater is female, the differences in rater rating and subject rating reduce 

mod1 <- lm(ratingoverall_diff ~ id.agro + dealer_fem, data=merged_dealer)
alias(mod1)
summary(lm(ratingoverall_diff ~ id.agro + dealer_fem, data=merged_dealer)) #NA

##### with interactions 
summary(lm(ratingoverall_diff ~ id.agro + farmer_fem + farmer_fem*dealer_fem, data=merged_dealer)) #female rater and subject
#farmer_fem             0.01047    0.07187   0.146 0.884173    
#dealer_fem                  NA         NA      NA       NA    
#farmer_fem:dealer_fem -0.05415    0.14287  -0.379 0.704831 
#if the rater is a female, the differences in rater rating and subject rating increase :  0.01047  
#if the dealer is a female, having a female rater reduces differences between rater and subject ratings
#if the dealer is male, having a female rater increases differences between rater and subject ratings

summary(lm(ratingoverall_diff ~ id.agro + farmer_male + farmer_male*dealer_male, data=merged_dealer))  #male rater and subject 
#if the rater is male, diff increase:   0.04367
#if the dealer is male, having a male rater reduces differences between rater and subject ratings 
#if the dealer is female, having a male rater increases differences between rater and subject ratings 

####################LOCATION RATING
summary(lm(ratingloc_diff ~ id.agro + farmer_fem, data=merged_dealer)) # 0.14541    
#if the rater is female, the differences in rater rating and subject rating increase for location 

summary(lm(ratingloc_diff ~ id.agro + dealer_fem, data=merged_dealer)) #NA

##### with interactions 
summary(lm(ratingloc_diff ~ id.agro + farmer_fem + farmer_fem*dealer_fem, data=merged_dealer)) #female rater and subject
#if the rater is a female, the differences in rater rating and subject rating increase :0.18122   
#if the dealer is a female, having a female rater reduces differences between rater and subject ratings
#if the dealer is male, having a female rater increases differences between rater and subject ratings

summary(lm(ratingloc_diff ~ id.agro + farmer_male + farmer_male*dealer_male, data=merged_dealer))  #male rater and subject 
# if the rater is male, the diff in rater and subject ratings reduce :  -0.039693
#if the dealer is male, having a male rater reduces differences between rater and subject ratings 
#if the dealer is female, having a male rater increases differences between rater and subject ratings

####################PRICE RATING
summary(lm(ratingprice_diff ~ id.agro + farmer_fem, data=merged_dealer)) # 0.08326    
#if the rater is female, the differences in rater rating and subject rating increase

summary(lm(ratingprice_diff ~ id.agro + dealer_fem, data=merged_dealer)) #NA

##### with interactions 
summary(lm(ratingprice_diff ~ id.agro + farmer_fem + farmer_fem*dealer_fem, data=merged_dealer)) #female rater and subject
#if the rater is a female, the differences in rater rating and subject rating increase :0.10976  
#if the dealer is a female, having a female rater reduces differences between rater and subject ratings
#if the dealer is male, having a female rater increases differences between rater and subject ratings

summary(lm(ratingprice_diff ~ id.agro + farmer_male + farmer_male*dealer_male, data=merged_dealer))  #male rater and subject 
# if the rater is male, the diff in rater and subject ratings reduce :  -0.005039 
#if the dealer is male, having a male rater reduces differences between rater and subject ratings 
#if the dealer is female, having a male rater increases differences between rater and subject ratings 

#################### QUALITY RATING
summary(lm(ratingqual_diff ~ id.agro + farmer_fem, data=merged_dealer)) #-0.12861   
#if the rater is female, the differences in rater rating and subject rating reduce

summary(lm(ratingqual_diff ~ id.agro + dealer_fem, data=merged_dealer)) #NA

##### with interactions 
summary(lm(ratingqual_diff ~ id.agro + farmer_fem + farmer_fem*dealer_fem, data=merged_dealer)) #female rater and subject
#if the rater is a female, the differences in rater rating and subject rating reduce :-0.18249 
#if the dealer is a female, having a female rater increases differences between rater and subject ratings
#if the dealer is male, having a female rater reduces differences between rater and subject ratings

summary(lm(ratingqual_diff ~ id.agro + farmer_male + farmer_male*dealer_male, data=merged_dealer))  #male rater and subject 
# if the rater is male, the diff in rater and subject ratings reduce :   -0.03043  
#if the dealer is male, having a male rater increases differences between rater and subject ratings 
#if the dealer is female, having a male rater reduces differences between rater and subject ratings 

#################### STOCK RATING
summary(lm(ratingstock_diff ~ id.agro + farmer_fem, data=merged_dealer)) #-0.07370 
#if the rater is female, the differences in rater rating and subject rating reduce

summary(lm(ratingstock_diff ~ id.agro + dealer_fem, data=merged_dealer)) #NA

##### with interactions 
summary(lm(ratingstock_diff ~ id.agro + farmer_fem + farmer_fem*dealer_fem, data=merged_dealer)) #female rater and subject
#if the rater is a female, the differences in rater rating and subject rating reduce : -0.03813
#if the dealer is a female, having a female rater reduces differences between rater and subject ratings
#if the dealer is male, having a female rater increases differences between rater and subject ratings

summary(lm(ratingstock_diff ~ id.agro + farmer_male + farmer_male*dealer_male, data=merged_dealer))  #male rater and subject 
# if the rater is male, the diff in rater and subject ratings increase :  0.17870  
#if the dealer is male, having a male rater reduces differences between rater and subject ratings 
#if the dealer is female, having a male rater increases differences between rater and subject ratings 

#################### REPUTATION RATING
summary(lm(ratingrepu_diff ~ id.agro + farmer_fem, data=merged_dealer)) # -0.04249 
#if the rater is female, the differences in rater rating and subject rating reduce

summary(lm(ratingrepu_diff ~ id.agro + dealer_fem, data=merged_dealer)) #NA

##### with interactions 
summary(lm(ratingrepu_diff ~ id.agro + farmer_fem + farmer_fem*dealer_fem, data=merged_dealer)) #female rater and subject
#if the rater is a female, the differences in rater rating and subject rating reduce : -0.01799 
#if the dealer is a female, having a female rater reduces differences between rater and subject ratings
#if the dealer is male, having a female rater increases differences between rater and subject ratings

summary(lm(ratingrepu_diff ~ id.agro + farmer_male + farmer_male*dealer_male, data=merged_dealer))  #male rater and subject 
# if the rater is male, the diff in rater and subject ratings increase :   0.11482 
#if the dealer is male, having a male rater reduces differences between rater and subject ratings 
#if the dealer is female, having a male rater increases differences between rater and subject ratings


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
#4.08733
fe_interceptloct20 <- lm(rating_location ~ 1, data=gt20_trad)
summary(fe_interceptloct20)
#4.2466 

fe_modlocationt <- lm(rating_location ~ id.trader, data = ratings_trader)
summary(fe_modlocationt)
##reject null, individual means diff from overall mean
#F-statistic: 1.434 on 309 and 1190 DF, 1.22 (F-table value)<F-stat (reject null), 5% level
fe_modlocationt20 <- lm(rating_location ~ id.trader, data = gt20_trad)
summary(fe_modlocationt20)
#reject null, F-stat > 3.15, reject null

tab_model(fe_interceptloct, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocationt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptloct, fe_modlocationt, show.se = TRUE, show.stat = TRUE)
tab_model( fe_modlocationt, fe_modlocationt20, show.se = TRUE, show.stat = TRUE)


########################################
##RATING PRICE 
fe_interceptpricet <- lm(rating_price ~ 1, data=ratings_trader)
summary(fe_interceptpricet)
#3.06133

fe_interceptpricet20 <- lm(rating_price ~ 1, data=gt20_trad)
summary(fe_interceptpricet20) #3.1507 

fe_modpricet <- lm(rating_price ~ id.trader, data = ratings_trader)
summary(fe_modpricet)
## p-value: 0.00286, reject null, individual means diff from overall mean
#F-statistic: 1.274 on 309 and 1190 DF > 1.22 (F-table value), reject null
fe_modpricet20 <- lm(rating_price ~ id.trader, data = gt20_trad)
summary(fe_modpricet20)
#cannot reject null

tab_model(fe_interceptpricet, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricet, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptpricet, fe_modpricet, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricet, fe_modpricet20, show.se = TRUE, show.stat = TRUE)

########################################
##RATING QUALITY
fe_interceptqualt <- lm(rating_quality ~ 1, data=ratings_trader)
summary(fe_interceptqualt) #3.52067 

fe_interceptqualt20 <- lm(rating_quality ~ 1, data=gt20_trad)
summary(fe_interceptqualt20) #  3.7123

fe_modqualt <- lm(rating_quality ~ id.trader, data = ratings_trader)
summary(fe_modqualt)
## p-value: 0.001169, reject null, individual means diff from overall mean
#F-statistic: 1.305, 1.22 (F-table value); F-table value<F-stat, reject null (5% level)
fe_modqualt20 <- lm(rating_quality ~ id.trader, data = gt20_trad)
summary(fe_modqualt20) #cannot reject at 5%

tab_model(fe_interceptqualt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptqualt, fe_modqualt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualt, fe_modqualt20, show.se = TRUE, show.stat = TRUE)

###############################################################
##RATING HONESTY
fe_intercepthonesty <- lm(rating_honesty ~ 1, data=ratings_trader)
summary(fe_intercepthonesty)   # 3.74400

fe_intercepthonesty20 <- lm(rating_honesty ~ 1, data=gt20_trad)
summary(fe_intercepthonesty20) #3.8904  

fe_modhonesty <- lm(rating_honesty ~ id.trader, data = ratings_trader)
summary(fe_modhonesty)
## p-value: 0.001378, reject null, individual means diff from overall mean
#F-statistic:  1.3 > F-table value (1.22), reject null
fe_modhonesty20 <- lm(rating_honesty ~ id.trader, data = gt20_trad)
summary(fe_modhonesty20) #can reject at 5%, F-stat>3.15, reject at 5%

tab_model(fe_intercepthonesty, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modhonesty, show.se = TRUE, show.stat = TRUE)
tab_model(fe_intercepthonesty, fe_modhonesty, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modhonesty, fe_modhonesty20, show.se = TRUE, show.stat = TRUE)

###############################################################
##RATING REPUTATION
fe_interceptrept <- lm(rating_reputation ~ 1, data=ratings_trader)
summary(fe_interceptrept)  #3.82000

fe_interceptrept20 <- lm(rating_reputation ~ 1, data=gt20_trad)
summary(fe_interceptrept20) #3.9452 

fe_modrept <- lm(rating_reputation ~ id.trader, data = ratings_trader)
summary(fe_modrept)
## p-value:   0.001767, reject null, individual means diff from overall means 
#F-statistic: 1.291, F-table value 1.22<F-stat, reject null, 5% level
fe_modrept20 <- lm(rating_reputation ~ id.trader, data = gt20_trad)
summary(fe_modrept20) #reject null, F stat>3.15, reject at 5%

tab_model(fe_interceptrept, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrept, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptrept, fe_modrept, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrept, fe_modrept20, show.se = TRUE, show.stat = TRUE)


###############################################################
##RATING OVERALL
fe_interceptoverallt <- lm(rating_overall ~ 1, data=ratings_trader)
summary(fe_interceptoverallt)
#3.64667
fe_interceptoverallt20 <- lm(rating_overall ~ 1, data=gt20_trad)
summary(fe_interceptoverallt20) # 3.78904

fe_modoverallt <- lm(rating_overall ~ id.trader, data = ratings_trader)
summary(fe_modoverallt)
## p-value: 1.342e-05, reject null, individual means diff from overall mean
#F-statistic: 1.44, F-table value 1.22 < F-stat, reject null (5% level)
fe_modoverallt20 <- lm(rating_overall ~ id.trader, data = gt20_trad)
summary(fe_modoverallt20) #reject at 5%, F-stat greater, reject at 5% 

tab_model(fe_interceptoverallt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptoverallt, fe_modoverallt, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallt, fe_modoverallt20, show.se = TRUE, show.stat = TRUE)


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
#3.81978
fe_interceptlocm20 <- lm(rating_location ~ 1, data=gt20_mill)
summary(fe_interceptlocm20)  # 3.86111

fe_modlocationm <- lm(rating_location ~ id.miller, data = ratings_mill)
summary(fe_modlocationm)
##reject null, individual means diff from overall mean
#F-statistic: 2.971 on 147 and 1561 DF, 1.22 (F-table value)<F-stat (reject null), 5% level
fe_modlocationm20 <- lm(rating_location ~ id.miller, data = gt20_mill)
summary(fe_modlocationm20) #reject null, F-stat>1.52, reject

tab_model(fe_interceptlocm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocationm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptlocm, fe_modlocationm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modlocationm, fe_modlocationm20, show.se = TRUE, show.stat = TRUE)

########################################
##RATING PRICE 
fe_interceptpricem <- lm(rating_price ~ 1, data=ratings_mill)
summary(fe_interceptpricem)
#  3.01170 
fe_interceptpricem20 <- lm(rating_price ~ 1, data=gt20_mill)
summary(fe_interceptpricem20) # 3.0144    

fe_modpricem <- lm(rating_price ~ id.miller, data = ratings_mill)
summary(fe_modpricem)
##reject null, individual means diff from overall mean
#F-statistic: 1.694, 1.22 (F-table value)<F-stat (reject null), 5% level
fe_modpricem20 <- lm(rating_price ~ id.miller, data = gt20_mill)
summary(fe_modpricem20) #reject null, F-stat larger, reject

tab_model(fe_interceptpricem, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricem, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptpricem, fe_modpricem, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modpricem, fe_modpricem20, show.se = TRUE, show.stat = TRUE)


########################################
##RATING QUALITY
fe_interceptqualm <- lm(rating_quality ~ 1, data=ratings_mill)
summary(fe_interceptqualm)
#3.39263 
fe_interceptqualm20 <- lm(rating_quality ~ 1, data=gt20_mill)
summary(fe_interceptqualm20)   #3.5 

fe_modqualm <- lm(rating_quality ~ id.miller, data = ratings_mill)
summary(fe_modqualm)
## reject null, individual means diff from overall mean
#F-statistic:4.369 , 1.22 (F-table value)<F-stat (reject null), 5% level
fe_modqualm20 <- lm(rating_quality ~ id.miller, data = gt20_mill)
summary(fe_modqualm20) #reject, F-stat larger, reject

tab_model(fe_interceptqualm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptqualm, fe_modqualm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modqualm, fe_modqualm20, show.se = TRUE, show.stat = TRUE)

###############################################################
##RATING SERVICE
fe_interceptservice <- lm(rating_service ~ 1, data=ratings_mill)
summary(fe_interceptservice)
# 3.61322
fe_interceptservice20 <- lm(rating_service ~ 1, data=gt20_mill)
summary(fe_interceptservice20)   # 3.6

fe_modservice <- lm(rating_service ~ id.miller, data = ratings_mill)
summary(fe_modservice)
## reject null, individual means diff from overall mean
#F-statistic:  1.679 , 1.22 (F-table value)<F-stat (reject null), 5% level
fe_modservice20 <- lm(rating_service ~ id.miller, data = gt20_mill)
summary(fe_modservice20) #reject null, Fstat larger, reject

tab_model(fe_interceptservice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modservice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptservice, fe_modservice, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modservice, fe_modservice20, show.se = TRUE, show.stat = TRUE)


###############################################################
##RATING REPUTATION
fe_interceptrepm <- lm(rating_reputation ~ 1, data=ratings_mill)
summary(fe_interceptrepm)
#3.82095
fe_interceptrepm20 <- lm(rating_reputation ~ 1, data=gt20_mill)
summary(fe_interceptrepm20)   # 3.76222

fe_modrepm <- lm(rating_reputation ~ id.miller, data = ratings_mill)
summary(fe_modrepm)
##  reject null, individual means diff from overall means 
#F-statistic:  1.339 on 147 and 1561 DF, 1.22 (F-table value)<F-stat (reject null), 5% level
fe_modrepm20 <- lm(rating_reputation ~ id.miller, data = gt20_mill)
summary(fe_modrepm20) #reject null, F-stat larger, reject 

tab_model(fe_interceptrepm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrepm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptrepm, fe_modrepm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modrepm, fe_modrepm20, show.se = TRUE, show.stat = TRUE)

###############################################################
##RATING OVERALL
fe_interceptoverallm <- lm(rating_overall ~ 1, data=ratings_mill)
summary(fe_interceptoverallm)
# 3.53166
fe_interceptoverallm20 <- lm(rating_overall ~ 1, data=gt20_mill)
summary(fe_interceptoverallm20) #3.54756 

fe_modoverallm <- lm(rating_overall ~ id.miller, data = ratings_mill)
summary(fe_modoverallm)
## reject null, individual means diff from overall mean
#F-statistic:2.251, 1.22 (F-table value)<F-stat (reject null), 5% level
fe_modoverallm20 <- lm(rating_overall ~ id.miller, data = gt20_mill)
summary(fe_modoverallm20)   #reject, F-stat larger, reject null

tab_model(fe_interceptoverallm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_interceptoverallm, fe_modoverallm, show.se = TRUE, show.stat = TRUE)
tab_model(fe_modoverallm, fe_modoverallm20, show.se = TRUE, show.stat = TRUE)


################################################################################################################
#regression without FE
install.packages("stargazer")
library(stargazer)

stargazer(lm(ratingoverall_diff ~ extension_training + seed_sale + seed_credit + dealer_fem + opv_sale + farmer_fem, data=merged_dealer), type = "text")
#extension training significant at 5%, intercept and seed credit significant at 1%

stargazer(lm(dealer_rating_overall ~ extension_training + seed_sale + seed_credit + dealer_fem + opv_sale, data=merged_dealer), type ="text")
#1% significance = extension training, seed credit, opv sale, intercept; dealer female at 10%

stargazer(lm(rating_overall ~ extension_training + seed_sale + seed_credit + dealer_fem + farmer_fem + opv_sale, data=merged_dealer), type="text")
#interecpt at 1%

##############################################################################################################
##MILLERS

###looking at ratings based on different aspects 

### Min quantity to be processed - yes/no
table(merged_miller$hh.maize.q23)

mean(merged_miller$rating_overall[merged_miller$hh.maize.q23=="Yes"])  # 3.571594
mean(merged_miller$rating_overall[merged_miller$hh.maize.q23=="No"])   # 3.504126

mean(merged_miller$rating_location[merged_miller$hh.maize.q23=="Yes"])  #3.811594
mean(merged_miller$rating_location[merged_miller$hh.maize.q23=="No"])  #3.824165

mean(merged_miller$rating_price[merged_miller$hh.maize.q23=="Yes"])  #  3.013043
mean(merged_miller$rating_price[merged_miller$hh.maize.q23=="No"])  #3.009823

mean(merged_miller$rating_quality[merged_miller$hh.maize.q23=="Yes"])  #3.523188
mean(merged_miller$rating_quality[merged_miller$hh.maize.q23=="No"])    #3.304519

mean(merged_miller$rating_service[merged_miller$hh.maize.q23=="Yes"])  #3.655072
mean(merged_miller$rating_service[merged_miller$hh.maize.q23=="No"])   # 3.584479

mean(merged_miller$rating_reputation[merged_miller$hh.maize.q23=="Yes"])  #3.855072
mean(merged_miller$rating_reputation[merged_miller$hh.maize.q23=="No"]) #3.797642

### Number of milling machines 
table(merged_miller$hh.maize.q26)

mean(merged_miller$rating_overall[merged_miller$hh.maize.q26>3])  # 3.448889
mean(merged_miller$rating_overall[merged_miller$hh.maize.q26<2])  #3.52244

mean(merged_miller$rating_location[merged_miller$hh.maize.q26>3])  #3.577778
mean(merged_miller$rating_location[merged_miller$hh.maize.q26<2])    #3.871988

mean(merged_miller$rating_price[merged_miller$hh.maize.q26>3])  #2.822222
mean(merged_miller$rating_price[merged_miller$hh.maize.q26<2])    #3.03238

mean(merged_miller$rating_quality[merged_miller$hh.maize.q26>3])  #3.533333
mean(merged_miller$rating_quality[merged_miller$hh.maize.q26<2])    #3.313253

mean(merged_miller$rating_service[merged_miller$hh.maize.q26>3])  #3.688889
mean(merged_miller$rating_service[merged_miller$hh.maize.q26<2])  #3.588855

mean(merged_miller$rating_reputation[merged_miller$hh.maize.q26>3])  # 3.622222
mean(merged_miller$rating_reputation[merged_miller$hh.maize.q26<2])    #3.805723

### Number of debranning machines 
table(merged_miller$hh.maize.q27)

mean(merged_miller$rating_overall[merged_miller$hh.maize.q27==1])  #3.530322
mean(merged_miller$rating_overall[merged_miller$hh.maize.q27==2])   #3.802198

mean(merged_miller$rating_location[merged_miller$hh.maize.q27==1])  #3.817933
mean(merged_miller$rating_location[merged_miller$hh.maize.q27==2])   #3.725275

mean(merged_miller$rating_price[merged_miller$hh.maize.q27==1])  #3.008898
mean(merged_miller$rating_price[merged_miller$hh.maize.q27==2])   # 3.197802

mean(merged_miller$rating_quality[merged_miller$hh.maize.q27==1])  #3.382615
mean(merged_miller$rating_quality[merged_miller$hh.maize.q27==2])  #4.153846

mean(merged_miller$rating_service[merged_miller$hh.maize.q27==1])  # 3.612594
mean(merged_miller$rating_service[merged_miller$hh.maize.q27==2])   #3.912088

mean(merged_miller$rating_reputation[merged_miller$hh.maize.q27==1])  # 3.829569
mean(merged_miller$rating_reputation[merged_miller$hh.maize.q27==2])  # 4.021978

### Discounts for processing large quantities  
table(merged_miller$hh.maize.q24)

mean(merged_miller$rating_overall[merged_miller$hh.maize.q24=="Yes"])  #3.542492
mean(merged_miller$rating_overall[merged_miller$hh.maize.q24=="No"])   #3.500877

mean(merged_miller$rating_location[merged_miller$hh.maize.q24=="Yes"])  #3.77476
mean(merged_miller$rating_location[merged_miller$hh.maize.q24=="No"])   #3.940789

mean(merged_miller$rating_price[merged_miller$hh.maize.q24=="Yes"])  #2.984824
mean(merged_miller$rating_price[merged_miller$hh.maize.q24=="No"])    #3.083333

mean(merged_miller$rating_quality[merged_miller$hh.maize.q24=="Yes"])  #3.480831
mean(merged_miller$rating_quality[merged_miller$hh.maize.q24=="No"])   #3.151316

mean(merged_miller$rating_service[merged_miller$hh.maize.q24=="Yes"])  #3.627796
mean(merged_miller$rating_service[merged_miller$hh.maize.q24=="No"])   #3.572368

mean(merged_miller$rating_reputation[merged_miller$hh.maize.q24=="Yes"])  #3.844249
mean(merged_miller$rating_reputation[merged_miller$hh.maize.q24=="No"])   # 3.756579






