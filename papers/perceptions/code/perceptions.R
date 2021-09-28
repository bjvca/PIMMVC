path <- getwd()

library(irrICC)
library(reshape2)
library(miceadds)

options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

################# FARMERS ######################

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

farmers1 <- farmers
farmers1[farmers1=="n/a"] <- NA 

farmers[farmers=="999"] <- NA

#dummy
farmers$gender <- 0
farmers$gender [farmers$hh.maize.q25=="Female"] <- 1 #female farmers
farmers$educ <- 0
farmers$educ[farmers$hh.maize.q27=="c" | farmers$hh.maize.q27=="d" | farmers$hh.maize.q27=="e" | 
               farmers$hh.maize.q27=="f" ] <- 1 #educated farmers
farmers$educ[ farmers$hh.maize.q27=="g" ] <- NA
farmers$married <- ifelse(farmers$hh.maize.q26 == 'a', 1, 0)  #married farmers 

################# INPUT DEALERS ######################

## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))

dealers1 <- dealers 

##Index for overall rating by dealers for themselves 
dealers1$ratee_rating_overall <- rowSums(dealers1[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers1$ratee_rating_overall)

dealers1[dealers1=="n/a"] <- NA

##Index for overall rating by dealers for themselves 
dealers$ratee_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$ratee_rating_overall)


################# TRADERS ######################

###Getting traders' data 
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))
traders[traders=="n/a"] <- NA
traders[traders=="999"] <- NA

#### SUMMARY STATS 
#22a. On a typical day after harvest, when maize prices have reached their lowest levels, how many sellers (farmers, assemblers,...) do you visit to collect maize from?
table(as.numeric(traders$hh.maize.q22a))
mean(as.numeric(traders$hh.maize.q22a), na.rm=TRUE)
sd(as.numeric(traders$hh.maize.q22a), na.rm=TRUE)
min(as.numeric(traders$hh.maize.q22a), na.rm=TRUE)
max(as.numeric(traders$hh.maize.q22a), na.rm=TRUE)
quantile(as.numeric(traders$hh.maize.q22a), na.rm=TRUE, 0.25)
quantile(as.numeric(traders$hh.maize.q22a), na.rm=TRUE, 0.75)

#22b. On a typical day after harvest, when maize prices have reached their lowest levels, how much maize do you collect in total. (in kg)
table(traders$hh.maize.q22b)
mean(traders$hh.maize.q22b)
#22d. On a typical day after harvest, when maize prices have reached their lowest levels, how many buyers do you deliver to
table(traders$hh.maize.q22d)
mean(as.numeric(traders$hh.maize.q22d), na.rm=TRUE)

#23a. During planting and growing season of second season of 2018 when maize prices have reached their highest levels, how many sellers (farmers, assemblers,...) do you visit to collect maize from?
table(traders$hh.maize.q23a)
mean(as.numeric(traders$hh.maize.q23a), na.rm=TRUE)  
#23b. On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels, how much maize do you collect in total.
table(traders$hh.maize.q23b)
mean(as.numeric(traders$hh.maize.q23b), na.rm=TRUE)
#23d. On a typical day during planting and growing season of second season of 2018 when maize prices have reached their highest levels, how many buyers do you deliver to (integer between 0 and 100)
table(traders$hh.maize.q23d)
mean(as.numeric(traders$hh.maize.q23d), na.rm=TRUE)

#28. What is your storage capacity (kgs)? 
table(traders$hh.maize.q28)
mean(as.numeric(traders$hh.maize.q28), na.rm=TRUE)


traders1<-traders 

###Index for overall rating from traders
traders1$ratee_rating_overall <- rowSums(traders1[c("hh.maize.q40a","hh.maize.q40b","hh.maize.q40c","hh.maize.q40d","hh.maize.q40e")])/5
summary(traders1$ratee_rating_overall)

traders1[traders1=="n/a"] <- NA

###Index for overall rating from traders
traders$ratee_rating_overall <- rowSums(traders[c("hh.maize.q40a","hh.maize.q40b","hh.maize.q40c","hh.maize.q40d","hh.maize.q40e")])/5
summary(traders$ratee_rating_overall)


################# MILLERS ######################

###Getting MILLERS' data 
millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"))
millers[millers=="n/a"] <- NA
millers[millers=="999"] <- NA

#### SUMMARY STATS
#14. What grades of flour do you produce? 
mean(as.numeric(millers$hh.maize.q14.1), na.rm=TRUE)
mean(as.numeric(millers$hh.maize.q14.2), na.rm=TRUE)
mean(as.numeric(millers$hh.maize.q14.3), na.rm=TRUE)
#What minimum quantity do you mill
mean(as.numeric(millers$hh.maize.q23a), na.rm=TRUE)
#26. How many milling machines do you have? 
mean(as.numeric(millers$hh.maize.q26), na.rm=TRUE)
#27. How many debranning machines do you have? 
mean(as.numeric(millers$hh.maize.q27), na.rm=TRUE)
#28. How is your milling machine powered?
mean(as.numeric(millers$hh.maize.q28), na.rm=TRUE)
millers$elec <- ifelse(millers$hh.maize.q28 == 'a', 1, 0)  #a=three phase electricity
mean(millers$elec)

millers1 <- millers

###Index for overall rating from millers
millers1$ratee_rating_overall <- rowSums(millers1[c("hh.maize.q36","hh.maize.q37","hh.maize.q38","hh.maize.q39","hh.maize.q40")])/5
summary(millers1$rateerating_overall)

millers1[millers1=="n/a"] <- NA

###Index for overall rating from millers
millers$ratee_rating_overall <- rowSums(millers[c("hh.maize.q36","hh.maize.q37","hh.maize.q38","hh.maize.q39","hh.maize.q40")])/5
summary(millers$ratee_rating_overall)

###############################################################################################
###############################################################################################
###############################################################################################


#### PREPPING POOLED DATASET ####


###############################################################################################
###############################################################################################
###############################################################################################

#############INPUT DEALERS#####################

##Prepping data
trans <- c("hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.agro1","hh.maize.q25","hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l","hh.maize.q24","hh.maize.q27"
                          ,"hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120")],"Yes")
names(stack1) <- c("farmerID","id.ratee", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation","age","education",
                   "tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell","total_maize_sold","storage_capacity","interaction")

stack2 <- cbind(farmers[c("ID","id.agro2","hh.maize.q25","hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l","hh.maize.agro2.q110","hh.maize.q24","hh.maize.q27"
                          ,"hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120")])
names(stack2) <- c("farmerID","id.ratee", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "interaction","age","education"
                   ,"tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell","total_maize_sold","storage_capacity")

stack3 <- cbind(farmers[c("ID","id.agro3","hh.maize.q25","hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l","hh.maize.agro3.q112","hh.maize.q24","hh.maize.q27"
                          ,"hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120")])
names(stack3) <- c("farmerID","id.ratee", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "interaction","age","education"
                   ,"tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell","total_maize_sold","storage_capacity")

ratings <-rbind(stack1,stack2,stack3)
ratings[c("id.ratee","interaction")] <- lapply(ratings[c("id.ratee","interaction")], function(x) as.factor(as.character(x)) )

##Subsetting 
ratings <- subset(ratings, !is.na(rating_reputation) )
ratings <- subset(ratings[!apply(ratings == "", 1, any),])
ratings <- ratings[c(!duplicated(ratings[,1:2])),] #removing duplicates 

which(duplicated(ratings)) #no duplicates

### simple average (Index for overall rating)
ratings$rating_overall <- rowSums(ratings[c("rating_location","rating_price","rating_quality","rating_stock","rating_reputation")])/5
summary(ratings$rating_overall)

ratings$interaction_yes <- ifelse(ratings$interaction == 'Yes', 1, 0) 

## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))
##Index for overall rating by dealers for themselves 
dealers$ratee_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$ratee_rating_overall)

dealers[dealers=="999"] <- NA

#dummies
dealers$client_service <- 0
dealers$client_service[dealers$hh.maize.q67=="2" | dealers$hh.maize.q67=="3" |
                         dealers$hh.maize.q68=="2" | dealers$hh.maize.q68=="3"] <- 1 #providing client service =1
dealers$gender <- ifelse(dealers$hh.maize.q7 == 'Female', 1, 0) #female dealers
dealers$educ <- 0
dealers$educ[dealers$hh.maize.q9=="c" | dealers$hh.maize.q9=="d" | dealers$hh.maize.q9=="e" | 
               dealers$hh.maize.q9=="f" ] <- 1 #educated dealers 
dealers$educ[dealers$hh.maize.q9=="g" ] <- NA
dealers$married <- ifelse(dealers$hh.maize.q8 == 'a', 1, 0)  #married dealers

#subset for merging 
dealers_pool <- subset(dealers, select = c('id.agro' , 'hh.maize.q7','hh.maize.q79','hh.maize.q80','hh.maize.q81','hh.maize.q83','ratee_rating_overall','client_service'
                                           ,'hh.maize.q6d','hh.maize.q6g','hh.maize.q8','hh.maize.q9'))
names(dealers_pool) <- c("id.ratee","gender_ratee","rating_location_ratee","rating_price_ratee","rating_quality_ratee","rating_reputation_ratee", 
                         "ratee_rating_overall","client_service","other_competitors","age_ratee","marital_status_ratee","education_ratee")

merged_dealer_pool <- merge(ratings,dealers_pool, by="id.ratee")
merged_dealer_pool[merged_dealer_pool=="999"] <- 0
merged_dealer_pool[merged_dealer_pool=="n/a"] <- NA
merged_dealer_pool$ratee_who <- 1 #1 if a dealer

############################### TRADERS #################################

### Prepping data for ratings 
trans <- c("hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j","hh.maize.trader1.q102k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j","hh.maize.trader2.q103k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j","hh.maize.trader3.q104k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.trader1","hh.maize.q25","hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j",
                          "hh.maize.trader1.q102k","hh.maize.q24","hh.maize.q27","hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37",
                          "hh.maize.q40","hh.maize.q95","hh.maize.q101","hh.maize.q101a","hh.maize.q120")],"Yes")
names(stack1) <- c("farmerID","id.ratee","farmer_gender","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation","age","education",
                   "tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell","total_maize_sold","storage_capacity","interaction")

stack2 <- cbind(farmers[c("ID","id.trader2","hh.maize.q25","hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j",
                          "hh.maize.trader2.q103k","hh.maize.q24","hh.maize.q27","hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120","hh.maize.trader2.q103l")])
names(stack2) <- c("farmerID","id.ratee","farmer_gender","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", 
                   "age","education","tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell",
                   "total_maize_sold","storage_capacity","interaction")

stack3 <- cbind(farmers[c("ID","id.trader3","hh.maize.q25","hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j",
                          "hh.maize.trader3.q104k","hh.maize.q24","hh.maize.q27","hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120","hh.maize.trader3.q104l")])
names(stack3) <- c("farmerID","id.ratee","farmer_gender","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", 
                   "age","education","tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell",
                   "total_maize_sold","storage_capacity","interaction")

ratings_trader <-rbind(stack1,stack2,stack3)
ratings_trader[c("id.ratee","interaction")] <- lapply(ratings_trader[c("id.ratee","interaction")], function(x) as.factor(as.character(x)) )

##subsetting and cleaning missing values 
ratings_trader <- subset(ratings_trader,!is.na(rating_reputation))
ratings_trader <- subset(ratings_trader[!apply(ratings_trader == "", 1, any),])
ratings_trader <- subset(ratings_trader[!apply(ratings_trader == ".", 1, any),])
table(ratings_trader=="")
ratings_trader <- ratings_trader[c(!duplicated(ratings_trader[,1:2])),] #removing duplicates 

### simple average of the ratings (index) --- from farmers
ratings_trader$rating_overall <- rowSums(ratings_trader[c("rating_location","rating_price","rating_quality","rating_honesty","rating_reputation")])/5
summary(ratings_trader$rating_overall)

ratings_trader$interaction_yes <- ifelse(ratings_trader$interaction == 'Yes', 1, 0)

###Getting traders' data 
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

###Index for overall rating from traders
traders$ratee_rating_overall <- rowSums(traders[c("hh.maize.q40a","hh.maize.q40b","hh.maize.q40c","hh.maize.q40d","hh.maize.q40e")])/5
summary(traders$ratee_rating_overall)

traders[traders=="999"] <- NA

#dummies 
traders$client_service <- 1
traders$client_service [traders$hh.maize.q30.g=="TRUE"]<- 0 #providing client service=1
traders$gender <- ifelse(traders$hh.maize.q7 == 'Female', 1, 0) #female traders 
traders$educ <- 0
traders$educ[ traders$hh.maize.q9=="c" | traders$hh.maize.q9=="d" | traders$hh.maize.q9=="e" | 
               traders$hh.maize.q9=="f" ] <- 1  #educated traders 
traders$educ[ traders$hh.maize.q9=="g" ] <- NA 
traders$married <- ifelse(traders$hh.maize.q8 == 'a', 1, 0) #married traders 

#subset for merging 
traders_pool <- subset(traders, select = c('id.trader' , 'hh.maize.q7','hh.maize.q40a','hh.maize.q40b','hh.maize.q40c','hh.maize.q40e','ratee_rating_overall','client_service'
                                           ,'hh.maize.q21','hh.maize.q6','hh.maize.q8','hh.maize.q9'))
names(traders_pool) <- c("id.ratee","gender_ratee","rating_location_ratee","rating_price_ratee","rating_quality_ratee","rating_reputation_ratee", 
                         "ratee_rating_overall","client_service","other_competitors","age_ratee","marital_status_ratee","education_ratee")

#Merging the datasets
merged_trader_pool <- merge(ratings_trader,traders_pool, by="id.ratee")
merged_trader_pool[merged_trader_pool=="999"] <- 0
merged_trader_pool[merged_trader_pool=="n/a"] <- NA

merged_trader_pool$ratee_who <- 2 #if trader, then 2

################### MILLERS ########################

###Prepping data for ratings 
trans <- c("hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.miller1","hh.maize.q25","hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k",
                          "hh.maize.q24","hh.maize.q27","hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120")],"Yes")
names(stack1) <- c("farmerID","id.ratee","farmer_gender","rating_location","rating_price","rating_quality","rating_service","rating_reputation","age","education"
                   ,"tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell","total_maize_sold","storage_capacity","interaction")

stack2 <- cbind(farmers[c("ID", "id.miller2","hh.maize.q25","hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k",
                          "hh.maize.q24","hh.maize.q27","hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120","hh.maize.miller2.q99l")])
names(stack2) <- c("farmerID","id.ratee","farmer_gender","rating_location","rating_price","rating_quality","rating_service","rating_reputation","age","education"
                   ,"tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell","total_maize_sold","storage_capacity","interaction")

stack3 <- cbind(farmers[c("ID","id.miller3","hh.maize.q25","hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k",
                          "hh.maize.q24","hh.maize.q27","hh.maize.q13","hh.maize.q14","hh.maize.q26","hh.maize.q29","hh.maize.q37","hh.maize.q40","hh.maize.q95","hh.maize.q101"
                          ,"hh.maize.q101a","hh.maize.q120","hh.maize.miller3.q100l")])
names(stack3) <- c("farmerID","id.ratee","farmer_gender","rating_location","rating_price","rating_quality","rating_service","rating_reputation","age","education"
                   ,"tarmac","murram","marital_status","source_income","member","number_plots","total_harvest","maize_sell","total_maize_sold","storage_capacity","interaction")

ratings_mill <-rbind(stack1,stack2,stack3)
ratings_mill[c("id.ratee","interaction")] <- lapply(ratings_mill[c("id.ratee","interaction")], function(x) as.factor(as.character(x)) )

##subsetting and cleaning missing values 
ratings_mill <- subset(ratings_mill, !is.na(rating_reputation) )
ratings_mill <- subset(ratings_mill[!apply(ratings_mill == "", 1, any),])
ratings_mill <- subset(ratings_mill[!apply(ratings_mill == ".", 1, any),])
table(ratings_mill=="")
ratings_mill <- ratings_mill[c(!duplicated(ratings_mill[,1:2])),] #removing duplicates 

### simple average of the ratings (index) --- from farmers
ratings_mill$rating_overall <- rowSums(ratings_mill[c("rating_location","rating_price","rating_quality","rating_service","rating_reputation")])/5
summary(ratings_mill$rating_overall)

ratings_mill$interaction_yes <- ifelse(ratings_mill$interaction == 'Yes', 1, 0)

###Getting millers' data 
millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"))

###Index for overall rating from millers
millers$ratee_rating_overall <- rowSums(millers[c("hh.maize.q36","hh.maize.q37","hh.maize.q38","hh.maize.q39","hh.maize.q40")])/5
summary(millers$ratee_rating_overall)

millers[millers=="999"] <- NA

#dummies 
millers$client_service <- 1
millers$client_service [millers$hh.maize.q25.k=="TRUE"] <- 0 #providing client service =1 
millers$gender <- ifelse(millers$hh.maize.q7 == 'Female', 1, 0) #female millers
millers$educ <- 0
millers$educ[ millers$hh.maize.q9=="c" | millers$hh.maize.q9=="d" | millers$hh.maize.q9=="e" | 
               millers$hh.maize.q9=="f"  ] <- 1 #educated millers 
millers$educ[  millers$hh.maize.q9=="g" ] <- NA
millers$married <- ifelse(millers$hh.maize.q8 == 'a', 1, 0)   #married millers 

#subset for merging 
millers_pool <- subset(millers, select = c('id.miller' , 'hh.maize.q7','hh.maize.q36','hh.maize.q37','hh.maize.q38','hh.maize.q40','ratee_rating_overall','client_service'
                                           ,'hh.maize.q6d','hh.maize.q6g','hh.maize.q8','hh.maize.q9'))
names(millers_pool) <- c("id.ratee","gender_ratee","rating_location_ratee","rating_price_ratee","rating_quality_ratee","rating_reputation_ratee", 
                         "ratee_rating_overall","client_service","other_competitors","age_ratee","marital_status_ratee","education_ratee")
#Merging the datasets
merged_miller_pool <- merge(ratings_mill,millers_pool, by="id.ratee")
merged_miller_pool[merged_miller_pool=="999"] <- 0
merged_miller_pool[merged_miller_pool=="n/a"] <- NA

merged_miller_pool$ratee_who  <- 3 #if miller, then 3 

#deleting the columns not needed
merged_dealer_pool <- merged_dealer_pool[-c(7)]
merged_trader_pool <- merged_trader_pool[-c(7)]
merged_miller_pool <- merged_miller_pool[-c(7)]

####################################
##### DATASET JUST FOR FARMERS #####
####################################

ratings_d <- ratings[-c(7)]
ratings_t <- ratings_trader[-c(7)]
ratings_m <- ratings_mill[-c(7)]

ratings_d$ratee_dummy <- 1
ratings_t$ratee_dummy <- 2
ratings_m$ratee_dummy <- 3

farmers_pool <-rbind(ratings_d,ratings_t,ratings_m)

farmers_pool$farmer_fem <- ifelse(farmers_pool$farmer_gender == 'Female', 1, 0) #gender dummy for farmers 
farmers_pool$interaction_yes <- ifelse(farmers_pool$interaction == 'Yes', 1, 0) #dummy for interaction between rater and ratee 
farmers_pool$educ <- 0
farmers_pool$educ[ farmers_pool$education=="c" | farmers_pool$education=="d" | farmers_pool$education=="e"
          | farmers_pool$education=="f" ] <- 1
farmers_pool$educ[ farmers_pool$education=="g" ] <- NA
#MARITAL STATUS
#a	Married /b	Widowed / c	Divorced / d	Separated / e	Single
farmers_pool$married <- ifelse(farmers_pool$marital_status == 'a', 1, 0) #dummy = 1 if married---- farmers 

#dummy for dealer id
farmers_pool$dealer_dummy <- ifelse(farmers_pool$ratee_dummy == '1', 1, 0) 
#dummy for trader id 
farmers_pool$trader_dummy <- ifelse(farmers_pool$ratee_dummy == '2', 1, 0) 
#dummy for miller id 
farmers_pool$miller_dummy <- ifelse(farmers_pool$ratee_dummy == '3', 1, 0) 

#cleaning the data
farmers_pool<-farmers_pool[!(farmers_pool$id.ratee=="n/a"),]
farmers_pool<-farmers_pool[!(farmers_pool$id.ratee=="."),]

####################################
##### DATASET JUST FOR RATEES ######
####################################

dealers_pool$which_ratee <- 1
traders_pool$which_ratee <- 2
millers_pool$which_ratee <- 3

ratee_pool <-rbind(dealers_pool,millers_pool,traders_pool)

ratee_pool$ratee_fem <- ifelse(ratee_pool$gender_ratee == 'Female', 1, 0)   #gender dummy for ratees 

ratee_pool$educ <- 0
ratee_pool$educ[ ratee_pool$education_ratee=="c" | ratee_pool$education_ratee=="d" | ratee_pool$education_ratee=="e"
          | ratee_pool$education_ratee=="f"  ] <- 1
ratee_pool$educ[  ratee_pool$education_ratee=="g" ] <- NA

#MARITAL STATUS
#a	Married /b	Widowed / c	Divorced / d	Separated / e	Single
ratee_pool$married_ratee <- ifelse(ratee_pool$marital_status_ratee == 'a', 1, 0) #dummy = 1 if married---- ratee

#dummy for dealer id
ratee_pool$dealer_dummy <- ifelse(ratee_pool$which_ratee == '1', 1, 0) 
#dummy for trader id 
ratee_pool$trader_dummy <- ifelse(ratee_pool$which_ratee == '2', 1, 0) 
#dummy for miller id 
ratee_pool$miller_dummy <- ifelse(ratee_pool$which_ratee == '3', 1, 0) 

#cleaning the data
ratee_pool<-ratee_pool[!(ratee_pool$id.ratee=="n/a"),]
ratee_pool<-ratee_pool[!(ratee_pool$id.ratee=="."),]

##################  FINAL DATASET = POOLED ########################

pool <-rbind(merged_dealer_pool,merged_miller_pool,merged_trader_pool)

pool$farmer_fem <- ifelse(pool$farmer_gender == 'Female', 1, 0) #gender dummy for farmers 
pool$ratee_fem <- ifelse(pool$gender_ratee == 'Female', 1, 0)   #gender dummy for ratees 
pool$interaction_yes <- ifelse(pool$interaction == 'Yes', 1, 0) #dummy for interaction between rater and ratee 

#a	No formal education / b	Some primary / c	Finished primary / d	Some secondary / e	Finished secondary / f	Higher than secondary / g Other
### Finished primary educ --- dummy == 1, otherwise 0
#farmers
pool$educ <- 0
pool$educ[pool$education=="c" | pool$education=="d" | pool$education=="e" | pool$education=="f" ] <- 1
pool$educ[ pool$education=="g" ] <- NA
#ratees
pool$educ_ratee <- 0
pool$educ_ratee[ pool$education_ratee=="c" | pool$education_ratee=="d" | 
                  pool$education_ratee=="e" | pool$education_ratee=="f" ] <- 1
pool$educ_ratee[  pool$education_ratee=="g" ] <- NA


#MARITAL STATUS
#a	Married /b	Widowed / c	Divorced / d	Separated / e	Single
pool$married <- ifelse(pool$marital_status == 'a', 1, 0) #dummy = 1 if married---- farmers 
pool$married_ratee <- ifelse(pool$marital_status_ratee == 'a', 1, 0) #dummy = 1 if married---- ratee

#dummy for dealer id
pool$dealer_dummy <- ifelse(pool$ratee_who == '1', 1, 0) 
#dummy for trader id 
pool$trader_dummy <- ifelse(pool$ratee_who == '2', 1, 0) 
#dummy for miller id 
pool$miller_dummy <- ifelse(pool$ratee_who == '3', 1, 0) 

#cleaning the data
pool<-pool[!(pool$id.ratee=="n/a"),]
pool<-pool[!(pool$id.ratee=="."),]

####################################################################################################
####################################################################################################

### CLUSTERED REGRESSIONS - LOOKING AT BOTH FARMERS' AND RATEES' GENDER ###

#################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS ##############
#################################################################

################# OVERALL RATING ###########################

#all variables 
mod1_gender<- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem + ratee_fem + age  + educ + tarmac
                         + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se1 <- sqrt(diag(vcov(mod1_gender)))
lm_res1<-mod1_gender$lm_res


##Interaction between sex of farmer and ratee
mod5_gender <- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem*ratee_fem + educ  + age + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se5 <- sqrt(diag(vcov(mod5_gender)))
lm_res5<-mod5_gender$lm_res


#saving regression
#screenreg(list(mod1_gender,mod5_gender), file="gen_overall_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod1_gender,mod5_gender), file="gen_overall_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# LOCATION RATING ###########################

#all variables 
mod11_gender<- lm.cluster(data = pool, formula = rating_location ~  farmer_fem + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy, cluster = "id.ratee") 
se11 <- sqrt(diag(vcov(mod11_gender)))
lm_res11<-mod11_gender$lm_res


##Interaction between sex of farmer and ratee
mod15_gender <- lm.cluster(data = pool, formula = rating_location ~  farmer_fem*ratee_fem + educ  + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy, cluster = "id.ratee") 
se15 <- sqrt(diag(vcov(mod15_gender)))
lm_res15<-mod15_gender$lm_res


#saving regression
#screenreg(list(mod11_gender,mod15_gender), file="gen_loc_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod11_gender,mod15_gender), file="gen_loc_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# QUALITY RATING ###########################

#all variables 
mod21_gender<- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se21 <- sqrt(diag(vcov(mod21_gender)))
lm_res21<-mod21_gender$lm_res


##Interaction between sex of farmer and ratee
mod25_gender <- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem*ratee_fem + educ  + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se25 <- sqrt(diag(vcov(mod25_gender)))
lm_res25<-mod25_gender$lm_res


#saving regression
#screenreg(list(mod21_gender,mod25_gender), file="gen_qual_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod21_gender,mod25_gender), file="gen_qual_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# PRICE RATING ###########################

#all variables 
mod31_gender<- lm.cluster(data = pool, formula = rating_price ~  farmer_fem + ratee_fem + age + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se31 <- sqrt(diag(vcov(mod31_gender)))
lm_res31<-mod31_gender$lm_res


##Interaction between sex of farmer and ratee
mod35_gender <- lm.cluster(data = pool, formula = rating_price ~  farmer_fem*ratee_fem + educ  + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se35 <- sqrt(diag(vcov(mod35_gender)))
lm_res35<-mod35_gender$lm_res


#saving regression
#screenreg(list(mod31_gender,mod35_gender), file="gen_price_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod31_gender,mod35_gender), file="gen_price_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# REPUTATION RATING ###########################

#all variables 
mod41_gender<- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se41 <- sqrt(diag(vcov(mod41_gender)))
lm_res41<-mod41_gender$lm_res


##Interaction between sex of farmer and ratee
mod45_gender <- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem*ratee_fem + educ  + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se45 <- sqrt(diag(vcov(mod45_gender)))
lm_res45<-mod45_gender$lm_res

#saving regression
#screenreg(list(mod41_gender,mod45_gender), file="gen_reputation_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod41_gender,mod45_gender), file="gen_reputation_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)


#########################################################################################################################
#########################################################################################################################


### CLUSTERED REGRESSIONS - LOOKING AT ONLY FARMERS' GENDER ###

#################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS ##############
#################################################################

################# OVERALL RATING ###########################

#all variables 
farm1<- lm.cluster(data = farmers_pool, formula = rating_overall ~  farmer_fem  + age  + educ + tarmac
                         + murram + married + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm1 <- sqrt(diag(vcov(farm1)))
res_farm1<-farm1$lm_res

################# LOCATION RATING ###########################

#all variables 
farm2<- lm.cluster(data = farmers_pool, formula = rating_location ~  farmer_fem  + age  + educ + tarmac
                   + murram + married + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm2 <- sqrt(diag(vcov(farm2)))
res_farm2<-farm2$lm_res

################# QUALITY RATING ###########################

#all variables 
farm3<- lm.cluster(data = farmers_pool, formula = rating_quality ~  farmer_fem  + age  + educ + tarmac
                   + murram + married + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm3 <- sqrt(diag(vcov(farm3)))
res_farm3<-farm3$lm_res

################# PRICE RATING ###########################

#all variables 
farm4<- lm.cluster(data = farmers_pool, formula = rating_price ~  farmer_fem  + age  + educ + tarmac
                   + murram + married + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm4 <- sqrt(diag(vcov(farm4)))
res_farm4<-farm4$lm_res

################# REPUTATION RATING ###########################

#all variables 
farm5<- lm.cluster(data = farmers_pool, formula = rating_reputation ~  farmer_fem  + age + educ + tarmac
                   + murram + married + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm5 <- sqrt(diag(vcov(farm5)))
res_farm5<-farm5$lm_res

#########################################################################################################################
#########################################################################################################################

### CLUSTERED REGRESSIONS - LOOKING AT ONLY RATEES' GENDER ###

#################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS ##############
#################################################################

################# OVERALL RATING ###########################

#all variables 
farm6<- lm.cluster(data = pool, formula = rating_overall ~  ratee_fem  + age  + educ + tarmac
                   + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm6 <- sqrt(diag(vcov(farm6)))
res_farm6<-farm6$lm_res

################# LOCATION RATING ###########################

#all variables 
farm7<- lm.cluster(data = pool, formula = rating_location ~  ratee_fem  + age  + educ + tarmac
                   + murram + married +  age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm7 <- sqrt(diag(vcov(farm7)))
res_farm7<-farm7$lm_res

################# QUALITY RATING ###########################

#all variables 
farm8<- lm.cluster(data = pool, formula = rating_quality ~  ratee_fem  + age + educ + tarmac
                   + murram + married +  age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm8 <- sqrt(diag(vcov(farm8)))
res_farm8<-farm8$lm_res

################# PRICE RATING ###########################

#all variables 
farm9<- lm.cluster(data = pool, formula = rating_price ~  ratee_fem  + age  + educ + tarmac
                   + murram + married +  age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm9 <- sqrt(diag(vcov(farm9)))
res_farm9<-farm9$lm_res

################# REPUTATION RATING ###########################

#all variables 
farm10<- lm.cluster(data = pool, formula = rating_reputation ~  ratee_fem  + age  + educ + tarmac
                    + murram + married +  age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se_farm10 <- sqrt(diag(vcov(farm10)))
res_farm10<-farm10$lm_res

#########################################################################################################################
#########################################################################################################################


####### DEPENDENT VARIABLE -- RATINGS FROM RATEES -- DATASET ONLY HAVING THE RATEES ##############
##################################################################################################

################# OVERALL RATING #################################################################

ratee1<- lm(data = ratee_pool, formula = ratee_rating_overall ~ ratee_fem + age_ratee + married_ratee + educ
                  + dealer_dummy + trader_dummy) 
se_ratee1 <- sqrt(diag(vcov(ratee1)))

#all variables 
mod51_gender<- lm(data = pool, formula = ratee_rating_overall ~ ratee_fem + age_ratee + married_ratee + educ_ratee
                  + dealer_dummy + trader_dummy) 
se51 <- sqrt(diag(vcov(mod51_gender)))

#saving regression
#screenreg(list(mod51_gender), file="gen_overall_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod51_gender), file="gen_overall_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)

################# LOCATION RATING ###########################

ratee2<- lm(data = ratee_pool, formula = rating_location_ratee ~ ratee_fem + age_ratee + married_ratee + educ
            + dealer_dummy + trader_dummy) 
se_ratee2 <- sqrt(diag(vcov(ratee2)))

#all variables 
mod55_gender<- lm(data = pool, formula = rating_location_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                  + dealer_dummy + trader_dummy ) 
se55 <- sqrt(diag(vcov(mod55_gender)))

#saving regression
#screenreg(list(mod55_gender), file="gen_location_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod55_gender), file="gen_location_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)

################# QUALITY RATING ###########################

ratee3<- lm(data = ratee_pool, formula = rating_quality_ratee ~ ratee_fem + age_ratee + married_ratee + educ
            + dealer_dummy + trader_dummy) 
se_ratee3 <- sqrt(diag(vcov(ratee3)))

#all variables 
mod59_gender<- lm(data = pool, formula = rating_quality_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                  + dealer_dummy + trader_dummy) 
se59<- sqrt(diag(vcov(mod59_gender)))

#saving regression
#screenreg(list(mod59_gender), file="gen_quality_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod59_gender), file="gen_quality_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)

################# PRICE RATING ###########################

ratee4<- lm(data = ratee_pool, formula = rating_price_ratee ~ ratee_fem + age_ratee + married_ratee + educ
            + dealer_dummy + trader_dummy) 
se_ratee4 <- sqrt(diag(vcov(ratee4)))

#all variables 
mod63_gender<- lm(data = pool, formula = rating_price_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee
                  + dealer_dummy + trader_dummy ) 
se63 <- sqrt(diag(vcov(mod63_gender)))

#saving regression
#screenreg(list(mod63_gender), file="gen_price_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod63_gender), file="gen_price_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# REPUTATION RATING ###########################

ratee5<- lm(data = ratee_pool, formula = rating_reputation_ratee ~ ratee_fem + age_ratee + married_ratee + educ
            + dealer_dummy + trader_dummy) 
se_ratee5 <- sqrt(diag(vcov(ratee5)))

#all variables 
mod67_gender<- lm(data = pool, formula = rating_reputation_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                  + dealer_dummy + trader_dummy ) 
se67 <- sqrt(diag(vcov(mod67_gender)))

#saving regression
#screenreg(list(mod67_gender), file="gen_reputation_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod67_gender), file="gen_reputation_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)

################################################################################################################
################################################################################################################

####### DEPENDENT VARIABLE -- DIFFERENCES IN RATINGS (RATER RATING - RATEE RATING) ##############
#################################################################################################

#differences in ratings 
# pool$ratingoverall_diff <- pool$rating_overall - pool$ratee_rating_overall #difference in overall ratings
# pool$ratingloc_diff <- pool$rating_location - pool$rating_location_ratee ##difference in location ratings
# pool$ratingprice_diff <- pool$rating_price - pool$rating_price_ratee ##difference in price ratings
# pool$ratingqual_diff <- pool$rating_quality - pool$rating_quality_ratee ##difference in quality ratings
# pool$ratingrepu_diff <- pool$rating_reputation - pool$rating_reputation_ratee ##difference in reputation ratings

pool$ratingoverall_diff <- pool$ratee_rating_overall - pool$rating_overall #difference in overall ratings
pool$ratingloc_diff <- pool$rating_location_ratee - pool$rating_location ##difference in location ratings
pool$ratingprice_diff <- pool$rating_price_ratee - pool$rating_price ##difference in price ratings
pool$ratingqual_diff <- pool$rating_quality_ratee - pool$rating_quality  ##difference in quality ratings
pool$ratingrepu_diff <- pool$rating_reputation_ratee - pool$rating_reputation ##difference in reputation ratings

################# OVERALL RATING ###########################

#all variables 
mod71_gender<- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem + ratee_fem + age + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se71 <- sqrt(diag(vcov(mod71_gender)))
lm_res71<-mod71_gender$lm_res

##Interaction between sex of farmer and ratee
mod75_gender <- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem*ratee_fem + educ + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se75 <- sqrt(diag(vcov(mod75_gender)))
lm_res75<-mod75_gender$lm_res

#saving regression
#screenreg(list(mod71_gender, mod75_gender), file="gen_overall_diff", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod71_gender, mod75_gender), file="gen_overall_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)

################# LOCATION RATING ###########################

#all variables 
mod78_gender<- lm.cluster(data = pool, formula = ratingloc_diff ~  farmer_fem + ratee_fem + age +  educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se78 <- sqrt(diag(vcov(mod78_gender)))
lm_res78<-mod78_gender$lm_res

##Interaction between sex of farmer and ratee
mod82_gender <- lm.cluster(data = pool, formula = ratingloc_diff ~  farmer_fem*ratee_fem + educ + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se82 <- sqrt(diag(vcov(mod82_gender)))
lm_res82<-mod82_gender$lm_res

#saving regression
#screenreg(list(mod78_gender,mod82_gender), file="gen_loc_diff", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod78_gender, mod82_gender), file="gen_loc_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# PRICE RATING ###########################

#all variables 
mod85_gender<- lm.cluster(data = pool, formula = ratingprice_diff ~  farmer_fem + ratee_fem + age + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se85 <- sqrt(diag(vcov(mod85_gender)))
lm_res85<-mod85_gender$lm_res

##Interaction between sex of farmer and ratee
mod89_gender <- lm.cluster(data = pool, formula = ratingprice_diff ~  farmer_fem*ratee_fem + educ + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se89 <- sqrt(diag(vcov(mod89_gender)))
lm_res89<-mod89_gender$lm_res

#saving regression
#screenreg(list(mod85_gender, mod89_gender), file="gen_price_diff", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod85_gender,mod89_gender), file="gen_price_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# QUALITY RATING ###########################

#all variables 
mod92_gender<- lm.cluster(data = pool, formula = ratingqual_diff ~  farmer_fem + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se92 <- sqrt(diag(vcov(mod92_gender)))
lm_res92<-mod92_gender$lm_res

##Interaction between sex of farmer and ratee
mod96_gender <- lm.cluster(data = pool, formula = ratingqual_diff ~  farmer_fem*ratee_fem + educ + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se96 <- sqrt(diag(vcov(mod96_gender)))
lm_res96<-mod96_gender$lm_res

#saving regression
#screenreg(list(mod92_gender, mod96_gender), file="gen_qual_diff", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod92_gender, mod96_gender), file="gen_qual_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)


###################### REPUTATION RATING #########################

#all variables 
mod99_gender<- lm.cluster(data = pool, formula = ratingrepu_diff ~  farmer_fem + ratee_fem + age + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se99 <- sqrt(diag(vcov(mod99_gender)))
lm_res99<-mod99_gender$lm_res

##Interaction between sex of farmer and ratee
mod103_gender <- lm.cluster(data = pool, formula = ratingrepu_diff ~  farmer_fem*ratee_fem + educ + age + tarmac
                            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se103 <- sqrt(diag(vcov(mod103_gender)))
lm_res103<-mod103_gender$lm_res

#saving regression
#screenreg(list(mod99_gender, mod103_gender), file="gen_repu_diff", stars = c(0.01, 0.05, 0.1), digits=4)
#texreg(list(mod99_gender,mod103_gender), file="gen_repu_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)

###########################################################################
###########################################################################
###########################################################################
###########################################################################

############################ MEAN RATINGS ######################################

options(scipen=99)

###### HYPOTHESIS 1 : "Self-ratings are higher than ratings given by raters to ratees" #######

## MEAN RATINGS --- ALL raters and ratees 
mean(pool$ratee_rating_overall, na.rm=T)
mean(pool$rating_overall, na.rm=T)

mean(pool$rating_location_ratee, na.rm=T)
mean(pool$rating_location, na.rm=T)

mean(pool$rating_quality_ratee, na.rm=T)
mean(pool$rating_quality, na.rm=T)

mean(pool$rating_price_ratee, na.rm=T)
mean(pool$rating_price, na.rm=T)

mean(pool$rating_reputation_ratee, na.rm=T)
mean(pool$rating_reputation, na.rm=T)

## MEAN RATINGS --- ALL raters and input dealers 
mean(pool$ratee_rating_overall [pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_location_ratee[pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_quality_ratee[pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_price_ratee[pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_reputation_ratee[pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_reputation[pool$dealer_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and traders 
mean(pool$ratee_rating_overall [pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_location_ratee[pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_quality_ratee[pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_price_ratee[pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_reputation_ratee[pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_reputation[pool$trader_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and millers
mean(pool$ratee_rating_overall [pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_location_ratee[pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_quality_ratee[pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_price_ratee[pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_reputation_ratee[pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_reputation[pool$miller_dummy=="1"], na.rm=T)


########## HYPOTHESIS 2: "Women give higher ratings than men" ##############

## MEAN RATINGS --- ALL raters and ratees --- raters are women
mean(pool$rating_overall [pool$farmer_fem=="1"], na.rm=T)
mean(pool$rating_overall[pool$farmer_fem=="0"], na.rm=T)

mean(pool$rating_location [pool$farmer_fem=="1"], na.rm=T)
mean(pool$rating_location[pool$farmer_fem=="0"], na.rm=T)

mean(pool$rating_price [pool$farmer_fem=="1"], na.rm=T)
mean(pool$rating_price[pool$farmer_fem=="0"], na.rm=T)

mean(pool$rating_quality [pool$farmer_fem=="1"], na.rm=T)
mean(pool$rating_quality[pool$farmer_fem=="0"], na.rm=T)

mean(pool$rating_reputation [pool$farmer_fem=="1"], na.rm=T)
mean(pool$rating_reputation [pool$farmer_fem=="0"], na.rm=T)

## MEAN RATINGS --- ALL raters and input dealers --- raters are women

mean(pool$rating_overall [pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_location [pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_price [pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_quality [pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_reputation [pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_reputation [pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and traders --- raters are women

mean(pool$rating_overall [pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_location [pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_price [pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_quality [pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_reputation [pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_reputation [pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and millers --- raters are women

mean(pool$rating_overall [pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_location [pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_price [pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_quality [pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_reputation [pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_reputation [pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T)


########## HYPOTHESIS 3: "Women self-assess themselves less favourably" ##############

## MEAN RATINGS --- ALL raters and ratees --- ratees are women and men

mean(pool$ratee_rating_overall [pool$ratee_fem=="1"], na.rm=T)
mean(pool$ratee_rating_overall[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_location_ratee [pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_location_ratee[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_price_ratee [pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_price_ratee[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_quality_ratee [pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_quality_ratee[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_reputation_ratee [pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_reputation_ratee [pool$ratee_fem=="0"], na.rm=T)

## MEAN RATINGS --- ALL raters and input dealers --- ratees are women and men

mean(pool$ratee_rating_overall [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$ratee_rating_overall[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_location_ratee [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_location_ratee[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_price_ratee [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_price_ratee[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_quality_ratee [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_quality_ratee[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_reputation_ratee [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_reputation_ratee [pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and traders --- ratees are women and men

mean(pool$ratee_rating_overall [pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$ratee_rating_overall[pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_location_ratee [pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_location_ratee[pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_price_ratee [pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_price_ratee [pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_quality_ratee [pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_quality_ratee[pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_reputation_ratee [pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_reputation_ratee [pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and millers --- ratees are women and men

mean(pool$ratee_rating_overall [pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$ratee_rating_overall[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_location_ratee [pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_location_ratee[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_price_ratee [pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_price_ratee[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_quality_ratee [pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_quality_ratee[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_reputation_ratee [pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_reputation_ratee [pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)


########## HYPOTHESIS 4: " Female ratees receive lower ratings from raters than male ratees. " ##############

## MEAN RATINGS --- ALL raters and ratees --- ratees are women and men

mean(pool$rating_overall [pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_overall[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_location[pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_location[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_price[pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_price[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_quality [pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_quality[pool$ratee_fem=="0"], na.rm=T)

mean(pool$rating_reputation [pool$ratee_fem=="1"], na.rm=T)
mean(pool$rating_reputation [pool$ratee_fem=="0"], na.rm=T)


## MEAN RATINGS --- ALL raters and input dealers --- ratees are women and men

mean(pool$rating_overall [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_location [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_price [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_quality [pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

mean(pool$rating_reputation[pool$ratee_fem=="1" & pool$dealer_dummy=="1"], na.rm=T)
mean(pool$rating_reputation[pool$ratee_fem=="0" & pool$dealer_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and traders --- ratees are women and men

mean(pool$rating_overall [pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_location [pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_price[pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_price [pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_quality[pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

mean(pool$rating_reputation[pool$ratee_fem=="1" & pool$trader_dummy=="1"], na.rm=T)
mean(pool$rating_reputation[pool$ratee_fem=="0" & pool$trader_dummy=="1"], na.rm=T)

## MEAN RATINGS --- ALL raters and millers --- ratees are women and men

mean(pool$rating_overall [pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_overall[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_location [pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_location[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_price[pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_price[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_quality[pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_quality[pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)

mean(pool$rating_reputation[pool$ratee_fem=="1" & pool$miller_dummy=="1"], na.rm=T)
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$miller_dummy=="1"], na.rm=T)


########## HYPOTHESIS 5: "The impact of both rater (farmer) and ratee being a woman is significant on the ratings" ##############

##MEAN RATINGS ---- ALL RATEES 

#overall
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="0"], na.rm=T) #both men
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="1"], na.rm=T) #both women
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="0"], na.rm=T) #female ratee, male farmer
#location
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="0"], na.rm=T) #both men
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="1"], na.rm=T) #both women
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="0"], na.rm=T) #female ratee, male farmer
#quality
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="0"], na.rm=T) #both men
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="1"], na.rm=T) #both women
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="0"], na.rm=T) #female ratee, male farmer
#price
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="0"], na.rm=T) #both men
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="1"], na.rm=T) #both women
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="0"], na.rm=T) #female ratee, male farmer
#reputation
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="0"], na.rm=T) #both men
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="1"], na.rm=T) #both women
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="0"], na.rm=T) #female ratee, male farmer


##MEAN RATINGS ---- only input dealers 

#overall
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #both men
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #both women
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #female ratee, male farmer
#location
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #both men
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #both women
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #female ratee, male farmer
#quality
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #both men
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #both women
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #female ratee, male farmer
#price
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #both men
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #both women
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #female ratee, male farmer
#reputation
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #both men
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #both women
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$dealer_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$dealer_dummy=="1"], na.rm=T) #female ratee, male farmer


##MEAN RATINGS ---- only traders  

#overall
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #both men
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #both women
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #female ratee, male farmer
#location
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #both men
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #both women
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #female ratee, male farmer
#quality
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #both men
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #both women
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #female ratee, male farmer
#price
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #both men
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #both women
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #female ratee, male farmer
#reputation
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #both men
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #both women
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$trader_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$trader_dummy=="1"], na.rm=T) #female ratee, male farmer


##MEAN RATINGS ---- only millers 

#overall
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #both men
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #both women
mean(pool$rating_overall [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_overall [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #female ratee, male farmer
#location
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #both men
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #both women
mean(pool$rating_location [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_location [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #female ratee, male farmer
#quality
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #both men
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #both women
mean(pool$rating_quality [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_quality [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #female ratee, male farmer
#price
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #both men
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #both women
mean(pool$rating_price [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_price [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #female ratee, male farmer
#reputation
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #both men
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #both women
mean(pool$rating_reputation [pool$ratee_fem=="0" & pool$farmer_fem=="1" & pool$miller_dummy=="1"], na.rm=T) #male ratee, female farmer
mean(pool$rating_reputation [pool$ratee_fem=="1" & pool$farmer_fem=="0" & pool$miller_dummy=="1"], na.rm=T) #female ratee, male farmer

##########################################################################################################################
##########################################################################################################################



#### SUMMARY STATS (MAIN VARIABLES) - FARMERS ####

#mean
round(mean(pool$rating_overall, na.rm=T), digits=2)
round(mean(pool$rating_location, na.rm=T), digits=2)
round(mean(pool$rating_quality, na.rm=T), digits=2)
round(mean(pool$rating_price, na.rm=T), digits=2)
round(mean(pool$rating_reputation, na.rm=T), digits=2)

round(mean(ratings$rating_overall, na.rm=T), digits=2)
round(mean(ratings$rating_location, na.rm=T), digits=2)
round(mean(ratings$rating_quality, na.rm=T), digits=2)
round(mean(ratings$rating_price, na.rm=T), digits=2)
round(mean(ratings$rating_reputation, na.rm=T), digits=2)

round(mean(ratings_trader$rating_overall, na.rm=T), digits=2)
round(mean(ratings_trader$rating_location, na.rm=T), digits=2)
round(mean(ratings_trader$rating_quality, na.rm=T), digits=2)
round(mean(ratings_trader$rating_price, na.rm=T), digits=2)
round(mean(ratings_trader$rating_reputation, na.rm=T), digits=2)

round(mean(ratings_mill$rating_overall, na.rm=T), digits=2)
round(mean(ratings_mill$rating_location, na.rm=T), digits=2)
round(mean(ratings_mill$rating_quality, na.rm=T), digits=2)
round(mean(ratings_mill$rating_price, na.rm=T), digits=2)
round(mean(ratings_mill$rating_reputation, na.rm=T), digits=2)

round(mean(farmers$gender, na.rm=T), digits=2)
round(mean(farmers$hh.maize.q24, na.rm=T), digits=2) #age 
round(mean(farmers$educ, na.rm=T), digits=2)
round(mean(farmers$married, na.rm=T), digits=2)
round(mean(farmers$hh.maize.q13, na.rm=T), digits=2) #distance of homestead to tarmac road 
round(mean(farmers$hh.maize.q14, na.rm=T), digits=2) #distance of homestead to murram road 
round(mean(pool$interaction_yes, na.rm=T), digits=2)  #interaction between farmers and all ratees 
round(mean(ratings$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and dealers
round(mean(ratings_trader$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and traders
round(mean(ratings_mill$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and millers


#standard deviation 
round(sd(pool$rating_overall, na.rm=T), digits=2)
round(sd(pool$rating_location, na.rm=T), digits=2)
round(sd(pool$rating_quality, na.rm=T), digits=2)
round(sd(pool$rating_price, na.rm=T), digits=2)
round(sd(pool$rating_reputation, na.rm=T), digits=2)

round(sd(ratings$rating_overall, na.rm=T), digits=2)
round(sd(ratings$rating_location, na.rm=T), digits=2)
round(sd(ratings$rating_quality, na.rm=T), digits=2)
round(sd(ratings$rating_price, na.rm=T), digits=2)
round(sd(ratings$rating_reputation, na.rm=T), digits=2)

round(sd(ratings_trader$rating_overall, na.rm=T), digits=2)
round(sd(ratings_trader$rating_location, na.rm=T), digits=2)
round(sd(ratings_trader$rating_quality, na.rm=T), digits=2)
round(sd(ratings_trader$rating_price, na.rm=T), digits=2)
round(sd(ratings_trader$rating_reputation, na.rm=T), digits=2)

round(sd(ratings_mill$rating_overall, na.rm=T), digits=2)
round(sd(ratings_mill$rating_location, na.rm=T), digits=2)
round(sd(ratings_mill$rating_quality, na.rm=T), digits=2)
round(sd(ratings_mill$rating_price, na.rm=T), digits=2)
round(sd(ratings_mill$rating_reputation, na.rm=T), digits=2)

round(sd(farmers$gender, na.rm=T), digits=2)
round(sd(farmers$hh.maize.q24, na.rm=T), digits=2) #age 
round(sd(farmers$educ, na.rm=T), digits=2)
round(sd(farmers$married, na.rm=T), digits=2)
round(sd(farmers$hh.maize.q13, na.rm=T), digits=2) #distance of homestead to tarmac road 
round(sd(farmers$hh.maize.q14, na.rm=T), digits=2) #distance of homestead to murram road 
round(sd(pool$interaction_yes, na.rm=T), digits=2)  #interaction between farmers and all ratees 
round(sd(ratings$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and dealers
round(sd(ratings_trader$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and traders
round(sd(ratings_mill$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and millers

#Minimum
round(min(pool$rating_overall, na.rm=T), digits=2)
round(min(pool$rating_location, na.rm=T), digits=2)
round(min(pool$rating_quality, na.rm=T), digits=2)
round(min(pool$rating_price, na.rm=T), digits=2)
round(min(pool$rating_reputation, na.rm=T), digits=2)

round(min(ratings$rating_overall, na.rm=T), digits=2)
round(min(ratings$rating_location, na.rm=T), digits=2)
round(min(ratings$rating_quality, na.rm=T), digits=2)
round(min(ratings$rating_price, na.rm=T), digits=2)
round(min(ratings$rating_reputation, na.rm=T), digits=2)

round(min(ratings_trader$rating_overall, na.rm=T), digits=2)
round(min(ratings_trader$rating_location, na.rm=T), digits=2)
round(min(ratings_trader$rating_quality, na.rm=T), digits=2)
round(min(ratings_trader$rating_price, na.rm=T), digits=2)
round(min(ratings_trader$rating_reputation, na.rm=T), digits=2)

round(min(ratings_mill$rating_overall, na.rm=T), digits=2)
round(min(ratings_mill$rating_location, na.rm=T), digits=2)
round(min(ratings_mill$rating_quality, na.rm=T), digits=2)
round(min(ratings_mill$rating_price, na.rm=T), digits=2)
round(min(ratings_mill$rating_reputation, na.rm=T), digits=2)

round(min(farmers$gender, na.rm=T), digits=2)
round(min(farmers$hh.maize.q24, na.rm=T), digits=2) #age 
round(min(farmers$educ, na.rm=T), digits=2)
round(min(farmers$married, na.rm=T), digits=2)
round(min(farmers$hh.maize.q13, na.rm=T), digits=2) #distance of homestead to tarmac road 
round(min(farmers$hh.maize.q14, na.rm=T), digits=2) #distance of homestead to murram road 
round(min(pool$interaction_yes, na.rm=T), digits=2)  #interaction between farmers and all ratees 
round(min(ratings$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and dealers
round(min(ratings_trader$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and traders
round(min(ratings_mill$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and millers

#maximum
round(max(pool$rating_overall, na.rm=T), digits=2)
round(max(pool$rating_location, na.rm=T), digits=2)
round(max(pool$rating_quality, na.rm=T), digits=2)
round(max(pool$rating_price, na.rm=T), digits=2)
round(max(pool$rating_reputation, na.rm=T), digits=2)

round(max(ratings$rating_overall, na.rm=T), digits=2)
round(max(ratings$rating_location, na.rm=T), digits=2)
round(max(ratings$rating_quality, na.rm=T), digits=2)
round(max(ratings$rating_price, na.rm=T), digits=2)
round(max(ratings$rating_reputation, na.rm=T), digits=2)

round(max(ratings_trader$rating_overall, na.rm=T), digits=2)
round(max(ratings_trader$rating_location, na.rm=T), digits=2)
round(max(ratings_trader$rating_quality, na.rm=T), digits=2)
round(max(ratings_trader$rating_price, na.rm=T), digits=2)
round(max(ratings_trader$rating_reputation, na.rm=T), digits=2)

round(max(ratings_mill$rating_overall, na.rm=T), digits=2)
round(max(ratings_mill$rating_location, na.rm=T), digits=2)
round(max(ratings_mill$rating_quality, na.rm=T), digits=2)
round(max(ratings_mill$rating_price, na.rm=T), digits=2)
round(max(ratings_mill$rating_reputation, na.rm=T), digits=2)

round(max(farmers$gender, na.rm=T), digits=2)
round(max(farmers$hh.maize.q24, na.rm=T), digits=2) #age 
round(max(farmers$educ, na.rm=T), digits=2)
round(max(farmers$married, na.rm=T), digits=2)
round(max(farmers$hh.maize.q13, na.rm=T), digits=2) #distance of homestead to tarmac road 
round(max(farmers$hh.maize.q14, na.rm=T), digits=2) #distance of homestead to murram road 
round(max(pool$interaction_yes, na.rm=T), digits=2)  #interaction between farmers and all ratees 
round(max(ratings$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and dealers
round(max(ratings_trader$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and traders
round(max(ratings_mill$interaction_yes, na.rm=T), digits=2)   #interaction between farmers and millers


#### SUMMARY STATS (MAIN VARIABLES) - RATEES ####

#DEALERS
round(mean(dealers$ratee_rating_overall, na.rm=T), digits=2)
round(mean(dealers$hh.maize.q79, na.rm=T), digits=2) #location
round(mean(dealers$hh.maize.q80, na.rm=T), digits=2) #price
round(mean(dealers$hh.maize.q81, na.rm=T), digits=2) #quality
round(mean(dealers$hh.maize.q83, na.rm=T), digits=2)  #reputation
round(mean(dealers$gender, na.rm=T), digits=2)
round(mean(dealers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(mean(dealers$educ, na.rm=T), digits=2)
round(mean(dealers$married, na.rm=T), digits=2)

round(sd(dealers$ratee_rating_overall, na.rm=T), digits=2)
round(sd(dealers$hh.maize.q79, na.rm=T), digits=2) #location
round(sd(dealers$hh.maize.q80, na.rm=T), digits=2) #price
round(sd(dealers$hh.maize.q81, na.rm=T), digits=2) #quality
round(sd(dealers$hh.maize.q83, na.rm=T), digits=2)  #reputation
round(sd(dealers$gender, na.rm=T), digits=2)
round(sd(dealers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(sd(dealers$educ, na.rm=T), digits=2)
round(sd(dealers$married, na.rm=T), digits=2)

round(min(dealers$ratee_rating_overall, na.rm=T), digits=2)
round(min(dealers$hh.maize.q79, na.rm=T), digits=2) #location
round(min(dealers$hh.maize.q80, na.rm=T), digits=2) #price
round(min(dealers$hh.maize.q81, na.rm=T), digits=2) #quality
round(min(dealers$hh.maize.q83, na.rm=T), digits=2)  #reputation
round(min(dealers$gender, na.rm=T), digits=2)
round(min(dealers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(min(dealers$educ, na.rm=T), digits=2)
round(min(dealers$married, na.rm=T), digits=2)

round(max(dealers$ratee_rating_overall, na.rm=T), digits=2)
round(max(dealers$hh.maize.q79, na.rm=T), digits=2) #location
round(max(dealers$hh.maize.q80, na.rm=T), digits=2) #price
round(max(dealers$hh.maize.q81, na.rm=T), digits=2) #quality
round(max(dealers$hh.maize.q83, na.rm=T), digits=2)  #reputation
round(max(dealers$gender, na.rm=T), digits=2)
round(max(dealers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(max(dealers$educ, na.rm=T), digits=2)
round(max(dealers$married, na.rm=T), digits=2)

#TRADERS
round(mean(traders$ratee_rating_overall, na.rm=T), digits=2)
round(mean(traders$hh.maize.q40a, na.rm=T), digits=2) #location
round(mean(traders$hh.maize.q40b, na.rm=T), digits=2) #price
round(mean(traders$hh.maize.q40c, na.rm=T), digits=2) #quality
round(mean(traders$hh.maize.q40e, na.rm=T), digits=2)  #reputation
round(mean(traders$gender, na.rm=T), digits=2)
round(mean(traders$hh.maize.q6, na.rm=T), digits=2)  #age
round(mean(traders$educ, na.rm=T), digits=2)
round(mean(traders$married, na.rm=T), digits=2)

round(sd(traders$ratee_rating_overall, na.rm=T), digits=2)
round(sd(traders$hh.maize.q40a, na.rm=T), digits=2) #location
round(sd(traders$hh.maize.q40b, na.rm=T), digits=2) #price
round(sd(traders$hh.maize.q40c, na.rm=T), digits=2) #quality
round(sd(traders$hh.maize.q40e, na.rm=T), digits=2)  #reputation
round(sd(traders$gender, na.rm=T), digits=2)
round(sd(traders$hh.maize.q6, na.rm=T), digits=2)  #age
round(sd(traders$educ, na.rm=T), digits=2)
round(sd(traders$married, na.rm=T), digits=2)

round(min(traders$ratee_rating_overall, na.rm=T), digits=2)
round(min(traders$hh.maize.q40a, na.rm=T), digits=2) #location
round(min(traders$hh.maize.q40b, na.rm=T), digits=2) #price
round(min(traders$hh.maize.q40c, na.rm=T), digits=2) #quality
round(min(traders$hh.maize.q40e, na.rm=T), digits=2)  #reputation
round(min(traders$gender, na.rm=T), digits=2)
round(min(traders$hh.maize.q6, na.rm=T), digits=2)  #age
round(min(traders$educ, na.rm=T), digits=2)
round(min(traders$married, na.rm=T), digits=2)

round(max(traders$ratee_rating_overall, na.rm=T), digits=2)
round(max(traders$hh.maize.q40a, na.rm=T), digits=2) #location
round(max(traders$hh.maize.q40b, na.rm=T), digits=2) #price
round(max(traders$hh.maize.q40c, na.rm=T), digits=2) #quality
round(max(traders$hh.maize.q40e, na.rm=T), digits=2)  #reputation
round(max(traders$gender, na.rm=T), digits=2)
round(max(traders$hh.maize.q6, na.rm=T), digits=2)  #age
round(max(traders$educ, na.rm=T), digits=2)
round(max(traders$married, na.rm=T), digits=2)

#MILLERS
round(mean(millers$ratee_rating_overall, na.rm=T), digits=2)
round(mean(millers$hh.maize.q36, na.rm=T), digits=2) #location
round(mean(millers$hh.maize.q37, na.rm=T), digits=2) #price
round(mean(millers$hh.maize.q38, na.rm=T), digits=2) #quality
round(mean(millers$hh.maize.q40, na.rm=T), digits=2)  #reputation
round(mean(millers$gender, na.rm=T), digits=2)
round(mean(millers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(mean(millers$educ, na.rm=T), digits=2)
round(mean(millers$married, na.rm=T), digits=2)

round(sd(millers$ratee_rating_overall, na.rm=T), digits=2)
round(sd(millers$hh.maize.q36, na.rm=T), digits=2) #location
round(sd(millers$hh.maize.q37, na.rm=T), digits=2) #price
round(sd(millers$hh.maize.q38, na.rm=T), digits=2) #quality
round(sd(millers$hh.maize.q40, na.rm=T), digits=2)  #reputation
round(sd(millers$gender, na.rm=T), digits=2)
round(sd(millers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(sd(millers$educ, na.rm=T), digits=2)
round(sd(millers$married, na.rm=T), digits=2)

round(min(millers$ratee_rating_overall, na.rm=T), digits=2)
round(min(millers$hh.maize.q36, na.rm=T), digits=2) #location
round(min(millers$hh.maize.q37, na.rm=T), digits=2) #price
round(min(millers$hh.maize.q38, na.rm=T), digits=2) #quality
round(min(millers$hh.maize.q40, na.rm=T), digits=2)  #reputation
round(min(millers$gender, na.rm=T), digits=2)
round(min(millers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(min(millers$educ, na.rm=T), digits=2)
round(min(millers$married, na.rm=T), digits=2)

round(max(millers$ratee_rating_overall, na.rm=T), digits=2)
round(max(millers$hh.maize.q36, na.rm=T), digits=2) #location
round(max(millers$hh.maize.q37, na.rm=T), digits=2) #price
round(max(millers$hh.maize.q38, na.rm=T), digits=2) #quality
round(max(millers$hh.maize.q40, na.rm=T), digits=2)  #reputation
round(max(millers$gender, na.rm=T), digits=2)
round(max(millers$hh.maize.q6g, na.rm=T), digits=2)  #age
round(max(millers$educ, na.rm=T), digits=2)
round(max(millers$married, na.rm=T), digits=2)


##########################################################################################################################
##########################################################################################################################

##########################################################################
################################# ICC ####################################
##########################################################################


#MODEL 1A INTER RATER RELIABILITY, MODEL 1B INTRA RATER RELIABILITY
#transposing data
icc <- pool[c("id.ratee","farmerID","rating_overall")]

#ratings from raters subset 
icc_6 <- icc[icc$farmerID %in%  names(table(icc$farmerID))[table(icc$farmerID) >6] , ]
icc_recast_overall6 <- recast(icc_6, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))

#overall rating
icc1a.fn(icc_recast_overall6) #0.5364983
icc1b.fn(icc_recast_overall6) #0.6431951

#getting data ready
icc_loc1 <- pool[c("id.ratee","farmerID","rating_location")]
icc_qual1 <- pool[c("id.ratee","farmerID","rating_quality")]
icc_price1 <- pool[c("id.ratee","farmerID","rating_price")]
icc_rep1 <- pool[c("id.ratee","farmerID","rating_reputation")]

#location
icc_6_loc <- icc_loc1[icc_loc1$farmerID %in%  names(table(icc_loc1$farmerID))[table(icc_loc1$farmerID) >6] , ]
icc_recast_loc <- recast(icc_6_loc, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))
icc1a.fn(icc_recast_loc) #0.4689853
icc1b.fn(icc_recast_loc) #0.6175318

#quality
icc_6_qual <- icc_qual1[icc_qual1$farmerID %in%  names(table(icc_qual1$farmerID))[table(icc_qual1$farmerID) >6] , ]
icc_recast_qual <- recast(icc_6_qual, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))
icc1a.fn(icc_recast_qual) #0.1515282 
icc1b.fn(icc_recast_qual) #0.3144065

#price
icc_6_price <- icc_price1[icc_price1$farmerID %in%  names(table(icc_price1$farmerID))[table(icc_price1$farmerID) >6] , ]
icc_recast_price <- recast(icc_6_price, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))
icc1a.fn(icc_recast_price) #0.4279139 
icc1b.fn(icc_recast_price) #0.4278701

#reputation
icc_6_rep <- icc_rep1[icc_rep1$farmerID %in%  names(table(icc_rep1$farmerID))[table(icc_rep1$farmerID) >6] , ]
icc_recast_rep <- recast(icc_6_rep, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))
icc1a.fn(icc_recast_rep) # 0.2441795
icc1b.fn(icc_recast_rep) # 0.6776563


############################################################################################################
############################################################################################################
############################################################################################################

############## VARIABLES SECTION CODE ####################
##########################################################

#percentage of farmers scoring atmost 1 for location
round(sum(table(pool$farmerID[pool$rating_location<=1]))/sum(table(pool$farmerID))*100,digits=0)
#percentage of farmers scoring atleast 5 for location
round(sum(table(pool$farmerID[pool$rating_location>=5]))/sum(table(pool$farmerID))*100,digits=0)
#percentage of female farmers scoring atleast 5 for location
round(sum(table(pool$farmerID[pool$rating_location>=5 & pool$farmer_fem=="1"]))
      /sum(table(pool$farmerID[pool$farmer_fem=="1"]))*100,digits=0)
#percentage of male farmers scoring atleast 5 for location
round(sum(table(pool$farmerID[pool$rating_location>=5 & pool$farmer_fem=="0"]))/
        sum(table(pool$farmerID[pool$farmer_fem=="0"]))*100,digits=0)
#percentage of female farmers 
round(table(farmers$hh.maize.q25)[1]/sum(table(farmers$hh.maize.q25))*100,digits=0)
#percentage of farmers having some level of education 
round(table(farmers$educ)[2]/sum(table(farmers$educ))*100,digits=0)
#percentage of married farmers 
round(table(farmers$married)[2]/sum(table(farmers$married))*100,digits=0)

#average age of farmers
round(mean(farmers$hh.maize.q24, na.rm=T),digits=2)
#average distance of farmer's homestead to tarmac road 
round(mean(farmers$hh.maize.q13, na.rm=T),digits=2)
#average distance of farmer's homestead to murram road
round(mean(farmers$hh.maize.q14, na.rm=T),digits=2)

#percentage of farmers who had some interaction
round(table(pool$interaction_yes)[2]/sum(table(pool$interaction_yes))*100,digits=0)

#percentage of ratees who give self-rating of atleast 5 for reputation
round(sum(table(pool$id.ratee[pool$rating_reputation_ratee>=5]))/sum(table(pool$id.ratee))*100,digits=0)
#percentage of ratees who give a self-rating of atmost 2 for location 
round(sum(table(pool$id.ratee[pool$rating_location_ratee<=2]))/sum(table(pool$id.ratee))*100,digits=0)
#percentage of female input dealers 
round(table(dealers$hh.maize.q7)[1]/sum(table(dealers$hh.maize.q7))*100,digits=0)
#percentage of female traders 
round(table(traders$hh.maize.q7)[1]/sum(table(traders$hh.maize.q7))*100,digits=0)
#percentage of female millers
round(table(millers$hh.maize.q7)[1]/sum(table(millers$hh.maize.q7))*100,digits=0)

#oldest input dealer
round(max(dealers$hh.maize.q6g, na.rm=T),digits=2)
#oldest trader
round(max(traders$hh.maize.q6, na.rm=T),digits=2)
#oldest miller
round(max(millers$hh.maize.q6g, na.rm=T),digits=2)

#percentage of female ratees rating themselves atmost 5 for quality
round(sum(table(pool$id.ratee[pool$rating_quality_ratee>=5 & pool$ratee_fem=="1"]))
      /sum(table(pool$id.ratee[pool$ratee_fem=="1"]))*100,digits=0)
#percentage of male ratees rating themselves atmost 5 for reputation
round(sum(table(pool$id.ratee[pool$rating_reputation_ratee>=5 & pool$ratee_fem=="0"]))/
        sum(table(pool$id.ratee[pool$ratee_fem=="0"]))*100,digits=0)
#percentage of male ratees rating themselves atmost 2 for reputation
round(sum(table(pool$id.ratee[pool$rating_reputation_ratee<=2 & pool$ratee_fem=="0"]))/
        sum(table(pool$id.ratee[pool$ratee_fem=="0"]))*100,digits=0)



##########################################################################################################################
##########################################################################################################################

##############################################################
############## REGRESSION TABLE CODES ########################
##############################################################

######Dependent variable: Ratings from Farmers (Raters)

#Intercept 
round(sum(lm_res1$coefficients[1]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[1,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res5$coefficients[1]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[1,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res11$coefficients[1]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res15$coefficients[1]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res21$coefficients[1]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[1,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[1]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res31$coefficients[1]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res35$coefficients[1]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res41$coefficients[1]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res45$coefficients[1]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[1,4]<.1,"*","")))
round(se1[1],digits=4)
round(se5[1],digits=4)
round(se11[1],digits=4)   
round(se15[1],digits=4)
round(se21[1],digits=4)
round(se25[1],digits=4)
round(se31[1],digits=4)
round(se35[1],digits=4)
round(se41[1],digits=4)
round(se45[1],digits=4) 

#Gender --- Farmer 
round(sum(lm_res1$coefficients[2]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[2,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res5$coefficients[2]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[2,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res11$coefficients[2]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res15$coefficients[2]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res21$coefficients[2]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[2,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[2]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res31$coefficients[2]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res35$coefficients[2]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res41$coefficients[2]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res45$coefficients[2]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[2,4]<.1,"*","")))
round(se1[2],digits=4)
round(se5[2],digits=4)
round(se11[2],digits=4)   
round(se15[2],digits=4)
round(se21[2],digits=4)
round(se25[2],digits=4)
round(se31[2],digits=4)
round(se35[2],digits=4)
round(se41[2],digits=4)
round(se45[2],digits=4) 

#Gender -- Ratee
round(sum(lm_res1$coefficients[3]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[3,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res5$coefficients[3]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[3,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res11$coefficients[3]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res15$coefficients[3]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res21$coefficients[3]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[3,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[3]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res31$coefficients[3]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res35$coefficients[3]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res41$coefficients[3]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res45$coefficients[3]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[3,4]<.1,"*","")))
round(se1[3],digits=4)
round(se5[3],digits=4)
round(se11[3],digits=4)   
round(se15[3],digits=4)
round(se21[3],digits=4)
round(se25[3],digits=4)
round(se31[3],digits=4)
round(se35[3],digits=4)
round(se41[3],digits=4)
round(se45[3],digits=4) 


#Age of farmer 
round(sum(lm_res1$coefficients[4]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[4,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res5$coefficients[4]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[4,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res11$coefficients[4]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res15$coefficients[4]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res21$coefficients[4]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[4,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[4]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res31$coefficients[4]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res35$coefficients[4]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res41$coefficients[4]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res45$coefficients[4]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[4,4]<.1,"*","")))
round(se1[4],digits=4)
round(se5[4],digits=4)
round(se11[4],digits=4)   
round(se15[4],digits=4)
round(se21[4],digits=4)
round(se25[4],digits=4)
round(se31[4],digits=4)
round(se35[4],digits=4)
round(se41[4],digits=4)
round(se45[4],digits=4) 

#Education -- Farmer
round(sum(lm_res1$coefficients[5]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[5,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res5$coefficients[5]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[5,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res11$coefficients[5]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res15$coefficients[5]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res21$coefficients[5]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[5,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[5]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res31$coefficients[5]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res35$coefficients[5]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res41$coefficients[5]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res45$coefficients[5]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[5,4]<.1,"*","")))
round(se1[5],digits=4)
round(se5[5],digits=4)
round(se11[5],digits=4)   
round(se15[5],digits=4)
round(se21[5],digits=4)
round(se25[5],digits=4)
round(se31[5],digits=4)
round(se35[5],digits=4)
round(se41[5],digits=4)
round(se45[5],digits=4) 

#Tarmac
round(sum(lm_res1$coefficients[6]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[6,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res5$coefficients[6]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[6,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res11$coefficients[6]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res15$coefficients[6]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res21$coefficients[6]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[6,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[6]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res31$coefficients[6]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res35$coefficients[6]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res41$coefficients[6]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res45$coefficients[6]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[6,4]<.1,"*","")))
round(se1[6],digits=4)
round(se5[6],digits=4)
round(se11[6],digits=4)   
round(se15[6],digits=4)
round(se21[6],digits=4)
round(se25[6],digits=4)
round(se31[6],digits=4)
round(se35[6],digits=4)
round(se41[6],digits=4)
round(se45[6],digits=4) 

#Murram
round(sum(lm_res1$coefficients[7]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[7,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res5$coefficients[7]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[7,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res11$coefficients[7]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res15$coefficients[7]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res21$coefficients[7]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[7,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[7]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res31$coefficients[7]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res35$coefficients[7]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res41$coefficients[7]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res45$coefficients[7]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[7,4]<.1,"*","")))
round(se1[7],digits=4)
round(se5[7],digits=4)
round(se11[7],digits=4)   
round(se15[7],digits=4)
round(se21[7],digits=4)
round(se25[7],digits=4)
round(se31[7],digits=4)
round(se35[7],digits=4)
round(se41[7],digits=4)
round(se45[7],digits=4) 

#Marital status -- farmer
round(sum(lm_res1$coefficients[8]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[8,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res5$coefficients[8]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[8,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res11$coefficients[8]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res15$coefficients[8]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res21$coefficients[8]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[8,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[8]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res31$coefficients[8]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res35$coefficients[8]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res41$coefficients[8]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res45$coefficients[8]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[8,4]<.1,"*","")))
round(se1[8],digits=4)
round(se5[8],digits=4)
round(se11[8],digits=4)   
round(se15[8],digits=4)
round(se21[8],digits=4)
round(se25[8],digits=4)
round(se31[8],digits=4)
round(se35[8],digits=4)
round(se41[8],digits=4)
round(se45[8],digits=4) 

#ratee's age
round(sum(lm_res1$coefficients[9]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[9,4]<.05,
                                                           "**",ifelse(summary(lm_res1)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res5$coefficients[9]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[9,4]<.05,
                                                           "**",ifelse(summary(lm_res5)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res11$coefficients[9]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res11)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res15$coefficients[9]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res15)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res21$coefficients[9]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res21)$coefficients[9,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[9]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res25)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res31$coefficients[9]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res31)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res35$coefficients[9]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res35)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res41$coefficients[9]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res41)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res45$coefficients[9]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res45)$coefficients[9,4]<.1,"*","")))
round(se1[9],digits=4)
round(se5[9],digits=4)
round(se11[9],digits=4)   
round(se15[9],digits=4)
round(se21[9],digits=4)
round(se25[9],digits=4)
round(se31[9],digits=4)
round(se35[9],digits=4)
round(se41[9],digits=4)
round(se45[9],digits=4) 

#marital status --- ratee
round(sum(lm_res1$coefficients[10]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[10,4]<.05,
                                                            "**",ifelse(summary(lm_res1)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res5$coefficients[10]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[10,4]<.05,
                                                            "**",ifelse(summary(lm_res5)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res11$coefficients[10]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res11)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res15$coefficients[10]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res15)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res21$coefficients[10]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res21)$coefficients[10,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[10]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res25)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res31$coefficients[10]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res31)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res35$coefficients[10]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res35)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res41$coefficients[10]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res41)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res45$coefficients[10]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res45)$coefficients[10,4]<.1,"*","")))
round(se1[10],digits=4)
round(se5[10],digits=4)
round(se11[10],digits=4)   
round(se15[10],digits=4)
round(se21[10],digits=4)
round(se25[10],digits=4)
round(se31[10],digits=4)
round(se35[10],digits=4)
round(se41[10],digits=4)
round(se45[10],digits=4) 

#education -- Ratee 
round(sum(lm_res1$coefficients[11]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[11,4]<.05,
                                                            "**",ifelse(summary(lm_res1)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res5$coefficients[11]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[11,4]<.05,
                                                            "**",ifelse(summary(lm_res5)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res11$coefficients[11]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res11)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res15$coefficients[11]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res15)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res21$coefficients[11]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res21)$coefficients[11,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[11]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res25)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res31$coefficients[11]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res31)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res35$coefficients[11]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res35)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res41$coefficients[11]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res41)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res45$coefficients[11]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res45)$coefficients[11,4]<.1,"*","")))
round(se1[11],digits=4)
round(se5[11],digits=4)
round(se11[11],digits=4)   
round(se15[11],digits=4)
round(se21[11],digits=4)
round(se25[11],digits=4)
round(se31[11],digits=4)
round(se35[11],digits=4)
round(se41[11],digits=4)
round(se45[11],digits=4) 

#dealer dummy 
round(sum(lm_res1$coefficients[12]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[12,4]<.05,
                                                            "**",ifelse(summary(lm_res1)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res5$coefficients[12]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[12,4]<.05,
                                                            "**",ifelse(summary(lm_res5)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res11$coefficients[12]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res11)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res15$coefficients[12]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res15)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res21$coefficients[12]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res21)$coefficients[12,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[12]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res25)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res31$coefficients[12]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res31)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res35$coefficients[12]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res35)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res41$coefficients[12]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res41)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res45$coefficients[12]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res45)$coefficients[12,4]<.1,"*","")))
round(se1[12],digits=4)
round(se5[12],digits=4)
round(se11[12],digits=4)   
round(se15[12],digits=4)
round(se21[12],digits=4)
round(se25[12],digits=4)
round(se31[12],digits=4)
round(se35[12],digits=4)
round(se41[12],digits=4)
round(se45[12],digits=4) 

#trader dummy
round(sum(lm_res1$coefficients[13]),digits=4) #overall
ifelse(summary(lm_res1)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res1)$coefficients[13,4]<.05,
                                                            "**",ifelse(summary(lm_res1)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res5$coefficients[13]),digits=4) #with interaction
ifelse(summary(lm_res5)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[13,4]<.05,
                                                            "**",ifelse(summary(lm_res5)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res11$coefficients[13]),digits=4) #location 
ifelse(summary(lm_res11)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res11)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res11)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res15$coefficients[13]),digits=4)  #with interaction
ifelse(summary(lm_res15)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res15)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res21$coefficients[13]),digits=4)   #quality
ifelse(summary(lm_res21)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res21)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res21)$coefficients[13,4]<.1,"*","")))  
round(sum(lm_res25$coefficients[13]),digits=4)  #with interaction
ifelse(summary(lm_res25)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res25)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res31$coefficients[13]),digits=4)  #price
ifelse(summary(lm_res31)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res31)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res31)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res35$coefficients[13]),digits=4) #with interaction
ifelse(summary(lm_res35)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res35)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res41$coefficients[13]),digits=4)  #reputation
ifelse(summary(lm_res41)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res41)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res41)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res45$coefficients[13]),digits=4) #with interaction
ifelse(summary(lm_res45)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res45)$coefficients[13,4]<.1,"*","")))
round(se1[13],digits=4)
round(se5[13],digits=4)
round(se11[13],digits=4)   
round(se15[13],digits=4)
round(se21[13],digits=4)
round(se25[13],digits=4)
round(se31[13],digits=4)
round(se35[13],digits=4)
round(se41[13],digits=4)
round(se45[13],digits=4) 

#gender - farmer : gender - ratee

round(sum(lm_res5$coefficients[14]),digits=4) #OVERALL with interaction
ifelse(summary(lm_res5)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res5)$coefficients[14,4]<.05,
                                                            "**",ifelse(summary(lm_res5)$coefficients[14,4]<.1,"*","")))
round(sum(lm_res15$coefficients[14]),digits=4)  #LOCATION with interaction
ifelse(summary(lm_res15)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res15)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res15)$coefficients[14,4]<.1,"*","")))
round(sum(lm_res25$coefficients[14]),digits=4)  #QUALITY with interaction
ifelse(summary(lm_res25)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res25)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res25)$coefficients[14,4]<.1,"*","")))
round(sum(lm_res35$coefficients[14]),digits=4) #PRICE with interaction
ifelse(summary(lm_res35)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res35)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res35)$coefficients[14,4]<.1,"*","")))
round(sum(lm_res45$coefficients[14]),digits=4) #REPUTATION with interaction
ifelse(summary(lm_res45)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res45)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res45)$coefficients[14,4]<.1,"*","")))
round(se5[14],digits=4)
round(se15[14],digits=4)
round(se25[14],digits=4)
round(se35[14],digits=4)
round(se45[14],digits=4) 

#R^2                
round(summary(lm_res1)$r.squared,digits=4) #overall    
round(summary(lm_res5)$r.squared,digits=4) #with interaction
round(summary(lm_res11)$r.squared,digits=4) #location
round(summary(lm_res15)$r.squared,digits=4)  #with interaction
round(summary(lm_res21)$r.squared,digits=4)  #quality
round(summary(lm_res25)$r.squared,digits=4)  #with interaction
round(summary(lm_res31)$r.squared,digits=4) #price
round(summary(lm_res35)$r.squared,digits=4)   #with interaction
round(summary(lm_res41)$r.squared,digits=4)  #reputation
round(summary(lm_res45)$r.squared,digits=4)   #with interaction

#Adj. R^2
round(summary(lm_res1)$adj.r.squared,digits=4) #overall    
round(summary(lm_res5)$adj.r.squared,digits=4) #with interaction
round(summary(lm_res11)$adj.r.squared,digits=4) #location
round(summary(lm_res15)$adj.r.squared,digits=4)  #with interaction
round(summary(lm_res21)$adj.r.squared,digits=4)  #quality
round(summary(lm_res25)$adj.r.squared,digits=4)  #with interaction
round(summary(lm_res31)$adj.r.squared,digits=4) #price
round(summary(lm_res35)$adj.r.squared,digits=4)   #with interaction
round(summary(lm_res41)$adj.r.squared,digits=4)  #reputation
round(summary(lm_res45)$adj.r.squared,digits=4)   #with interaction

#Number of obs 
round(nobs(lm_res1),digits=4) #overall
round(nobs(lm_res5),digits=4)  #with interaction
round(nobs(lm_res11),digits=4)  #location
round(nobs(lm_res15),digits=4)  #with interaction
round(nobs(lm_res21),digits=4)  #quality
round(nobs(lm_res25),digits=4)  #with interaction
round(nobs(lm_res31),digits=4)   #price
round(nobs(lm_res35),digits=4)   #with interaction
round(nobs(lm_res41),digits=4)   #reputation
round(nobs(lm_res45),digits=4)    #with interaction

###########################################################################################


######Dependent variable: Self-ratings by dealers, traders and millers

#Intercept
round(sum(mod51_gender$coefficients[1]),digits=4) #overall
ifelse(summary(mod51_gender)$coefficients[1,4]<.01,"***",ifelse(summary(mod51_gender)$coefficients[1,4]<.05,
                                                                "**",ifelse(summary(mod51_gender)$coefficients[1,4]<.1,"*",""))) 
round(sum(mod55_gender$coefficients[1]),digits=4) #location
ifelse(summary(mod55_gender)$coefficients[1,4]<.01,"***",ifelse(summary(mod55_gender)$coefficients[1,4]<.05,
                                                                "**",ifelse(summary(mod55_gender)$coefficients[1,4]<.1,"*","")))
round(sum(mod59_gender$coefficients[1]),digits=4) #quality
ifelse(summary(mod59_gender)$coefficients[1,4]<.01,"***",ifelse(summary(mod59_gender)$coefficients[1,4]<.05,
                                                                "**",ifelse(summary(mod59_gender)$coefficients[1,4]<.1,"*","")))
round(sum(mod63_gender$coefficients[1]),digits=4)  #price
ifelse(summary(mod63_gender)$coefficients[1,4]<.01,"***",ifelse(summary(mod63_gender)$coefficients[1,4]<.05,
                                                                "**",ifelse(summary(mod63_gender)$coefficients[1,4]<.1,"*","")))
round(sum(mod67_gender$coefficients[1]),digits=4)  #reputation
ifelse(summary(mod67_gender)$coefficients[1,4]<.01,"***",ifelse(summary(mod67_gender)$coefficients[1,4]<.05,
                                                                "**",ifelse(summary(mod67_gender)$coefficients[1,4]<.1,"*","")))   
round(se51[1],digits=4)
round(se55[1],digits=4)
round(se59[1],digits=4)
round(se63[1],digits=4)
round(se67[1],digits=4)

#Gender - Ratee
round(sum(mod51_gender$coefficients[2]),digits=4) #overall
ifelse(summary(mod51_gender)$coefficients[2,4]<.01,"***",ifelse(summary(mod51_gender)$coefficients[2,4]<.05,
                                                                "**",ifelse(summary(mod51_gender)$coefficients[2,4]<.1,"*",""))) 
round(sum(mod55_gender$coefficients[2]),digits=4) #location
ifelse(summary(mod55_gender)$coefficients[2,4]<.01,"***",ifelse(summary(mod55_gender)$coefficients[2,4]<.05,
                                                                "**",ifelse(summary(mod55_gender)$coefficients[2,4]<.1,"*","")))
round(sum(mod59_gender$coefficients[2]),digits=4) #quality
ifelse(summary(mod59_gender)$coefficients[2,4]<.01,"***",ifelse(summary(mod59_gender)$coefficients[2,4]<.05,
                                                                "**",ifelse(summary(mod59_gender)$coefficients[2,4]<.1,"*","")))
round(sum(mod63_gender$coefficients[2]),digits=4)  #price
ifelse(summary(mod63_gender)$coefficients[2,4]<.01,"***",ifelse(summary(mod63_gender)$coefficients[2,4]<.05,
                                                                "**",ifelse(summary(mod63_gender)$coefficients[2,4]<.1,"*","")))
round(sum(mod67_gender$coefficients[2]),digits=4)  #reputation
ifelse(summary(mod67_gender)$coefficients[2,4]<.01,"***",ifelse(summary(mod67_gender)$coefficients[2,4]<.05,
                                                                "**",ifelse(summary(mod67_gender)$coefficients[2,4]<.1,"*","")))   
round(se51[2],digits=4)
round(se55[2],digits=4)
round(se59[2],digits=4)
round(se63[2],digits=4)
round(se67[2],digits=4)

#Age of ratee
round(sum(mod51_gender$coefficients[3]),digits=4) #overall
ifelse(summary(mod51_gender)$coefficients[3,4]<.01,"***",ifelse(summary(mod51_gender)$coefficients[3,4]<.05,
                                                                "**",ifelse(summary(mod51_gender)$coefficients[3,4]<.1,"*",""))) 
round(sum(mod55_gender$coefficients[3]),digits=4) #location
ifelse(summary(mod55_gender)$coefficients[3,4]<.01,"***",ifelse(summary(mod55_gender)$coefficients[3,4]<.05,
                                                                "**",ifelse(summary(mod55_gender)$coefficients[3,4]<.1,"*","")))
round(sum(mod59_gender$coefficients[3]),digits=4) #quality
ifelse(summary(mod59_gender)$coefficients[3,4]<.01,"***",ifelse(summary(mod59_gender)$coefficients[3,4]<.05,
                                                                "**",ifelse(summary(mod59_gender)$coefficients[3,4]<.1,"*","")))
round(sum(mod63_gender$coefficients[3]),digits=4)  #price
ifelse(summary(mod63_gender)$coefficients[3,4]<.01,"***",ifelse(summary(mod63_gender)$coefficients[3,4]<.05,
                                                                "**",ifelse(summary(mod63_gender)$coefficients[3,4]<.1,"*","")))
round(sum(mod67_gender$coefficients[3]),digits=4)  #reputation
ifelse(summary(mod67_gender)$coefficients[3,4]<.01,"***",ifelse(summary(mod67_gender)$coefficients[3,4]<.05,
                                                                "**",ifelse(summary(mod67_gender)$coefficients[3,4]<.1,"*","")))   
round(se51[3],digits=4)
round(se55[3],digits=4)
round(se59[3],digits=4)
round(se63[3],digits=4)
round(se67[3],digits=4)

#Ratee marital status 
round(sum(mod51_gender$coefficients[4]),digits=4) #overall
ifelse(summary(mod51_gender)$coefficients[4,4]<.01,"***",ifelse(summary(mod51_gender)$coefficients[4,4]<.05,
                                                                "**",ifelse(summary(mod51_gender)$coefficients[4,4]<.1,"*",""))) 
round(sum(mod55_gender$coefficients[4]),digits=4) #location
ifelse(summary(mod55_gender)$coefficients[4,4]<.01,"***",ifelse(summary(mod55_gender)$coefficients[4,4]<.05,
                                                                "**",ifelse(summary(mod55_gender)$coefficients[4,4]<.1,"*","")))
round(sum(mod59_gender$coefficients[4]),digits=4) #quality
ifelse(summary(mod59_gender)$coefficients[4,4]<.01,"***",ifelse(summary(mod59_gender)$coefficients[4,4]<.05,
                                                                "**",ifelse(summary(mod59_gender)$coefficients[4,4]<.1,"*","")))
round(sum(mod63_gender$coefficients[4]),digits=4)  #price
ifelse(summary(mod63_gender)$coefficients[4,4]<.01,"***",ifelse(summary(mod63_gender)$coefficients[4,4]<.05,
                                                                "**",ifelse(summary(mod63_gender)$coefficients[4,4]<.1,"*","")))
round(sum(mod67_gender$coefficients[4]),digits=4)  #reputation
ifelse(summary(mod67_gender)$coefficients[4,4]<.01,"***",ifelse(summary(mod67_gender)$coefficients[4,4]<.05,
                                                                "**",ifelse(summary(mod67_gender)$coefficients[4,4]<.1,"*","")))   
round(se51[4],digits=4)
round(se55[4],digits=4)
round(se59[4],digits=4)
round(se63[4],digits=4)
round(se67[4],digits=4)

#education - ratee 
round(sum(mod51_gender$coefficients[5]),digits=4) #overall
ifelse(summary(mod51_gender)$coefficients[5,4]<.01,"***",ifelse(summary(mod51_gender)$coefficients[5,4]<.05,
                                                                "**",ifelse(summary(mod51_gender)$coefficients[5,4]<.1,"*",""))) 
round(sum(mod55_gender$coefficients[5]),digits=4) #location
ifelse(summary(mod55_gender)$coefficients[5,4]<.01,"***",ifelse(summary(mod55_gender)$coefficients[5,4]<.05,
                                                                "**",ifelse(summary(mod55_gender)$coefficients[5,4]<.1,"*","")))
round(sum(mod59_gender$coefficients[5]),digits=4) #quality
ifelse(summary(mod59_gender)$coefficients[5,4]<.01,"***",ifelse(summary(mod59_gender)$coefficients[5,4]<.05,
                                                                "**",ifelse(summary(mod59_gender)$coefficients[5,4]<.1,"*","")))
round(sum(mod63_gender$coefficients[5]),digits=4)  #price
ifelse(summary(mod63_gender)$coefficients[5,4]<.01,"***",ifelse(summary(mod63_gender)$coefficients[5,4]<.05,
                                                                "**",ifelse(summary(mod63_gender)$coefficients[5,4]<.1,"*","")))
round(sum(mod67_gender$coefficients[5]),digits=4)  #reputation
ifelse(summary(mod67_gender)$coefficients[5,4]<.01,"***",ifelse(summary(mod67_gender)$coefficients[5,4]<.05,
                                                                "**",ifelse(summary(mod67_gender)$coefficients[5,4]<.1,"*","")))   
round(se51[5],digits=4)
round(se55[5],digits=4)
round(se59[5],digits=4)
round(se63[5],digits=4)
round(se67[5],digits=4)

#dealer dummy
round(sum(mod51_gender$coefficients[6]),digits=4) #overall
ifelse(summary(mod51_gender)$coefficients[6,4]<.01,"***",ifelse(summary(mod51_gender)$coefficients[6,4]<.05,
                                                                "**",ifelse(summary(mod51_gender)$coefficients[6,4]<.1,"*",""))) 
round(sum(mod55_gender$coefficients[6]),digits=4) #location
ifelse(summary(mod55_gender)$coefficients[6,4]<.01,"***",ifelse(summary(mod55_gender)$coefficients[6,4]<.05,
                                                                "**",ifelse(summary(mod55_gender)$coefficients[6,4]<.1,"*","")))
round(sum(mod59_gender$coefficients[6]),digits=4) #quality
ifelse(summary(mod59_gender)$coefficients[6,4]<.01,"***",ifelse(summary(mod59_gender)$coefficients[6,4]<.05,
                                                                "**",ifelse(summary(mod59_gender)$coefficients[6,4]<.1,"*","")))
round(sum(mod63_gender$coefficients[6]),digits=4)  #price
ifelse(summary(mod63_gender)$coefficients[6,4]<.01,"***",ifelse(summary(mod63_gender)$coefficients[6,4]<.05,
                                                                "**",ifelse(summary(mod63_gender)$coefficients[6,4]<.1,"*","")))
round(sum(mod67_gender$coefficients[6]),digits=4)  #reputation
ifelse(summary(mod67_gender)$coefficients[6,4]<.01,"***",ifelse(summary(mod67_gender)$coefficients[6,4]<.05,
                                                                "**",ifelse(summary(mod67_gender)$coefficients[6,4]<.1,"*","")))   
round(se51[6],digits=4)
round(se55[6],digits=4)
round(se59[6],digits=4)
round(se63[6],digits=4)
round(se67[6],digits=4)

#trader dummy
round(sum(mod51_gender$coefficients[7]),digits=4) #overall
ifelse(summary(mod51_gender)$coefficients[7,4]<.01,"***",ifelse(summary(mod51_gender)$coefficients[7,4]<.05,
                                                                "**",ifelse(summary(mod51_gender)$coefficients[7,4]<.1,"*",""))) 
round(sum(mod55_gender$coefficients[7]),digits=4) #location
ifelse(summary(mod55_gender)$coefficients[7,4]<.01,"***",ifelse(summary(mod55_gender)$coefficients[7,4]<.05,
                                                                "**",ifelse(summary(mod55_gender)$coefficients[7,4]<.1,"*","")))
round(sum(mod59_gender$coefficients[7]),digits=4) #quality
ifelse(summary(mod59_gender)$coefficients[7,4]<.01,"***",ifelse(summary(mod59_gender)$coefficients[7,4]<.05,
                                                                "**",ifelse(summary(mod59_gender)$coefficients[7,4]<.1,"*","")))
round(sum(mod63_gender$coefficients[7]),digits=4)  #price
ifelse(summary(mod63_gender)$coefficients[7,4]<.01,"***",ifelse(summary(mod63_gender)$coefficients[7,4]<.05,
                                                                "**",ifelse(summary(mod63_gender)$coefficients[7,4]<.1,"*","")))
round(sum(mod67_gender$coefficients[7]),digits=4)  #reputation
ifelse(summary(mod67_gender)$coefficients[7,4]<.01,"***",ifelse(summary(mod67_gender)$coefficients[7,4]<.05,
                                                                "**",ifelse(summary(mod67_gender)$coefficients[7,4]<.1,"*","")))   
round(se51[7],digits=4)
round(se55[7],digits=4)
round(se59[7],digits=4)
round(se63[7],digits=4)
round(se67[7],digits=4)
  
#R^2  
round(summary(mod51_gender)$r.squared,digits=4) #overall
round(summary(mod55_gender)$r.squared,digits=4) #location
round(summary(mod59_gender)$r.squared,digits=4) #qualiy
round(summary(mod63_gender)$r.squared,digits=4) #price 
round(summary(mod67_gender)$r.squared,digits=4) #reputation

#Adj. R^2                     
round(summary(mod51_gender)$adj.r.squared,digits=4)
round(summary(mod55_gender)$adj.r.squared,digits=4)
round(summary(mod59_gender)$adj.r.squared,digits=4) 
round(summary(mod63_gender)$adj.r.squared,digits=4)
round(summary(mod67_gender)$adj.r.squared,digits=4) 

#Number of observations 
round(nobs(mod51_gender),digits=4)
round(nobs(mod55_gender),digits=4)
round(nobs(mod59_gender),digits=4)
round(nobs(mod63_gender),digits=4)
round(nobs(mod67_gender),digits=4)


###########################################################################################

######Dependent variable: Differences between farmer ratings and ratee self-ratings

#Intercept
round(sum(lm_res71$coefficients[1]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res75$coefficients[1]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res78$coefficients[1]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res82$coefficients[1]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res92$coefficients[1]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[1,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[1]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res85$coefficients[1]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res89$coefficients[1]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[1,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[1]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[1,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[1,4]<.1,"*","")))
round(sum(lm_res103$coefficients[1]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[1,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[1,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[1,4]<.1,"*","")))
round(se71[1],digits=4)
round(se75[1],digits=4)
round(se78[1],digits=4)
round(se82[1],digits=4)
round(se92[1],digits=4)
round(se96[1],digits=4)
round(se85[1],digits=4)
round(se89[1],digits=4)
round(se99[1],digits=4)
round(se103[1],digits=4)

#Gender - Farmer
round(sum(lm_res71$coefficients[2]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res75$coefficients[2]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res78$coefficients[2]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res82$coefficients[2]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res92$coefficients[2]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[2,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[2]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res85$coefficients[2]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res89$coefficients[2]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[2,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[2]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[2,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[2,4]<.1,"*","")))
round(sum(lm_res103$coefficients[2]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[2,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[2,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[2,4]<.1,"*","")))
round(se71[2],digits=4)
round(se75[2],digits=4)
round(se78[2],digits=4)
round(se82[2],digits=4)
round(se92[2],digits=4)
round(se96[2],digits=4)
round(se85[2],digits=4)
round(se89[2],digits=4)
round(se99[2],digits=4)
round(se103[2],digits=4)

#Gender - ratee 
round(sum(lm_res71$coefficients[3]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res75$coefficients[3]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res78$coefficients[3]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res82$coefficients[3]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res92$coefficients[3]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[3,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[3]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res85$coefficients[3]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res89$coefficients[3]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[3,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[3]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[3,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[3,4]<.1,"*","")))
round(sum(lm_res103$coefficients[3]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[3,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[3,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[3,4]<.1,"*","")))
round(se71[3],digits=4)
round(se75[3],digits=4)
round(se78[3],digits=4)
round(se82[3],digits=4)
round(se92[3],digits=4)
round(se96[3],digits=4)
round(se85[3],digits=4)
round(se89[3],digits=4)
round(se99[3],digits=4)
round(se103[3],digits=4)

#Age of farmer
round(sum(lm_res71$coefficients[4]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res75$coefficients[4]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res78$coefficients[4]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res82$coefficients[4]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res92$coefficients[4]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[4,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[4]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res85$coefficients[4]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res89$coefficients[4]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[4,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[4]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[4,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[4,4]<.1,"*","")))
round(sum(lm_res103$coefficients[4]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[4,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[4,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[4,4]<.1,"*","")))
round(se71[4],digits=4)
round(se75[4],digits=4)
round(se78[4],digits=4)
round(se82[4],digits=4)
round(se92[4],digits=4)
round(se96[4],digits=4)
round(se85[4],digits=4)
round(se89[4],digits=4)
round(se99[4],digits=4)
round(se103[4],digits=4)


#education - farmer 
round(sum(lm_res71$coefficients[5]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res75$coefficients[5]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res78$coefficients[5]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res82$coefficients[5]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res92$coefficients[5]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[5,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[5]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res85$coefficients[5]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res89$coefficients[5]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[5,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[5]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[5,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[5,4]<.1,"*","")))
round(sum(lm_res103$coefficients[5]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[5,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[5,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[5,4]<.1,"*","")))
round(se71[5],digits=4)
round(se75[5],digits=4)
round(se78[5],digits=4)
round(se82[5],digits=4)
round(se92[5],digits=4)
round(se96[5],digits=4)
round(se85[5],digits=4)
round(se89[5],digits=4)
round(se99[5],digits=4)
round(se103[5],digits=4)

#tarmac
round(sum(lm_res71$coefficients[6]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res75$coefficients[6]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res78$coefficients[6]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res82$coefficients[6]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res92$coefficients[6]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[6,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[6]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res85$coefficients[6]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res89$coefficients[6]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[6,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[6]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[6,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[6,4]<.1,"*","")))
round(sum(lm_res103$coefficients[6]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[6,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[6,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[6,4]<.1,"*","")))
round(se71[6],digits=4)
round(se75[6],digits=4)
round(se78[6],digits=4)
round(se82[6],digits=4)
round(se92[6],digits=4)
round(se96[6],digits=4)
round(se85[6],digits=4)
round(se89[6],digits=4)
round(se99[6],digits=4)
round(se103[6],digits=4)

#murram
round(sum(lm_res71$coefficients[7]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res75$coefficients[7]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res78$coefficients[7]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res82$coefficients[7]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res92$coefficients[7]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[7,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[7]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res85$coefficients[7]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res89$coefficients[7]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[7,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[7]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[7,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[7,4]<.1,"*","")))
round(sum(lm_res103$coefficients[7]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[7,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[7,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[7,4]<.1,"*","")))
round(se71[7],digits=4)
round(se75[7],digits=4)
round(se78[7],digits=4)
round(se82[7],digits=4)
round(se92[7],digits=4)
round(se96[7],digits=4)
round(se85[7],digits=4)
round(se89[7],digits=4)
round(se99[7],digits=4)
round(se103[7],digits=4)

#farmer marital status 

round(sum(lm_res71$coefficients[8]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res75$coefficients[8]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res78$coefficients[8]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res82$coefficients[8]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res92$coefficients[8]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[8,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[8]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res85$coefficients[8]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res89$coefficients[8]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[8,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[8]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[8,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[8,4]<.1,"*","")))
round(sum(lm_res103$coefficients[8]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[8,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[8,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[8,4]<.1,"*","")))
round(se71[8],digits=4)
round(se75[8],digits=4)
round(se78[8],digits=4)
round(se82[8],digits=4)
round(se92[8],digits=4)
round(se96[8],digits=4)
round(se85[8],digits=4)
round(se89[8],digits=4)
round(se99[8],digits=4)
round(se103[8],digits=4)

#age of ratee 
round(sum(lm_res71$coefficients[9]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res71)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res75$coefficients[9]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res75)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res78$coefficients[9]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res78)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res82$coefficients[9]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res82)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res92$coefficients[9]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res92)$coefficients[9,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[9]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res96)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res85$coefficients[9]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res85)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res89$coefficients[9]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res89)$coefficients[9,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[9]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[9,4]<.05,
                                                            "**",ifelse(summary(lm_res99)$coefficients[9,4]<.1,"*","")))
round(sum(lm_res103$coefficients[9]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[9,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[9,4]<.05,
                                                             "**",ifelse(summary(lm_res103)$coefficients[9,4]<.1,"*","")))
round(se71[9],digits=4)
round(se75[9],digits=4)
round(se78[9],digits=4)
round(se82[9],digits=4)
round(se92[9],digits=4)
round(se96[9],digits=4)
round(se85[9],digits=4)
round(se89[9],digits=4)
round(se99[9],digits=4)
round(se103[9],digits=4)

#ratee marital status 
round(sum(lm_res71$coefficients[10]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res71)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res75$coefficients[10]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res75)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res78$coefficients[10]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res78)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res82$coefficients[10]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res82)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res92$coefficients[10]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res92)$coefficients[10,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[10]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res96)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res85$coefficients[10]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res85)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res89$coefficients[10]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res89)$coefficients[10,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[10]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[10,4]<.05,
                                                             "**",ifelse(summary(lm_res99)$coefficients[10,4]<.1,"*","")))
round(sum(lm_res103$coefficients[10]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[10,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[10,4]<.05,
                                                              "**",ifelse(summary(lm_res103)$coefficients[10,4]<.1,"*","")))
round(se71[10],digits=4)
round(se75[10],digits=4)
round(se78[10],digits=4)
round(se82[10],digits=4)
round(se92[10],digits=4)
round(se96[10],digits=4)
round(se85[10],digits=4)
round(se89[10],digits=4)
round(se99[10],digits=4)
round(se103[10],digits=4)

#education - ratee 
round(sum(lm_res71$coefficients[11]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res71)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res75$coefficients[11]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res75)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res78$coefficients[11]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res78)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res82$coefficients[11]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res82)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res92$coefficients[11]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res92)$coefficients[11,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[11]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res96)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res85$coefficients[11]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res85)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res89$coefficients[11]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res89)$coefficients[11,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[11]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[11,4]<.05,
                                                             "**",ifelse(summary(lm_res99)$coefficients[11,4]<.1,"*","")))
round(sum(lm_res103$coefficients[11]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[11,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[11,4]<.05,
                                                              "**",ifelse(summary(lm_res103)$coefficients[11,4]<.1,"*","")))
round(se71[11],digits=4)
round(se75[11],digits=4)
round(se78[11],digits=4)
round(se82[11],digits=4)
round(se92[11],digits=4)
round(se96[11],digits=4)
round(se85[11],digits=4)
round(se89[11],digits=4)
round(se99[11],digits=4)
round(se103[11],digits=4)

#dealer dummy 
round(sum(lm_res71$coefficients[12]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res71)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res75$coefficients[12]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res75)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res78$coefficients[12]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res78)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res82$coefficients[12]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res82)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res92$coefficients[12]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res92)$coefficients[12,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[12]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res96)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res85$coefficients[12]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res85)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res89$coefficients[12]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res89)$coefficients[12,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[12]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[12,4]<.05,
                                                             "**",ifelse(summary(lm_res99)$coefficients[12,4]<.1,"*","")))
round(sum(lm_res103$coefficients[12]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[12,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[12,4]<.05,
                                                              "**",ifelse(summary(lm_res103)$coefficients[12,4]<.1,"*","")))
round(se71[12],digits=4)
round(se75[12],digits=4)
round(se78[12],digits=4)
round(se82[12],digits=4)
round(se92[12],digits=4)
round(se96[12],digits=4)
round(se85[12],digits=4)
round(se89[12],digits=4)
round(se99[12],digits=4)
round(se103[12],digits=4)

#trader dummy 
round(sum(lm_res71$coefficients[13]),digits=4) #overall
ifelse(summary(lm_res71)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res71)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res71)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res75$coefficients[13]),digits=4)  #with interaction
ifelse(summary(lm_res75)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res75)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res78$coefficients[13]),digits=4) #location
ifelse(summary(lm_res78)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res78)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res78)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res82$coefficients[13]),digits=4)#with interaction
ifelse(summary(lm_res82)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res82)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res92$coefficients[13]),digits=4) #quality
ifelse(summary(lm_res92)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res92)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res92)$coefficients[13,4]<.1,"*",""))) 
round(sum(lm_res96$coefficients[13]),digits=4)#with interaction
ifelse(summary(lm_res96)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res96)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res85$coefficients[13]),digits=4) #price
ifelse(summary(lm_res85)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res85)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res85)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res89$coefficients[13]),digits=4)#with interaction
ifelse(summary(lm_res89)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res89)$coefficients[13,4]<.1,"*",""))) 
round(sum(lm_res99$coefficients[13]),digits=4) #reputation
ifelse(summary(lm_res99)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res99)$coefficients[13,4]<.05,
                                                             "**",ifelse(summary(lm_res99)$coefficients[13,4]<.1,"*","")))
round(sum(lm_res103$coefficients[13]),digits=4)#with interaction
ifelse(summary(lm_res103)$coefficients[13,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[13,4]<.05,
                                                              "**",ifelse(summary(lm_res103)$coefficients[13,4]<.1,"*","")))
round(se71[13],digits=4)
round(se75[13],digits=4)
round(se78[13],digits=4)
round(se82[13],digits=4)
round(se92[13],digits=4)
round(se96[13],digits=4)
round(se85[13],digits=4)
round(se89[13],digits=4)
round(se99[13],digits=4)
round(se103[13],digits=4)

#gender - farmer: gender - ratee 
round(sum(lm_res75$coefficients[14]),digits=4)  #overall with interaction
ifelse(summary(lm_res75)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res75)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res75)$coefficients[14,4]<.1,"*","")))
round(sum(lm_res82$coefficients[14]),digits=4)#location with interaction
ifelse(summary(lm_res82)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res82)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res82)$coefficients[14,4]<.1,"*","")))
round(sum(lm_res96$coefficients[14]),digits=4)#quality with interaction
ifelse(summary(lm_res96)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res96)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res96)$coefficients[14,4]<.1,"*","")))
round(sum(lm_res89$coefficients[14]),digits=4)#price with interaction
ifelse(summary(lm_res89)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res89)$coefficients[14,4]<.05,
                                                             "**",ifelse(summary(lm_res89)$coefficients[14,4]<.1,"*",""))) 
round(sum(lm_res103$coefficients[14]),digits=4)#reputation with interaction
ifelse(summary(lm_res103)$coefficients[14,4]<.01,"***",ifelse(summary(lm_res103)$coefficients[14,4]<.05,
                                                              "**",ifelse(summary(lm_res103)$coefficients[14,4]<.1,"*","")))

round(se75[14],digits=4)
round(se82[14],digits=4)
round(se96[14],digits=4)
round(se89[14],digits=4)
round(se103[14],digits=4)

#R^2              
round(summary(lm_res71)$r.squared,digits=4) #overall 
round(summary(lm_res75)$r.squared,digits=4) #with interaction
round(summary(lm_res78)$r.squared,digits=4) #location
round(summary(lm_res82)$r.squared,digits=4) #with interaction
round(summary(lm_res92)$r.squared,digits=4) #quality
round(summary(lm_res96)$r.squared,digits=4) #with interaction
round(summary(lm_res85)$r.squared,digits=4) #price
round(summary(lm_res89)$r.squared,digits=4) #with interaction
round(summary(lm_res99)$r.squared,digits=4) #reputation
round(summary(lm_res103)$r.squared,digits=4) #with interaction

#Adjusted R^2              
round(summary(lm_res71)$adj.r.squared,digits=4) #overall 
round(summary(lm_res75)$adj.r.squared,digits=4) #with interaction
round(summary(lm_res78)$adj.r.squared,digits=4) #location
round(summary(lm_res82)$adj.r.squared,digits=4) #with interaction
round(summary(lm_res92)$adj.r.squared,digits=4) #quality
round(summary(lm_res96)$adj.r.squared,digits=4) #with interaction
round(summary(lm_res85)$adj.r.squared,digits=4) #price
round(summary(lm_res89)$adj.r.squared,digits=4) #with interaction
round(summary(lm_res99)$adj.r.squared,digits=4) #reputation
round(summary(lm_res103)$adj.r.squared,digits=4) #with interaction

#Number of obs.                     
round(nobs(lm_res71),digits=4)
round(nobs(lm_res75),digits=4)
round(nobs(lm_res78),digits=4)
round(nobs(lm_res82),digits=4)
round(nobs(lm_res92),digits=4)
round(nobs(lm_res96),digits=4)
round(nobs(lm_res85),digits=4)
round(nobs(lm_res89),digits=4)
round(nobs(lm_res99),digits=4)
round(nobs(lm_res103),digits=4)


#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


###Likert scale bar charts for the different components of the scores###
#########################################################################

#All ratees 
plot_ratee <- data.frame(cbind(prop.table(table(pool$rating_location_ratee),1),
                               prop.table(table(pool$rating_price_ratee),1),
                               prop.table(table(pool$rating_quality_ratee),1),
                               prop.table(table(pool$rating_reputation_ratee),1)))
names(plot_ratee) <- c("Location","Price","Quality","Reputation")

#female ratees
plot_fem_ratee <- data.frame(cbind(prop.table(table(pool$rating_location_ratee, pool$gender_ratee=="Female"),2)[,1],
                                   prop.table(table(pool$rating_price_ratee, pool$gender_ratee=="Female"),2)[,1],
                                   prop.table(table(pool$rating_quality_ratee, pool$gender_ratee=="Female"),2)[,1],
                                   prop.table(table(pool$rating_reputation_ratee, pool$gender_ratee=="Female"),2)[,1]))
names(plot_fem_ratee) <- c("Location","Price","Quality","Reputation")

#male ratees
plot_male_ratee <- data.frame(cbind(prop.table(table(pool$rating_location_ratee, pool$gender_ratee=="Male"),2)[,2],
                                    prop.table(table(pool$rating_price_ratee, pool$gender_ratee=="Male"),2)[,2],
                                    prop.table(table(pool$rating_quality_ratee, pool$gender_ratee=="Male"),2)[,2],
                                    prop.table(table(pool$rating_reputation_ratee, pool$gender_ratee=="Male"),2)[,2]))
names(plot_male_ratee) <- c("Location","Price","Quality","Reputation")

#all raters
plot_rater <- data.frame(cbind(prop.table(table(pool$rating_location),1),
                               prop.table(table(pool$rating_price),1),
                               prop.table(table(pool$rating_quality),1),
                               prop.table(table(pool$rating_reputation),1)))
names(plot_rater) <- c("Location","Price","Quality","Reputation")

#female raters 
plot_fem_farm <- data.frame(cbind(prop.table(table(pool$rating_location, pool$farmer_gender=="Female"),2)[,1],
                                  prop.table(table(pool$rating_price, pool$farmer_gender=="Female"),2)[,1],
                                  prop.table(table(pool$rating_quality, pool$farmer_gender=="Female"),2)[,1],
                                  prop.table(table(pool$rating_reputation, pool$farmer_gender=="Female"),2)[,1]))
names(plot_fem_farm) <- c("Location","Price","Quality","Reputation")

#male raters 
plot_male_farm <- data.frame(cbind(prop.table(table(pool$rating_location, pool$farmer_gender=="Male"),2)[,2],
                                   prop.table(table(pool$rating_price, pool$farmer_gender=="Male"),2)[,2],
                                   prop.table(table(pool$rating_quality, pool$farmer_gender=="Male"),2)[,2],
                                   prop.table(table(pool$rating_reputation, pool$farmer_gender=="Male"),2)[,2]  ))
names(plot_male_farm) <- c("Location","Price","Quality","Reputation")

png(paste(path_2, "/papers/perceptions/figure/likert.png",sep = "/"), units="px", height=3200, width= 6000, res=600)
par(mfrow=c(1,6), xpd=NA, mar = c(10, 5,5, 1)) 
colfunc<-colorRampPalette(c("red", "green"))
barplot(as.matrix(plot_fem_ratee), col=colfunc(5), main="Ratees", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_male_ratee), col=colfunc(5), main="Female Ratees", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_fem_ratee), col=colfunc(5), main="Male Ratees", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_male_ratee), col=colfunc(5), main="Raters", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_fem_ratee), col=colfunc(5), main="Female Raters", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_male_ratee), col=colfunc(5), main="Male Raters", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()


#################################################################################################################
############# FIXED EFFECTS ############
#################################################################################################################


#AT RATEE LEVEL --- ENTIRE GROUP OF RATEES --- POOLED

fe_modoverall_pool <- lm(rating_overall ~ id.ratee, data = pool)
summary(fe_modoverall_pool)
#1 < 1.713 (f stat), reject null at 5% level - significant .... 515, 3082 DF

fe_modloc_pool <- lm(rating_location ~ id.ratee, data = pool)
summary(fe_modloc_pool)
#reject null at 5% -- significant 

fe_modprice_pool <- lm(rating_price ~ id.ratee, data = pool)
summary(fe_modprice_pool)
#reject null at 5% -- significant 

fe_modqual_pool <- lm(rating_quality ~ id.ratee, data = pool)
summary(fe_modqual_pool)
#reject null at 5% -- significant 

fe_modrep_pool <- lm(rating_reputation ~ id.ratee, data = pool)
summary(fe_modrep_pool)
#reject null at 5% -- significant 


#AT FARMER LEVEL

fe_modoverall <- lm(rating_overall ~ farmerID, data = pool)
summary(fe_modoverall)
#1 < 3.367 (f stat), reject null at 5% level - significant .... 1290, 2307 DF

fe_modloc <- lm(rating_location ~ farmerID, data = pool)
summary(fe_modloc)
#reject null at 5% -- significant 

fe_modprice <- lm(rating_price ~ farmerID, data = pool)
summary(fe_modprice)
#reject null at 5% -- significant 

fe_modqual <- lm(rating_quality ~ farmerID, data = pool)
summary(fe_modqual)
#reject null at 5% -- significant 

fe_modrep <- lm(rating_reputation ~ farmerID, data = pool)
summary(fe_modrep)
#reject null at 5% -- significant 

