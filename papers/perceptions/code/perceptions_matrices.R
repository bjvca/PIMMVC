path <- getwd()

library(irrICC)
library(reshape2)
library(miceadds)
library(multiwayvcov)
library(lmtest)
library(tseries)
options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

################# FARMERS ######################

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"),stringsAsFactors=FALSE )

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

farmers$plot1 <- as.numeric(farmers$hh.maize.plot.1..q42)
farmers$plot2 <- as.numeric(farmers$hh.maize.plot.2..q42)
farmers$plot3 <- as.numeric(farmers$hh.maize.plot.3..q42)
farmers$plot4 <- as.numeric(farmers$hh.maize.plot.4..q42)
farmers$plot5 <- as.numeric(farmers$hh.maize.plot.5..q42)
farmers$plot6 <- as.numeric(farmers$hh.maize.plot.6..q42)


################# INPUT DEALERS ######################

## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"),stringsAsFactors=FALSE)

dealers1 <- dealers 

##Index for overall rating by dealers for themselves 
dealers1$ratee_rating_overall <- rowSums(dealers1[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers1$ratee_rating_overall)

dealers1[dealers1=="n/a"] <- NA

##Index for overall rating by dealers for themselves 
dealers$ratee_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$ratee_rating_overall)
#### SUMMARY STATS
dealers$hh.maize.q19[dealers$hh.maize.q19==999] <- NA
dealers$hh.maize.q35[dealers$hh.maize.q35==999] <- NA
dealers$hh.maize.q19 + dealers$hh.maize.q35
dealers$hh.maize.seed.1..q22 <- as.numeric(as.character(dealers$hh.maize.seed.1..q22))
dealers$hh.maize.seed.1..q22[dealers$hh.maize.seed.1..q22==999] <- NA

dealers$hh.maize.seed.2..q22 <- as.numeric(as.character(dealers$hh.maize.seed.2..q22))
dealers$hh.maize.seed.2..q22[dealers$hh.maize.seed.2..q22==999] <- NA
dealers$hh.maize.seed.3..q22 <- as.numeric(as.character(dealers$hh.maize.seed.3..q22))
dealers$hh.maize.seed.3..q22[dealers$hh.maize.seed.3..q22==999] <- NA

################# TRADERS ######################

###Getting traders' data 
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"),stringsAsFactors=FALSE)
traders[traders=="n/a"] <- NA
traders[traders=="999"] <- NA

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
millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"),stringsAsFactors=FALSE)
millers[millers=="n/a"] <- NA
millers[millers=="999"] <- NA

millers1<-millers

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
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"),stringsAsFactors=FALSE)
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
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"),stringsAsFactors=FALSE)

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
millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"),stringsAsFactors=FALSE)

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

### CLUSTERED REGRESSIONS - LOOKING AT BOTH FARMERS' AND RATEES' GENDER AND INTERACTION TERM ###

#################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS ##############
#################################################################


############# TWO WAY CLUSTERING

test1 <- lm(rating_overall ~  farmer_fem*ratee_fem + educ  + age + tarmac
            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
test_se1 <- cluster.boot(test1,c(pool$id.ratee,pool$farmerID))                        
coef1 <- coeftest(test1,test_se1)

test2 <- lm(rating_location ~  farmer_fem*ratee_fem + educ  + age + tarmac
            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
test_se2 <- cluster.boot(test2,c(pool$id.ratee,pool$farmerID))                        
coef2 <- coeftest(test2,test_se2)

test3 <- lm(rating_quality ~  farmer_fem*ratee_fem + educ  + age + tarmac
            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
test_se3 <- cluster.boot(test3,c(pool$id.ratee,pool$farmerID))                        
coef3 <- coeftest(test3,test_se3)

test4 <- lm(rating_price ~  farmer_fem*ratee_fem + educ  + age + tarmac
            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
test_se4 <- cluster.boot(test4,c(pool$id.ratee,pool$farmerID))                        
coef4 <- coeftest(test4,test_se4)

test5 <- lm(rating_reputation ~  farmer_fem*ratee_fem + educ  + age + tarmac
            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
test_se5 <- cluster.boot(test5,c(pool$id.ratee,pool$farmerID))                        
coef5 <- coeftest(test5,test_se5)


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

# Regressions

testt1 <- lm(ratingoverall_diff ~  farmer_fem*ratee_fem + educ  + age + tarmac
            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
testt_se1 <- cluster.boot(testt1,c(pool$id.ratee,pool$farmerID))                        
coeft1 <- coeftest(testt1,testt_se1)


testt2 <- lm(ratingloc_diff ~  farmer_fem*ratee_fem + educ  + age + tarmac
             + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
testt_se2 <- cluster.boot(testt2,c(pool$id.ratee,pool$farmerID))                        
coeft2 <- coeftest(testt2,testt_se2)


testt3 <- lm(ratingprice_diff ~  farmer_fem*ratee_fem + educ  + age + tarmac
             + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
testt_se3 <- cluster.boot(testt3,c(pool$id.ratee,pool$farmerID))                        
coeft3 <- coeftest(testt3,testt_se3)

testt4 <- lm(ratingqual_diff ~  farmer_fem*ratee_fem + educ  + age + tarmac
             + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
testt_se4 <- cluster.boot(testt4,c(pool$id.ratee,pool$farmerID))                        
coeft4 <- coeftest(testt4,testt_se4)

testt5 <- lm(ratingrepu_diff ~  farmer_fem*ratee_fem + educ  + age + tarmac
             + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, data=pool)
testt_se5 <- cluster.boot(testt5,c(pool$id.ratee,pool$farmerID))                        
coeft5 <- coeftest(testt5,testt_se4)



mat<- rbind(         #dependent variable is farmer ratings 
  
                 c((format(round(sum(coef1[1,1]),digits=3),nsmall=0)), #coefficients 
                  (format(round(sum(coef2[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coef3[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coef4[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coef5[1,1]),digits=3),nsmall=0))),
                 
             c((format(round(sum(coef1[1,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[1,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[1,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[1,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[1,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[1,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[1,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[1,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[1,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[1,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[2,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[2,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[2,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[2,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[2,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[2,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[2,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[2,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[2,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[2,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[2,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[2,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[2,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[2,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[2,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[3,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[3,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[3,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[3,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[3,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[3,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[3,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[3,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[3,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[3,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[3,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[3,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[3,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[3,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[3,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[4,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[4,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[4,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[4,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[4,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[4,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[4,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[4,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[4,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[4,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[4,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[4,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[4,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[4,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[4,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[5,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[5,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[5,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[5,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[5,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[5,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[5,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[5,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[5,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[5,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[5,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[5,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[5,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[5,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[5,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[6,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[6,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[6,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[6,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[6,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[6,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[6,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[6,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[6,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[6,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[6,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[6,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[6,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[6,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[6,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[7,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[7,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[7,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[7,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[7,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[7,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[7,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[7,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[7,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[7,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[7,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[7,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[7,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[7,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[7,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[8,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[8,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[8,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[8,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[8,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[8,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[8,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[8,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[8,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[8,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[8,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[8,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[8,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[8,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[8,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[9,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[9,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[9,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[9,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[9,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[9,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[9,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[9,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[9,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[9,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[9,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[9,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[9,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[9,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[9,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[10,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[10,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[10,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[10,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[10,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[10,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[10,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[10,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[10,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[10,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[10,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[10,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[10,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[10,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[10,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[11,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[11,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[11,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[11,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[11,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[11,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[11,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[11,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[11,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[11,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[11,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[11,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[11,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[11,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[11,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[12,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[12,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[12,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[12,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[12,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[12,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[12,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[12,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[12,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[12,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[12,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[12,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[12,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[12,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[12,4]),digits=3),nsmall=0))),
             c((format(round(sum(coef1[13,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[13,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[13,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[13,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[13,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[13,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[13,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[13,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[13,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[13,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[13,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[13,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[13,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[13,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[13,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[14,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coef2[14,1]),digits=3),nsmall=0)),
               (format(round(sum(coef3[14,1]),digits=3),nsmall=0)),
               (format(round(sum(coef4[14,1]),digits=3),nsmall=0)),
               (format(round(sum(coef5[14,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[14,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coef2[14,2]),digits=3),nsmall=0)),
               (format(round(sum(coef3[14,2]),digits=3),nsmall=0)),
               (format(round(sum(coef4[14,2]),digits=3),nsmall=0)),
               (format(round(sum(coef5[14,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coef1[14,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coef2[14,4]),digits=3),nsmall=0)),
               (format(round(sum(coef3[14,4]),digits=3),nsmall=0)),
               (format(round(sum(coef4[14,4]),digits=3),nsmall=0)),
               (format(round(sum(coef5[14,4]),digits=3),nsmall=0))),
             
             
                c((format(round(summary(test1)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(test2)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(test3)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(test4)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(test5)$r.squared[1],digits=3),nsmall=0))),
                  
             c((format(round(summary(test1)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(test2)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(test3)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(test4)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(test5)$adj.r.squared[1],digits=3),nsmall=0))),
             
                c((format(round(nobs(test1),digits=3),nsmall=0)),
                  (format(round(nobs(test2),digits=3),nsmall=0)),
                  (format(round(nobs(test3),digits=3),nsmall=0)),
                  (format(round(nobs(test4),digits=3),nsmall=0)),
                  (format(round(nobs(test5),digits=3),nsmall=0))), 
             
             #### dependent variable is differences = self rating - farmer rating 
             
             c((format(round(sum(coeft1[1,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[1,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[1,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[1,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[1,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[1,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[1,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[1,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[1,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[1,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[1,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[1,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[1,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[1,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[1,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[2,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[2,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[2,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[2,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[2,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[2,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[2,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[2,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[2,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[2,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[2,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[2,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[2,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[2,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[2,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[3,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[3,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[3,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[3,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[3,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[3,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[3,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[3,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[3,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[3,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[3,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[3,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[3,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[3,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[3,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[4,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[4,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[4,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[4,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[4,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[4,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[4,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[4,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[4,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[4,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[4,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[4,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[4,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[4,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[4,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[5,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[5,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[5,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[5,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[5,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[5,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[5,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[5,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[5,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[5,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[5,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[5,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[5,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[5,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[5,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[6,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[6,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[6,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[6,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[6,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[6,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[6,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[6,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[6,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[6,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[6,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[6,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[6,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[6,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[6,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[7,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[7,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[7,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[7,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[7,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[7,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[7,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[7,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[7,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[7,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[7,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[7,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[7,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[7,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[7,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[8,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[8,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[8,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[8,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[8,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[8,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[8,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[8,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[8,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[8,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[8,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[8,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[8,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[8,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[8,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[9,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[9,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[9,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[9,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[9,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[9,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[9,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[9,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[9,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[9,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[9,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[9,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[9,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[9,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[9,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[10,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[10,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[10,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[10,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[10,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[10,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[10,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[10,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[10,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[10,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[10,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[10,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[10,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[10,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[10,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[11,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[11,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[11,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[11,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[11,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[11,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[11,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[11,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[11,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[11,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[11,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[11,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[11,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[11,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[11,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[12,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[12,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[12,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[12,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[12,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[12,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[12,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[12,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[12,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[12,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[12,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[12,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[12,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[12,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[12,4]),digits=3),nsmall=0))),
             c((format(round(sum(coeft1[13,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[13,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[13,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[13,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[13,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[13,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[13,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[13,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[13,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[13,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[13,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[13,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[13,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[13,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[13,4]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[14,1]),digits=3),nsmall=0)), #coefficients 
               (format(round(sum(coeft2[14,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[14,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[14,1]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[14,1]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[14,2]),digits=3),nsmall=0)),  #SE
               (format(round(sum(coeft2[14,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[14,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[14,2]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[14,2]),digits=3),nsmall=0))),
             
             c((format(round(sum(coeft1[14,4]),digits=3),nsmall=0)),  #pvalue
               (format(round(sum(coeft2[14,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft3[14,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft4[14,4]),digits=3),nsmall=0)),
               (format(round(sum(coeft5[14,4]),digits=3),nsmall=0))),
             
             
             c((format(round(summary(testt1)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt2)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt3)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt4)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt5)$r.squared[1],digits=3),nsmall=0))),
             
             c((format(round(summary(testt1)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt2)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt3)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt4)$adj.r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(testt5)$adj.r.squared[1],digits=3),nsmall=0))),
             
             c((format(round(nobs(testt1),digits=3),nsmall=0)),
               (format(round(nobs(testt2),digits=3),nsmall=0)),
               (format(round(nobs(testt3),digits=3),nsmall=0)),
               (format(round(nobs(testt4),digits=3),nsmall=0)),
               (format(round(nobs(testt5),digits=3),nsmall=0))))


save(mat, paste(path_2,"papers/perceptions/results/res_clust.Rd", sep = "/"))
