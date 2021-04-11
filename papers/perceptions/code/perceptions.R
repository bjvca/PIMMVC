path <- getwd()
library(ggplot2)
library(ggsignif) 
library(miceadds)
library(fBasics)
library(sjPlot)
library(mice)
library(texreg)
library(graphics)
library(data.table)
library(formattable)
library(expss)
library(dplyr)
library(tidyr)
library(gridExtra)
library(irrICC)
library(reshape2)

options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

################# FARMERS ######################

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

farmers1 <- farmers
farmers1[farmers1=="n/a"] <- NA 

farmers[farmers=="999"] <- NA

farmers$gender <- 0
farmers$gender [farmers$hh.maize.q25=="Female"] <- 1 #female farmers

farmers$educ <- 0
farmers$educ[farmers$hh.maize.q27=="b" | farmers$hh.maize.q27=="c" | farmers$hh.maize.q27=="d" | farmers$hh.maize.q27=="e" | 
               farmers$hh.maize.q27=="f" | farmers$hh.maize.q27=="g" ] <- 1 #educated farmers

farmers$married <- ifelse(farmers$hh.maize.q26 == 'a', 1, 0)  #married farmers 

################# INPUT DEALERS ######################

## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))

dealers1 <- dealers 
##Index for overall rating by dealers for themselves table
dealers1$ratee_rating_overall <- rowSums(dealers1[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers1$ratee_rating_overall)

dealers1[dealers1=="n/a"] <- NA

##Index for overall rating by dealers for themselves table
dealers$ratee_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$ratee_rating_overall)

################# TRADERS ######################


###Getting traders' data 
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

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

dealers$client_service <- 0
dealers$client_service[dealers$hh.maize.q67=="2" | dealers$hh.maize.q67=="3" | dealers$hh.maize.q68=="2" | dealers$hh.maize.q68=="3"] <- 1

dealers$gender <- ifelse(dealers$hh.maize.q7 == 'Female', 1, 0)
dealers$educ <- 0
dealers$educ[dealers$hh.maize.q9=="b" | dealers$hh.maize.q9=="c" | dealers$hh.maize.q9=="d" | dealers$hh.maize.q9=="e" | 
               dealers$hh.maize.q9=="f" | dealers$hh.maize.q9=="g" ] <- 1 
dealers$married <- ifelse(dealers$hh.maize.q8 == 'a', 1, 0) 


#subset for merging 
dealers_pool <- subset(dealers, select = c('id.agro' , 'hh.maize.q7','hh.maize.q79','hh.maize.q80','hh.maize.q81','hh.maize.q83','ratee_rating_overall','client_service'
                                           ,'hh.maize.q6d','hh.maize.q6g','hh.maize.q8','hh.maize.q9'))
names(dealers_pool) <- c("id.ratee","gender_ratee","rating_location_ratee","rating_price_ratee","rating_quality_ratee","rating_reputation_ratee", 
                         "ratee_rating_overall","client_service","other_competitors","age_ratee","marital_status_ratee","education_ratee")

merged_dealer_pool <- merge(ratings,dealers_pool, by="id.ratee")
merged_dealer_pool[merged_dealer_pool=="999"] <- 0
merged_dealer_pool[merged_dealer_pool=="n/a"] <- NA
merged_dealer_pool$ratee_who <- 1

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

traders$client_service <- 1
traders$client_service [traders$hh.maize.q30.g=="TRUE"]<- 0

traders$gender <- ifelse(traders$hh.maize.q7 == 'Female', 1, 0)
traders$educ <- 0
traders$educ[traders$hh.maize.q9=="b" | traders$hh.maize.q9=="c" | traders$hh.maize.q9=="d" | traders$hh.maize.q9=="e" | 
               traders$hh.maize.q9=="f" | traders$hh.maize.q9=="g" ] <- 1 
traders$married <- ifelse(traders$hh.maize.q8 == 'a', 1, 0) 


#subset for merging 
traders_pool <- subset(traders, select = c('id.trader' , 'hh.maize.q7','hh.maize.q40a','hh.maize.q40b','hh.maize.q40c','hh.maize.q40e','ratee_rating_overall','client_service'
                                           ,'hh.maize.q21','hh.maize.q6','hh.maize.q8','hh.maize.q9'))
names(traders_pool) <- c("id.ratee","gender_ratee","rating_location_ratee","rating_price_ratee","rating_quality_ratee","rating_reputation_ratee", 
                         "ratee_rating_overall","client_service","other_competitors","age_ratee","marital_status_ratee","education_ratee")

#Merging the datasets
merged_trader_pool <- merge(ratings_trader,traders_pool, by="id.ratee")
merged_trader_pool[merged_trader_pool=="999"] <- 0
merged_trader_pool[merged_trader_pool=="n/a"] <- NA

merged_trader_pool$ratee_who <- 2

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
millers$client_service <- 1
millers$client_service [millers$hh.maize.q25.k=="TRUE"] <- 0

millers[millers=="999"] <- NA

millers$gender <- ifelse(millers$hh.maize.q7 == 'Female', 1, 0)
millers$educ <- 0
millers$educ[millers$hh.maize.q9=="b" | millers$hh.maize.q9=="c" | millers$hh.maize.q9=="d" | millers$hh.maize.q9=="e" | 
               millers$hh.maize.q9=="f" | millers$hh.maize.q9=="g" ] <- 1 
millers$married <- ifelse(millers$hh.maize.q8 == 'a', 1, 0) 

#subset for merging 
millers_pool <- subset(millers, select = c('id.miller' , 'hh.maize.q7','hh.maize.q36','hh.maize.q37','hh.maize.q38','hh.maize.q40','ratee_rating_overall','client_service'
                                           ,'hh.maize.q6d','hh.maize.q6g','hh.maize.q8','hh.maize.q9'))
names(millers_pool) <- c("id.ratee","gender_ratee","rating_location_ratee","rating_price_ratee","rating_quality_ratee","rating_reputation_ratee", 
                         "ratee_rating_overall","client_service","other_competitors","age_ratee","marital_status_ratee","education_ratee")
#Merging the datasets
merged_miller_pool <- merge(ratings_mill,millers_pool, by="id.ratee")
merged_miller_pool[merged_miller_pool=="999"] <- 0
merged_miller_pool[merged_miller_pool=="n/a"] <- NA

merged_miller_pool$ratee_who  <- 3

#deleting the columns not needed
merged_dealer_pool <- merged_dealer_pool[-c(7)]
merged_trader_pool <- merged_trader_pool[-c(7)]
merged_miller_pool <- merged_miller_pool[-c(7)]

##################  FINAL DATASET ########################

pool <-rbind(merged_dealer_pool,merged_miller_pool,merged_trader_pool)


pool$farmer_fem <- ifelse(pool$farmer_gender == 'Female', 1, 0) #gender dummy for farmers 
pool$ratee_fem <- ifelse(pool$gender_ratee == 'Female', 1, 0)   #gender dummy for ratees 
pool$interaction_yes <- ifelse(pool$interaction == 'Yes', 1, 0) #dummy for interaction between rater and ratee 

#a	No formal education / b	Some primary / c	Finished primary / d	Some secondary / e	Finished secondary / f	Higher than secondary / g Other
pool$no_educ <- ifelse(pool$education == 'a', 1, 0) #no education

#some primary educ/finished primary educ 
pool$primary_educ <- 0
pool$primary_educ[pool$education=="b" | pool$education=="c"] <- 1

#some secondary educ/finished secondary educ 
pool$secondary_educ <- 0
pool$secondary_educ[pool$education=="d" | pool$education=="e"] <- 1

pool$higher_secondary_educ <- ifelse(pool$education == 'f', 1, 0) #higher than secondary education

### Some level of education --- dummy = 1 if some level of education, 0 if no formal education
#farmers
pool$educ <- 0
pool$educ[pool$education=="b" | pool$education=="c" | pool$education=="d" | pool$education=="e" | pool$education=="f" | pool$education=="g" ] <- 1
#ratees
pool$educ_ratee <- 0
pool$educ_ratee[pool$education_ratee=="b" | pool$education_ratee=="c" | pool$education_ratee=="d" | 
                  pool$education_ratee=="e" | pool$education_ratee=="f" | pool$education_ratee=="g" ] <- 1

#MARITAL STATUS
#a	Married /b	Widowed / c	Divorced / d	Separated / e	Single
pool$married <- ifelse(pool$marital_status == 'a', 1, 0) #dummy = 1 if married---- farmers 
pool$married_ratee <- ifelse(pool$marital_status_ratee == 'a', 1, 0) #dummy = 1 if married---- ratee

#most imp income source  
#a = crop farming 
pool$crop_farming <- ifelse(pool$source_income == 'a', 1, 0) #dummy = 1 if crop farming 

#member of farming organization 
pool$member_dummy <- ifelse(pool$member == 'Yes', 1, 0) #dummy = 1 if member

#dummy for maize sell - any maize sold from the harvest in the first season of 2018
pool$maize_sold <- ifelse(pool$maize_sell == 'Yes', 1, 0) #dummy = 1 if sold

#dummy for dealer id
pool$dealer_dummy <- ifelse(pool$ratee_who == '1', 1, 0) 
#dummy for trader id 
pool$trader_dummy <- ifelse(pool$ratee_who == '2', 1, 0) 
#dummy for miller id 
pool$miller_dummy <- ifelse(pool$ratee_who == '3', 1, 0) 

#cleaning the data
pool<-pool[!(pool$id.ratee=="n/a"),]
pool<-pool[!(pool$id.ratee=="."),]

### CLUSTERED REGRESSIONS 

####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS ##############



################# OVERALL RATING ###########################


#all variables 
mod1_gender<- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                         + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 

se1 <- sqrt(diag(vcov(mod1_gender)))
lm_res1<-mod1_gender$lm_res

##Interaction with farmer_fem and gender of ratee
mod5_gender <- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster="id.ratee") 
se5 <- sqrt(diag(vcov(mod5_gender)))
lm_res5<-mod5_gender$lm_res


#saving regression
screenreg(list(mod1_gender,mod5_gender), file="gen_overall_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod1_gender,mod5_gender), file="gen_overall_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)




################# LOCATION RATING ###########################

#all variables 
mod11_gender<- lm.cluster(data = pool, formula = rating_location ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy, cluster = "id.ratee") 
se11 <- sqrt(diag(vcov(mod11_gender)))
lm_res11<-mod11_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod15_gender <- lm.cluster(data = pool, formula = rating_location ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy, cluster = "id.ratee") 
se15 <- sqrt(diag(vcov(mod15_gender)))
lm_res15<-mod15_gender$lm_res


#saving regression
screenreg(list(mod11_gender,mod15_gender), file="gen_loc_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod11_gender,mod15_gender), file="gen_loc_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)



################# QUALITY RATING ###########################

#all variables 
mod21_gender<- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se21 <- sqrt(diag(vcov(mod21_gender)))
lm_res21<-mod21_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod25_gender <- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se25 <- sqrt(diag(vcov(mod25_gender)))
lm_res25<-mod25_gender$lm_res



#saving regression
screenreg(list(mod21_gender,mod25_gender), file="gen_qual_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod21_gender,mod25_gender), file="gen_qual_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)




################# PRICE RATING ###########################

#all variables 
mod31_gender<- lm.cluster(data = pool, formula = rating_price ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se31 <- sqrt(diag(vcov(mod31_gender)))
lm_res31<-mod31_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod35_gender <- lm.cluster(data = pool, formula = rating_price ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se35 <- sqrt(diag(vcov(mod35_gender)))
lm_res35<-mod35_gender$lm_res


#saving regression
screenreg(list(mod31_gender,mod35_gender), file="gen_price_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod31_gender,mod35_gender), file="gen_price_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)





################# REPUTATION RATING ###########################

#all variables 
mod41_gender<- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se41 <- sqrt(diag(vcov(mod41_gender)))
lm_res41<-mod41_gender$lm_res



##Interaction with farmer_fem and gender of ratee
mod45_gender <- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se45 <- sqrt(diag(vcov(mod45_gender)))
lm_res45<-mod45_gender$lm_res


#saving regression
screenreg(list(mod41_gender,mod45_gender), file="gen_reputation_farmer", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod41_gender,mod45_gender), file="gen_reputation_farmer_latex", stars = c(0.01, 0.05, 0.1), digits=4)






#########################################################################################################################





####### DEPENDENT VARIABLE -- RATINGS FROM RATEES ##############


################# OVERALL RATING ###########################

#all variables 
mod51_gender<- lm(data = pool, formula = ratee_rating_overall ~ ratee_fem + age_ratee + married_ratee + educ_ratee
                  + dealer_dummy + trader_dummy) 
se51 <- sqrt(diag(vcov(mod51_gender)))


#saving regression
screenreg(list(mod51_gender), file="gen_overall_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod51_gender), file="gen_overall_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)



################# LOCATION RATING ###########################

#all variables 
mod55_gender<- lm(data = pool, formula = rating_location_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                  + dealer_dummy + trader_dummy ) 
se55 <- sqrt(diag(vcov(mod55_gender)))


#saving regression
screenreg(list(mod55_gender), file="gen_location_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod55_gender), file="gen_location_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# QUALITY RATING ###########################

#all variables 
mod59_gender<- lm(data = pool, formula = rating_quality_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                  + dealer_dummy + trader_dummy) 
se59<- sqrt(diag(vcov(mod59_gender)))



#saving regression
screenreg(list(mod59_gender), file="gen_quality_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod59_gender), file="gen_quality_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)



################# PRICE RATING ###########################

#all variables 
mod63_gender<- lm(data = pool, formula = rating_price_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee
                  + dealer_dummy + trader_dummy ) 
se63 <- sqrt(diag(vcov(mod63_gender)))


#saving regression
screenreg(list(mod63_gender), file="gen_price_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod63_gender), file="gen_price_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# REPUTATION RATING ###########################

#all variables 
mod67_gender<- lm(data = pool, formula = rating_reputation_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                  + dealer_dummy + trader_dummy ) 
se67 <- sqrt(diag(vcov(mod67_gender)))



#saving regression
screenreg(list(mod67_gender), file="gen_reputation_ratee", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod67_gender), file="gen_reputation_ratee_latex", stars = c(0.01, 0.05, 0.1), digits=4)



####### DEPENDENT VARIABLE -- DIFFERENCES IN RATINGS (RATER RATING - RATEE RATING) ##############

#differences in ratings 
pool$ratingoverall_diff <- pool$rating_overall - pool$ratee_rating_overall #difference in overall ratings 
pool$ratingloc_diff <- pool$rating_location - pool$rating_location_ratee ##diff in location ratings 
pool$ratingprice_diff <- pool$rating_price - pool$rating_price_ratee ##diff in price ratings 
pool$ratingqual_diff <- pool$rating_quality - pool$rating_quality_ratee ##diff in quality ratings 
pool$ratingrepu_diff <- pool$rating_reputation - pool$rating_reputation_ratee ##diff in reputation ratings 

################# OVERALL RATING ###########################

#all variables 
mod71_gender<- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se71 <- sqrt(diag(vcov(mod71_gender)))
lm_res71<-mod71_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod75_gender <- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se75 <- sqrt(diag(vcov(mod75_gender)))
lm_res75<-mod75_gender$lm_res


#saving regression
screenreg(list(mod71_gender, mod75_gender), file="gen_overall_diff", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod71_gender, mod75_gender), file="gen_overall_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)


################# LOCATION RATING ###########################

#all variables 
mod78_gender<- lm.cluster(data = pool, formula = ratingloc_diff ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se78 <- sqrt(diag(vcov(mod78_gender)))
lm_res78<-mod78_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod82_gender <- lm.cluster(data = pool, formula = ratingloc_diff ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se82 <- sqrt(diag(vcov(mod82_gender)))
lm_res82<-mod82_gender$lm_res


#saving regression
screenreg(list(mod78_gender,mod82_gender), file="gen_loc_diff", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod78_gender, mod82_gender), file="gen_loc_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)




################# PRICE RATING ###########################

#all variables 
mod85_gender<- lm.cluster(data = pool, formula = ratingprice_diff ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se85 <- sqrt(diag(vcov(mod85_gender)))
lm_res85<-mod85_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod89_gender <- lm.cluster(data = pool, formula = ratingprice_diff ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se89 <- sqrt(diag(vcov(mod89_gender)))
lm_res89<-mod89_gender$lm_res

#saving regression
screenreg(list(mod85_gender, mod89_gender), file="gen_price_diff", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod85_gender,mod89_gender), file="gen_price_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)



################# QUALITY RATING ###########################

#all variables 
mod92_gender<- lm.cluster(data = pool, formula = ratingqual_diff ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se92 <- sqrt(diag(vcov(mod92_gender)))
lm_res92<-mod92_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod96_gender <- lm.cluster(data = pool, formula = ratingqual_diff ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy, cluster = "id.ratee") 
se96 <- sqrt(diag(vcov(mod96_gender)))
lm_res96<-mod96_gender$lm_res

#saving regression
screenreg(list(mod92_gender, mod96_gender), file="gen_qual_diff", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod92_gender, mod96_gender), file="gen_qual_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)


###################### REPUTATION RATING #########################

#all variables 
mod99_gender<- lm.cluster(data = pool, formula = ratingrepu_diff ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se99 <- sqrt(diag(vcov(mod99_gender)))
lm_res99<-mod99_gender$lm_res


##Interaction with farmer_fem and gender of ratee
mod103_gender <- lm.cluster(data = pool, formula = ratingrepu_diff ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                            + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy , cluster = "id.ratee") 
se103 <- sqrt(diag(vcov(mod103_gender)))
lm_res103<-mod103_gender$lm_res

#saving regression
screenreg(list(mod99_gender, mod103_gender), file="gen_repu_diff", stars = c(0.01, 0.05, 0.1), digits=4)
texreg(list(mod99_gender,mod103_gender), file="gen_repu_diff_latex", stars = c(0.01, 0.05, 0.1), digits=4)

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




########## HYPOTHESIS 2: "Female raters give higher ratings when compared to male raters" ##############


## MEAN RATINGS --- ALL raters and ratees --- raters are female
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



## MEAN RATINGS --- ALL raters and input dealers --- raters are female

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



## MEAN RATINGS --- ALL raters and traders --- raters are female

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


## MEAN RATINGS --- ALL raters and millers --- raters are female
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








########## HYPOTHESIS 3: "Self-ratings given by females are lower compared to males" ##############


## MEAN RATINGS --- ALL raters and ratees --- ratees are female or male 
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


## MEAN RATINGS --- ALL raters and input dealers --- ratees are female and male 

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



## MEAN RATINGS --- ALL raters and traders --- ratees are female and male 

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


## MEAN RATINGS --- ALL raters and millers --- ratees are female and male 
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







########## HYPOTHESIS 4: "1. Female ratees receive lower ratings from raters than male ratees. " ##############


## MEAN RATINGS --- ALL raters and ratees --- ratees are female or male 
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


## MEAN RATINGS --- ALL raters and input dealers --- ratees are female and male 

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



## MEAN RATINGS --- ALL raters and traders --- ratees are female and male 

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


## MEAN RATINGS --- ALL raters and millers --- ratees are female and male 
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




##########################################################################################################################





#### SUMMARY STATS (MAIN VARIABLES) ####


sum_stat <- matrix(c(round(mean(pool$rating_location, na.rm=T), digits=2),
                     round(mean(pool$rating_price, na.rm=T), digits=2),
                     round(mean(pool$rating_quality, na.rm=T), digits=2),
                     round(mean(pool$rating_reputation, na.rm=T), digits=2),
                     round(mean(pool$rating_overall, na.rm=T), digits=2),
                     round(mean(pool$farmer_fem, na.rm=T), digits=2),
                     round(mean(pool$ratee_fem, na.rm=T), digits=2),
                     round(mean(pool$age, na.rm=T), digits=2),
                     round(mean(pool$married, na.rm=T), digits=2),
                     round(mean(pool$educ, na.rm=T), digits=2),
                     round(mean(pool$tarmac, na.rm=T), digits=2),
                     round(mean(pool$murram, na.rm=T), digits=2),
                     round(mean(pool$interaction_yes, na.rm=T), digits=2),
                     round(mean(pool$rating_location_ratee, na.rm=T), digits=2),
                     round(mean(pool$rating_price_ratee, na.rm=T), digits=2),
                     round(mean(pool$rating_quality_ratee, na.rm=T), digits=2),
                     round(mean(pool$rating_reputation_ratee, na.rm=T), digits=2),
                     round(mean(pool$ratee_rating_overall, na.rm=T), digits=2),
                     round(mean(pool$age_ratee, na.rm=T), digits=2),
                     round(mean(pool$educ_ratee, na.rm=T), digits=2),
                     round(mean(pool$married_ratee, na.rm=T), digits=2),           
                     round(mean(pool$dealer_dummy, na.rm=T), digits=2),
                     round(mean(pool$trader_dummy, na.rm=T), digits=2),
                     round(mean(pool$ratingoverall_diff, na.rm=T), digits=2),
                     round(mean(pool$ratingloc_diff, na.rm=T), digits=2),
                     round(mean(pool$ratingprice_diff, na.rm=T), digits=2),
                     round(mean(pool$ratingqual_diff, na.rm=T), digits=2),
                     round(mean(pool$ratingrepu_diff, na.rm=T), digits=2),
                     
                     round(sd(pool$rating_location, na.rm=T), digits=2),
                     round(sd(pool$rating_price, na.rm=T), digits=2),
                     round(sd(pool$rating_quality, na.rm=T), digits=2),
                     round(sd(pool$rating_reputation, na.rm=T), digits=2),
                     round(sd(pool$rating_overall, na.rm=T), digits=2),
                     round(sd(pool$farmer_fem, na.rm=T), digits=2),
                     round(sd(pool$ratee_fem, na.rm=T), digits=2),
                     round(sd(pool$age, na.rm=T), digits=2),
                     round(sd(pool$married, na.rm=T), digits=2),
                     round(sd(pool$educ, na.rm=T), digits=2),
                     round(sd(pool$tarmac, na.rm=T), digits=2),
                     round(sd(pool$murram, na.rm=T), digits=2),
                     round(sd(pool$interaction_yes, na.rm=T), digits=2),
                     round(sd(pool$rating_location_ratee, na.rm=T), digits=2),
                     round(sd(pool$rating_price_ratee, na.rm=T), digits=2),
                     round(sd(pool$rating_quality_ratee, na.rm=T), digits=2),
                     round(sd(pool$rating_reputation_ratee, na.rm=T), digits=2),
                     round(sd(pool$ratee_rating_overall, na.rm=T), digits=2),
                     round(sd(pool$age_ratee, na.rm=T), digits=2),
                     round(sd(pool$educ_ratee, na.rm=T), digits=2),
                     round(sd(pool$married_ratee, na.rm=T), digits=2),           
                     round(sd(pool$dealer_dummy, na.rm=T), digits=2),
                     round(sd(pool$trader_dummy, na.rm=T), digits=2),
                     round(sd(pool$ratingoverall_diff, na.rm=T), digits=2),
                     round(sd(pool$ratingloc_diff, na.rm=T), digits=2),
                     round(sd(pool$ratingprice_diff, na.rm=T), digits=2),
                     round(sd(pool$ratingqual_diff, na.rm=T), digits=2),
                     round(sd(pool$ratingrepu_diff, na.rm=T), digits=2),
                     
                     round(min(pool$rating_location, na.rm=T), digits=2),
                     round(min(pool$rating_price, na.rm=T), digits=2),
                     round(min(pool$rating_quality, na.rm=T), digits=2),
                     round(min(pool$rating_reputation, na.rm=T), digits=2),
                     round(min(pool$rating_overall, na.rm=T), digits=2),
                     round(min(pool$farmer_fem, na.rm=T), digits=2),
                     round(min(pool$ratee_fem, na.rm=T), digits=2),
                     round(min(pool$age, na.rm=T), digits=2),
                     round(min(pool$married, na.rm=T), digits=2),
                     round(min(pool$educ, na.rm=T), digits=2),
                     round(min(pool$tarmac, na.rm=T), digits=2),
                     round(min(pool$murram, na.rm=T), digits=2),
                     round(min(pool$interaction_yes, na.rm=T), digits=2),
                     round(min(pool$rating_location_ratee, na.rm=T), digits=2),
                     round(min(pool$rating_price_ratee, na.rm=T), digits=2),
                     round(min(pool$rating_quality_ratee, na.rm=T), digits=2),
                     round(min(pool$rating_reputation_ratee, na.rm=T), digits=2),
                     round(min(pool$ratee_rating_overall, na.rm=T), digits=2),
                     round(min(pool$age_ratee, na.rm=T), digits=2),
                     round(min(pool$educ_ratee, na.rm=T), digits=2),
                     round(min(pool$married_ratee, na.rm=T), digits=2),           
                     round(min(pool$dealer_dummy, na.rm=T), digits=2),
                     round(min(pool$trader_dummy, na.rm=T), digits=2),
                     round(min(pool$ratingoverall_diff, na.rm=T), digits=2),
                     round(min(pool$ratingloc_diff, na.rm=T), digits=2),
                     round(min(pool$ratingprice_diff, na.rm=T), digits=2),
                     round(min(pool$ratingqual_diff, na.rm=T), digits=2),
                     round(min(pool$ratingrepu_diff, na.rm=T), digits=2),
                     
                     round(max(pool$rating_location, na.rm=T), digits=2),
                     round(max(pool$rating_price, na.rm=T), digits=2),
                     round(max(pool$rating_quality, na.rm=T), digits=2),
                     round(max(pool$rating_reputation, na.rm=T), digits=2),
                     round(max(pool$rating_overall, na.rm=T), digits=2),
                     round(max(pool$farmer_fem, na.rm=T), digits=2),
                     round(max(pool$ratee_fem, na.rm=T), digits=2),
                     round(max(pool$age, na.rm=T), digits=2),
                     round(max(pool$married, na.rm=T), digits=2),
                     round(max(pool$educ, na.rm=T), digits=2),
                     round(max(pool$tarmac, na.rm=T), digits=2),
                     round(max(pool$murram, na.rm=T), digits=2),
                     round(max(pool$interaction_yes, na.rm=T), digits=2),
                     round(max(pool$rating_location_ratee, na.rm=T), digits=2),
                     round(max(pool$rating_price_ratee, na.rm=T), digits=2),
                     round(max(pool$rating_quality_ratee, na.rm=T), digits=2),
                     round(max(pool$rating_reputation_ratee, na.rm=T), digits=2),
                     round(max(pool$ratee_rating_overall, na.rm=T), digits=2),
                     round(max(pool$age_ratee, na.rm=T), digits=2),
                     round(max(pool$educ_ratee, na.rm=T), digits=2),
                     round(max(pool$married_ratee, na.rm=T), digits=2),           
                     round(max(pool$dealer_dummy, na.rm=T), digits=2),
                     round(max(pool$trader_dummy, na.rm=T), digits=2),
                     round(max(pool$ratingoverall_diff, na.rm=T), digits=2),
                     round(max(pool$ratingloc_diff, na.rm=T), digits=2),
                     round(max(pool$ratingprice_diff, na.rm=T), digits=2),
                     round(max(pool$ratingqual_diff, na.rm=T), digits=2),
                     round(max(pool$ratingrepu_diff, na.rm=T), digits=2)
),ncol=4)

rownames(sum_stat) <- c('Location rating by rater', 'Price rating by rater','Quality rating by rater', 
                        'Reputation rating by rater', 'Overall rating by rater','Gender of rater','Gender of ratee',
                        'Age of raters', 'Marital status (raters)', 'Education (Raters)','Distance of homestead to tarmac road',
                        'Distance of homestead to murram road', 'Interaction between rater and ratee','Location rating by ratee', 
                        'Price rating by ratee','Quality rating by ratee','Reputation rating by ratee', 'Overall rating by ratee', 
                        'Age of ratee','Education (Ratee)','Marital Status (Ratee)', 'Dealer dummy','Trader dummy',
                        'Difference in overall rating','Difference in location rating','Difference in price rating',
                        'Difference in quality rating', 'Difference in reputation rating')
colnames(sum_stat) <- c('Mean','Standard Deviation','Minimum','Maximum')
trial.table <- as.table(sum_stat)
formattable(sum_stat)

png(paste(path_2, "/papers/perceptions/figure/sum_stat.png",sep = "/"), units="px", height=5000, width= 4500, res=600)
grid.table(sum_stat)
dev.off()




##########################################################################################################################


############# GRAPHS WITH WILCOX TEST ####################

df <- data.frame(c(mean(pool$ratee_rating_overall),tapply(pool$rating_overall,pool$interaction, mean )[2:3]))
names(df) <- "score"
rownames(df) <- NULL
df$levels <- c("Ratee","No Business Interaction","Business Interaction")
df <- df[order(df$score,decreasing = TRUE),]
df$levels <- factor(df$levels,levels= c("Ratee","No Business Interaction","Business Interaction"))

png(paste(path_2, "/papers/perceptions/figure/wilcox_bar.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + 
  geom_signif(comparisons = list(c("Ratee", "Business Interaction")), annotations="***", y_position = 4.3, tip_length = 0.03) +
  geom_signif(comparisons = list(c("Ratee", "No Business Interaction")), annotations="***", y_position = 4.5, tip_length = 0.03) +
  geom_signif(comparisons = list(c("Business Interaction", "No Business Interaction")), annotations="***", y_position = 4.1, tip_length = 0.03)
dev.off()

###Tests in the graph
### OVERALL RATING 
wilcox.test(pool$rating_overall[pool$interaction_yes=="0"],pool$ratee_rating_overall)
wilcox.test(pool$rating_overall[pool$interaction_yes=="1"],pool$ratee_rating_overall)
wilcox.test(pool$rating_overall[pool$interaction_yes=="1"],pool$rating_overall[pool$interaction_yes=="0"])
#for all 3, we reject the null that the distributions are same


##Likert scales bar charts for the different components of the scores

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

####################################################################################################

#### WHETHER INDIVIDUAL MEANS DIFFERENT FROM OVERALL MEAN #####
#### FIXED EFFECTS ######

########### rating from farmers ############
fe_modoverall <- lm(rating_overall ~ id.ratee, data = pool)
summary(fe_modoverall)
#F-statistic:  1.76 on 525 and 3384 DF,  p-value: < 0.00000000000000022
#reject null at 5% (f-stat > f-table value)
#indv means diff from overall mean 

fe_modlocation <- lm(rating_location ~ id.ratee, data = pool)
summary(fe_modlocation)
#reject null --- therefore, indv means diff from overall mean 

fe_modqual <- lm(rating_quality ~ id.ratee, data = pool)
summary(fe_modqual) #reject null, indv means diff from overall mean 

fe_modprice <- lm(rating_price ~ id.ratee, data = pool)
summary(fe_modprice) #reject null, indv means diff from overall mean 

fe_modrepu <- lm(rating_reputation ~ id.ratee, data = pool)
summary(fe_modrepu)  #reject null, indv means diff from overall mean 



########### difference between rating from raters and ratees ############
fe_modoverall_diff <- lm(ratingoverall_diff ~ id.ratee, data = pool)
summary(fe_modoverall_diff)
#reject null 
#indv means diff from overall mean 

fe_modlocation_diff <- lm(ratingloc_diff ~ id.ratee, data = pool)
summary(fe_modlocation_diff)
#reject null --- therefore, indv means diff from overall mean 

fe_modqual_diff <- lm(ratingqual_diff ~ id.ratee, data = pool)
summary(fe_modqual_diff) #reject null, indv means diff from overall mean 

fe_modprice_diff <- lm(ratingprice_diff ~ id.ratee, data = pool)
summary(fe_modprice_diff) #reject null, indv means diff from overall mean 

fe_modrepu_diff <- lm(ratingrepu_diff ~ id.ratee, data = pool)
summary(fe_modrepu_diff)  #reject null, indv means diff from overall mean 


################################# ICC ####################################

#https://cran.r-project.org/web/packages/irrICC/vignettes/UserGuide.pdf

###### RATINGS FROM FARMERS VS SELF RATINGS 

#MODEL 1A INTER RATER RELIABILITY, MODEL 1B INTRA RATER RELIABILITY
#WE FOCUS ON MODEL 1A TO SEE HOW SIMILAR THE RATINGS ARE BETWEEN FARMERS AND RATEES

icc_overall <- pool[c("id.ratee","rating_overall","ratee_rating_overall")]
icc1a.fn(icc_overall)   #0.1256276 

icc_loc <- pool[c("id.ratee","rating_location","rating_location_ratee")]
icc1a.fn(icc_loc)   #0.2043416

icc_qual <- pool[c("id.ratee","rating_quality","rating_quality_ratee")]
icc1a.fn(icc_qual)   #0.1948547

icc_price <- pool[c("id.ratee","rating_price","rating_price_ratee")]
icc1a.fn(icc_price)   #0.1620352 

icc_rep <- pool[c("id.ratee","rating_reputation","rating_reputation_ratee")]
icc1a.fn(icc_rep)   #0.141539 



### Both inter and intra rater reliability ###

###########################################################################################
#transposing data
### overall rating 
icc <- pool[c("id.ratee","farmerID","rating_overall")]
icc_recast_overall <- recast(icc, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))
icc1a.fn(icc_recast_overall) #0.13741
icc1b.fn(icc_recast_overall) # 0.4892155

#ratings from ratees subset
icc_30 <- icc[icc$id.ratee %in%  names(table(icc$id.ratee))[table(icc$id.ratee) >30] , ]
icc_recast_overall30 <- recast(icc_30, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))
icc1a.fn(icc_recast_overall30) #0.4251301
icc1b.fn(icc_recast_overall30) # 0.4250636

#ratings from raters subset 
icc_6 <- icc[icc$farmerID %in%  names(table(icc$farmerID))[table(icc$farmerID) >6] , ]
icc_recast_overall6 <- recast(icc_6, id.ratee ~ farmerID, id.var = c("farmerID", "id.ratee"))
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
