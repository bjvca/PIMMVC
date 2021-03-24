path <- getwd()
library(ggplot2)
library(ggsignif) 
library(miceadds)
library(fBasics)
library(sjPlot)
library(mice)
library(texreg)
library(graphics)
library(png)
options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

################# FARMERS ######################

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

farmers1 <- farmers

farmers1[farmers1=="n/a"] <- NA 

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

## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))
##Index for overall rating by dealers for themselves 
dealers$ratee_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$ratee_rating_overall)

dealers$client_service <- 0
dealers$client_service[dealers$hh.maize.q67=="2" | dealers$hh.maize.q67=="3" | dealers$hh.maize.q68=="2" | dealers$hh.maize.q68=="3"] <- 1

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

###Getting traders' data 
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

###Index for overall rating from traders
traders$ratee_rating_overall <- rowSums(traders[c("hh.maize.q40a","hh.maize.q40b","hh.maize.q40c","hh.maize.q40d","hh.maize.q40e")])/5
summary(traders$ratee_rating_overall)
traders$client_service <- 1
traders$client_service [traders$hh.maize.q30.g=="TRUE"]<- 0

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

###Getting millers' data 
millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"))

###Index for overall rating from millers
millers$ratee_rating_overall <- rowSums(millers[c("hh.maize.q36","hh.maize.q37","hh.maize.q38","hh.maize.q39","hh.maize.q40")])/5
summary(millers$ratee_rating_overall)
millers$client_service <- 1
millers$client_service [millers$hh.maize.q25.k=="TRUE"] <- 0

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
                         + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with farmer_fem and interaction_yes
mod2_gender<- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem* interaction_yes + ratee_fem + age  + educ + tarmac
                         + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot1 <- interaction.plot(pool$farmer_fem, pool$interaction_yes, pool$rating_overall)

##Interaction with farmer_fem and education
mod3_gender <- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem*educ + interaction_yes + ratee_fem + age + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot2 <- interaction.plot(pool$farmer_fem, pool$educ, pool$rating_overall)

##Interaction with farmer_fem and age
mod4_gender <- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem*age + educ + interaction_yes + ratee_fem + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot3 <- interaction.plot(pool$farmer_fem, pool$age, pool$rating_overall)

##Interaction with farmer_fem and gender of ratee
mod5_gender <- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot4 <- interaction.plot(pool$farmer_fem, pool$ratee_fem, pool$rating_overall)

##Interaction with farmer_fem and marital status of farmer
mod6_gender <- lm.cluster(data = pool, formula = rating_overall ~  farmer_fem*married + ratee_fem + educ + interaction_yes + age + tarmac
                          + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot5 <- interaction.plot(pool$farmer_fem, pool$married, pool$rating_overall)


##Interaction with ratee_fem and interaction_yes 
mod7_gender <- lm.cluster(data = pool, formula = rating_overall ~  ratee_fem*interaction_yes + farmer_fem + married + educ + age + tarmac
                          + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot6 <- interaction.plot(pool$ratee_fem, pool$interaction_yes, pool$rating_overall)

##Interaction with ratee_fem and age of ratee
mod8_gender <- lm.cluster(data = pool, formula = rating_overall ~  ratee_fem* age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                          + murram + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot7 <- interaction.plot(pool$ratee_fem, pool$age_ratee, pool$rating_overall)

##Interaction with ratee_fem and marital status of ratee
mod9_gender <- lm.cluster(data = pool, formula = rating_overall ~  ratee_fem* married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                          + murram + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplot8 <- interaction.plot(pool$ratee_fem, pool$married_ratee, pool$rating_overall)

##Interaction with ratee_fem and education of ratee
mod10_gender <- lm.cluster(data = pool, formula = rating_overall ~  ratee_fem * educ_ratee + married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee")

#intplot9 <- interaction.plot(pool$ratee_fem, pool$educ_ratee, pool$rating_overall)

#saving regression
screenreg(list(mod1_gender,mod2_gender,mod3_gender,mod4_gender,mod5_gender,mod6_gender,mod7_gender,mod8_gender,mod9_gender,
               mod10_gender), file="gen_overall_farmer", stars = c(0.01, 0.05, 0.1), digits=4)


################# LOCATION RATING ###########################

#all variables 
mod11_gender<- lm.cluster(data = pool, formula = rating_location ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with farmer_fem and interaction_yes
mod12_gender<- lm.cluster(data = pool, formula = rating_location ~  farmer_fem* interaction_yes + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc1 <- interaction.plot(pool$farmer_fem, pool$interaction_yes, pool$rating_location)

##Interaction with farmer_fem and education
mod13_gender <- lm.cluster(data = pool, formula = rating_location ~  farmer_fem*educ + interaction_yes + ratee_fem + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc2 <- interaction.plot(pool$farmer_fem, pool$educ, pool$rating_location)

##Interaction with farmer_fem and age
mod14_gender <- lm.cluster(data = pool, formula = rating_location ~  farmer_fem*age + educ + interaction_yes + ratee_fem + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc3 <- interaction.plot(pool$farmer_fem, pool$age, pool$rating_location)

##Interaction with farmer_fem and gender of ratee
mod15_gender <- lm.cluster(data = pool, formula = rating_location ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc4 <- interaction.plot(pool$farmer_fem, pool$ratee_fem, pool$rating_location)

##Interaction with farmer_fem and marital status of farmer
mod16_gender <- lm.cluster(data = pool, formula = rating_location ~  farmer_fem*married + ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc5 <- interaction.plot(pool$farmer_fem, pool$married, pool$rating_location)


##Interaction with ratee_fem and interaction_yes 
mod17_gender <- lm.cluster(data = pool, formula = rating_location ~  ratee_fem*interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc6 <- interaction.plot(pool$ratee_fem, pool$interaction_yes, pool$rating_location)

##Interaction with ratee_fem and age of ratee
mod18_gender <- lm.cluster(data = pool, formula = rating_location ~  ratee_fem* age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + married_ratee + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc7 <- interaction.plot(pool$ratee_fem, pool$age_ratee, pool$rating_location)

##Interaction with ratee_fem and marital status of ratee
mod19_gender <- lm.cluster(data = pool, formula = rating_location ~  ratee_fem* married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + educ_ratee+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotloc8 <- interaction.plot(pool$ratee_fem, pool$married_ratee, pool$rating_location)

##Interaction with ratee_fem and education of ratee
mod20_gender <- lm.cluster(data = pool, formula = rating_location ~  ratee_fem * educ_ratee + married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram+ dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee")

#intplotloc9 <- interaction.plot(pool$ratee_fem, pool$educ_ratee, pool$rating_location)

#saving regression
screenreg(list(mod11_gender,mod12_gender,mod13_gender,mod14_gender,mod15_gender,mod16_gender,mod17_gender,mod18_gender,mod19_gender,
               mod20_gender), file="gen_loc_farmer", stars = c(0.01, 0.05, 0.1), digits=4)




################# QUALITY RATING ###########################

#all variables 
mod21_gender<- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with farmer_fem and interaction_yes
mod22_gender<- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem* interaction_yes + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual1 <- interaction.plot(pool$farmer_fem, pool$interaction_yes, pool$rating_quality)

##Interaction with farmer_fem and education
mod23_gender <- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem*educ + interaction_yes + ratee_fem + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual2 <- interaction.plot(pool$farmer_fem, pool$educ, pool$rating_quality)

##Interaction with farmer_fem and age
mod24_gender <- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem*age + educ + interaction_yes + ratee_fem + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual3 <- interaction.plot(pool$farmer_fem, pool$age, pool$rating_quality)

##Interaction with farmer_fem and gender of ratee
mod25_gender <- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual4 <- interaction.plot(pool$farmer_fem, pool$ratee_fem, pool$rating_quality)

##Interaction with farmer_fem and marital status of farmer
mod26_gender <- lm.cluster(data = pool, formula = rating_quality ~  farmer_fem*married + ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual5 <- interaction.plot(pool$farmer_fem, pool$married, pool$rating_quality)


##Interaction with ratee_fem and interaction_yes 
mod27_gender <- lm.cluster(data = pool, formula = rating_quality ~  ratee_fem*interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual6 <- interaction.plot(pool$ratee_fem, pool$interaction_yes, pool$rating_quality)

##Interaction with ratee_fem and age of ratee
mod28_gender <- lm.cluster(data = pool, formula = rating_quality ~  ratee_fem* age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual7 <- interaction.plot(pool$ratee_fem, pool$age_ratee, pool$rating_quality)

##Interaction with ratee_fem and marital status of ratee
mod29_gender <- lm.cluster(data = pool, formula = rating_quality ~  ratee_fem* married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotqual8 <- interaction.plot(pool$ratee_fem, pool$married_ratee, pool$rating_quality)

##Interaction with ratee_fem and education of ratee
mod30_gender <- lm.cluster(data = pool, formula = rating_quality ~  ratee_fem * educ_ratee + married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee")

#intplotqual9 <- interaction.plot(pool$ratee_fem, pool$educ_ratee, pool$rating_quality)


#saving regression
screenreg(list(mod21_gender,mod22_gender,mod23_gender,mod24_gender,mod25_gender,mod26_gender,mod27_gender,mod28_gender,mod29_gender,
               mod30_gender), file="gen_qual_farmer", stars = c(0.01, 0.05, 0.1), digits=4)





################# PRICE RATING ###########################

#all variables 
mod31_gender<- lm.cluster(data = pool, formula = rating_price ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with farmer_fem and interaction_yes
mod32_gender<- lm.cluster(data = pool, formula = rating_price ~  farmer_fem* interaction_yes + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice1 <- interaction.plot(pool$farmer_fem, pool$interaction_yes, pool$rating_price)

##Interaction with farmer_fem and education
mod33_gender <- lm.cluster(data = pool, formula = rating_price ~  farmer_fem*educ + interaction_yes + ratee_fem + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice2 <- interaction.plot(pool$farmer_fem, pool$educ, pool$rating_price)

##Interaction with farmer_fem and age
mod34_gender <- lm.cluster(data = pool, formula = rating_price ~  farmer_fem*age + educ + interaction_yes + ratee_fem + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice3 <- interaction.plot(pool$farmer_fem, pool$age, pool$rating_price)

##Interaction with farmer_fem and gender of ratee
mod35_gender <- lm.cluster(data = pool, formula = rating_price ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice4 <- interaction.plot(pool$farmer_fem, pool$ratee_fem, pool$rating_price)

##Interaction with farmer_fem and marital status of farmer
mod36_gender <- lm.cluster(data = pool, formula = rating_price ~  farmer_fem*married + ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice5 <- interaction.plot(pool$farmer_fem, pool$married, pool$rating_price)


##Interaction with ratee_fem and interaction_yes 
mod37_gender <- lm.cluster(data = pool, formula = rating_price ~  ratee_fem*interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice6 <- interaction.plot(pool$ratee_fem, pool$interaction_yes, pool$rating_price)

##Interaction with ratee_fem and age of ratee
mod38_gender <- lm.cluster(data = pool, formula = rating_price ~  ratee_fem* age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice7 <- interaction.plot(pool$ratee_fem, pool$age_ratee, pool$rating_price)

##Interaction with ratee_fem and marital status of ratee
mod39_gender <- lm.cluster(data = pool, formula = rating_price ~  ratee_fem* married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotprice8 <- interaction.plot(pool$ratee_fem, pool$married_ratee, pool$rating_price)

##Interaction with ratee_fem and education of ratee
mod40_gender <- lm.cluster(data = pool, formula = rating_price ~  ratee_fem * educ_ratee + married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee")

#intplotprice9 <- interaction.plot(pool$ratee_fem, pool$educ_ratee, pool$rating_price)

#saving regression
screenreg(list(mod31_gender,mod32_gender,mod33_gender,mod34_gender,mod35_gender,mod36_gender,mod37_gender,mod38_gender,mod39_gender,
               mod40_gender), file="gen_price_farmer", stars = c(0.01, 0.05, 0.1), digits=4)






################# REPUTATION RATING ###########################


#all variables 
mod41_gender<- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem + ratee_fem + age + interaction_yes + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with farmer_fem and interaction_yes
mod42_gender<- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem* interaction_yes + ratee_fem + age  + educ + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation1 <- interaction.plot(pool$farmer_fem, pool$interaction_yes, pool$rating_reputation)

##Interaction with farmer_fem and education
mod43_gender <- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem*educ + interaction_yes + ratee_fem + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation3 <- interaction.plot(pool$farmer_fem, pool$educ, pool$rating_reputation)

##Interaction with farmer_fem and age
mod44_gender <- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem*age + educ + interaction_yes + ratee_fem + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation3 <- interaction.plot(pool$farmer_fem, pool$age, pool$rating_reputation)

##Interaction with farmer_fem and gender of ratee
mod45_gender <- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation4 <- interaction.plot(pool$farmer_fem, pool$ratee_fem, pool$rating_reputation)

##Interaction with farmer_fem and marital status of farmer
mod46_gender <- lm.cluster(data = pool, formula = rating_reputation ~  farmer_fem*married + ratee_fem + educ + interaction_yes + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation5 <- interaction.plot(pool$farmer_fem, pool$married, pool$rating_reputation)


##Interaction with ratee_fem and interaction_yes 
mod47_gender <- lm.cluster(data = pool, formula = rating_reputation ~  ratee_fem*interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation6 <- interaction.plot(pool$ratee_fem, pool$interaction_yes, pool$rating_reputation)

##Interaction with ratee_fem and age of ratee
mod48_gender <- lm.cluster(data = pool, formula = rating_reputation ~  ratee_fem* age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation7 <- interaction.plot(pool$ratee_fem, pool$age_ratee, pool$rating_reputation)

##Interaction with ratee_fem and marital status of ratee
mod49_gender <- lm.cluster(data = pool, formula = rating_reputation ~  ratee_fem* married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#intplotreputation8 <- interaction.plot(pool$ratee_fem, pool$married_ratee, pool$rating_reputation)

##Interaction with ratee_fem and education of ratee
mod50_gender <- lm.cluster(data = pool, formula = rating_reputation ~  ratee_fem * educ_ratee + married_ratee + age_ratee + interaction_yes + farmer_fem + married + educ + age + tarmac
                           + murram + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee")

#intplotreputation9 <- interaction.plot(pool$ratee_fem, pool$educ_ratee, pool$rating_reputation)

#saving regression
screenreg(list(mod41_gender,mod42_gender,mod43_gender,mod44_gender,mod45_gender,mod46_gender,mod47_gender,mod48_gender,mod49_gender,
               mod50_gender), file="gen_reputation_farmer", stars = c(0.01, 0.05, 0.1), digits=4)





#########################################################################################################################





####### DEPENDENT VARIABLE -- RATINGS FROM RATEES ##############


################# OVERALL RATING ###########################

#all variables 
mod51_gender<- lm.cluster(data = pool, formula = ratee_rating_overall ~ ratee_fem + age_ratee + married_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with ratee_fem and age of ratee
mod52_gender<- lm.cluster(data = pool, formula = ratee_rating_overall ~ ratee_fem*age_ratee + married_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and marital status of ratee 
mod53_gender<- lm.cluster(data = pool, formula = ratee_rating_overall ~ ratee_fem*married_ratee + age_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and education of ratee 
mod54_gender<- lm.cluster(data = pool, formula = ratee_rating_overall ~ ratee_fem*educ_ratee + age_ratee + married_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 


#saving regression
screenreg(list(mod51_gender,mod52_gender,mod53_gender,mod54_gender), file="gen_overall_ratee", stars = c(0.01, 0.05, 0.1), digits=4)




################# LOCATION RATING ###########################

#all variables 
mod55_gender<- lm.cluster(data = pool, formula = rating_location_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with ratee_fem and age of ratee
mod56_gender<- lm.cluster(data = pool, formula = rating_location_ratee ~ ratee_fem*age_ratee + married_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and marital status of ratee 
mod57_gender<- lm.cluster(data = pool, formula = rating_location_ratee ~ ratee_fem*married_ratee + age_ratee + educ_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and education of ratee 
mod58_gender<- lm.cluster(data = pool, formula = rating_location_ratee ~ ratee_fem*educ_ratee + age_ratee + married_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 


#saving regression
screenreg(list(mod55_gender,mod56_gender,mod57_gender,mod58_gender), file="gen_location_ratee", stars = c(0.01, 0.05, 0.1), digits=4)



################# QUALITY RATING ###########################

#all variables 
mod59_gender<- lm.cluster(data = pool, formula = rating_quality_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with ratee_fem and age of ratee
mod60_gender<- lm.cluster(data = pool, formula = rating_quality_ratee ~ ratee_fem*age_ratee + married_ratee + educ_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and marital status of ratee 
mod61_gender<- lm.cluster(data = pool, formula = rating_quality_ratee ~ ratee_fem*married_ratee + age_ratee + educ_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and education of ratee 
mod62_gender<- lm.cluster(data = pool, formula = rating_quality_ratee ~ ratee_fem*educ_ratee + age_ratee + married_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#saving regression
screenreg(list(mod59_gender,mod60_gender,mod61_gender,mod62_gender), file="gen_quality_ratee", stars = c(0.01, 0.05, 0.1), digits=4)



################# PRICE RATING ###########################

#all variables 
mod63_gender<- lm.cluster(data = pool, formula = rating_price_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with ratee_fem and age of ratee
mod64_gender<- lm.cluster(data = pool, formula = rating_price_ratee ~ ratee_fem*age_ratee + married_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and marital status of ratee 
mod65_gender<- lm.cluster(data = pool, formula = rating_price_ratee ~ ratee_fem*married_ratee + age_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and education of ratee 
mod66_gender<- lm.cluster(data = pool, formula = rating_price_ratee ~ ratee_fem*educ_ratee + age_ratee + married_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#saving regression
screenreg(list(mod63_gender,mod64_gender,mod65_gender,mod66_gender), file="gen_price_ratee", stars = c(0.01, 0.05, 0.1), digits=4)



################# REPUTATION RATING ###########################

#all variables 
mod67_gender<- lm.cluster(data = pool, formula = rating_reputation_ratee ~ ratee_fem + age_ratee + married_ratee + educ_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with ratee_fem and age of ratee
mod68_gender<- lm.cluster(data = pool, formula = rating_reputation_ratee ~ ratee_fem*age_ratee + married_ratee + educ_ratee 
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and marital status of ratee 
mod69_gender<- lm.cluster(data = pool, formula = rating_reputation_ratee ~ ratee_fem*married_ratee + age_ratee + educ_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and education of ratee 
mod70_gender<- lm.cluster(data = pool, formula = rating_reputation_ratee ~ ratee_fem*educ_ratee + age_ratee + married_ratee
                          + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

#saving regression
screenreg(list(mod67_gender,mod68_gender,mod69_gender,mod70_gender), file="gen_reputation_ratee", stars = c(0.01, 0.05, 0.1), digits=4)



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
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##### interactions 
##Interaction with farmer_fem and interaction_yes
mod72_gender<- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem* interaction_yes + ratee_fem + age  + educ + tarmac
                         + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with farmer_fem and education
mod73_gender <- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem*educ + interaction_yes + ratee_fem + age + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with farmer_fem and age
mod74_gender <- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem*age + educ + interaction_yes + ratee_fem + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with farmer_fem and gender of ratee
mod75_gender <- lm.cluster(data = pool, formula = ratingoverall_diff ~  farmer_fem*ratee_fem + educ + interaction_yes + age + tarmac
                          + murram + married + age_ratee + married_ratee + educ_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and educ of ratee 
mod76_gender <- lm.cluster(data = pool, formula = ratingoverall_diff ~  ratee_fem*educ_ratee + farmer_fem + educ + interaction_yes + age + tarmac
                           + murram + married + age_ratee + married_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 

##Interaction with ratee_fem and age of ratee 
mod77_gender <- lm.cluster(data = pool, formula = ratingoverall_diff ~  ratee_fem*age_ratee + farmer_fem + educ + interaction_yes + age + tarmac
                           + murram + married + educ_ratee + married_ratee + dealer_dummy + trader_dummy + miller_dummy, cluster = "id.ratee") 


#saving regression
screenreg(list(mod71_gender,mod72_gender,mod73_gender,mod74_gender, mod75_gender, mod76_gender, mod77_gender), file="gen_overall_diff", stars = c(0.01, 0.05, 0.1), digits=4)
