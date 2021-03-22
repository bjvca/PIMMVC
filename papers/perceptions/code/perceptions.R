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



################### FARMERS ###############################
##Prepping data
trans <- c("hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.agro1","hh.maize.q25","hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l","hh.maize.beans.bean.q81","hh.maize.beans.bean.q81a","hh.maize.q106","hh.maize.q24","hh.maize.q27")],"Yes")
names(stack1) <- c("farmerID","id.agro", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation","seedqual_satis","seed_fake","seed_purchase","age","education","bought")
stack2 <- cbind(farmers[c("ID","id.agro2","hh.maize.q25","hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l","hh.maize.agro2.q110","hh.maize.beans.bean.q81","hh.maize.beans.bean.q81a","hh.maize.q106","hh.maize.q24","hh.maize.q27")])
names(stack2) <- c("farmerID","id.agro", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought","seedqual_satis","seed_fake","seed_purchase","age","education")
stack3 <- cbind(farmers[c("ID","id.agro3","hh.maize.q25","hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l","hh.maize.agro3.q112","hh.maize.beans.bean.q81","hh.maize.beans.bean.q81a","hh.maize.q106","hh.maize.q24","hh.maize.q27")])
names(stack3) <- c("farmerID","id.agro", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought","seedqual_satis","seed_fake","seed_purchase","age","education")

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
