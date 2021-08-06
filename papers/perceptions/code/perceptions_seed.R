### run in:  ../PIMMVC/papers/perceptions
rm(list=ls())
path <- getwd()

options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

library(miceadds)
library(texreg)
library(plyr)
library(plm)

################# FARMERS:SEED SYSTEMS ######################

##Farmers' dataset
farmers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_farmers.csv", sep = "/"))
#extracting variables from the baseline data
farmers_seedsub <- farmers_seed[ , c("Check2.check.maize.q15", "Check2.check.maize.q14",
                                     "Check2.check.maize.q16", "Check2.check.maize.q17",
                                     "Check2.check.maize.q8", "farmer_ID")]  

##Farmers' dataset --- long form --- with ratings
farmers_long <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))

#merging baseline and long form
merged_farmers <- merge(farmers_seedsub,farmers_long, by=c("farmer_ID"))

#### Seed related ratings --- farmer's gender
mfarm<- merged_farmers

mfarm[mfarm=="98"]<-NA

mfarm$seed_quality_general_rating<-as.numeric(mfarm$seed_quality_general_rating)
mfarm$general_rating<-as.numeric(mfarm$general_rating)
mfarm$seed_yield_rating<-as.numeric(mfarm$seed_yield_rating)
mfarm$seed_drought_rating<-as.numeric(mfarm$seed_drought_rating)
mfarm$seed_disease_rating<-as.numeric(mfarm$seed_disease_rating)
mfarm$seed_maturing_rating<-as.numeric(mfarm$seed_maturing_rating)
mfarm$seed_germinate_rating<-as.numeric(mfarm$seed_germinate_rating)
mfarm$score <-  rowMeans(mfarm[c("seed_quality_general_rating","general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm=T)

mfarm$gender_f <- ifelse(mfarm$Check2.check.maize.q15 == 'Male', 1, 0)

summary(lm.cluster(data = mfarm, formula = score ~  gender_f, cluster="farmer_ID"))
summary(lm.cluster(data = mfarm, formula = seed_quality_general_rating ~  gender_f, cluster="farmer_ID"))
summary(lm.cluster(data = mfarm, formula = general_rating ~  gender_f, cluster="farmer_ID"))
summary(lm.cluster(data = mfarm, formula = seed_yield_rating ~  gender_f, cluster="farmer_ID"))
summary(lm.cluster(data = mfarm, formula = seed_drought_rating ~  gender_f, cluster="farmer_ID"))
summary(lm.cluster(data = mfarm, formula = seed_disease_rating ~  gender_f, cluster="farmer_ID"))
summary(lm.cluster(data = mfarm, formula = seed_maturing_rating ~  gender_f, cluster="farmer_ID"))
summary(lm.cluster(data = mfarm, formula = seed_germinate_rating ~  gender_f, cluster="farmer_ID"))

summary(lm.cluster(data = mfarm, formula = score ~  gender_f, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_quality_general_rating ~  gender_f, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = general_rating ~  gender_f, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_yield_rating ~  gender_f, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_drought_rating ~  gender_f, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_disease_rating ~  gender_f, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_maturing_rating ~  gender_f, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_germinate_rating ~  gender_f, cluster="shop_ID"))

#creating dummies for analysis 
mfarm$educ_f <- 0
mfarm$educ_f[mfarm$Check2.check.maize.q17=="b" |mfarm$Check2.check.maize.q17=="c" | mfarm$Check2.check.maize.q17=="d" | mfarm$Check2.check.maize.q17=="e" | 
                    mfarm$Check2.check.maize.q17=="f" |mfarm$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
mfarm$married <- ifelse(mfarm$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers

summary(lm.cluster(data = mfarm, formula = score ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_quality_general_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = general_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_yield_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_drought_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_disease_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_maturing_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))
summary(lm.cluster(data = mfarm, formula = seed_germinate_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8, cluster="shop_ID"))

#fixed effects
summary(lm(data = mfarm, formula = score ~  gender_f + shop_ID))
summary(lm(data = mfarm, formula = seed_quality_general_rating ~  gender_f +shop_ID))
summary(lm(data = mfarm, formula = general_rating ~  gender_f + shop_ID))
summary(lm(data = mfarm, formula = seed_yield_rating ~  gender_f +shop_ID))
summary(lm(data = mfarm, formula = seed_drought_rating ~  gender_f+shop_ID))
summary(lm(data = mfarm, formula = seed_disease_rating ~  gender_f + shop_ID))
summary(lm(data = mfarm, formula = seed_maturing_rating ~  gender_f +shop_ID))
summary(lm(data = mfarm, formula = seed_germinate_rating ~  gender_f + shop_ID))

summary(lm(data = mfarm, formula = score ~  gender_f   + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8+shop_ID))
summary(lm(data = mfarm, formula = seed_quality_general_rating ~  gender_f  + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8+shop_ID))
summary(lm(data = mfarm, formula = general_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8 + shop_ID))
summary(lm(data = mfarm, formula = seed_yield_rating ~  gender_f  + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8+shop_ID))
summary(lm(data = mfarm, formula = seed_drought_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8+shop_ID))
summary(lm(data = mfarm, formula = seed_disease_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8 + shop_ID))
summary(lm(data = mfarm, formula = seed_maturing_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8 +shop_ID))
summary(lm(data = mfarm, formula = seed_germinate_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8 + shop_ID))

### BETWEEN ESTIMATOR - CARO's APPROACH 
betw_farm <- data.frame(cbind(tapply(as.numeric(mfarm$score), mfarm$farmer_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(mfarm$seed_quality_general_rating), mfarm$farmer_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(mfarm$general_rating), mfarm$farmer_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(mfarm$seed_yield_rating), mfarm$farmer_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(mfarm$seed_drought_rating), mfarm$farmer_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(mfarm$seed_disease_rating), mfarm$farmer_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(mfarm$seed_maturing_rating), mfarm$farmer_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(mfarm$seed_germinate_rating), mfarm$farmer_ID,mean,na.rm=TRUE)))
names(betw_farm) <- c("score_avg","seed_quality_general","general","seed_yield","seed_drought",
                      "seed_disease","seed_maturing","seed_germinate")

betw_farm$farmer_ID <- rownames(betw_farm)

b_farm <- merge(betw_farm, mfarm, by="farmer_ID")

#between estimator --- gender of farmer 
summary(lm(data = b_farm, formula = score_avg ~  gender_f))
summary(lm(data = b_farm, formula = seed_quality_general ~  gender_f ))
summary(lm(data = b_farm, formula = general ~  gender_f ))
summary(lm(data = b_farm, formula = seed_yield ~  gender_f ))
summary(lm(data = b_farm, formula = seed_drought~  gender_f))
summary(lm(data = b_farm, formula = seed_disease ~  gender_f ))
summary(lm(data = b_farm, formula = seed_maturing ~  gender_f ))
summary(lm(data = b_farm, formula = seed_germinate ~  gender_f ))

summary(lm(data = b_farm, formula = score_avg ~  gender_f +Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8))
summary(lm(data = b_farm, formula = seed_quality_general ~  gender_f+Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8 ))
summary(lm(data = b_farm, formula = general ~  gender_f +Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8))
summary(lm(data = b_farm, formula = seed_yield ~  gender_f +Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8))
summary(lm(data = b_farm, formula = seed_drought~  gender_f+Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8))
summary(lm(data = b_farm, formula = seed_disease ~  gender_f+Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8 ))
summary(lm(data = b_farm, formula = seed_maturing ~  gender_f+Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8 ))
summary(lm(data = b_farm, formula = seed_germinate ~  gender_f +Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8))


################# FARMERS : STACK SURVEYS ######################

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

#############INPUT DEALERS#####################
#getting dealer ratings from farmers' stack surveys 

##Prepping data
trans <- c("hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.agro1","hh.maize.q25","hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l","hh.maize.q24","hh.maize.q27"
                          ,"hh.maize.q13","hh.maize.q26")],"Yes")
names(stack1) <- c("farmerID","id.ratee", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation","age","education",
                   "tarmac","marital_status","interaction")

stack2 <- cbind(farmers[c("ID","id.agro2","hh.maize.q25","hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l","hh.maize.agro2.q110","hh.maize.q24","hh.maize.q27"
                          ,"hh.maize.q13","hh.maize.q26")])
names(stack2) <- c("farmerID","id.ratee", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "interaction","age","education"
                   ,"tarmac","marital_status")

stack3 <- cbind(farmers[c("ID","id.agro3","hh.maize.q25","hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l","hh.maize.agro3.q112","hh.maize.q24","hh.maize.q27"
                          ,"hh.maize.q13","hh.maize.q26")])
names(stack3) <- c("farmerID","id.ratee", "farmer_gender","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "interaction","age","education"
                   ,"tarmac","marital_status")

ratings <-rbind(stack1,stack2,stack3) #all 3 stacks together
ratings[c("id.ratee","interaction")] <- lapply(ratings[c("id.ratee","interaction")], function(x) as.factor(as.character(x)) )

##Subsetting 
ratings <- subset(ratings, !is.na(rating_reputation) )
ratings <- subset(ratings[!apply(ratings == "", 1, any),])
ratings <- ratings[c(!duplicated(ratings[,1:2])),] #removing duplicates 

which(duplicated(ratings)) #no duplicates

### simple average (Index for overall rating)
ratings$rating_overall <- rowSums(ratings[c("rating_location","rating_price","rating_quality","rating_stock","rating_reputation")])/5
summary(ratings$rating_overall)

########### DATA WITH SEED AND STACK SURVEYS ########

#extracting variables from merged seed data
merged_farmers_seed <- merged_farmers[ , c("Check2.check.maize.q15", "Check2.check.maize.q14",
                                    "Check2.check.maize.q16", "Check2.check.maize.q17",
                                    "Check2.check.maize.q8", "farmer_ID", "shop_ID", "bought_at_dealer", 
                                    "location_rating", "price_rating", "quality_rating", "stock_rating",
                                    "reputation_rating")] 
#changing names to match
names(merged_farmers_seed) <- c("farmer_gender","age","marital_status","education","tarmac","farmerID","id.ratee",
                                "interaction","rating_location","rating_price", "rating_quality", "rating_stock",
                                "rating_reputation") 

merged_farmers_seed <- merged_farmers_seed[!(merged_farmers_seed$rating_location=="n/a"),] #removing ratings having n/a

#changing to numeric from character
merged_farmers_seed$rating_location<-as.numeric(merged_farmers_seed$rating_location)
merged_farmers_seed$rating_price<-as.numeric(merged_farmers_seed$rating_price)
merged_farmers_seed$rating_quality<-as.numeric(merged_farmers_seed$rating_quality)
merged_farmers_seed$rating_stock<-as.numeric(merged_farmers_seed$rating_stock)
merged_farmers_seed$rating_reputation<-as.numeric(merged_farmers_seed$rating_reputation)
#getting overall rating measure -- average 
merged_farmers_seed$rating_overall <- rowSums(merged_farmers_seed[c("rating_location","rating_price",
                                                                     "rating_quality","rating_stock","rating_reputation")])/5
### SEED AND STACK DATA TOGETHER
farmers_seed_stack <-rbind(ratings,merged_farmers_seed)
#creating dummies for analysis 
farmers_seed_stack$interaction_yes <- ifelse(farmers_seed_stack$interaction == 'Yes', 1, 0) 
farmers_seed_stack$gender <- ifelse(farmers_seed_stack$farmer_gender == 'Female', 1, 0) #female farmers
farmers_seed_stack$educ <- 0
farmers_seed_stack$educ[farmers_seed_stack$education=="b" | farmers_seed_stack$education=="c" | farmers_seed_stack$education=="d" | farmers_seed_stack$education=="e" | 
                          farmers_seed_stack$education=="f" |farmers_seed_stack$education=="g" ] <- 1 #educated farmers
farmers_seed_stack$married <- ifelse(farmers_seed_stack$marital_status == 'a', 1, 0)  #married farmers

farmers_seed_stack[farmers_seed_stack=="999"] <- NA # removing 999


####################################################################################################
####################################################################################################

### CLUSTERED REGRESSIONS - LOOKING AT FARMERS' GENDER ###

#################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS ##############
#################################################################

################# OVERALL RATING ###########################

#all variables 
ss1<- lm.cluster(data = farmers_seed_stack, formula = rating_overall ~  gender + age + interaction_yes + educ + tarmac
                         + married, cluster="id.ratee") 
s1 <- sqrt(diag(vcov(ss1)))
ss_res1<-ss1$lm_res

summary(lm.cluster(data = farmers_seed_stack, formula = rating_overall ~  gender , cluster="farmerID") )
summary(lm.cluster(data = farmers_seed_stack, formula = rating_overall ~  gender , cluster="id.ratee") )

################# LOCATION RATING ###########################

#all variables 
ss2<- lm.cluster(data = farmers_seed_stack, formula = rating_location ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s2 <- sqrt(diag(vcov(ss2)))
ss_res2<-ss2$lm_res

summary(lm.cluster(data = farmers_seed_stack, formula = rating_location ~  gender , cluster="farmerID") )
summary(lm.cluster(data = farmers_seed_stack, formula = rating_location ~  gender , cluster="id.ratee") )



################# QUALITY RATING ###########################

#all variables 
ss3<- lm.cluster(data = farmers_seed_stack, formula = rating_quality ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s3 <- sqrt(diag(vcov(ss3)))
ss_res3<-ss3$lm_res

summary(lm.cluster(data = farmers_seed_stack, formula = rating_quality ~  gender , cluster="farmerID") )
summary(lm.cluster(data = farmers_seed_stack, formula = rating_quality ~  gender , cluster="id.ratee") )


################# PRICE RATING ###########################

#all variables 
ss4<- lm.cluster(data = farmers_seed_stack, formula = rating_price ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s4 <- sqrt(diag(vcov(ss4)))
ss_res4<-ss4$lm_res

summary(lm.cluster(data = farmers_seed_stack, formula = rating_price ~  gender , cluster="farmerID") )
summary(lm.cluster(data = farmers_seed_stack, formula = rating_price ~  gender , cluster="id.ratee") )


################# STOCK RATING ###########################
#all variables
ss5<- lm.cluster(data = farmers_seed_stack, formula = rating_stock ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s5 <- sqrt(diag(vcov(ss5)))
ss_res5<-ss5$lm_res

summary(lm.cluster(data = farmers_seed_stack, formula = rating_stock ~  gender , cluster="farmerID") )
summary(lm.cluster(data = farmers_seed_stack, formula = rating_stock ~  gender , cluster="id.ratee") )


################# REPUTATION RATING ###########################
#all variables
ss6<- lm.cluster(data = farmers_seed_stack, formula = rating_reputation ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s6 <- sqrt(diag(vcov(ss6)))
ss_res6<-ss6$lm_res

summary(lm.cluster(data = farmers_seed_stack, formula = rating_reputation ~  gender , cluster="farmerID") )
summary(lm.cluster(data = farmers_seed_stack, formula = rating_reputation ~  gender , cluster="id.ratee") )


#######################################################################
#######################################################################

################# DEALERS:SEED SYSTEMS ######################
dealers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep = "/"))

merge_seed <- merge(dealers_seed,mfarm, by=c("shop_ID"))

##### SE clustered reg -- farmer's gender ---- farmer + dealer characteristics as controls 
merge_seed$prim <- FALSE
merge_seed$prim <- (merge_seed$maize.owner.agree.educ %in% c("c","d","e","f"))
merge_seed$maize.owner.agree.q3[merge_seed$maize.owner.agree.q3==999] <- NA

#Q8. When was this agro-input shop established? (year)
merge_seed$years_shop <- 2020 - as.numeric(as.character(substr(merge_seed$maize.owner.agree.q8, start=1, stop=4)))

#Q77. Material of floor in areas where seed is stored?
merge_seed$goodfloor <- FALSE
merge_seed$goodfloor <- (merge_seed$maize.owner.agree.temp.q77 %in% c("Cement","Tiles"))

#Q78. Lighting conditions in area where seed is stored?
merge_seed$badlighting <- FALSE
merge_seed$badlighting <- (merge_seed$maize.owner.agree.temp.q78 %in% c("1"))

#Q79. On what surface are seed stored?
merge_seed$badstored <- FALSE
merge_seed$badstored <- (merge_seed$maize.owner.agree.temp.q79 %in% c("1", "2", "96"))

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
as.numeric(as.character(merge_seed$maize.owner.agree.temp.q82))

summary(lm.cluster(data = merge_seed, formula = score ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = general_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = seed_quality_general_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = seed_yield_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = seed_drought_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = seed_disease_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = seed_maturing_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = seed_germinate_rating ~  gender_f + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))

#### Looking at interaction of farmer's and dealer's gender in the seed data
merge_seed$gender_d <- ifelse(merge_seed$maize.owner.agree.gender == 'Male', 1, 0)

####OVERALL SCORE 

summary(lm(data = merge_seed, formula = score ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = score ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))


#two way FE
summary(lm(data = merge_seed, formula = score ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm1<-plm(score ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
         effect = "twoways")
summary(plm1)

#twoway random effects 
random1<-plm(score ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random1)

ran1<-plm(score ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
             +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
             + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
               maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
             + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
               maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(ran1)

#### MIXED MODELS

require(car)
require(MASS)
#https://ase.tufts.edu/bugs/guide/assets/mixed_model_guide.html

#checking out distribution of the response variable - the distribution where most of the points are under the dashed line should be considered
merge_seed$score.t <- merge_seed$score + 1
qqp(merge_seed$score.t, "norm")
qqp(merge_seed$score.t, "lnorm")
#lognormal seems to be the correct distribution

#thus, our case is not normal distribution 
#the REML and maximum likelihood methods for estimating the effect sizes in the model make assumptions of normality -- so cannot be used
#we need to test whether we can use penalized quasilikelihood (PQL) or not. 
#it produces biased estimates if response variable fits a discrete count distribution and the mean is less than 5 - or if your response variable is binary.
#in our case, score's mean is less than 5, so PQL will likely create biased estimates 

#but still I try PQL
PQL1 <- glmmPQL(score.t ~ gender_f + gender_d, ~1 | shop_ID/farmer_ID, family = gaussian(link = "log"),
                data = merge_seed, verbose = FALSE)
#verbose --- logical:print out record of iterations?

summary(PQL1)
PQL2 <- glmmPQL(score.t ~ gender_f + gender_d + gender_f*gender_d, ~1 | shop_ID/farmer_ID, family = gaussian(link = "log"),
                data = merge_seed, verbose = FALSE)
summary(PQL2)
PQL3 <- glmmPQL(score.t ~ gender_f + gender_d + gender_f*gender_d, ~1 | farmer_ID/shop_ID, family = gaussian(link = "log"),
                data = merge_seed, verbose = FALSE)
summary(PQL3)

PQL2c <- glmmPQL(score.t ~ gender_f + gender_d + gender_f*gender_d+Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                 +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                   maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, ~1 | shop_ID/farmer_ID, family = gaussian(link = "log"),
                data = merge_seed, verbose = FALSE)
summary(PQL2c)


library(nlme)
library("mlmRev")
library(Rcpp)
library(lme4)
####Unbalanced data page 25 from book
##### DOES NOT WORK
nlme::lme(score ~ gender_d*gender_f, random = ~ 1 | shop_ID/farmer_ID, data = merge_seed)

########## IF I REMOVE NAN FROM RESPONSE VARIABLE SCORE IT WORKS 
datanan<-subset(merge_seed, score!="NaN" )

########### need to decide which one 
summary(nlme::lme(score ~ gender_d+gender_f+gender_d*gender_f, random = ~ 1 | shop_ID/farmer_ID, data = datanan))
#fixed effect for each farmer and a random effect for each dealer, random effects at two levels-- the effect for the dealer and the effect for the different farmer rating each dealer

summary(nlme::lme(score ~ gender_d+gender_f+gender_d*gender_f, random = ~ 1 | farmer_ID/shop_ID, data = datanan))

summary(nlme::lme(score ~ gender_d, random = ~ 1 | shop_ID/farmer_ID, data = datanan))
summary(nlme::lme(score ~gender_f, random = ~ 1 | farmer_ID/shop_ID, data = datanan))



#https://rpsychologist.com/r-guide-longitudinal-lme-lmer 


##### does not run 

#Laplace approximation when distribution is not normal

#the Laplace approximation is a special case of a parameter estimation method called Gauss-Hermite quadrature (GHQ), with one iteration. 
#GHQ is more accurate than Laplace due to repeated iterations, but becomes less flexible after the first iteration, so can only use it for one random effect. 
#GHQ1 <- glmer(score ~ gender_f + gender_d + gender_f * gender_d + (1 | shop_ID) , data = merge_seed,
              family = binomial(link = "logit"), nAGQ = 1)

#GHQ2 <- glmer(score ~ gender_f + gender_d + gender_f * gender_d + (1 | shop_ID) + (1 | farmer_ID), data = merge_seed,
              family = binomial(link = "logit"), nAGQ = 1)

#does not run 
#lmm1 <- lmer(score ~ gender_f+gender_d+gender_f*gender_d + farmer_ID + (1 | shop_ID), data =merge_seed)
#lmm2 <- lmer(score ~ gender_f*gender_d + farmer_ID + (1 | shop_ID), data =merge_seed)
#lmm3 <- lmer(score ~ gender_f*gender_d + shop_ID + (1 | farmer_ID), data =merge_seed)
#lmm4 <- lmer(score ~  gender_f+gender_d+gender_f*gender_d + shop_ID + (1 | farmer_ID), data =merge_seed)
#returns ---- fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

#Intercept varying among shop_ID and farmer_ID, grouping factors are shop_ID and farmer_ID
#(1 | g), is the simplest possible mixed-model formula, where each level of the grouping factor, g, has its own random intercept. 
#any nonzero mean of the random effects as fixed-effects parameters. 
#https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf 
#does not run
fm1 <- lmer(score ~ gender_f+gender_d+gender_f*gender_d + (1| shop_ID)+(1| farmer_ID),merge_seed)


summary(lm.cluster(data = merge_seed, formula = score ~  gender_f + gender_d+ gender_f*gender_d, cluster="shop_ID"))
summary(lm.cluster(data = merge_seed, formula = score ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                   +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                   + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                     maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                   + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                     maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID"))


####GENERAL RATING 

summary(lm(data = merge_seed, formula = general_rating ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = general_rating ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
           +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
           + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
             maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
           + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
             maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))

#two way FE
summary(lm(data = merge_seed, formula = general_rating ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm2<-plm(general_rating ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
          effect = "twoways")
summary(plm2)

#twoway random effects 
random2<-plm(general_rating ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random2)

ran2<-plm(general_rating ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
          +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
          + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
            maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
          + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
          effect = "twoways")
summary(ran2)

#### MIXED MODELS

require(car)
require(MASS)
#https://ase.tufts.edu/bugs/guide/assets/mixed_model_guide.html

#checking out distribution of the response variable - the distribution where most of the points are under the dashed line should be considered
merge_seed$general_rating.t <- merge_seed$general_rating + 1
qqp(merge_seed$general_rating.t, "norm")
qqp(merge_seed$general_rating.t, "lnorm")
#lognormal


genPQL1 <- glmmPQL(general_rating.t ~ gender_f + gender_d, ~1 | shop_ID/farmer_ID, family = gaussian(link = "log"),
                data = merge_seed, verbose = FALSE)
#verbose --- logical:print out record of iterations?
summary(genPQL1)
genPQL2 <- glmmPQL(general_rating.t ~ gender_f + gender_d + gender_f*gender_d, ~1 | shop_ID/farmer_ID, family = gaussian(link = "log"),
                data = merge_seed, verbose = FALSE)
summary(genPQL2)
genPQL3 <- glmmPQL(general_rating.t ~ gender_f + gender_d + gender_f*gender_d, ~1 | farmer_ID/shop_ID, family = gaussian(link = "log"),
                data = merge_seed, verbose = FALSE)
#message = iteration limit reached without convergence (10)

genPQL2c <- glmmPQL(general_rating.t ~ gender_f + gender_d + gender_f*gender_d+Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
                 +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
                 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
                   maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
                 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, ~1 | shop_ID/farmer_ID, family = gaussian(link = "log"),
                 data = merge_seed, verbose = FALSE)
summary(genPQL2c)








summary(lm(data = merge_seed, formula = seed_quality_general_rating ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = seed_quality_general_rating ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
           +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
           + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
             maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
           + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
             maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))

#two way FE
summary(lm(data = merge_seed, formula = seed_quality_general_rating ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm3<-plm(seed_quality_general_rating ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
          effect = "twoways")
summary(plm3)

#twoway random effects 
random3<-plm(seed_quality_general_rating ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random3)

ran3<-plm(seed_quality_general_rating ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
          +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
          + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
            maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
          + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
          effect = "twoways")
summary(ran3)



summary(lm(data = merge_seed, formula = seed_yield_rating ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = seed_yield_rating ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
           +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
           + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
             maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
           + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
             maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))

#two way FE
summary(lm(data = merge_seed, formula = seed_yield_rating ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm4<-plm(seed_yield_rating ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
          effect = "twoways")
summary(plm4)


#twoway random effects 
random4<-plm(seed_yield_rating ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random4)

ran4<-plm(seed_yield_rating ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
          +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
          + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
            maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
          + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
          effect = "twoways")
summary(ran4)



summary(lm(data = merge_seed, formula = seed_drought_rating ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = seed_drought_rating ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
           +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
           + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
             maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
           + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
             maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))

#two way FE
summary(lm(data = merge_seed, formula = seed_drought_rating ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm5<-plm(seed_drought_rating ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
          effect = "twoways")
summary(plm5)

#twoway random effects 
random5<-plm(seed_drought_rating ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random5)

ran5<-plm(seed_drought_rating ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
          +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
          + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
            maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
          + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
          effect = "twoways")
summary(ran5)


summary(lm(data = merge_seed, formula = seed_disease_rating ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = seed_disease_rating ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
           +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
           + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
             maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
           + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
             maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))


#two way FE
summary(lm(data = merge_seed, formula = seed_disease_rating ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm6<-plm(seed_disease_rating ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
          effect = "twoways")
summary(plm6)

#twoway random effects 
random6<-plm(seed_disease_rating ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random6)

ran6<-plm(seed_disease_rating ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
          +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
          + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
            maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
          + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
          effect = "twoways")
summary(ran6)

summary(lm(data = merge_seed, formula = seed_maturing_rating ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = seed_maturing_rating ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
           +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
           + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
             maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
           + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
             maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))

#two way FE
summary(lm(data = merge_seed, formula = seed_maturing_rating ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm7<-plm(seed_maturing_rating ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
          effect = "twoways")
summary(plm7)

#twoway random effects 
random7<-plm(seed_maturing_rating ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random7)

ran7<-plm(seed_maturing_rating ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
          +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
          + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
            maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
          + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
          effect = "twoways")
summary(ran7)


summary(lm(data = merge_seed, formula = seed_germinate_rating ~  gender_f + gender_d+ gender_f*gender_d))
summary(lm(data = merge_seed, formula = seed_germinate_rating ~  gender_f +gender_d+ gender_f*gender_d+ Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
           +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
           + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
             maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
           + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
             maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70))

#two way FE
summary(lm(data = merge_seed, formula = seed_germinate_rating ~  gender_f + gender_d+ gender_f*gender_d + factor(shop_ID) +factor(farmer_ID)))
plm8<-plm(seed_germinate_rating ~ gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "within", 
          effect = "twoways")
summary(plm8)

#twoway random effects 
random8<-plm(seed_germinate_rating ~ gender_f+gender_d+gender_f*gender_d , data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
             effect = "twoways")
summary(random8)

ran8<-plm(seed_germinate_rating ~ gender_f+gender_d+gender_f*gender_d + Check2.check.maize.q14 + married +educ_f + Check2.check.maize.q8
          +maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 
          + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 +
            maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76
          + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = merge_seed, index = c("shop_ID", "farmer_ID"), model = "random", 
          effect = "twoways")
summary(ran8)




#extracting variables from the baseline data
dealers_sub <- dealers_seed[ , c("maize.owner.agree.q99", "maize.owner.agree.q100",
                                     "maize.owner.agree.q101", "maize.owner.agree.q102",
                                     "maize.owner.agree.q103", "shop_ID","maize.owner.agree.age",
                                 "maize.owner.agree.gender", "maize.owner.agree.educ")] 
names(dealers_sub) <- c("self_rating_location", "self_rating_price","self_rating_quality", "self_rating_stock",
                        "self_rating_reputation", "id.ratee","age_dealer", "gender_dealer", "educ_dealer") 

################# DEALERS:STACK SURVEYS ######################
## Getting Dealers' data
dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))

dealers_sub1 <- dealers[ , c("hh.maize.q6g", "hh.maize.q7","hh.maize.q9","hh.maize.q79",
                             "hh.maize.q80", "hh.maize.q81", "hh.maize.q82", "hh.maize.q83",
                              "id.agro")]  

names(dealers_sub1) <- c("age_dealer", "gender_dealer", "educ_dealer","self_rating_location", "self_rating_price","self_rating_quality", "self_rating_stock",
                        "self_rating_reputation", "id.ratee") 

dealers_ss <-rbind(dealers_sub, dealers_sub1) ### putting both datasets together

##Index for overall rating by dealers for themselves 
dealers_ss$ratee_rating_overall <- rowSums(dealers_ss[c("self_rating_location", "self_rating_price","self_rating_quality", "self_rating_stock",
                                                        "self_rating_reputation")])/5
summary(dealers_ss$ratee_rating_overall)

## creating dummies
dealers_ss$dealer_fem <- ifelse(dealers_ss$gender_dealer == 'Female', 1, 0) #female dealers
dealers_ss$education_dealer <- 0
dealers_ss$education_dealer[dealers_ss$educ_dealer=="b" | dealers_ss$educ_dealer=="c" | dealers_ss$educ_dealer=="d" | dealers_ss$educ_dealer=="e" | 
                         dealers_ss$educ_dealer=="f" |dealers_ss$educ_dealer=="g" ] <- 1 #educated dealers 

dealers_ss[dealers_ss=="999"]<- NA


####### DEPENDENT VARIABLE -- RATINGS FROM DEALERS -- DATASET ONLY HAVING ALL THE DEALERS ##############
#######################################################################################################

################# OVERALL RATING #################################################################

r1<- lm(data = dealers_ss, formula = ratee_rating_overall ~ dealer_fem + age_dealer + education_dealer) 
se_dealer1 <- sqrt(diag(vcov(r1)))

################# LOCATION RATING ###########################

r2<- lm(data = dealers_ss, formula = self_rating_location ~ dealer_fem + age_dealer + education_dealer) 
se_dealer2 <- sqrt(diag(vcov(r2)))

################# QUALITY RATING ###########################

r3<- lm(data = dealers_ss, formula = self_rating_quality ~ dealer_fem + age_dealer + education_dealer) 
se_dealer3 <- sqrt(diag(vcov(r3)))

################# PRICE RATING ###########################

r4<- lm(data = dealers_ss, formula = self_rating_price ~ dealer_fem + age_dealer + education_dealer) 
se_dealer4 <- sqrt(diag(vcov(r4)))

################# STOCK RATING ###########################

r5<- lm(data = dealers_ss, formula = self_rating_stock ~ dealer_fem + age_dealer + education_dealer) 
se_dealer5 <- sqrt(diag(vcov(r5)))

################# REPUTATION RATING ###########################

r6<- lm(data = dealers_ss, formula = self_rating_reputation ~ dealer_fem + age_dealer + education_dealer) 
se_dealer6 <- sqrt(diag(vcov(r6)))

################################################################################################################
################################################################################################################


################# CREATING DATASET WITH BOTH FARMER AND DEALER DETAILS ###########################
##################################################################################################
#getting variables needed from dealer dataset
d <- dealers_ss[ , c("id.ratee", "age_dealer","dealer_fem","education_dealer")]  

#merging farmers and dealers data by dealer id
m <- merge(farmers_seed_stack,d, by=c("id.ratee"))


###########  Replicating Caro's code ####################################################

avg <- data.frame(cbind(tapply(as.numeric(m$rating_location), m$id.ratee,mean,na.rm=TRUE),
                            tapply(as.numeric(m$rating_quality), m$id.ratee,mean,na.rm=TRUE),
                            tapply(as.numeric(m$rating_price), m$id.ratee,mean,na.rm=TRUE),
                            tapply(as.numeric(m$rating_stock),m$id.ratee,mean,na.rm=TRUE),
                            tapply(as.numeric(m$rating_reputation), m$id.ratee,mean,na.rm=TRUE),
                                   tapply(as.numeric(m$rating_overall), m$id.ratee,mean,na.rm=TRUE)))
names(avg) <- c("rating_overall","rating_location","rating_quality","rating_price","rating_stock","rating_reputation")

avg$id.ratee <- rownames(avg)

avg<- na.omit(avg)

avgm <- m[ , c("age_dealer", "education_dealer","gender","age","educ","tarmac","married",
                                           "dealer_fem", "id.ratee")] 

a <- merge(avg, avgm, by="id.ratee")


############################################################################
### CLUSTERED REGRESSIONS - LOOKING AT BOTH FARMERS' AND DEALERS' GENDER ###
############################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS #########################
############################################################################

################# OVERALL RATING ###########################

#all variables 
fd1<- lm.cluster(data = m, formula = rating_overall ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                         + married + age_dealer + education_dealer, cluster="id.ratee") 

summary(lm.cluster(data = m, formula = rating_overall ~  gender + age + educ + tarmac
           + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_overall ~  gender + age + educ + tarmac
                   + married, cluster="id.ratee") )
summary(lm(data = m, formula = rating_overall ~  gender + id.ratee) )
summary(lm(data = m, formula = rating_overall ~  gender + age + educ + tarmac
           + married  + id.ratee) )


se_fd1 <- sqrt(diag(vcov(fd1)))
res_fd1<-fd1$lm_res

fd1_1<- lm.cluster(data = m, formula = rating_overall ~  dealer_fem, cluster="farmerID")
summary(fd1_1)

summary(lm.cluster(data = m, formula = rating_overall ~  dealer_fem, cluster="id.ratee"))
summary(lm.cluster(data = m, formula = rating_overall ~  dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_overall ~  dealer_fem + 
                    age_dealer + education_dealer, cluster="id.ratee") )
summary(lm(data = m, formula = rating_overall ~  dealer_fem + farmerID))
summary(lm(data = m, formula = rating_overall ~  dealer_fem + age_dealer + education_dealer+farmerID))

#both farmer and dealer gender, gender: 1= Female; dealer_fem: 1= Female 
summary(lm(data = m, formula = rating_overall ~  gender + dealer_fem + gender*dealer_fem) )
summary(lm(data = m, formula = rating_overall ~  gender + dealer_fem + gender*dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer) )

#two way FE
summary(lm(data = m, formula = rating_overall ~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) ))
summary(lm(data = m, formula = rating_overall ~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) +age + educ + tarmac
           + married + age_dealer + education_dealer))

#twoway random effects 
mran1<-plm(rating_overall ~ gender+dealer_fem + gender*dealer_fem , data = m, index = c("id.ratee", "farmerID"), model = "random", 
             effect = "twoways")
summary(mran1)

mranc1<-plm(rating_overall ~ gender+dealer_fem + gender*dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer , data = m, index = c("id.ratee", "farmerID"), model = "random", 
           effect = "twoways")
summary(mranc1)


#caro's approach 
summary(lm(data = a, formula = rating_overall ~  dealer_fem ))
summary(lm(data = a, formula = rating_overall ~  dealer_fem + age_dealer + education_dealer))

summary(lm(data = a, formula = rating_overall ~ gender ))
summary(lm(data = a, formula = rating_overall ~ gender + age + educ + tarmac
           + married))

##Interaction between sex of farmer and dealer
fd2<- lm.cluster(data = m, formula = rating_overall ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd2 <- sqrt(diag(vcov(fd2)))
res_fd2<-fd2$lm_res

################# LOCATION RATING ###########################

#all variables 
fd3<- lm.cluster(data = m, formula = rating_location ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 

summary(lm.cluster(data = m, formula = rating_location ~  gender + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_location ~  gender + age + educ + tarmac
                   + married, cluster="id.ratee") )
summary(lm(data = m, formula = rating_location ~  gender + id.ratee) )
summary(lm(data = m, formula = rating_location ~  gender + age  + educ + tarmac
           + married  + id.ratee) )


se_fd3 <- sqrt(diag(vcov(fd3)))
res_fd3<-fd3$lm_res

fd3_3<- lm.cluster(data = m, formula = rating_location ~ dealer_fem , cluster="farmerID") 
summary(fd3_3)

summary(lm.cluster(data = m, formula = rating_location ~ dealer_fem , cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_location ~  dealer_fem + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_location ~  dealer_fem + 
                     age_dealer + education_dealer, cluster="id.ratee") )
summary(lm(data = m, formula = rating_location ~  dealer_fem + farmerID))
summary(lm(data = m, formula = rating_location ~  dealer_fem + age_dealer + education_dealer+farmerID))

#both farmer and dealer gender, gender: 1= Female; dealer_fem: 1= Female 
summary(lm(data = m, formula = rating_location ~  gender + dealer_fem + gender*dealer_fem) )
summary(lm(data = m, formula = rating_location ~  gender + dealer_fem + gender*dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer) )

#two way FE
summary(lm(data = m, formula = rating_location ~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) ))
summary(lm(data = m, formula = rating_location~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) +age + educ + tarmac
           + married + age_dealer + education_dealer))

#twoway random effects 
mran2<-plm(rating_location ~ gender+dealer_fem + gender*dealer_fem , data = m, index = c("id.ratee", "farmerID"), model = "random", 
           effect = "twoways")
summary(mran2)

mranc2<-plm(rating_location ~ gender+dealer_fem + gender*dealer_fem + age + educ + tarmac
            + married + age_dealer + education_dealer , data = m, index = c("id.ratee", "farmerID"), model = "random", 
            effect = "twoways")
summary(mranc2)

#caro's approach 
summary(lm(data = a, formula = rating_location ~  dealer_fem ))
summary(lm(data = a, formula = rating_location ~  dealer_fem + age_dealer + education_dealer))

summary(lm(data = a, formula = rating_location ~ gender ))
summary(lm(data = a, formula = rating_location ~ gender + age + educ + tarmac
           + married))


##Interaction between sex of farmer and dealer
fd4<- lm.cluster(data = m, formula = rating_location ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd4 <- sqrt(diag(vcov(fd4)))
res_fd4<-fd4$lm_res

################# QUALITY RATING ###########################

#all variables 
fd5<- lm.cluster(data = m, formula = rating_quality ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 

summary(lm.cluster(data = m, formula = rating_quality ~  gender + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_quality ~  gender + age  + educ + tarmac
                   + married, cluster="id.ratee") )
summary(lm(data = m, formula = rating_quality ~  gender + id.ratee) )
summary(lm(data = m, formula = rating_quality ~  gender + age  + educ + tarmac
           + married  + id.ratee) )

se_fd5 <- sqrt(diag(vcov(fd5)))
res_fd5<-fd5$lm_res

fd5_5<- lm.cluster(data = m, formula = rating_quality ~  dealer_fem , cluster="farmerID") 
summary(fd5_5)

summary(lm.cluster(data = m, formula = rating_quality ~  dealer_fem , cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_quality ~  dealer_fem + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_quality ~  dealer_fem + 
                     age_dealer + education_dealer, cluster="id.ratee") )
summary(lm(data = m, formula = rating_quality ~  dealer_fem + farmerID))
summary(lm(data = m, formula = rating_quality ~  dealer_fem + age_dealer + education_dealer+farmerID))

#both farmer and dealer gender, gender: 1= Female; dealer_fem: 1= Female 
summary(lm(data = m, formula = rating_quality ~  gender + dealer_fem + gender*dealer_fem) )
summary(lm(data = m, formula = rating_quality ~  gender + dealer_fem + gender*dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer) )

#two way FE
summary(lm(data = m, formula = rating_quality~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) ))
summary(lm(data = m, formula = rating_quality~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) +age + educ + tarmac
           + married + age_dealer + education_dealer))

#twoway random effects 
mran3<-plm(rating_quality ~ gender+dealer_fem + gender*dealer_fem , data = m, index = c("id.ratee", "farmerID"), model = "random", 
           effect = "twoways")
summary(mran3)

mranc3<-plm(rating_quality ~ gender+dealer_fem + gender*dealer_fem + age + educ + tarmac
            + married + age_dealer + education_dealer , data = m, index = c("id.ratee", "farmerID"), model = "random", 
            effect = "twoways")
summary(mranc3)


#caro's approach 
summary(lm(data = a, formula = rating_quality ~  dealer_fem ))
summary(lm(data = a, formula = rating_quality ~  dealer_fem + age_dealer + education_dealer))

summary(lm(data = a, formula = rating_quality ~ gender ))
summary(lm(data = a, formula = rating_quality ~ gender + age + educ + tarmac
           + married))


##Interaction between sex of farmer and dealer
fd6<- lm.cluster(data = m, formula = rating_quality ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd6 <- sqrt(diag(vcov(fd6)))
res_fd6<-fd6$lm_res

################# PRICE RATING ###########################

#all variables 
fd7<- lm.cluster(data = m, formula = rating_price ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 

summary(lm.cluster(data = m, formula = rating_price ~  gender + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_price ~  gender + age + educ + tarmac
                   + married, cluster="id.ratee") )
summary(lm(data = m, formula = rating_price ~  gender + id.ratee) )
summary(lm(data = m, formula = rating_price~  gender + age + educ + tarmac
           + married  + id.ratee) )


se_fd7 <- sqrt(diag(vcov(fd7)))
res_fd7<-fd7$lm_res

fd7_7<- lm.cluster(data = m, formula = rating_price ~  dealer_fem , cluster="farmerID")
summary(fd7_7)

summary(lm.cluster(data = m, formula = rating_price ~  dealer_fem , cluster="id.ratee"))
summary(lm.cluster(data = m, formula = rating_price ~  dealer_fem + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_price~  dealer_fem + 
                     age_dealer + education_dealer, cluster="id.ratee") )
summary(lm(data = m, formula = rating_price ~  dealer_fem + farmerID))
summary(lm(data = m, formula = rating_price ~  dealer_fem + age_dealer + education_dealer+farmerID))

#both farmer and dealer gender, gender: 1= Female; dealer_fem: 1= Female 
summary(lm(data = m, formula = rating_price ~  gender + dealer_fem + gender*dealer_fem) )
summary(lm(data = m, formula = rating_price ~  gender + dealer_fem + gender*dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer) )

#two way FE
summary(lm(data = m, formula = rating_price~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) ))
summary(lm(data = m, formula = rating_price~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) +age + educ + tarmac
           + married + age_dealer + education_dealer))

#twoway random effects 
mran4<-plm(rating_price ~ gender+dealer_fem + gender*dealer_fem , data = m, index = c("id.ratee", "farmerID"), model = "random", 
           effect = "twoways")
summary(mran4)

mranc4<-plm(rating_price ~ gender+dealer_fem + gender*dealer_fem + age + educ + tarmac
            + married + age_dealer + education_dealer , data = m, index = c("id.ratee", "farmerID"), model = "random", 
            effect = "twoways")
summary(mranc4)



#caro's approach 
summary(lm(data = a, formula = rating_price ~  dealer_fem ))
summary(lm(data = a, formula = rating_price ~  dealer_fem + age_dealer + education_dealer))

summary(lm(data = a, formula = rating_price ~ gender ))
summary(lm(data = a, formula = rating_price ~ gender + age + educ + tarmac
           + married))


##Interaction between sex of farmer and dealer
fd8<- lm.cluster(data = m, formula = rating_price ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd8 <- sqrt(diag(vcov(fd8)))
res_fd8<-fd8$lm_res

################# STOCK RATING ###########################

#all variables 
fd9<- lm.cluster(data = m, formula = rating_stock ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 

summary(lm.cluster(data = m, formula = rating_stock ~  gender + age+ educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_stock ~  gender + age+ educ + tarmac
                   + married, cluster="id.ratee") )
summary(lm(data = m, formula = rating_stock~  gender + id.ratee) )
summary(lm(data = m, formula = rating_stock ~  gender + age + educ + tarmac
           + married  + id.ratee) )

se_fd9 <- sqrt(diag(vcov(fd9)))
res_fd9<-fd9$lm_res

fd9_9<- lm.cluster(data = m, formula = rating_stock ~  dealer_fem, cluster="farmerID") 
summary(fd9_9)

summary(lm.cluster(data = m, formula = rating_stock ~  dealer_fem, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_stock ~  dealer_fem + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_stock~  dealer_fem + 
                     age_dealer + education_dealer, cluster="id.ratee") )
summary(lm(data = m, formula = rating_stock ~  dealer_fem + farmerID))
summary(lm(data = m, formula = rating_stock ~  dealer_fem + age_dealer + education_dealer+farmerID))

#both farmer and dealer gender, gender: 1= Female; dealer_fem: 1= Female 
summary(lm(data = m, formula = rating_stock~  gender + dealer_fem + gender*dealer_fem) )
summary(lm(data = m, formula = rating_stock ~  gender + dealer_fem + gender*dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer) )

#two way FE
summary(lm(data = m, formula = rating_stock~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) ))
summary(lm(data = m, formula = rating_stock~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) +age + educ + tarmac
           + married + age_dealer + education_dealer))

#twoway random effects 
mran5<-plm(rating_stock ~ gender+dealer_fem + gender*dealer_fem , data = m, index = c("id.ratee", "farmerID"), model = "random", 
           effect = "twoways")
summary(mran5)

mranc5<-plm(rating_stock ~ gender+dealer_fem + gender*dealer_fem + age + educ + tarmac
            + married + age_dealer + education_dealer , data = m, index = c("id.ratee", "farmerID"), model = "random", 
            effect = "twoways")
summary(mranc5)



#caro's approach 
summary(lm(data = a, formula = rating_stock ~  dealer_fem ))
summary(lm(data = a, formula = rating_stock ~  dealer_fem + age_dealer + education_dealer))

summary(lm(data = a, formula = rating_stock ~ gender ))
summary(lm(data = a, formula = rating_stock ~ gender + age + educ + tarmac
           + married))

##Interaction between sex of farmer and dealer
fd10<- lm.cluster(data = m, formula = rating_stock ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd10 <- sqrt(diag(vcov(fd10)))
res_fd10<-fd10$lm_res

################# REPUTATION RATING ###########################

#all variables 
fd11<- lm.cluster(data = m, formula = rating_reputation ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 

summary(lm.cluster(data = m, formula = rating_reputation ~  gender + age  + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_reputation ~  gender + age + educ + tarmac
                   + married, cluster="id.ratee") )
summary(lm(data = m, formula = rating_reputation~  gender + id.ratee) )
summary(lm(data = m, formula = rating_reputation ~  gender + age  + educ + tarmac
           + married  + id.ratee) )


se_fd11 <- sqrt(diag(vcov(fd11)))
res_fd11<-fd11$lm_res

fd11_11<- lm.cluster(data = m, formula = rating_reputation ~  dealer_fem , cluster="farmerID") 
summary(fd11_11)

summary(lm.cluster(data = m, formula = rating_reputation ~  dealer_fem , cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_reputation ~  dealer_fem + age + educ + tarmac
                   + married + age_dealer + education_dealer, cluster="id.ratee") )
summary(lm.cluster(data = m, formula = rating_reputation~  dealer_fem + 
                     age_dealer + education_dealer, cluster="id.ratee") )
summary(lm(data = m, formula = rating_reputation ~  dealer_fem + farmerID))
summary(lm(data = m, formula = rating_reputation ~  dealer_fem + age_dealer + education_dealer+farmerID))

#both farmer and dealer gender, gender: 1= Female; dealer_fem: 1= Female 
summary(lm(data = m, formula = rating_reputation~  gender + dealer_fem + gender*dealer_fem) )
summary(lm(data = m, formula = rating_reputation ~  gender + dealer_fem + gender*dealer_fem + age + educ + tarmac
           + married + age_dealer + education_dealer) )

#two way FE
summary(lm(data = m, formula = rating_reputation~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) ))
summary(lm(data = m, formula = rating_reputation~  gender+ dealer_fem+ gender*dealer_fem + factor(id.ratee) +factor(farmerID) +age + educ + tarmac
           + married + age_dealer + education_dealer))

#twoway random effects 
mran6<-plm(rating_reputation ~ gender+dealer_fem + gender*dealer_fem , data = m, index = c("id.ratee", "farmerID"), model = "random", 
           effect = "twoways")
summary(mran6)

mranc6<-plm(rating_reputation ~ gender+dealer_fem + gender*dealer_fem + age + educ + tarmac
            + married + age_dealer + education_dealer , data = m, index = c("id.ratee", "farmerID"), model = "random", 
            effect = "twoways")
summary(mranc6)


#caro's approach 
summary(lm(data = a, formula = rating_reputation ~  dealer_fem ))
summary(lm(data = a, formula = rating_reputation ~  dealer_fem + age_dealer + education_dealer))

summary(lm(data = a, formula = rating_reputation~ gender ))
summary(lm(data = a, formula = rating_reputation ~ gender + age + educ + tarmac
           + married))

##Interaction between sex of farmer and dealer
fd12<- lm.cluster(data = m, formula = rating_reputation ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                  + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd12 <- sqrt(diag(vcov(fd12)))
res_fd12<-fd12$lm_res

#########################################################################################################################
#########################################################################################################################


##########################################################
##########################################################
############## ONLY SEED SYSTEMS DATA ####################

f_seed<-merged_farmers_seed

#creating dummies for analysis 
f_seed$interaction_yes <- ifelse(f_seed$interaction == 'Yes', 1, 0) 
f_seed$gender <- ifelse(f_seed$farmer_gender == 'Female', 1, 0) #female farmers
f_seed$educ <- 0
f_seed$educ[f_seed$education=="b" | f_seed$education=="c" | f_seed$education=="d" | f_seed$education=="e" | 
           f_seed$education=="f" |f_seed$education=="g" ] <- 1 #educated farmers
f_seed$married <- ifelse(f_seed$marital_status == 'a', 1, 0)  #married farmers

f_seed[f_seed=="999"] <- NA # removing 999


### CLUSTERED REGRESSIONS - LOOKING AT FARMERS' GENDER ###

#################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS ##############
#################################################################

################# OVERALL RATING ###########################

#all variables 
f_seed1<- lm.cluster(data = f_seed, formula = rating_overall ~  gender + age + educ + tarmac
                 + married, cluster="id.ratee") 
fss1 <- sqrt(diag(vcov(f_seed1)))
res_fss1<-f_seed1$lm_res

################# LOCATION RATING ###########################

#all variables 
f_seed2<- lm.cluster(data = f_seed, formula = rating_location ~  gender + age  + educ + tarmac
                     + married, cluster="id.ratee") 
fss2 <- sqrt(diag(vcov(f_seed2)))
res_fss2<-f_seed2$lm_res


################# QUALITY RATING ###########################

#all variables 
f_seed3<- lm.cluster(data = f_seed, formula = rating_quality ~  gender + age  + educ + tarmac
                     + married, cluster="id.ratee") 
fss3 <- sqrt(diag(vcov(f_seed3)))
res_fss3<-f_seed3$lm_res

################# PRICE RATING ###########################

#all variables 
f_seed4<- lm.cluster(data = f_seed, formula = rating_price ~  gender + age  + educ + tarmac
                     + married, cluster="id.ratee") 
fss4 <- sqrt(diag(vcov(f_seed4)))
res_fss4<-f_seed4$lm_res

################# STOCK RATING ###########################
#all variables
f_seed5<- lm.cluster(data = f_seed, formula = rating_stock ~  gender + age  + educ + tarmac
                     + married, cluster="id.ratee") 
fss5 <- sqrt(diag(vcov(f_seed5)))
res_fss5<-f_seed5$lm_res

################# REPUTATION RATING ###########################
#all variables
f_seed6<- lm.cluster(data = f_seed, formula = rating_reputation ~  gender + age + educ + tarmac
                     + married, cluster="id.ratee") 
fss6 <- sqrt(diag(vcov(f_seed6)))
res_fss6<-f_seed6$lm_res


screenreg(list(f_seed1, f_seed2, f_seed3, f_seed4, f_seed5, f_seed6), stars = c(0.01, 0.05, 0.1))

#######################################################################
#######################################################################

############## DATA FROM ONLY SEED SYSTEMS ##################
deal<-dealers_sub

##Index for overall rating by dealers for themselves 
deal$ratee_rating_overall <- rowSums(deal[c("self_rating_location", "self_rating_price","self_rating_quality", "self_rating_stock",
                                                        "self_rating_reputation")])/5
summary(deal$ratee_rating_overall)

## creating dummies
deal$dealer_fem <- ifelse(deal$gender_dealer == 'Female', 1, 0) #female dealers
deal$education_dealer <- 0
deal$education_dealer[deal$educ_dealer=="b" | deal$educ_dealer=="c" | deal$educ_dealer=="d" | deal$educ_dealer=="e" | 
                              deal$educ_dealer=="f" |deal$educ_dealer=="g" ] <- 1 #educated dealers 

deal[deal=="999"]<- NA


####### DEPENDENT VARIABLE -- RATINGS FROM DEALERS -- DATASET ONLY HAVING ALL THE DEALERS ##############
#######################################################################################################

################# OVERALL RATING #################################################################

d1<- lm(data = deal, formula = ratee_rating_overall ~ dealer_fem + age_dealer + education_dealer) 
se_d1 <- sqrt(diag(vcov(d1)))

################# LOCATION RATING ###########################

d2<- lm(data = deal, formula = self_rating_location ~ dealer_fem + age_dealer + education_dealer) 
se_d2 <- sqrt(diag(vcov(d2)))

################# QUALITY RATING ###########################

d3<- lm(data = deal, formula = self_rating_quality ~ dealer_fem + age_dealer + education_dealer) 
se_d3 <- sqrt(diag(vcov(d3)))

################# PRICE RATING ###########################

d4<- lm(data = deal, formula = self_rating_price ~ dealer_fem + age_dealer + education_dealer) 
se_d4 <- sqrt(diag(vcov(d4)))

################# STOCK RATING ###########################

d5<- lm(data = deal, formula = self_rating_stock ~ dealer_fem + age_dealer + education_dealer) 
se_d5 <- sqrt(diag(vcov(d5)))

################# REPUTATION RATING ###########################

d6<- lm(data = deal, formula = self_rating_reputation ~ dealer_fem + age_dealer + education_dealer) 
se_d6 <- sqrt(diag(vcov(d6)))


screenreg(list(d1,d2,d3,d4,d5,d6), stars = c(0.01, 0.05, 0.1))


################################################################################################################
################################################################################################################



################# CREATING DATASET WITH BOTH FARMER AND DEALER DETAILS FROM SEED SYSTEMS ###########################
####################################################################################################################
#getting variables needed from dealer dataset
deal_all <- deal[ , c("id.ratee", "age_dealer","dealer_fem","education_dealer")]  

#merging farmers and dealers data by dealer id
m_all <- merge(f_seed,deal_all, by=c("id.ratee"))

############################################################################
### CLUSTERED REGRESSIONS - LOOKING AT BOTH FARMERS' AND DEALERS' GENDER ###
############################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS #########################
############################################################################

################# OVERALL RATING ###########################

#all variables 
fds1<- lm.cluster(data = m_all, formula = rating_overall ~  gender + dealer_fem + age + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds1 <- sqrt(diag(vcov(fds1)))
res_fds1<-fds1$lm_res

##Interaction between sex of farmer and dealer
fds2<- lm.cluster(data = m_all, formula = rating_overall ~  gender*dealer_fem + age  + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds2 <- sqrt(diag(vcov(fds2)))
res_fds2<-fds2$lm_res

################# LOCATION RATING ###########################

#all variables 
fds3<- lm.cluster(data = m_all, formula = rating_location ~  gender + dealer_fem + age  + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds3 <- sqrt(diag(vcov(fds3)))
res_fds3<-fds3$lm_res

##Interaction between sex of farmer and dealer
fds4<- lm.cluster(data = m_all, formula = rating_location ~  gender*dealer_fem + age  + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds4 <- sqrt(diag(vcov(fds4)))
res_fds4<-fds4$lm_res

################# QUALITY RATING ###########################

#all variables 
fds5<- lm.cluster(data = m_all, formula = rating_quality ~  gender + dealer_fem + age + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds5 <- sqrt(diag(vcov(fds5)))
res_fds5<-fds5$lm_res

##Interaction between sex of farmer and dealer
fds6<- lm.cluster(data = m_all, formula = rating_quality ~  gender*dealer_fem + age  + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds6 <- sqrt(diag(vcov(fds6)))
res_fds6<-fds6$lm_res

################# PRICE RATING ###########################

#all variables 
fds7<- lm.cluster(data = m_all, formula = rating_price ~  gender + dealer_fem + age + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds7 <- sqrt(diag(vcov(fds7)))
res_fds7<-fds7$lm_res

##Interaction between sex of farmer and dealer
fds8<- lm.cluster(data = m_all, formula = rating_price ~  gender*dealer_fem + age  + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds8 <- sqrt(diag(vcov(fds8)))
res_fds8<-fds8$lm_res

################# STOCK RATING ###########################

#all variables 
fds9<- lm.cluster(data = m_all, formula = rating_stock ~  gender + dealer_fem + age + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds9 <- sqrt(diag(vcov(fds9)))
res_fds9<-fds9$lm_res

##Interaction between sex of farmer and dealer
fds10<- lm.cluster(data = m_all, formula = rating_stock ~  gender*dealer_fem + age + educ + tarmac
                  + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds10 <- sqrt(diag(vcov(fds10)))
res_fds10<-fds10$lm_res

################# REPUTATION RATING ###########################

#all variables 
fds11<- lm.cluster(data = m_all, formula = rating_reputation ~  gender + dealer_fem + age  + educ + tarmac
                  + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds11 <- sqrt(diag(vcov(fds11)))
res_fds11<-fds11$lm_res

##Interaction between sex of farmer and dealer
fds12<- lm.cluster(data = m_all, formula = rating_reputation ~  gender*dealer_fem + age + educ + tarmac
                  + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds12 <- sqrt(diag(vcov(fds12)))
res_fds12<-fds12$lm_res


screenreg(list(fds1,fds2,fds3,fds4,fds5,fds6,fds7,fds8,fds9,fds10,fds11,fds12), stars = c(0.01, 0.05, 0.1))


#########################################################################################################################
#########################################################################################################################


###################################################################################################
###################################################################################################

#SECTION 1 : LOOKING AT GENDER OF DEALER

################# OVERALL RATING ###########################
model1<- lm.cluster(data = m, formula = rating_overall ~ dealer_fem, cluster="id.ratee") 
se_model1 <- sqrt(diag(vcov(model1)))
res_model1<-model1$lm_res

model2<- lm.cluster(data = m, formula = rating_overall ~ dealer_fem + age + interaction_yes + educ + tarmac
                    + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model1 <- sqrt(diag(vcov(model1)))
res_model1<-model1$lm_res

model3<- lm(data = m, formula = rating_overall ~ dealer_fem + age + interaction_yes + educ + tarmac
                    + married + age_dealer + education_dealer + farmerID) 


######################################################
############ GETTING DATA READY FOR AVERAGES #########
######################################################

#averages for each dealer

m[c("rating_overall","rating_location","rating_price","rating_quality",
    "rating_stock","rating_reputation") ] <- lapply(m[c("rating_overall",
                                                        "rating_location","rating_price","rating_quality","rating_stock","rating_reputation") ], function(x) as.numeric(as.character(x)) )

m_reviews <- data.frame(cbind(tapply(as.numeric(m$rating_overall), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_location), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_price), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_quality), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_stock), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_reputation), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$age_dealer), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$dealer_fem), m$id.ratee,mean,na.rm=TRUE),
                              tapply(as.numeric(m$education_dealer), m$id.ratee,mean,na.rm=TRUE),
                              tapply(m$interaction=="Yes", m$id.ratee,sum)))
names(m_reviews) <- c("rating_overall","rating_location","rating_price",
                      "rating_quality","rating_stock","rating_reputation","age_dealer",
                      "dealer_fem","education_dealer","nr_rating")

m_reviews$id.ratee <- rownames(m_reviews)

m_reviews <- na.omit(m_reviews)

##### Regressions #####
model4<- lm(data = m_reviews, formula = rating_overall ~ dealer_fem ) 

model5<- lm(data = m_reviews, formula = rating_overall ~ dealer_fem +
              age_dealer + education_dealer + nr_rating) 


screenreg(list(model1,model2, model4, model5), stars = c(0.01, 0.05, 0.1))
summary(model3)


################# LOCATION RATING ###########################

model6<- lm.cluster(data = m, formula = rating_location ~ dealer_fem, cluster="id.ratee") 
se_model6 <- sqrt(diag(vcov(model6)))
res_model6<-model6$lm_res

model7<- lm.cluster(data = m, formula = rating_location ~ dealer_fem + age + interaction_yes + educ + tarmac
                    + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model7 <- sqrt(diag(vcov(model7)))
res_model7<-model7$lm_res

model8<- lm(data = m, formula = rating_location ~ dealer_fem + age + interaction_yes + educ + tarmac
            + married + age_dealer + education_dealer + farmerID) 

model9<- lm(data = m_reviews, formula = rating_location ~ dealer_fem ) 

model10<- lm(data = m_reviews, formula = rating_location ~ dealer_fem +
              age_dealer + education_dealer + nr_rating) 

screenreg(list(model6,model7, model9, model10), stars = c(0.01, 0.05, 0.1))
summary(model8)


################# PRICE RATING ###########################

model11<- lm.cluster(data = m, formula = rating_price ~ dealer_fem, cluster="id.ratee") 
se_model11 <- sqrt(diag(vcov(model11)))
res_model11<-model11$lm_res

model12<- lm.cluster(data = m, formula = rating_price ~ dealer_fem + age + interaction_yes + educ + tarmac
                    + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model12 <- sqrt(diag(vcov(model12)))
res_model12<-model12$lm_res

model13<- lm(data = m, formula = rating_price ~ dealer_fem + age + interaction_yes + educ + tarmac
            + married + age_dealer + education_dealer + farmerID) 

model14<- lm(data = m_reviews, formula = rating_price ~ dealer_fem ) 

model15<- lm(data = m_reviews, formula = rating_price ~ dealer_fem +
               age_dealer + education_dealer + nr_rating) 

screenreg(list(model11,model12, model14, model15), stars = c(0.01, 0.05, 0.1))
summary(model13)


################# QUALITY RATING ###########################

model16<- lm.cluster(data = m, formula = rating_quality ~ dealer_fem, cluster="id.ratee") 
se_model16 <- sqrt(diag(vcov(model16)))
res_model16<-model16$lm_res

model17<- lm.cluster(data = m, formula = rating_quality ~ dealer_fem + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model17 <- sqrt(diag(vcov(model17)))
res_model17<-model17$lm_res

model18<- lm(data = m, formula = rating_quality ~ dealer_fem + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + farmerID) 

model19<- lm(data = m_reviews, formula = rating_quality ~ dealer_fem ) 

model20<- lm(data = m_reviews, formula = rating_quality ~ dealer_fem +
               age_dealer + education_dealer + nr_rating) 

screenreg(list(model16,model17, model19, model20), stars = c(0.01, 0.05, 0.1))
summary(model18)


################# STOCK RATING ###########################

model21<- lm.cluster(data = m, formula = rating_stock ~ dealer_fem, cluster="id.ratee") 
se_model21 <- sqrt(diag(vcov(model21)))
res_model21<-model21$lm_res

model22<- lm.cluster(data = m, formula = rating_stock ~ dealer_fem + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model22 <- sqrt(diag(vcov(model22)))
res_model22<-model22$lm_res

model23<- lm(data = m, formula = rating_stock ~ dealer_fem + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + farmerID) 

model24<- lm(data = m_reviews, formula = rating_stock ~ dealer_fem ) 

model25<- lm(data = m_reviews, formula = rating_stock ~ dealer_fem +
               age_dealer + education_dealer + nr_rating) 

screenreg(list(model21,model22, model24, model25), stars = c(0.01, 0.05, 0.1))
summary(model23)


################# REPUTATION RATING ###########################

model26<- lm.cluster(data = m, formula = rating_reputation ~ dealer_fem, cluster="id.ratee") 
se_model26 <- sqrt(diag(vcov(model26)))
res_model26<-model26$lm_res

model27<- lm.cluster(data = m, formula = rating_reputation ~ dealer_fem + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model27 <- sqrt(diag(vcov(model27)))
res_model27<-model27$lm_res

model28<- lm(data = m, formula = rating_reputation ~ dealer_fem + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + farmerID) 

model29<- lm(data = m_reviews, formula = rating_reputation ~ dealer_fem ) 

model30<- lm(data = m_reviews, formula = rating_reputation ~ dealer_fem +
               age_dealer + education_dealer + nr_rating) 

screenreg(list(model26,model27, model29, model30), stars = c(0.01, 0.05, 0.1))
summary(model28)

##########################################################################################




#SECTION 2 : LOOKING AT GENDER OF FARMER

################# OVERALL RATING ###########################

model31<- lm.cluster(data = m, formula = rating_overall ~ gender, cluster="id.ratee") 
se_model31 <- sqrt(diag(vcov(model31)))
res_model31<-model31$lm_res

model32<- lm.cluster(data = m, formula = rating_overall ~ gender + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model32 <- sqrt(diag(vcov(model32)))
res_model32<-model32$lm_res

model33<- lm(data = m, formula = rating_overall ~ gender + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + id.ratee) 

######################################################
############ GETTING DATA READY FOR AVERAGES #########
######################################################

#averages for each farmer

mf_reviews <- data.frame(cbind(tapply(as.numeric(m$rating_overall), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_location), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_price), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_quality), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_stock), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$rating_reputation), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$age), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$tarmac), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$gender), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$educ), m$farmerID,mean,na.rm=TRUE),
                              tapply(as.numeric(m$married), m$farmerID,mean,na.rm=TRUE),
                              tapply(m$interaction=="Yes", m$farmerID,sum)))
names(mf_reviews) <- c("rating_overall","rating_location","rating_price",
                      "rating_quality","rating_stock","rating_reputation","age",
                      "tarmac","gender","educ","married","nr_rating_given")

mf_reviews$farmerID <- rownames(mf_reviews)

mf_reviews <- na.omit(mf_reviews)

mf_reviews<-mf_reviews[(mf_reviews$gender=="1" | mf_reviews$gender=="0" ),]
mf_reviews<-mf_reviews[(mf_reviews$educ=="1" | mf_reviews$educ=="0"),]
mf_reviews<-mf_reviews[( mf_reviews$married=="0"| mf_reviews$married=="1"),]

##################################################################################

#regressions 

model34<- lm(data = mf_reviews, formula = rating_overall ~ gender ) 

model35<- lm(data = mf_reviews, formula = rating_overall ~ gender +
               age + tarmac + educ + married + nr_rating_given) 

screenreg(list(model31,model32, model34, model35), stars = c(0.01, 0.05, 0.1))
summary(model33)



################# LOCATION RATING ###########################

model36<- lm.cluster(data = m, formula = rating_location ~ gender, cluster="id.ratee") 
se_model36 <- sqrt(diag(vcov(model36)))
res_model36<-model36$lm_res

model37<- lm.cluster(data = m, formula = rating_location ~ gender + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model37 <- sqrt(diag(vcov(model37)))
res_model37<-model37$lm_res

model38<- lm(data = m, formula = rating_location ~ gender + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + id.ratee) 

model39<- lm(data = mf_reviews, formula = rating_location ~ gender ) 

model40<- lm(data = mf_reviews, formula = rating_location ~ gender +
               age + tarmac + educ + married + nr_rating_given) 

screenreg(list(model36,model37, model39, model40), stars = c(0.01, 0.05, 0.1))
summary(model38)



################# PRICE RATING ###########################

model41<- lm.cluster(data = m, formula = rating_price ~ gender, cluster="id.ratee") 
se_model41 <- sqrt(diag(vcov(model41)))
res_model41<-model41$lm_res

model42<- lm.cluster(data = m, formula = rating_price ~ gender + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model42 <- sqrt(diag(vcov(model42)))
res_model42<-model42$lm_res

model43<- lm(data = m, formula = rating_price ~ gender + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + id.ratee) 

model44<- lm(data = mf_reviews, formula = rating_price ~ gender ) 

model45<- lm(data = mf_reviews, formula = rating_price ~ gender +
               age + tarmac + educ + married + nr_rating_given) 

screenreg(list(model41,model42, model44, model45), stars = c(0.01, 0.05, 0.1))
summary(model43)


################# QUALITY RATING ###########################

model46<- lm.cluster(data = m, formula = rating_quality ~ gender, cluster="id.ratee") 
se_model46 <- sqrt(diag(vcov(model46)))
res_model46<-model46$lm_res

model47<- lm.cluster(data = m, formula = rating_quality ~ gender + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model47 <- sqrt(diag(vcov(model47)))
res_model47<-model47$lm_res

model48<- lm(data = m, formula = rating_quality ~ gender + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + id.ratee) 

model49<- lm(data = mf_reviews, formula = rating_quality ~ gender ) 

model50<- lm(data = mf_reviews, formula = rating_quality ~ gender +
               age + tarmac + educ + married + nr_rating_given) 

screenreg(list(model46,model47, model49, model50), stars = c(0.01, 0.05, 0.1))
summary(model48)


################# STOCK RATING ###########################

model51<- lm.cluster(data = m, formula = rating_stock ~ gender, cluster="id.ratee") 
se_model51 <- sqrt(diag(vcov(model51)))
res_model51<-model51$lm_res

model52<- lm.cluster(data = m, formula = rating_stock ~ gender + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model52 <- sqrt(diag(vcov(model52)))
res_model52<-model52$lm_res

model53<- lm(data = m, formula = rating_stock ~ gender + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + id.ratee) 

model54<- lm(data = mf_reviews, formula = rating_stock ~ gender ) 

model55<- lm(data = mf_reviews, formula = rating_stock ~ gender +
               age + tarmac + educ + married + nr_rating_given) 

screenreg(list(model51,model52, model54, model55), stars = c(0.01, 0.05, 0.1))
summary(model53)



################# REPUTATION RATING ###########################

model56<- lm.cluster(data = m, formula = rating_reputation ~ gender, cluster="id.ratee") 
se_model56 <- sqrt(diag(vcov(model56)))
res_model56<-model56$lm_res

model57<- lm.cluster(data = m, formula = rating_reputation ~ gender + age + interaction_yes + educ + tarmac
                     + married + age_dealer + education_dealer, cluster="id.ratee") 
se_model57 <- sqrt(diag(vcov(model57)))
res_model57<-model57$lm_res

model58<- lm(data = m, formula = rating_reputation ~ gender + age + interaction_yes + educ + tarmac
             + married + age_dealer + education_dealer + id.ratee) 

model59<- lm(data = mf_reviews, formula = rating_reputation ~ gender ) 

model60<- lm(data = mf_reviews, formula = rating_reputation ~ gender +
              age + tarmac + educ + married + nr_rating_given) 

screenreg(list(model56,model57, model59, model60), stars = c(0.01, 0.05, 0.1))
summary(model58)


##########################################################################################

##########################################################################################$


###########  Replicating Caro's code ####################################################

rating_dyads <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))

rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ] <- lapply(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ], function(x) as.numeric(as.character(x)) )

reviews <- data.frame(cbind(tapply(as.numeric(rating_dyads$general_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$location_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$price_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$quality_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$stock_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$reputation_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(rating_dyads$bought_at_dealer=="Yes" | rating_dyads$knows_other_customer=="Yes", rating_dyads$shop_ID,sum)))
names(reviews) <- c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","nr_reviews")

reviews$shop_ID <- rownames(reviews)

#path <- strsplit(path, "/farmer")[[1]]
#write.csv(reviews, paste(path, "agro_input/raw/shiny_app/reviews_general.csv",sep="/"), row.names=FALSE)

### now specifically for seed:

rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x) as.numeric(as.character(x)) )

rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x)replace(x, x == 98,NA) )

rating_dyads$quality_rating[rating_dyads$shop_ID == "AD_99"]

reviews <- data.frame(cbind(tapply(as.numeric(rating_dyads$quality_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$seed_quality_general_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$seed_yield_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$seed_drought_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$seed_disease_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$seed_maturing_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(rating_dyads$seed_germinate_rating), rating_dyads$shop_ID,mean,na.rm=TRUE),
                            tapply(rating_dyads$bought_at_dealer=="Yes" | rating_dyads$knows_other_customer=="Yes", rating_dyads$shop_ID,sum)))
names(reviews) <- c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination","nr_reviews")

reviews$shop_ID <- rownames(reviews)

##missings or ratings based on few raters
# first calcuate catchement areas mean. For that, we need to merge catchment IDs in first
#treats_shop_level <- read.csv(paste(path,"agro_input/public/treats_shop_level.csv", sep="/"), stringsAsFactors = FALSE)

#reviews <- merge(reviews, treats_shop_level[c("shop_ID","catchID")], by.x="shop_ID", by.y="shop_ID")

#get subgroup means
#sg_means <- aggregate(reviews[c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination") ], list(reviews$catchID), mean, na.rm=T)
#names(sg_means) <- paste(names(sg_means),"av",sep="_")
#names(sg_means)[names(sg_means) == "Group.1_av"] <- "catchID"

#reviews <- merge(reviews, sg_means,by.x="catchID",by.y="catchID")
##not rated: CA average
#reviews[reviews$nr_reviews==0,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- reviews[reviews$nr_reviews==0,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]

##rated by 1: 33% 1 rating, 66% CA average
#reviews[reviews$nr_reviews==1,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- .3333*reviews[reviews$nr_reviews==1,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] + .6667*reviews[reviews$nr_reviews==1,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]
##rated by 2: 66% 2 ratings, 33% CA average
#reviews[reviews$nr_reviews==2,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] <- .6667*reviews[reviews$nr_reviews==2,c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")] + .3333*reviews[reviews$nr_reviews==2,c("general_av","yield_av","drought_resistent_av","disease_resistent_av","early_maturing_av","germination_av")]
#score
reviews$score <-  rowMeans(reviews[c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)


#path <- strsplit(path, "/farmer")[[1]]
#write.csv(reviews, paste(path, "agro_input/raw/shiny_app/reviews_seed.csv",sep="/"), row.names=FALSE)


#######################################################
#differences between ratings of male & female dealers?#
#######################################################

#merge with dealer baseline data to get gender variable
baseline_dealer <- read.csv(paste(path,"papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)
baseline_dealer_with_score <- merge(reviews, baseline_dealer, by="shop_ID")

#create gender dummy
baseline_dealer_with_score$genderdummy <- ifelse(baseline_dealer_with_score$maize.owner.agree.gender == "Male", 1, 0)
table(baseline_dealer_with_score$genderdummy)

#control for age, education, distance of to nearest tarmac road, distance to nearest murram road
summary(baseline_dealer_with_score$maize.owner.agree.age)
baseline_dealer_with_score$prim <- FALSE
baseline_dealer_with_score$prim <- (baseline_dealer_with_score$maize.owner.agree.educ %in% c("c","d","e","f"))
summary(baseline_dealer_with_score$prim)
baseline_dealer_with_score$maize.owner.agree.q3[baseline_dealer_with_score$maize.owner.agree.q3==999] <- NA
summary(baseline_dealer_with_score$maize.owner.agree.q3)
summary(baseline_dealer_with_score$maize.owner.agree.q4)

#regressions with only gender dummy#

c1 <- summary(lm(score~genderdummy , data = baseline_dealer_with_score))

c2 <- summary(lm(quality_rating~genderdummy , data = baseline_dealer_with_score))
c3<-summary(lm(general~genderdummy , data = baseline_dealer_with_score))
c4<-summary(lm(yield~genderdummy , data = baseline_dealer_with_score))
c5<-summary(lm(drought_resistent~genderdummy , data = baseline_dealer_with_score))
c6<-summary(lm(disease_resistent~genderdummy , data = baseline_dealer_with_score))
c7<-summary(lm(early_maturing~genderdummy , data = baseline_dealer_with_score))
c8<-summary(lm(germination~genderdummy , data = baseline_dealer_with_score))

screenreg(list(c1,c2,c3,c4,c5,c6,c7,c8), stars = c(0.01, 0.05, 0.1))

#regressions with only gender dummy and basic controls (age, education, distance to tarmac and murram road)#

c9 <- summary(lm(score~genderdummy+ maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4  , data = baseline_dealer_with_score))

c10 <- summary(lm(quality_rating~genderdummy + maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4, data = baseline_dealer_with_score))
c11<-summary(lm(general~genderdummy+ maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4 , data = baseline_dealer_with_score))
c12<-summary(lm(yield~genderdummy+ maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4 , data = baseline_dealer_with_score))
c13<-summary(lm(drought_resistent~genderdummy+ maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4 , data = baseline_dealer_with_score))
c14<-summary(lm(disease_resistent~genderdummy+ maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4 , data = baseline_dealer_with_score))
c15<-summary(lm(early_maturing~genderdummy+ maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4 , data = baseline_dealer_with_score))
c16<-summary(lm(germination~genderdummy+ maize.owner.agree.age+prim+maize.owner.agree.q3+maize.owner.agree.q4 , data = baseline_dealer_with_score))

screenreg(list(c9,c10,c11,c12,c13,c14,c15,c16), stars = c(0.01, 0.05, 0.1))

#additionally control for
#Q5. Is this a specialized agro-input shop that only sells farm inputs?
table(baseline_dealer_with_score$maize.owner.agree.q5)

#Q8. When was this agro-input shop established? (year)
baseline_dealer_with_score$years_shop <- 2020 - as.numeric(as.character(substr(baseline_dealer_with_score$maize.owner.agree.q8, start=1, stop=4)))
summary(baseline_dealer_with_score$years_shop)

#Q69. Are seed stored in a dedicated area, away from other merchandize?
table(baseline_dealer_with_score$maize.owner.agree.temp.q69)

#Q71. Do you have a problem with rats or pests (insects, rats)?
table(baseline_dealer_with_score$maize.owner.agree.temp.q71)

#Q72. Is the roof leak-proof?
table(baseline_dealer_with_score$maize.owner.agree.temp.q72)

#Q73. Is the roof insulated to keep heat out?
table(baseline_dealer_with_score$maize.owner.agree.temp.q73)

#Q74. Are the walls insulated to keep the heat out?
table(baseline_dealer_with_score$maize.owner.agree.temp.q74)

#Q75. Is the area ventilated
table(baseline_dealer_with_score$maize.owner.agree.temp.q75)

#Q76. Are the walls plastered?
table(baseline_dealer_with_score$maize.owner.agree.temp.q76)

#Q77. Material of floor in areas where seed is stored?
baseline_dealer_with_score$goodfloor <- FALSE
baseline_dealer_with_score$goodfloor <- (baseline_dealer_with_score$maize.owner.agree.temp.q77 %in% c("Cement","Tiles"))
table(baseline_dealer_with_score$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
baseline_dealer_with_score$badlighting <- FALSE
baseline_dealer_with_score$badlighting <- (baseline_dealer_with_score$maize.owner.agree.temp.q78 %in% c("1"))
table(baseline_dealer_with_score$badlighting)

#Q79. On what surface are seed stored?
baseline_dealer_with_score$badstored <- FALSE
baseline_dealer_with_score$badstored <- (baseline_dealer_with_score$maize.owner.agree.temp.q79 %in% c("1", "2", "96"))
table(baseline_dealer_with_score$badstored)

#Q80. Do you see maize seed that is stored in open bags or open containers?
table(baseline_dealer_with_score$maize.owner.agree.temp.q80)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings or that the business is registered with some association)
table(baseline_dealer_with_score$maize.owner.agree.temp.q81)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
as.numeric(as.character(baseline_dealer_with_score$maize.owner.agree.temp.q82))
summary(baseline_dealer_with_score$maize.owner.agree.temp.q82)

# #Q92. When repackaging seed, do you keep track of expiry date (eg include it in the bag/write it on the bag)
# baseline_dealer_with_score$maize.owner.agree.q92[baseline_dealer_with_score$maize.owner.agree.q92=="n/a"] <- NA
# summary(baseline_dealer_with_score$maize.owner.agree.q92)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
table(baseline_dealer_with_score$maize.owner.agree.q96)

#Q70. Entert the temperature in the seed store (where seed is stored)
table(baseline_dealer_with_score$maize.owner.agree.q70)

# #moisture
# summary(baseline_dealer_with_score$reading)

#regressions with gender dummy and all controls#

c17 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

c18 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)
c19 <- lm(general~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)
c20 <- lm(yield~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)
c21 <- lm(drought_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)
c22 <- lm(disease_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)
c23 <- lm(early_maturing~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)
c24 <- lm(germination~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_with_score)

screenreg(list(c17,c18,c19,c20,c21,c22,c23,c24), stars = c(0.01, 0.05, 0.1))

#restricting data and running the reg for Caro's approach
table(baseline_dealer_with_score$nr_reviews)   
#most dealers get rated twice 
base_caro<- baseline_dealer_with_score[baseline_dealer_with_score$nr_reviews==2, ]

base1 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)

base2 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)
base3 <- lm(general~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)
base4 <- lm(yield~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)
base5 <- lm(drought_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)
base6 <- lm(disease_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)
base7 <- lm(early_maturing~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)
base8 <- lm(germination~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = base_caro)

screenreg(list(base1, base2, base3, base4, base5, base6, base7, base8), stars = c(0.01, 0.05, 0.1))



###### Prepping data for analysing quality rating with both stack and seeds data ######## 

dealers_stack <- dealers[ , c("hh.maize.q6g", "hh.maize.q7","hh.maize.q9","hh.maize.q6a","hh.maize.q6b",
                             "id.agro")]  
names(dealers_stack) <- c("age_dealer", "gender_dealer", "educ_dealer","tarmac","murram","id.ratee") #getting dealer variables

###getting ratings given by farmers
ratings_dealer <- ratings[ , c("id.ratee", "rating_quality")]  

###merging
stack_d <- merge(dealers_stack,ratings_dealer, by=c("id.ratee"))

## creating dummies
stack_d$genderdummy <- ifelse(stack_d$gender_dealer == 'Male', 1, 0) #male dealers
stack_d$educ <- 0
stack_d$educ[stack_d$educ_dealer=="b" | stack_d$educ_dealer=="c" | stack_d$educ_dealer=="d" | stack_d$educ_dealer=="e" | 
                              stack_d$educ_dealer=="f" |stack_d$educ_dealer=="g" ] <- 1 #educated dealers 

stack_d[stack_d=="999"]<- NA

stack_d <- stack_d[ , c("id.ratee", "rating_quality","age_dealer", "genderdummy", "educ","tarmac","murram")]  

### extracting variables from seeds data
seed_qual <- baseline_dealer_with_score[ , c("genderdummy", "maize.owner.agree.age","prim","maize.owner.agree.q3","maize.owner.agree.q4",
                              "shop_ID", "quality_rating")]  
seed_qual$educ <- ifelse(seed_qual$prim == 'TRUE', 1, 0)

names(seed_qual) <- c("genderdummy","age_dealer", "prim","tarmac","murram","id.ratee","rating_quality","educ")
seed_qual <- seed_qual[ , c("genderdummy","age_dealer","tarmac","murram","id.ratee","rating_quality","educ")]  

dealer_stack_seed <-rbind(stack_d,seed_qual) ### putting both datasets together

### Regression
c25<- lm(rating_quality~genderdummy +age_dealer+tarmac+murram+educ , data = dealer_stack_seed)
c26<- lm(rating_quality~genderdummy  , data = dealer_stack_seed)
screenreg(list(c25, c26), stars = c(0.01, 0.05, 0.1))



#######################################################
#######################################################
#######################################################
#######################################################



rating_d <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))
baseline_d <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)
baseline <- merge(rating_d, baseline_d, by="shop_ID")

baseline$quality_rating<-as.numeric(baseline$quality_rating)
baseline$seed_quality_general_rating<-as.numeric(baseline$seed_quality_general_rating)
baseline$seed_yield_rating<-as.numeric(baseline$seed_yield_rating)
baseline$seed_drought_rating<-as.numeric(baseline$seed_drought_rating)
baseline$seed_disease_rating<-as.numeric(baseline$seed_disease_rating)
baseline$seed_maturing_rating<-as.numeric(baseline$seed_maturing_rating)
baseline$seed_germinate_rating<-as.numeric(baseline$seed_germinate_rating)

baseline[baseline=="98"]<-NA
baseline$score <-  rowMeans(baseline[c("quality_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm=T)

#create gender dummy
baseline$genderdummy <- ifelse(baseline$maize.owner.agree.gender == "Male", 1, 0)

#control for age, education, distance of to nearest tarmac road, distance to nearest murram road
summary(baseline$maize.owner.agree.age)
baseline$prim <- FALSE
baseline$prim <- (baseline$maize.owner.agree.educ %in% c("c","d","e","f"))

baseline$maize.owner.agree.q3[baseline$maize.owner.agree.q3==999] <- NA
summary(baseline$maize.owner.agree.q3)
summary(baseline$maize.owner.agree.q4)

#additionally control for
#Q5. Is this a specialized agro-input shop that only sells farm inputs?
table(baseline$maize.owner.agree.q5)

#Q8. When was this agro-input shop established? (year)
baseline$years_shop <- 2020 - as.numeric(as.character(substr(baseline$maize.owner.agree.q8, start=1, stop=4)))

#Q69. Are seed stored in a dedicated area, away from other merchandize?
table(baseline$maize.owner.agree.temp.q69)

#Q71. Do you have a problem with rats or pests (insects, rats)?
table(baseline$maize.owner.agree.temp.q71)

#Q72. Is the roof leak-proof?
table(baseline$maize.owner.agree.temp.q72)

#Q73. Is the roof insulated to keep heat out?
table(baseline$maize.owner.agree.temp.q73)

#Q74. Are the walls insulated to keep the heat out?
table(baseline$maize.owner.agree.temp.q74)

#Q75. Is the area ventilated
table(baseline$maize.owner.agree.temp.q75)

#Q76. Are the walls plastered?
table(baseline$maize.owner.agree.temp.q76)

#Q77. Material of floor in areas where seed is stored?
baseline$goodfloor <- FALSE
baseline$goodfloor <- (baseline$maize.owner.agree.temp.q77 %in% c("Cement","Tiles"))
table(baseline$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
baseline$badlighting <- FALSE
baseline$badlighting <- (baseline$maize.owner.agree.temp.q78 %in% c("1"))

#Q79. On what surface are seed stored?
baseline$badstored <- FALSE
baseline$badstored <- (baseline$maize.owner.agree.temp.q79 %in% c("1", "2", "96"))

#Q80. Do you see maize seed that is stored in open bags or open containers?
table(baseline$maize.owner.agree.temp.q80)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings or that the business is registered with some association)
table(baseline$maize.owner.agree.temp.q81)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
as.numeric(as.character(baseline$maize.owner.agree.temp.q82))
summary(baseline$maize.owner.agree.temp.q82)

# #Q92. When repackaging seed, do you keep track of expiry date (eg include it in the bag/write it on the bag)
# baseline_dealer_with_score$maize.owner.agree.q92[baseline_dealer_with_score$maize.owner.agree.q92=="n/a"] <- NA
# summary(baseline_dealer_with_score$maize.owner.agree.q92)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
table(baseline$maize.owner.agree.q96)

#Q70. Entert the temperature in the seed store (where seed is stored)
table(baseline$maize.owner.agree.q70)

baseline_f <- merge(baseline, farmers_seedsub, by="farmer_ID")

#creating dummies for analysis 
baseline_f$educ_f <- 0
baseline_f$educ_f[baseline_f$Check2.check.maize.q17=="b" |baseline_f$Check2.check.maize.q17=="c" | baseline_f$Check2.check.maize.q17=="d" | baseline_f$Check2.check.maize.q17=="e" | 
                    baseline_f$Check2.check.maize.q17=="f" |baseline_f$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
baseline_f$married <- ifelse(baseline_f$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers

baseline_f[baseline_f=="98"]<-NA


##### looking at only baseline, running the reg by restricting the data ######
baseline_complete<- baseline[complete.cases(baseline),]
count<- count(baseline_complete$shop_ID)
names(count) <- c("shop_ID", "freq")
bas <- merge (baseline_complete, count, by = "shop_ID")
table(bas$freq)
bas2<- bas[bas$freq==2,]

base9 <- lm.cluster(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + 
                      maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)

base10 <- lm.cluster(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + 
                       maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base11 <- lm.cluster(general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base12 <- lm.cluster(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base13 <- lm.cluster(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base14 <- lm.cluster(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base15 <- lm.cluster(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base16 <- lm.cluster(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)

screenreg(list(base9, base10, base11, base12, base13, base14, base15, base16), stars = c(0.01, 0.05, 0.1))

#bas2$floor <- ifelse(bas2$goodfloor == "TRUE", 1, 0)
#bas2$wall_plaster <- ifelse(bas2$maize.owner.agree.temp.q76 =="Yes",1,0)

##leaving the two variables out - goodfloor and q76 - with clustered SE

base17 <- lm.cluster(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + 
                      maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)

base18 <- lm.cluster(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + 
                       maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base19 <- lm.cluster(general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75  + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base20 <- lm.cluster(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base21 <- lm.cluster(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base22 <- lm.cluster(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base23 <- lm.cluster(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)
base24 <- lm.cluster(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, cluster="shop_ID", data = bas2)

screenreg(list(base17, base18, base19, base20, base21, base22, base23, base24), stars = c(0.01, 0.05, 0.1))


#without SE clustered
base17 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + 
                      maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = bas2)

base18 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + 
                       maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = bas2)
base19 <- lm(general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = bas2)
base20 <- lm(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = bas2)
base21 <- lm(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,  data = bas2)
base22<- lm(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,data = bas2)
base23 <- lm(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = bas2)
base24 <- lm(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,data = bas2)

screenreg(list(base17, base18, base19, base20, base21, base22, base23, base24), stars = c(0.01, 0.05, 0.1))

gg1 <- lm.cluster(score~genderdummy,cluster="shop_ID", data = bas2)
gg2 <- lm.cluster(quality_rating~genderdummy,cluster="shop_ID", data = bas2)
gg3 <- lm.cluster(general_rating~genderdummy, cluster="shop_ID",data = bas2)
gg4<- lm.cluster(seed_yield_rating~genderdummy , cluster="shop_ID",data = bas2)
gg5 <- lm.cluster(seed_drought_rating~genderdummy, cluster="shop_ID", data = bas2)
gg6<- lm.cluster(seed_disease_rating~genderdummy ,cluster="shop_ID",data = bas2)
gg7 <- lm.cluster(seed_maturing_rating~genderdummy ,cluster="shop_ID", data = bas2)
gg8 <- lm.cluster(seed_germinate_rating~genderdummy,cluster="shop_ID",data = bas2)

screenreg(list(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8), stars = c(0.01, 0.05, 0.1))

#### Caro's approach

reviews_caro <- data.frame(cbind(tapply(as.numeric(bas2$quality_rating), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(bas2$general_rating), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(bas2$seed_yield_rating), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(bas2$seed_drought_rating), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(bas2$seed_disease_rating), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(bas2$seed_maturing_rating), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(bas2$seed_germinate_rating), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(as.numeric(bas2$score), bas2$shop_ID,mean,na.rm=TRUE),
                            tapply(bas2$bought_at_dealer=="Yes" | bas2$knows_other_customer=="Yes", bas2$shop_ID,sum)))
names(reviews_caro) <- c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination","score","nr_reviews")

reviews_caro$shop_ID <- rownames(reviews_caro)

#reviews_caro$score <-  rowMeans(reviews_caro[c("quality_rating","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)

#merge with dealer baseline data to get gender variable
baseline_dealer_caro <- merge(reviews_caro, baseline_dealer, by="shop_ID")

#create gender dummy
baseline_dealer_caro$genderdummy <- ifelse(baseline_dealer_caro$maize.owner.agree.gender == "Male", 1, 0)
table(baseline_dealer_caro$genderdummy)

#control for age, education, distance of to nearest tarmac road, distance to nearest murram road
summary(baseline_dealer_caro$maize.owner.agree.age)
baseline_dealer_caro$prim <- FALSE
baseline_dealer_caro$prim <- (baseline_dealer_caro$maize.owner.agree.educ %in% c("c","d","e","f"))
summary(baseline_dealer_caro$prim)
baseline_dealer_caro$maize.owner.agree.q3[baseline_dealer_caro$maize.owner.agree.q3==999] <- NA
summary(baseline_dealer_caro$maize.owner.agree.q3)
summary(baseline_dealer_caro$maize.owner.agree.q4)

#Q8. When was this agro-input shop established? (year)
baseline_dealer_caro$years_shop <- 2020 - as.numeric(as.character(substr(baseline_dealer_caro$maize.owner.agree.q8, start=1, stop=4)))
summary(baseline_dealer_caro$years_shop)

#Q77. Material of floor in areas where seed is stored?
baseline_dealer_caro$goodfloor <- FALSE
baseline_dealer_caro$goodfloor <- (baseline_dealer_caro$maize.owner.agree.temp.q77 %in% c("Cement","Tiles"))
table(baseline_dealer_caro$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
baseline_dealer_caro$badlighting <- FALSE
baseline_dealer_caro$badlighting <- (baseline_dealer_caro$maize.owner.agree.temp.q78 %in% c("1"))
table(baseline_dealer_caro$badlighting)

#Q79. On what surface are seed stored?
baseline_dealer_caro$badstored <- FALSE
baseline_dealer_caro$badstored <- (baseline_dealer_caro$maize.owner.agree.temp.q79 %in% c("1", "2", "96"))
table(baseline_dealer_caro$badstored)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
as.numeric(as.character(baseline_dealer_caro$maize.owner.agree.temp.q82))
summary(baseline_dealer_caro$maize.owner.agree.temp.q82)

#running the reg for Caro's approach on restricted data
b1 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)

b2 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b3 <- lm(general~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b4 <- lm(yield~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b5 <- lm(drought_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b6 <- lm(disease_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b7 <- lm(early_maturing~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b8 <- lm(germination~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)

screenreg(list(b1, b2, b3, b4, b5, b6, b7, b8), stars = c(0.01, 0.05, 0.1))

gg9 <- lm(score~genderdummy,data = baseline_dealer_caro)
gg10 <- lm(quality_rating~genderdummy,data = baseline_dealer_caro)
gg11 <- lm(general~genderdummy, data = baseline_dealer_caro)
gg12<- lm(yield~genderdummy ,data = baseline_dealer_caro)
gg13 <- lm(drought_resistent~genderdummy,data = baseline_dealer_caro)
gg14<- lm(disease_resistent~genderdummy ,data = baseline_dealer_caro)
gg15 <- lm(early_maturing~genderdummy ,data = baseline_dealer_caro)
gg16 <- lm(germination~genderdummy,data = baseline_dealer_caro)

screenreg(list(gg9, gg10, gg11, gg12, gg13, gg14, gg15, gg16), stars = c(0.01, 0.05, 0.1))


#excluding q76 and goodfloor 

b9 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)

b10 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b11 <- lm(general~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b12 <- lm(yield~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b13 <- lm(drought_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b14 <- lm(disease_resistent~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b15 <- lm(early_maturing~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)
b16 <- lm(germination~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + maize.owner.agree.temp.q75 + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_dealer_caro)

screenreg(list(b9, b10, b11, b12, b13, b14, b15, b16), stars = c(0.01, 0.05, 0.1))

g <- lm(score~genderdummy, data = baseline_dealer_caro)

screenreg(list(g), stars = c(0.01, 0.05, 0.1))

#Regressions 

#Overall score
#cluster at dealer level
qual1 <- lm.cluster(score~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual1)
clus1 <- lm.cluster(score~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus1)

qual2 <- lm.cluster(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual3 <- lm.cluster(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
screenreg(list(qual3, qual2), stars = c(0.01, 0.05, 0.10))

qual4 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual1, qual2, qual3, c17), stars = c(0.01, 0.05, 0.15))
summary(qual4)

#farmer fixed effects without farmer vars
fe_f1 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 +farmer_ID, data = baseline_f)
              
fe_f1_1 <- lm(score~genderdummy +farmer_ID, data = baseline_f)
summary (fe_f1_1)

summary(fe_f1)

fe_f1_try <- plm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f1_try)

#QUALITY RATING 
#cluster at dealer level 
qual5 <- lm.cluster(quality_rating~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual5)
clus2 <- lm.cluster(quality_rating~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus2)

qual6 <- lm.cluster(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual7 <- lm.cluster(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
qual8 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual5, qual6, qual7, c18), stars = c(0.01, 0.05, 0.15))
screenreg(list(qual7, qual6), stars = c(0.01, 0.05, 0.10))
summary(qual8)

#farmer fixed effects without farmer vars 
fe_f2 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f2)

fe_f2_2 <- lm(quality_rating~genderdummy + farmer_ID, data = baseline_f)
summary(fe_f2_2)

fe_f2_try <- plm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f2_try)


#GENERAL RATING 
#cluster at dealer level 
qual9 <- lm.cluster(seed_quality_general_rating~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual9)
clus3 <- lm.cluster(seed_quality_general_rating~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus3)

qual10 <- lm.cluster(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual11 <- lm.cluster(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
qual12 <- lm(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual11, qual10), stars = c(0.01, 0.05, 0.10))

summary(qual12)

#farmer fixed effects without farmer vars 
fe_f3 <- lm(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f3)

fe_f3_3 <- lm(seed_quality_general_rating~genderdummy + farmer_ID, data = baseline_f)
summary(fe_f3_3)

fe_f3_try <- plm(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f3_try)

#yield rating 
#cluster at dealer level
qual13 <- lm.cluster(seed_yield_rating~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual13)
clus4 <- lm.cluster(seed_yield_rating~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus4)

qual14 <- lm.cluster(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual15 <- lm.cluster(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
qual16 <- lm(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
               maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
               maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
               maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual13, qual14, qual15, c20), stars = c(0.01, 0.05, 0.15))
screenreg(list(qual15, qual14), stars = c(0.01, 0.05, 0.10))
summary(qual16)

#farmer fixed effects without farmer vars 
fe_f4 <- lm(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f4)

fe_f4_4 <- lm(seed_yield_rating~genderdummy  + farmer_ID, data = baseline_f)
summary(fe_f4_4)

fe_f4_try <- plm(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f4_try)


#drought resistency rating 
#cluster at dealer level
qual17 <- lm.cluster(seed_drought_rating~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual17)
clus5 <- lm.cluster(seed_drought_rating~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus5)

qual18 <- lm.cluster(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual19 <- lm.cluster(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
qual20 <- lm(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
               maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
               maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
               maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual17, qual18, qual19, c21), stars = c(0.01, 0.05, 0.15))
screenreg(list(qual19, qual18), stars = c(0.01, 0.05, 0.10))
summary(qual20)

#farmer fixed effects without farmer vars 
fe_f5 <- lm(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f5)

fe_f5_5 <- lm(seed_drought_rating~genderdummy + farmer_ID, data = baseline_f)
summary(fe_f5_5)

fe_f5_try <- plm(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f5_try)


#disease resistency rating 
#cluster at dealer level
qual21 <- lm.cluster(seed_disease_rating~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual21)
clus6<- lm.cluster(seed_disease_rating~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus6)

qual22<- lm.cluster(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual23 <- lm.cluster(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
qual24 <- lm(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
               maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
               maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
               maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual21, qual22, qual23, c22), stars = c(0.01, 0.05, 0.15))
screenreg(list(qual23, qual22), stars = c(0.01, 0.05, 0.10))
summary(qual24)

#farmer fixed effects without farmer vars 
fe_f6 <- lm(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f6)

fe_f6_6 <- lm(seed_disease_rating~genderdummy +farmer_ID, data = baseline_f)
summary(fe_f6_6)

fe_f6_try <- plm(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f6_try)


#early maturing rating 
#cluster at dealer level 
qual25 <- lm.cluster(seed_maturing_rating~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual25)
clus7<- lm.cluster(seed_maturing_rating~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus7)

qual26<- lm.cluster(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual27 <- lm.cluster(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
qual28 <- lm(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
               maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
               maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
               maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual25, qual26, qual27, c23), stars = c(0.01, 0.05, 0.15))
screenreg(list(qual27, qual26), stars = c(0.01, 0.05, 0.10))
summary(qual28)

#farmer fixed effects without farmer vars 
fe_f7 <- lm(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f7)

fe_f7_7 <- lm(seed_maturing_rating~genderdummy + farmer_ID, data = baseline_f)
summary(fe_f7_7)


fe_f7_try <- plm(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f7_try)


#germination rating 
#cluster at dealer level
qual29 <- lm.cluster(seed_germinate_rating~genderdummy , cluster="shop_ID",data = baseline_f)
summary(qual29)
clus8 <- lm.cluster(seed_germinate_rating~genderdummy , cluster="farmer_ID",data = baseline_f)
summary(clus8)

qual30<- lm.cluster(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual31 <- lm.cluster(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                       maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                       maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                       maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
qual32 <- lm(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
               maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
               maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
               maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data = baseline_f)
screenreg(list(qual29, qual30, qual31, c24), stars = c(0.01, 0.05, 0.15))
screenreg(list(qual31, qual30), stars = c(0.01, 0.05, 0.10))
summary(qual32)


#farmer fixed effects without farmer vars 
fe_f8 <- lm(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f8)

fe_f8_8 <- lm(seed_germinate_rating~genderdummy  + farmer_ID, data = baseline_f)
summary(fe_f8_8)

fe_f8_try <- plm(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                   maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                   maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                   maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70, data = baseline_f, 
                 index=c("farmer_ID", "shop_ID"), model="within")
summary(fe_f8_try)

screenreg(list(fe_f1,fe_f2, fe_f3, fe_f4, fe_f5, fe_f6, fe_f7, fe_f8), stars = c(0.01, 0.05, 0.10), file="farmer_fe.doc")

#farmer fixed effects without farmer vars 
texreg(list(fe_f1,fe_f2, fe_f3, fe_f4, fe_f5, fe_f6, fe_f7, fe_f8), stars = c(0.01, 0.05, 0.10))
#saving the data
write.csv(baseline_f,paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_f_merged.csv", sep="/"), row.names=FALSE)


############################################################################################################
count<-count(baseline_f$farmer_ID) #saving the number of occurrences 
names(count)<-c("farmer_ID", "freq")
c <- merge(baseline_f,count, by=c("farmer_ID"))

farm <- subset(c, freq>=2)  #farmers who have rated atleast 2 times 

fe1 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)
fe2 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)
fe3 <- lm(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)
fe4<- lm(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)
fe5 <- lm(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)
fe6 <- lm(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)
fe7 <- lm(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)
fe8 <- lm(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm)

summary(fe1)
summary(fe2)
summary(fe3)
summary(fe4)
summary(fe5)
summary(fe6)
summary(fe7)
summary(fe8)
#still farmer characteristics in the reg, all insignificant 
screenreg(list(fe1, fe2, fe3, fe4, fe5, fe6, fe7, fe8), stars = c(0.01, 0.05, 0.10))



farm4 <- subset(c, freq>=4)  #farmers who have rated atleast 4 times 

fe9 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)
fe10 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)
fe11 <- lm(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)
fe12<- lm(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
           maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
           maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
           maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)
fe13 <- lm(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)
fe14 <- lm(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)
fe15 <- lm(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)
fe16 <- lm(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm4)

summary(fe9)
summary(fe10)
summary(fe11)
summary(fe12)
summary(fe13)
summary(fe14)
summary(fe15)
summary(fe16)
#still farmer characteristics in the reg, all insignificant 


#########################################################################################################


farm_1 <- subset(c, freq==1) #farmers who have rated just once

fe_lim1 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
            maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
            maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
            maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm_1)

summary(fe_lim1)

fe_lim2 <- lm(score~genderdummy + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =farm_1)

summary(fe_lim2)


dup<-farm_1[!duplicated(farm_1$shop_ID), ] #20 farmers rating 20 unique dealers
count(dup$farmer_ID)
count(dup$shop_ID)

fe_lim3 <- lm(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8+ farmer_ID, data =dup)


fe_lim4 <- lm(score~genderdummy + married + educ_f + farmer_ID, data =dup)

summary(fe_lim3)
summary(fe_lim4)

write.csv(baseline_f,paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_f_merged.csv", sep="/"), row.names=FALSE)










############### SEED SYSTEMS DATA 

##################################################################################################################
####################### BETWEEN FARMER --- FOCUS ON DEALER'S GENDER ##############################################

rating_dyads <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))
baseline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)

#how many farmers have rated at least one agro-input dealers (i.e. how many unique farmerIDs are in the dyads dataset)?
print(length(table(rating_dyads$farmer_ID)))


#MERGED_DATASET
between_farmer <- merge(rating_dyads, baseline_dealer, by="shop_ID")

#create gender dummy
between_farmer$genderdummy <- ifelse(between_farmer$maize.owner.agree.gender == "Male", 1, 0)


###dealer characteristics for  controls

#education of dealers 
between_farmer$prim <- 0
between_farmer$prim[between_farmer$maize.owner.agree.educ=="c"|between_farmer$maize.owner.agree.educ=="d"|between_farmer$maize.owner.agree.educ=="e"|
                      between_farmer$maize.owner.agree.educ=="f"] <- 1
table(between_farmer$prim)

#distance of shop to nearest tarmac road
table(between_farmer$maize.owner.agree.q3)
between_farmer$maize.owner.agree.q3[between_farmer$maize.owner.agree.q3==999] <- NA

#distance of shop to nearest murram road 
table(between_farmer$maize.owner.agree.q4)

#selling only farm inputs 
table(between_farmer$maize.owner.agree.q5)
between_farmer$inputsale<- ifelse(between_farmer$maize.owner.agree.q5== 'Yes', 1, 0)  

#Q8. When was this agro-input shop established? (year)
between_farmer$years_shop <- 2020 - as.numeric(as.character(substr(between_farmer$maize.owner.agree.q8, start=1, stop=4)))

#seed stored in dedicated area?
between_farmer$maize.owner.agree.q69
between_farmer$dedarea<-as.character(between_farmer$maize.owner.agree.temp.q69)
between_farmer$dedicated_area<- ifelse(between_farmer$dedarea== 'Yes', 1, 0)  
table(between_farmer$dedicated_area)

#problem with rats or pests?
between_farmer$maize.owner.agree.q71
between_farmer$pest<-as.character(between_farmer$maize.owner.agree.temp.q71)
between_farmer$pest_prob<- ifelse(between_farmer$pest== 'Yes', 1, 0)  
table(between_farmer$pest_prob)

#roof leak proof?  
between_farmer$maize.owner.agree.q72
between_farmer$roof<-as.character(between_farmer$maize.owner.agree.temp.q72)
between_farmer$leakproof<- ifelse(between_farmer$roof== 'Yes', 1, 0)  
table(between_farmer$leakproof)

#roof insulated?
between_farmer$maize.owner.agree.q73
between_farmer$roof_insu<-as.character(between_farmer$maize.owner.agree.temp.q73)
between_farmer$insulated<- ifelse(between_farmer$roof_insu== 'Yes', 1, 0)  
table(between_farmer$insulated)

#walls insulated?
between_farmer$maize.owner.agree.q74
between_farmer$wall_insu<-as.character(between_farmer$maize.owner.agree.temp.q74)
between_farmer$wall_heatproof<- ifelse(between_farmer$wall_insu== 'Yes', 1, 0)  
table(between_farmer$wall_heatproof)

#area ventilated?
between_farmer$maize.owner.agree.q75
between_farmer$vent<-as.character(between_farmer$maize.owner.agree.temp.q75)
between_farmer$ventilation<- ifelse(between_farmer$vent== 'Yes', 1, 0)  
table(between_farmer$ventilation)

#plastered walls?
between_farmer$maize.owner.agree.q76
between_farmer$plas<-as.character(between_farmer$maize.owner.agree.temp.q76)
between_farmer$wall_plastered<- ifelse(between_farmer$plas== 'Yes', 1, 0)  
table(between_farmer$wall_plastered)

#Q77. Material of floor in areas where seed is stored?
between_farmer$goodfloor <- 0
between_farmer$goodfloor[between_farmer$maize.owner.agree.temp.q77=="Cement"|between_farmer$maize.owner.agree.temp.q77=="Tiles"] <-1
table(between_farmer$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
between_farmer$badlighting <- 0
between_farmer$badlighting[between_farmer$maize.owner.agree.temp.q78=="1"]<-1
table(between_farmer$badlighting)

#Q79. On what surface are seed stored?
between_farmer$badstored <- 0
between_farmer$badstored[between_farmer$maize.owner.agree.temp.q79=="1"|between_farmer$maize.owner.agree.temp.q79=="2"| between_farmer$maize.owner.agree.temp.q79=="96"]<-1
table(between_farmer$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
between_farmer$maize.owner.agree.q80
between_farmer$open<-as.character(between_farmer$maize.owner.agree.temp.q80)
between_farmer$open_storage<- ifelse(between_farmer$open== 'Yes', 1, 0)  
table(between_farmer$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings 
#or that the business is registered with some association)
between_farmer$maize.owner.agree.q81
between_farmer$cert<-as.character(between_farmer$maize.owner.agree.temp.q81)
between_farmer$cert_yes<- ifelse(between_farmer$cert== 'Yes', 1, 0)  
table(between_farmer$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
between_farmer$shop_rate<-as.numeric(as.character(between_farmer$maize.owner.agree.temp.q82))
table(between_farmer$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
between_farmer$maize.owner.agree.q96
between_farmer$complaint<- ifelse(between_farmer$maize.owner.agree.q96== 'Yes', 1, 0)  
table(between_farmer$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
between_farmer$maize.owner.agree.q70

between_farmer[between_farmer=="n/a"]<- NA

### seed related ratings:
between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","quality_rating") ] <- lapply(between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating","quality_rating") ], function(x) as.numeric(as.character(x)) )

between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x) replace(x, x == 98,NA) )

between_farmer$quality_rating[between_farmer$shop_ID == "AD_99"]

reviews_bf <- data.frame(cbind(tapply(as.numeric(between_farmer$quality_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_quality_general_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_yield_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_drought_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_disease_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_maturing_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_germinate_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$genderdummy), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.age), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$prim), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.q3), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.q4), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$inputsale), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$years_shop), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$dedicated_area), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$pest_prob), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$insulated), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$wall_heatproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$ventilation), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$wall_plastered), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$goodfloor), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$badlighting), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$badstored), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$open_storage), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$cert_yes), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$shop_rate), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$complaint), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$leakproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.q70), between_farmer$farmer_ID,mean,na.rm=TRUE)))


names(reviews_bf) <- c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination","gender_avg","dealer_age","dealer_educ","tarmac_dealer",
                       "murram_dealer","farm_inputs","years_shop","dedicatedarea","pestprob","roof_insu","wall_heatproof","ventilation","plasterwall","goodfloor",
                       "badlighting","badstored","open_storage","cert","shop_rate","complaint","leakproof","temp")

reviews_bf$farmer_ID <- rownames(reviews_bf)

reviews_bf$score <-  rowMeans(reviews_bf[c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)



#### regressions without controls - seed related ratings 

summary(lm(score~gender_avg , data = reviews_bf))
summary(lm(quality~gender_avg , data = reviews_bf))
summary(lm(general~gender_avg , data = reviews_bf))
summary(lm(yield~gender_avg , data = reviews_bf))
summary(lm(drought_resistent~gender_avg , data = reviews_bf))
summary(lm(disease_resistent~gender_avg , data = reviews_bf))
summary(lm(early_maturing~gender_avg , data = reviews_bf))
summary(lm(germination~gender_avg , data = reviews_bf))



##Farmers' dataset
farmers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_farmers.csv", sep = "/"))
#extracting variables from the baseline data
farmers_seedsub <- farmers_seed[ , c("Check2.check.maize.q15", "Check2.check.maize.q14",
                                     "Check2.check.maize.q16", "Check2.check.maize.q17",
                                     "Check2.check.maize.q8", "farmer_ID")]  

######## merge to get farmer characteristics 
bfm <- merge(reviews_bf, farmers_seedsub, by="farmer_ID")

bfm$educ_f <- 0
bfm$educ_f[bfm$Check2.check.maize.q17=="b" |bfm$Check2.check.maize.q17=="c" | bfm$Check2.check.maize.q17=="d" | bfm$Check2.check.maize.q17=="e" | 
                    bfm$Check2.check.maize.q17=="f" |bfm$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
bfm$married <- ifelse(bfm$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers


#### regressions with dealer's gender (averaged) and farmer characteristics  --- seed related ratings 

summary(lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(yield~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 , data = bfm))
summary(lm(drought_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(disease_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(early_maturing~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(germination~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))


#### regressions with dealer's gender (averaged) and farmer+dealer characteristics  --- seed related ratings 

summary(lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm))

summary(lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(yield~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 +dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(drought_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(disease_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(early_maturing~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(germination~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))




#NON-SEED RELATED RATINGS 
##########################################
between_farmer[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ] <- lapply(between_farmer[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ], function(x) as.numeric(as.character(x)) )

between_farmer_long <- data.frame(cbind(tapply(as.numeric(between_farmer$general_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$location_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$price_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$quality_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$stock_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$reputation_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$genderdummy), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(between_farmer$bought_at_dealer=="Yes" | between_farmer$knows_other_customer=="Yes", between_farmer$farmer_ID,sum),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.age), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$prim), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.q3), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.q4), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$inputsale), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$years_shop), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$dedicated_area), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$pest_prob), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$insulated), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$wall_heatproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$ventilation), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$wall_plastered), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$goodfloor), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$badlighting), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$badstored), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$open_storage), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$cert_yes), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$shop_rate), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$complaint), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$leakproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.q70), between_farmer$farmer_ID,mean,na.rm=TRUE)))
names(between_farmer_long) <- c("general_rating_nonseed","location","price","qual","stock","reputation","gender_avg","nr_reviews","dealer_age","dealer_educ","tarmac_dealer",
                                "murram_dealer","farm_inputs","years_shop","dedicatedarea","pestprob","roof_insu","wall_heatproof","ventilation","plasterwall","goodfloor",
                                "badlighting","badstored","open_storage","cert","shop_rate","complaint","leakproof","temp")

between_farmer_long$farmer_ID <- rownames(between_farmer_long)

between_farmer_long$overall_rating <-  rowMeans(between_farmer_long[c("general_rating_nonseed", "location","price","qual","stock","reputation")],na.rm=T)


#### regressions without controls - non-seed related ratings 

summary(lm(overall_rating~gender_avg , data = between_farmer_long))
summary(lm(general_rating_nonseed~gender_avg , data = between_farmer_long))
summary(lm(location~gender_avg , data = between_farmer_long))
summary(lm(price~gender_avg , data = between_farmer_long))
summary(lm(qual~gender_avg , data = between_farmer_long))
summary(lm(stock ~gender_avg , data = between_farmer_long))
summary(lm(reputation ~gender_avg , data = between_farmer_long))

######## merge to get farmer characteristics 
bfmm <- merge(between_farmer_long, farmers_seedsub, by="farmer_ID")

bfmm$educ_f <- 0
bfmm$educ_f[bfmm$Check2.check.maize.q17=="b" |bfmm$Check2.check.maize.q17=="c" | bfmm$Check2.check.maize.q17=="d" | bfmm$Check2.check.maize.q17=="e" | 
             bfmm$Check2.check.maize.q17=="f" |bfmm$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
bfmm$married <- ifelse(bfmm$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers


#### regressions with dealer's gender (averaged) and farmer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(price~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 , data = bfmm))
summary(lm(qual~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(stock~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(reputation~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))


#### regressions with dealer's gender (averaged) and farmer + dealer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(price~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof , data = bfmm))

summary(lm(qual~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(stock~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(reputation~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))



##################################################################################################################
####################### BETWEEN FARMER --- FOCUS ON FARMER'S GENDER ##############################################

rating_dyads <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))
##Farmers' dataset
farmers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_farmers.csv", sep = "/"))

#MERGED_DATASET
between_dealer <- merge(rating_dyads, farmers_seed, by="shop_ID")

#create gender dummy
between_dealer$farmergen <- ifelse(between_dealer$Check2.check.maize.q15 == "Male", 1, 0)

###farmer characteristics for controls 
between_dealer$educ_f <- 0
between_dealer$educ_f[between_dealer$Check2.check.maize.q17=="b" |between_dealer$Check2.check.maize.q17=="c" | between_dealer$Check2.check.maize.q17=="d" | between_dealer$Check2.check.maize.q17=="e" | 
                        between_dealer$Check2.check.maize.q17=="f" |between_dealer$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
between_dealer$married <- ifelse(between_dealer$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers







### seed related ratings:

between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x) as.numeric(as.character(x)) )

between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(between_farmer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x)replace(x, x == 98,NA) )

between_farmer$quality_rating[between_farmer$shop_ID == "AD_99"]

reviews_bf <- data.frame(cbind(tapply(as.numeric(between_farmer$quality_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_quality_general_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_yield_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_drought_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_disease_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_maturing_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$seed_germinate_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$genderdummy), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.age), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$prim), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.q3), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.q4), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$inputsale), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$years_shop), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$dedicated_area), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$pest_prob), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$insulated), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$wall_heatproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$ventilation), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$wall_plastered), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$goodfloor), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$badlighting), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$badstored), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$open_storage), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$cert_yes), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$shop_rate), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$complaint), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$leakproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$maize.owner.agree.q70), between_farmer$farmer_ID,mean,na.rm=TRUE)))
names(reviews_bf) <- c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination","gender_avg","dealer_age","dealer_educ","tarmac_dealer",
                       "murram_dealer","farm_inputs","years_shop","dedicatedarea","pestprob","roof_insu","wall_heatproof","ventilation","plasterwall","goodfloor",
                       "badlighting","badstored","open_storage","cert","shop_rate","complaint","leakproof","temp")

reviews_bf$farmer_ID <- rownames(reviews_bf)

reviews_bf$score <-  rowMeans(reviews_bf[c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)



###dealer characteristics for  controls

#education of dealers 
between_farmer$prim <- 0
between_farmer$prim[between_farmer$maize.owner.agree.educ=="c"|between_farmer$maize.owner.agree.educ=="d"|between_farmer$maize.owner.agree.educ=="e"|
                      between_farmer$maize.owner.agree.educ=="f"] <- 1
table(between_farmer$prim)

#distance of shop to nearest tarmac road
table(between_farmer$maize.owner.agree.q3)
between_farmer$maize.owner.agree.q3[between_farmer$maize.owner.agree.q3==999] <- NA

#distance of shop to nearest murram road 
table(between_farmer$maize.owner.agree.q4)

#selling only farm inputs 
table(between_farmer$maize.owner.agree.q5)
between_farmer$inputsale<- ifelse(between_farmer$maize.owner.agree.q5== 'Yes', 1, 0)  

#Q8. When was this agro-input shop established? (year)
between_farmer$years_shop <- 2020 - as.numeric(as.character(substr(between_farmer$maize.owner.agree.q8, start=1, stop=4)))

#seed stored in dedicated area?
between_farmer$maize.owner.agree.q69
between_farmer$dedarea<-as.character(between_farmer$maize.owner.agree.temp.q69)
between_farmer$dedicated_area<- ifelse(between_farmer$dedarea== 'Yes', 1, 0)  
table(between_farmer$dedicated_area)

#problem with rats or pests?
between_farmer$maize.owner.agree.q71
between_farmer$pest<-as.character(between_farmer$maize.owner.agree.temp.q71)
between_farmer$pest_prob<- ifelse(between_farmer$pest== 'Yes', 1, 0)  
table(between_farmer$pest_prob)

#roof leak proof?  ------ NOT USED AS WHILE AVERAGING HAVING SOME ISSUES 
between_farmer$maize.owner.agree.q72
between_farmer$roof<-as.character(between_farmer$maize.owner.agree.temp.q72)
between_farmer$leakproof<- ifelse(between_farmer$roof== 'Yes', 1, 0)  
table(between_farmer$leakproof)

#roof insulated?
between_farmer$maize.owner.agree.q73
between_farmer$roof_insu<-as.character(between_farmer$maize.owner.agree.temp.q73)
between_farmer$insulated<- ifelse(between_farmer$roof_insu== 'Yes', 1, 0)  
table(between_farmer$insulated)

#walls insulated?
between_farmer$maize.owner.agree.q74
between_farmer$wall_insu<-as.character(between_farmer$maize.owner.agree.temp.q74)
between_farmer$wall_heatproof<- ifelse(between_farmer$wall_insu== 'Yes', 1, 0)  
table(between_farmer$wall_heatproof)

#area ventilated?
between_farmer$maize.owner.agree.q75
between_farmer$vent<-as.character(between_farmer$maize.owner.agree.temp.q75)
between_farmer$ventilation<- ifelse(between_farmer$vent== 'Yes', 1, 0)  
table(between_farmer$ventilation)

#plastered walls?
between_farmer$maize.owner.agree.q76
between_farmer$plas<-as.character(between_farmer$maize.owner.agree.temp.q76)
between_farmer$wall_plastered<- ifelse(between_farmer$plas== 'Yes', 1, 0)  
table(between_farmer$wall_plastered)

#Q77. Material of floor in areas where seed is stored?
between_farmer$goodfloor <- 0
between_farmer$goodfloor[between_farmer$maize.owner.agree.temp.q77=="Cement"|between_farmer$maize.owner.agree.temp.q77=="Tiles"] <-1
table(between_farmer$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
between_farmer$badlighting <- 0
between_farmer$badlighting[between_farmer$maize.owner.agree.temp.q78=="1"]<-1
table(between_farmer$badlighting)

#Q79. On what surface are seed stored?
between_farmer$badstored <- 0
between_farmer$badstored[between_farmer$maize.owner.agree.temp.q79=="1"|between_farmer$maize.owner.agree.temp.q79=="2"| between_farmer$maize.owner.agree.temp.q79=="96"]<-1
table(between_farmer$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
between_farmer$maize.owner.agree.q80
between_farmer$open<-as.character(between_farmer$maize.owner.agree.temp.q80)
between_farmer$open_storage<- ifelse(between_farmer$open== 'Yes', 1, 0)  
table(between_farmer$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings 
#or that the business is registered with some association)
between_farmer$maize.owner.agree.q81
between_farmer$cert<-as.character(between_farmer$maize.owner.agree.temp.q81)
between_farmer$cert_yes<- ifelse(between_farmer$cert== 'Yes', 1, 0)  
table(between_farmer$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
between_farmer$shop_rate<-as.numeric(as.character(between_farmer$maize.owner.agree.temp.q82))
table(between_farmer$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
between_farmer$maize.owner.agree.q96
between_farmer$complaint<- ifelse(between_farmer$maize.owner.agree.q96== 'Yes', 1, 0)  
table(between_farmer$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
between_farmer$maize.owner.agree.q70


#### regressions without controls - seed related ratings 

summary(lm(score~gender_avg , data = reviews_bf))
summary(lm(quality~gender_avg , data = reviews_bf))
summary(lm(general~gender_avg , data = reviews_bf))
summary(lm(yield~gender_avg , data = reviews_bf))
summary(lm(drought_resistent~gender_avg , data = reviews_bf))
summary(lm(disease_resistent~gender_avg , data = reviews_bf))
summary(lm(early_maturing~gender_avg , data = reviews_bf))
summary(lm(germination~gender_avg , data = reviews_bf))


##Farmers' dataset
farmers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_farmers.csv", sep = "/"))
#extracting variables from the baseline data
farmers_seedsub <- farmers_seed[ , c("Check2.check.maize.q15", "Check2.check.maize.q14",
                                     "Check2.check.maize.q16", "Check2.check.maize.q17",
                                     "Check2.check.maize.q8", "farmer_ID")]  

######## merge to get farmer characteristics 
bfm <- merge(reviews_bf, farmers_seedsub, by="farmer_ID")

bfm$educ_f <- 0
bfm$educ_f[bfm$Check2.check.maize.q17=="b" |bfm$Check2.check.maize.q17=="c" | bfm$Check2.check.maize.q17=="d" | bfm$Check2.check.maize.q17=="e" | 
             bfm$Check2.check.maize.q17=="f" |bfm$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
bfm$married <- ifelse(bfm$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers


#### regressions with dealer's gender (averaged) and farmer characteristics  --- seed related ratings 

summary(lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(yield~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 , data = bfm))
summary(lm(drought_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(disease_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(early_maturing~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(germination~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))


#### regressions with dealer's gender (averaged) and farmer+dealer characteristics  --- seed related ratings 

summary(lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm))

summary(lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(yield~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 +dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(drought_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(disease_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(early_maturing~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(germination~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))


#NON-SEED RELATED RATINGS 
##########################################
between_farmer[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ] <- lapply(between_farmer[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating") ], function(x) as.numeric(as.character(x)) )

between_farmer_long <- data.frame(cbind(tapply(as.numeric(between_farmer$general_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$location_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$price_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$quality_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$stock_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$reputation_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$genderdummy), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(between_farmer$bought_at_dealer=="Yes" | between_farmer$knows_other_customer=="Yes", between_farmer$farmer_ID,sum),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.age), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$prim), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.q3), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.q4), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$inputsale), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$years_shop), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$dedicated_area), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$pest_prob), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$insulated), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$wall_heatproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$ventilation), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$wall_plastered), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$goodfloor), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$badlighting), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$badstored), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$open_storage), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$cert_yes), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$shop_rate), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$complaint), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$leakproof), between_farmer$farmer_ID,mean,na.rm=TRUE),
                                        tapply(as.numeric(between_farmer$maize.owner.agree.q70), between_farmer$farmer_ID,mean,na.rm=TRUE)))
names(between_farmer_long) <- c("general_rating_nonseed","location","price","qual","stock","reputation","gender_avg","nr_reviews","dealer_age","dealer_educ","tarmac_dealer",
                                "murram_dealer","farm_inputs","years_shop","dedicatedarea","pestprob","roof_insu","wall_heatproof","ventilation","plasterwall","goodfloor",
                                "badlighting","badstored","open_storage","cert","shop_rate","complaint","leakproof","temp")

between_farmer_long$farmer_ID <- rownames(between_farmer_long)

between_farmer_long$overall_rating <-  rowMeans(between_farmer_long[c("general_rating_nonseed", "location","price","qual","stock","reputation")],na.rm=T)


#### regressions without controls - non-seed related ratings 

summary(lm(overall_rating~gender_avg , data = between_farmer_long))
summary(lm(general_rating_nonseed~gender_avg , data = between_farmer_long))
summary(lm(location~gender_avg , data = between_farmer_long))
summary(lm(price~gender_avg , data = between_farmer_long))
summary(lm(qual~gender_avg , data = between_farmer_long))
summary(lm(stock ~gender_avg , data = between_farmer_long))
summary(lm(reputation ~gender_avg , data = between_farmer_long))

######## merge to get farmer characteristics 
bfmm <- merge(between_farmer_long, farmers_seedsub, by="farmer_ID")

bfmm$educ_f <- 0
bfmm$educ_f[bfmm$Check2.check.maize.q17=="b" |bfmm$Check2.check.maize.q17=="c" | bfmm$Check2.check.maize.q17=="d" | bfmm$Check2.check.maize.q17=="e" | 
              bfmm$Check2.check.maize.q17=="f" |bfmm$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
bfmm$married <- ifelse(bfmm$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers


#### regressions with dealer's gender (averaged) and farmer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(price~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 , data = bfmm))
summary(lm(qual~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(stock~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))
summary(lm(reputation~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfmm))


#### regressions with dealer's gender (averaged) and farmer + dealer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(price~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof , data = bfmm))

summary(lm(qual~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(stock~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))

summary(lm(reputation~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfmm))









