path <- getwd()

options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

library(miceadds)
library(texreg)
library(plyr)

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

################# LOCATION RATING ###########################

#all variables 
ss2<- lm.cluster(data = farmers_seed_stack, formula = rating_location ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s2 <- sqrt(diag(vcov(ss2)))
ss_res2<-ss2$lm_res


################# QUALITY RATING ###########################

#all variables 
ss3<- lm.cluster(data = farmers_seed_stack, formula = rating_quality ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s3 <- sqrt(diag(vcov(ss3)))
ss_res3<-ss3$lm_res

################# PRICE RATING ###########################

#all variables 
ss4<- lm.cluster(data = farmers_seed_stack, formula = rating_price ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s4 <- sqrt(diag(vcov(ss4)))
ss_res4<-ss4$lm_res

################# STOCK RATING ###########################
#all variables
ss5<- lm.cluster(data = farmers_seed_stack, formula = rating_stock ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s5 <- sqrt(diag(vcov(ss5)))
ss_res5<-ss5$lm_res

################# REPUTATION RATING ###########################
#all variables
ss6<- lm.cluster(data = farmers_seed_stack, formula = rating_reputation ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
s6 <- sqrt(diag(vcov(ss6)))
ss_res6<-ss6$lm_res


#######################################################################
#######################################################################

################# DEALERS:SEED SYSTEMS ######################
dealers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep = "/"))

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

############################################################################
### CLUSTERED REGRESSIONS - LOOKING AT BOTH FARMERS' AND DEALERS' GENDER ###
############################################################################
####### DEPENDENT VARIABLE -- RATINGS FROM FARMERS #########################
############################################################################

################# OVERALL RATING ###########################

#all variables 
fd1<- lm.cluster(data = m, formula = rating_overall ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                         + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd1 <- sqrt(diag(vcov(fd1)))
res_fd1<-fd1$lm_res

##Interaction between sex of farmer and dealer
fd2<- lm.cluster(data = m, formula = rating_overall ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd2 <- sqrt(diag(vcov(fd2)))
res_fd2<-fd2$lm_res

################# LOCATION RATING ###########################

#all variables 
fd3<- lm.cluster(data = m, formula = rating_location ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd3 <- sqrt(diag(vcov(fd3)))
res_fd3<-fd3$lm_res

##Interaction between sex of farmer and dealer
fd4<- lm.cluster(data = m, formula = rating_location ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd4 <- sqrt(diag(vcov(fd4)))
res_fd4<-fd4$lm_res

################# QUALITY RATING ###########################

#all variables 
fd5<- lm.cluster(data = m, formula = rating_quality ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd5 <- sqrt(diag(vcov(fd5)))
res_fd5<-fd5$lm_res

##Interaction between sex of farmer and dealer
fd6<- lm.cluster(data = m, formula = rating_quality ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd6 <- sqrt(diag(vcov(fd6)))
res_fd6<-fd6$lm_res

################# PRICE RATING ###########################

#all variables 
fd7<- lm.cluster(data = m, formula = rating_price ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd7 <- sqrt(diag(vcov(fd7)))
res_fd7<-fd7$lm_res

##Interaction between sex of farmer and dealer
fd8<- lm.cluster(data = m, formula = rating_price ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd8 <- sqrt(diag(vcov(fd8)))
res_fd8<-fd8$lm_res

################# STOCK RATING ###########################

#all variables 
fd9<- lm.cluster(data = m, formula = rating_stock ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd9 <- sqrt(diag(vcov(fd9)))
res_fd9<-fd9$lm_res

##Interaction between sex of farmer and dealer
fd10<- lm.cluster(data = m, formula = rating_stock ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd10 <- sqrt(diag(vcov(fd10)))
res_fd10<-fd10$lm_res

################# REPUTATION RATING ###########################

#all variables 
fd11<- lm.cluster(data = m, formula = rating_reputation ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fd11 <- sqrt(diag(vcov(fd11)))
res_fd11<-fd11$lm_res

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

##########################################################################################


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
baseline_dealer <- read.csv(paste(path,"/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)
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

#Regressions 

#Overall score
qual1 <- lm.cluster(score~genderdummy , cluster="shop_ID",data = baseline_f)

qual2 <- lm.cluster(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70,cluster="shop_ID", data = baseline_f)
qual3 <- lm.cluster(score~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
                      maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
                      maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
                      maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + married + educ_f + Check2.check.maize.q14 + Check2.check.maize.q8 ,cluster="shop_ID", data = baseline_f)
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
summary(fe_f1)

#QUALITY RATING 
qual5 <- lm.cluster(quality_rating~genderdummy , cluster="shop_ID",data = baseline_f)

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
summary(qual8)

#farmer fixed effects without farmer vars 
fe_f2 <- lm(quality_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f2)


#GENERAL RATING 
qual9 <- lm.cluster(seed_quality_general_rating~genderdummy , cluster="shop_ID",data = baseline_f)

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
screenreg(list(qual9, qual10, qual11, c19), stars = c(0.01, 0.05, 0.15))
summary(qual12)

#farmer fixed effects without farmer vars 
fe_f3 <- lm(seed_quality_general_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f3)

#yield rating 
qual13 <- lm.cluster(seed_yield_rating~genderdummy , cluster="shop_ID",data = baseline_f)

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
summary(qual16)

#farmer fixed effects without farmer vars 
fe_f4 <- lm(seed_yield_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f4)


#drought resistency rating 
qual17 <- lm.cluster(seed_drought_rating~genderdummy , cluster="shop_ID",data = baseline_f)

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
summary(qual20)

#farmer fixed effects without farmer vars 
fe_f5 <- lm(seed_drought_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f5)


#disease resistency rating 
qual21 <- lm.cluster(seed_disease_rating~genderdummy , cluster="shop_ID",data = baseline_f)

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
summary(qual24)

#farmer fixed effects without farmer vars 
fe_f6 <- lm(seed_disease_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f6)

#early maturing rating 
qual25 <- lm.cluster(seed_maturing_rating~genderdummy , cluster="shop_ID",data = baseline_f)

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
summary(qual28)

#farmer fixed effects without farmer vars 
fe_f7 <- lm(seed_maturing_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f7)


#germination rating 
qual29 <- lm.cluster(seed_germinate_rating~genderdummy , cluster="shop_ID",data = baseline_f)

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
summary(qual32)


#farmer fixed effects without farmer vars 
fe_f8 <- lm(seed_germinate_rating~genderdummy + maize.owner.agree.age + prim + maize.owner.agree.q3 + maize.owner.agree.q4 + maize.owner.agree.q5 + years_shop + 
              maize.owner.agree.temp.q69 + maize.owner.agree.temp.q71 + maize.owner.agree.temp.q72 + maize.owner.agree.temp.q73 + maize.owner.agree.temp.q74 + 
              maize.owner.agree.temp.q75 + maize.owner.agree.temp.q76 + goodfloor + badlighting + badstored + maize.owner.agree.temp.q80 + maize.owner.agree.temp.q81 + 
              maize.owner.agree.temp.q82 + maize.owner.agree.q96 + maize.owner.agree.q70 + farmer_ID, data = baseline_f)
summary(fe_f8)

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

