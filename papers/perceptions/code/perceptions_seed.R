path <- getwd()

options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

library(miceadds)
library(texreg)

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
f_seed1<- lm.cluster(data = f_seed, formula = rating_overall ~  gender + age + interaction_yes + educ + tarmac
                 + married, cluster="id.ratee") 
fss1 <- sqrt(diag(vcov(f_seed1)))
res_fss1<-f_seed1$lm_res

################# LOCATION RATING ###########################

#all variables 
f_seed2<- lm.cluster(data = f_seed, formula = rating_location ~  gender + age + interaction_yes + educ + tarmac
                     + married, cluster="id.ratee") 
fss2 <- sqrt(diag(vcov(f_seed2)))
res_fss2<-f_seed2$lm_res


################# QUALITY RATING ###########################

#all variables 
f_seed3<- lm.cluster(data = f_seed, formula = rating_quality ~  gender + age + interaction_yes + educ + tarmac
                     + married, cluster="id.ratee") 
fss3 <- sqrt(diag(vcov(f_seed3)))
res_fss3<-f_seed3$lm_res

################# PRICE RATING ###########################

#all variables 
f_seed4<- lm.cluster(data = f_seed, formula = rating_price ~  gender + age + interaction_yes + educ + tarmac
                     + married, cluster="id.ratee") 
fss4 <- sqrt(diag(vcov(f_seed4)))
res_fss4<-f_seed4$lm_res

################# STOCK RATING ###########################
#all variables
f_seed5<- lm.cluster(data = f_seed, formula = rating_stock ~  gender + age + interaction_yes + educ + tarmac
                     + married, cluster="id.ratee") 
fss5 <- sqrt(diag(vcov(f_seed5)))
res_fss5<-f_seed5$lm_res

################# REPUTATION RATING ###########################
#all variables
f_seed6<- lm.cluster(data = f_seed, formula = rating_reputation ~  gender + age + interaction_yes + educ + tarmac
                     + married, cluster="id.ratee") 
fss6 <- sqrt(diag(vcov(f_seed6)))
res_fss6<-f_seed6$lm_res


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
fds1<- lm.cluster(data = m_all, formula = rating_overall ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds1 <- sqrt(diag(vcov(fds1)))
res_fds1<-fds1$lm_res

##Interaction between sex of farmer and dealer
fds2<- lm.cluster(data = m_all, formula = rating_overall ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds2 <- sqrt(diag(vcov(fds2)))
res_fds2<-fds2$lm_res

################# LOCATION RATING ###########################

#all variables 
fds3<- lm.cluster(data = m_all, formula = rating_location ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds3 <- sqrt(diag(vcov(fds3)))
res_fds3<-fds3$lm_res

##Interaction between sex of farmer and dealer
fds4<- lm.cluster(data = m_all, formula = rating_location ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds4 <- sqrt(diag(vcov(fds4)))
res_fds4<-fds4$lm_res

################# QUALITY RATING ###########################

#all variables 
fds5<- lm.cluster(data = m_all, formula = rating_quality ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds5 <- sqrt(diag(vcov(fds5)))
res_fds5<-fds5$lm_res

##Interaction between sex of farmer and dealer
fds6<- lm.cluster(data = m_all, formula = rating_quality ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds6 <- sqrt(diag(vcov(fds6)))
res_fds6<-fds6$lm_res

################# PRICE RATING ###########################

#all variables 
fds7<- lm.cluster(data = m_all, formula = rating_price ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds7 <- sqrt(diag(vcov(fds7)))
res_fds7<-fds7$lm_res

##Interaction between sex of farmer and dealer
fds8<- lm.cluster(data = m_all, formula = rating_price ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds8 <- sqrt(diag(vcov(fds8)))
res_fds8<-fds8$lm_res

################# STOCK RATING ###########################

#all variables 
fds9<- lm.cluster(data = m_all, formula = rating_stock ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                 + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds9 <- sqrt(diag(vcov(fds9)))
res_fds9<-fds9$lm_res

##Interaction between sex of farmer and dealer
fds10<- lm.cluster(data = m_all, formula = rating_stock ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                  + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds10 <- sqrt(diag(vcov(fds10)))
res_fds10<-fds10$lm_res

################# REPUTATION RATING ###########################

#all variables 
fds11<- lm.cluster(data = m_all, formula = rating_reputation ~  gender + dealer_fem + age + interaction_yes + educ + tarmac
                  + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds11 <- sqrt(diag(vcov(fds11)))
res_fds11<-fds11$lm_res

##Interaction between sex of farmer and dealer
fds12<- lm.cluster(data = m_all, formula = rating_reputation ~  gender*dealer_fem + age + interaction_yes + educ + tarmac
                  + married + age_dealer + education_dealer, cluster="id.ratee") 
se_fds12 <- sqrt(diag(vcov(fds12)))
res_fds12<-fds12$lm_res

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

