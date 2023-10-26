### run in:  ../PIMMVC/perceptions
rm(list=ls())
path <- getwd()

library(miceadds)
library(texreg)
library(plyr)
library(plm)
library(lmtest)
library(lme4)
library(Rcpp)
#library(sjPlot)
library(tidyverse)
library(clubSandwich)
library(dplyr)
options(scipen=999)
path_2 <- strsplit(path, "/perceptions")[[1]]




############################################ SEED SYSTEMS DATA ##################################################

##################################################################################################################
####################### BETWEEN FARMER --- FOCUS ON DEALER'S GENDER ##############################################

###########  MODEL 3 ###############
##################################################################################################################

rating_dyads <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"), stringsAsFactors = FALSE)[c("shop_ID", "farmer_ID",          "general_rating","location_rating","price_rating","quality_rating", "stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")]
rating_dyads_midline <- read.csv(paste(path_2,"perceptions/data_seed_systems_midline/data/farmer/midline_rating_dyads.csv", sep = "/"), stringsAsFactors = FALSE)[c("shop_ID", "farmer_ID", "general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")]

ratFE<- rating_dyads

### check if gender of agro input dealer in baseline corresponds to gender in midline:
#baseline data 
baseline_dealer <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)
baseline_dealer[baseline_dealer=="999"]<-NA

#midline data 
midline_dealer <- read.csv(paste(path_2,"perceptions/data_seed_systems_midline/data/input_dealer/midline_dealer.csv", sep="/"), stringsAsFactors = FALSE)

###############################################################################################
###Anderson, 2008: https://are.berkeley.edu/~mlanderson/pdf/Anderson%202008a.pdf p. 1485

###1. For all outcomes, switch signs where necessary so that the positive direction always indicates a "better" outcome.

#### Index of capital-intensive seed handling and storage practices observed by enumerator ####

#Is the roof leak-proof? yes=good
baseline_dealer$base_roofleak <- ifelse(baseline_dealer$maize.owner.agree.temp.q72=="Yes",1,0) 
midline_dealer$mid_roofleak <- ifelse(midline_dealer$owner.agree.temp.q72=="Yes",1,0) 

#Is the roof insulated to keep heat out? yes=good
baseline_dealer$base_roofinsul <- ifelse(baseline_dealer$maize.owner.agree.temp.q73=="Yes",1,0) 
midline_dealer$mid_roofinsul <- ifelse(midline_dealer$owner.agree.temp.q73=="Yes",1,0)

#Are the walls insulated to keep the heat out? yes=good
baseline_dealer$base_wallinsul <- ifelse(baseline_dealer$maize.owner.agree.temp.q74=="Yes",1,0) 
midline_dealer$mid_wallinsul <- ifelse(midline_dealer$owner.agree.temp.q74=="Yes",1,0)

#Is the area ventilated? yes=good
baseline_dealer$base_ventilation <- ifelse(baseline_dealer$maize.owner.agree.temp.q75=="Yes",1,0) 
midline_dealer$mid_ventilation <- ifelse(midline_dealer$owner.agree.temp.q75=="Yes",1,0)

#Do you see any official certificates displayed in the shop (e.g. inspection, trainings, registration with association)? yes=good
baseline_dealer$base_cert <- ifelse(baseline_dealer$maize.owner.agree.temp.q81=="Yes",1,0) 
midline_dealer$mid_cert <- ifelse(midline_dealer$owner.agree.temp.q81=="Yes",1,0)

### Index of all handling and storage practices observed by the enumerator ###
#What do you do with seed that have exceeded shelf live (expired)? yes=good
#(a This has never happened/b Return to supplier/c Sell at discount/d Given away/e Thrown away/f sell at normal price/g mix with other seed/96 Other)

baseline_dealer$goodpractice_expired2 <- 1
baseline_dealer$goodpractice_expired2[baseline_dealer$maize.owner.agree.q83.96=="True"] <- NA
baseline_dealer$goodpractice_expired2[baseline_dealer$maize.owner.agree.q83.c=="True"] <- 0
baseline_dealer$goodpractice_expired2[baseline_dealer$maize.owner.agree.q83.d=="True"] <- 0
baseline_dealer$goodpractice_expired2[baseline_dealer$maize.owner.agree.q83.f=="True"] <- 0
baseline_dealer$goodpractice_expired2[baseline_dealer$maize.owner.agree.q83.g=="True"] <- 0


midline_dealer$mid_goodpractice_expired2 <- 1 #x
midline_dealer$mid_goodpractice_expired2[midline_dealer$owner.agree.q83.96=="True"] <- NA
midline_dealer$mid_goodpractice_expired2[midline_dealer$owner.agree.q83.c=="True"] <- 0 
midline_dealer$mid_goodpractice_expired2[midline_dealer$owner.agree.q83.d=="True"] <- 0 
midline_dealer$mid_goodpractice_expired2[midline_dealer$owner.agree.q83.f=="True"] <- 0 
midline_dealer$mid_goodpractice_expired2[midline_dealer$owner.agree.q83.g=="True"] <- 0 


#Are seed stored in a dedicated area, away from other merchandize? yes=good
baseline_dealer$maize.owner.dedarea <- ifelse(baseline_dealer$maize.owner.agree.temp.q69=="Yes",1,0) 
midline_dealer$owner.dedarea <- ifelse(midline_dealer$owner.agree.temp.q69=="Yes",1,0) 


#Do you have a problem with rats or pests (insects, rats)? yes=BAD
baseline_dealer$maize.owner.ratpest<-ifelse(baseline_dealer$maize.owner.agree.temp.q71=="Yes",0,1)
midline_dealer$owner.ratpest<-ifelse(midline_dealer$owner.agree.temp.q71=="Yes",0,1) 


#Lighting conditions in area where seed is stored? yes=good
#Wilberforce: ambient lighting condition is ideal for seed storage
baseline_dealer$maize.owner.lighting[baseline_dealer$maize.owner.agree.temp.q78==1] <- 0
baseline_dealer$maize.owner.lighting[baseline_dealer$maize.owner.agree.temp.q78==2] <- 1
baseline_dealer$maize.owner.lighting[baseline_dealer$maize.owner.agree.temp.q78==3] <- 0

midline_dealer$owner.lighting[midline_dealer$owner.agree.temp.q78==1] <- 0
midline_dealer$owner.lighting[midline_dealer$owner.agree.temp.q78==2] <- 1
midline_dealer$owner.lighting[midline_dealer$owner.agree.temp.q78==3] <- 0


#On what surface are seed stored? yes=good
baseline_dealer$maize.owner.surface[baseline_dealer$maize.owner.agree.temp.q79==1] <- 0 
baseline_dealer$maize.owner.surface[baseline_dealer$maize.owner.agree.temp.q79==2] <- 0 
baseline_dealer$maize.owner.surface[baseline_dealer$maize.owner.agree.temp.q79==3] <- 0 
baseline_dealer$maize.owner.surface[baseline_dealer$maize.owner.agree.temp.q79==4] <- 1 
baseline_dealer$maize.owner.surface[baseline_dealer$maize.owner.agree.temp.q79==5] <- 1
baseline_dealer$maize.owner.surface[baseline_dealer$maize.owner.agree.temp.q79==96] <- NA

midline_dealer$owner.surface[midline_dealer$owner.agree.temp.q79==1] <- 0 
midline_dealer$owner.surface[midline_dealer$owner.agree.temp.q79==2] <- 0 
midline_dealer$owner.surface[midline_dealer$owner.agree.temp.q79==3] <- 0 
midline_dealer$owner.surface[midline_dealer$owner.agree.temp.q79==4] <- 1 
midline_dealer$owner.surface[midline_dealer$owner.agree.temp.q79==5] <- 1
midline_dealer$owner.surface[midline_dealer$owner.agree.temp.q79==96] <- NA

#Do you see maize seed that is stored in open bags or open containers? yes=BAD
baseline_dealer$maize.owner.openstorage<-ifelse(baseline_dealer$maize.owner.agree.temp.q80=="Yes",0,1)
midline_dealer$owner.openstorage<-ifelse(midline_dealer$owner.agree.temp.q80=="Yes",0,1)


#On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 
baseline_dealer$maize.owner.rate <- baseline_dealer$maize.owner.agree.temp.q82 
midline_dealer$owner.rate <- midline_dealer$owner.agree.temp.q82 


#### Index of efforts of dealer and services offered by dealer ####

#When farmers buy seed, do you explain how the seed should be used (seed spacing, seed rate, complementary inputs) yes=good
baseline_dealer$base_alwaysexplains[baseline_dealer$maize.owner.agree.q85=="a"] <- 0 
baseline_dealer$base_alwaysexplains[baseline_dealer$maize.owner.agree.q85=="b"] <- 0 
baseline_dealer$base_alwaysexplains[baseline_dealer$maize.owner.agree.q85=="c"] <- 1

midline_dealer$mid_alwaysexplains[midline_dealer$owner.agree.q85=="a"] <- 0 
midline_dealer$mid_alwaysexplains[midline_dealer$owner.agree.q85=="b"] <- 0 
midline_dealer$mid_alwaysexplains[midline_dealer$owner.agree.q85=="c"] <- 1


#When farmers buy seed, do you usually recommend complementary inputs (fertilizer, chemical,.) yes=good
baseline_dealer$base_alwaysrecom[baseline_dealer$maize.owner.agree.q86=="a"] <- 0 
baseline_dealer$base_alwaysrecom[baseline_dealer$maize.owner.agree.q86=="b"] <- 0 
baseline_dealer$base_alwaysrecom[baseline_dealer$maize.owner.agree.q86=="c"] <- 1

midline_dealer$mid_alwaysrecom[midline_dealer$owner.agree.q86=="a"] <- 0 
midline_dealer$mid_alwaysrecom[midline_dealer$owner.agree.q86=="b"] <- 0 
midline_dealer$mid_alwaysrecom[midline_dealer$owner.agree.q86=="c"] <- 1


#Do you offer extension/training to your clients on how to use improved seed varieties? yes=good
baseline_dealer$base_extension[baseline_dealer$maize.owner.agree.q87=="1"] <- 0 
baseline_dealer$base_extension[baseline_dealer$maize.owner.agree.q87=="2"] <- 1
baseline_dealer$base_extension[baseline_dealer$maize.owner.agree.q87=="3"] <- 1

midline_dealer$mid_extension[midline_dealer$owner.agree.q87=="1"] <- 0 
midline_dealer$mid_extension[midline_dealer$owner.agree.q87=="2"] <- 1 
midline_dealer$mid_extension[midline_dealer$owner.agree.q87=="3"] <- 1


#Did you offer discounts to clients that buy large quantities of maize seed during the second season of 2020? yes=good
baseline_dealer$base_largequan<-ifelse(baseline_dealer$maize.owner.agree.q88=="Yes",1,0)
midline_dealer$mid_largequan<-ifelse(midline_dealer$owner.agree.q88=="Yes",1,0)


#What is that smallest package of improved seed (OPV/hybird) that you stocked during this season (without repackaging) yes=good
baseline_dealer$base_smallpack[baseline_dealer$maize.owner.agree.q89=="1"] <- 1 
baseline_dealer$base_smallpack[baseline_dealer$maize.owner.agree.q89=="2"] <- 0
baseline_dealer$base_smallpack[baseline_dealer$maize.owner.agree.q89=="3"] <- 0
baseline_dealer$base_smallpack[baseline_dealer$maize.owner.agree.q89=="4"] <- 0
baseline_dealer$base_smallpack[baseline_dealer$maize.owner.agree.q89=="other"] <- NA

midline_dealer$mid_smallpack[midline_dealer$owner.agree.q89=="1"] <- 1 
midline_dealer$mid_smallpack[midline_dealer$owner.agree.q89=="2"] <- 0
midline_dealer$mid_smallpack[midline_dealer$owner.agree.q89=="3"] <- 0
midline_dealer$mid_smallpack[midline_dealer$owner.agree.q89=="4"] <- 0
midline_dealer$mid_smallpack[midline_dealer$owner.agree.q89=="other"] <- NA


#Do you provide seed on credit (pay after harvest)? yes=good
baseline_dealer$base_seedcredit[baseline_dealer$maize.owner.agree.q93=="1"] <- 0 
baseline_dealer$base_seedcredit[baseline_dealer$maize.owner.agree.q93=="2"] <- 1
baseline_dealer$base_seedcredit[baseline_dealer$maize.owner.agree.q93=="3"] <- 1

midline_dealer$mid_seedcredit[midline_dealer$owner.agree.q93=="1"] <- 0 
midline_dealer$mid_seedcredit[midline_dealer$owner.agree.q93=="2"] <- 1
midline_dealer$mid_seedcredit[midline_dealer$owner.agree.q93=="3"] <- 1


#Since last season, did you receive any complaint from a customer that seed you sold was not good? yes=BAD
baseline_dealer$maize.owner.complaint<-ifelse(baseline_dealer$maize.owner.agree.q96=="Yes",0,1)
midline_dealer$owner.complaint<-ifelse(midline_dealer$owner.agree.q96=="Yes",0,1)


#What payment modalities do you accept?
baseline_dealer$base_mobmoney<-ifelse(baseline_dealer$maize.owner.agree.q97.b=="True",1,0) 
midline_dealer$mid_mobmoney<-ifelse(midline_dealer$owner.agree.q97.b=="True",1,0) 

#random seed bag shows lot number
baseline_dealer$maize.owner.lot<-ifelse(baseline_dealer$lot=="Yes",1,0) 
midline_dealer$owner.lot<-ifelse(midline_dealer$lot=="Yes",1,0) 

############ shelflife ##############
# Days since packaging date/expiry date minus 6 months

###BASELINE 
baseline_dealer$base_date <- as.Date(baseline_dealer$date)  #date is the date of the interview
baseline_dealer$base_exp <- as.Date(baseline_dealer$exp)   #exp is the expiry date and in the future 
baseline_dealer$base_date_pack <- as.Date(baseline_dealer$date_pack) #date_pack is the date of packaging and in the past

#now we combine both because most seed bags only have one number (pack date or exp date)
baseline_dealer$base_date_pack_incltransformedexp <- baseline_dealer$base_date_pack   #this is just the pack date
baseline_dealer$base_transformedexp <- baseline_dealer$base_exp - 183 #6x366/12 
#this is the exp date minus half a year (as the exp date is approximately 6 months after packaging)

baseline_dealer$base_date_pack_incltransformedexp[is.na(baseline_dealer$base_date_pack)]<-baseline_dealer$base_transformedexp[is.na(baseline_dealer$base_date_pack)] 
#so we take this number as pack date if pack date is not on the bag

baseline_dealer$base_shelflife_Caro <- baseline_dealer$base_date - as.Date(baseline_dealer$base_date_pack_incltransformedexp)
#this is the number of days between (fake) pack date and interview, a positive number, 180 days (~6 months) in my example
baseline_dealer$base_shelflife_Caro[baseline_dealer$base_shelflife_Caro < 0] <- NA 
#a neg number means that the seed was packed AFTER in enumerator bought the bag, which is impossible and becomes NA
baseline_dealer$maize.owner.shelflife_Caro <- as.numeric(as.character(baseline_dealer$base_shelflife_Caro)) ##shelflife variable 

###MIDLINE 
midline_dealer$mid_date <- as.Date(midline_dealer$date)  #date is the date of the interview
midline_dealer$mid_exp <- as.Date(midline_dealer$exp)   #exp is the expiry date and in the future 
midline_dealer$mid_date_pack <- as.Date(midline_dealer$date_pack) #date_pack is the date of packaging and in the past

#now we combine both because most seed bags only have one number (pack date or exp date)
midline_dealer$mid_date_pack_incltransformedexp <- midline_dealer$mid_date_pack   #this is just the pack date
midline_dealer$mid_transformedexp <- midline_dealer$mid_exp - 183 #6x366/12 
#this is the exp date minus half a year (as the exp date is approximately 6 months after packaging)

midline_dealer$mid_date_pack_incltransformedexp[is.na(midline_dealer$mid_date_pack)]<-midline_dealer$mid_transformedexp[is.na(midline_dealer$mid_date_pack)] 
#so we take this number as pack date if pack date is not on the bag

midline_dealer$mid_shelflife_Caro <- midline_dealer$mid_date - as.Date(midline_dealer$mid_date_pack_incltransformedexp)
#this is the number of days between (fake) pack date and interview, a positive number, 180 days (~6 months) in my example
midline_dealer$mid_shelflife_Caro[midline_dealer$mid_shelflife_Caro < 0] <- NA 
#a neg number means that the seed was packed AFTER in enumerator bought the bag, which is impossible and becomes NA
midline_dealer$owner.shelflife_Caro <- as.numeric(as.character(midline_dealer$mid_shelflife_Caro)) ##shelflife variable 



###2. Demean and divide outcomes by control group standard deviation (normalizes outcomes to be on comparable scale)

#https://github.com/cdsamii/make_index/blob/master/r/index_comparison.R

#function to standardize columns of matrix, sgroup = control group = logical vector

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  
  for(j in 1:ncol(x)){
    
    x[,j] <- (x[,j] - mean(x[sgroup,j],na.rm = T))/sd(x[sgroup,j],na.rm = T)
    
  }
  
  return(x)
  
}

###3. Define groupings/areas/domains of outcomes: each outcome is assigned to one of these areas

## index of capital intensive seed handling and storage practices 
variables_practices_cap_mid <- cbind(midline_dealer$mid_roofleak,midline_dealer$mid_roofinsul, midline_dealer$mid_wallinsul, midline_dealer$mid_ventilation,
                                     midline_dealer$mid_cert) #x

variables_practices_cap_base <- cbind(baseline_dealer$base_roofleak,baseline_dealer$base_roofinsul, baseline_dealer$base_wallinsul, baseline_dealer$base_ventilation,
                                      baseline_dealer$base_cert)

#index of efforts by dealer 
variables_efforts_mid <- cbind(midline_dealer$mid_alwaysexplains,midline_dealer$mid_alwaysrecom,midline_dealer$mid_extension
                               ,midline_dealer$mid_largequan,midline_dealer$mid_smallpack,midline_dealer$mid_seedcredit 
                               ,midline_dealer$owner.complaint,midline_dealer$mid_mobmoney)

variables_efforts_base <- cbind(baseline_dealer$base_alwaysexplains,baseline_dealer$base_alwaysrecom,baseline_dealer$base_extension
                                ,baseline_dealer$base_largequan,baseline_dealer$base_smallpack,baseline_dealer$base_seedcredit 
                                ,baseline_dealer$maize.owner.complaint,baseline_dealer$base_mobmoney)


#index of all handling and storage practices observed by enumerator 
variables_practices_all_mid <- cbind(midline_dealer$mid_roofleak,midline_dealer$mid_roofinsul, midline_dealer$mid_wallinsul, midline_dealer$mid_ventilation,
                                     midline_dealer$mid_cert, midline_dealer$mid_goodpractice_expired2, midline_dealer$owner.dedarea, midline_dealer$owner.ratpest,
                                     midline_dealer$owner.lighting, midline_dealer$owner.surface, midline_dealer$owner.openstorage, midline_dealer$owner.rate) 

variables_practices_all_base <- cbind(baseline_dealer$base_roofleak,baseline_dealer$base_roofinsul, baseline_dealer$base_wallinsul, baseline_dealer$base_ventilation,
                                      baseline_dealer$base_cert, baseline_dealer$base_goodpractice_expired2, baseline_dealer$maize.owner.dedarea, baseline_dealer$maize.owner.ratpest,
                                      baseline_dealer$maize.owner.lighting, baseline_dealer$maize.owner.surface, baseline_dealer$maize.owner.openstorage, baseline_dealer$maize.owner.rate) 



###4. Create index: weighted average of outcomes for individual i in area j

###weight inputs (outcomes) by inverse of covariance matrix of transformed outcomes in area j

###simple way: set weight on each outcome equal to sum of its row entries in inverted covariance matrix for area j



#function that takes in data in matrix format and returns IC weights and ICW index

#wgts argument: weights can be incorporated

#revcols argument: takes vector indicating which columns should have reversed values (standardized values * -1) prior to construction of index

icwIndex <- function(     xmat,
                          
                          #wgts=rep(1, nrow(xmat)), #nrow: number of rows present in xmat --> many 1s
                          
                          revcols = NULL,
                          
                          sgroup = rep(TRUE, nrow(xmat))){
  
  X <- matStand(xmat, sgroup)
  
  if(length(revcols)>0){
    
    X[,revcols] <-  -1*X[,revcols]
    
  }
  
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  
  #Sx <- cov.wt(X, wt=wgts)[[1]]
  
  #list with estimates of the weighted covariance matrix and the mean of the data
  
  Sx <- cov(X,use = "pairwise.complete.obs")
  
  #cov: covariance of x and y if these are vectors/covariances between columns of x and columns of y are computed if these are matrices
  
  #use = "everything" produces NAs for the index.
  
  #use = "all.obs" produces an error.
  
  #use = "complete.obs" and use = "na.or.complete": works, NAs are handled by casewise deletion.
  
  #use = "pairwise.complete.obs": works, covariance between each pair of variables is computed using all complete pairs of observations on those variables
  
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  
  return(list(weights = weights, index = index))
  
}


### index of capital intensive seed handling and storage practices 

index_practices_cap_mid <- icwIndex(xmat=variables_practices_cap_mid) #x
### creating index in midline 
midline_dealer$owner.index_practices_cap <- index_practices_cap_mid$index #x

index_practices_cap_base <- icwIndex(xmat=variables_practices_cap_base)
### creating index in baseline 
baseline_dealer$maize.owner.index_practices_cap <- index_practices_cap_base$index

##index of all handling and storage practices observed by enumerator 
index_practices_all_mid <- icwIndex(xmat=variables_practices_all_mid) 
midline_dealer$owner.practices_all <- index_practices_all_mid$index 

index_practices_all_base <- icwIndex(xmat=variables_practices_all_base)
baseline_dealer$maize.owner.practices_all <- index_practices_all_base$index

### index of efforts by dealer 

index_efforts_mid <- icwIndex(xmat=variables_efforts_mid)
### creating index in midline 
midline_dealer$owner.index_efforts <- index_efforts_mid$index 

index_efforts_base <- icwIndex(xmat=variables_efforts_base)
### creating index in baseline 
baseline_dealer$maize.owner.index_efforts <- index_efforts_base$index


####moisture reading 
baseline_dealer$maize.owner.moistread<-baseline_dealer$reading
midline_dealer$owner.moistread<-midline_dealer$reading

###average sales price -- 4 types of maize seeds 
### BASELINE 
baseline_dealer$maize.owner.agree.long10h.q26[baseline_dealer$maize.owner.agree.long10h.q26=="n/a"]<-NA
baseline_dealer$maize.owner.agree.long10h.q26<- as.numeric(as.character(baseline_dealer$maize.owner.agree.long10h.q26))

baseline_dealer$maize.owner.agree.longe7h.q38[baseline_dealer$maize.owner.agree.longe7h.q38=="n/a"]<-NA
baseline_dealer$maize.owner.agree.longe7h.q38<- as.numeric(as.character(baseline_dealer$maize.owner.agree.longe7h.q38))

baseline_dealer$maize.owner.agree.longe5.q51[baseline_dealer$maize.owner.agree.longe5.q51=="n/a"]<-NA
baseline_dealer$maize.owner.agree.longe5.q51<- as.numeric(as.character(baseline_dealer$maize.owner.agree.longe5.q51))

baseline_dealer$maize.owner.agree.longe4.q63[baseline_dealer$maize.owner.agree.longe4.q63=="n/a"]<-NA
baseline_dealer$maize.owner.agree.longe4.q63<- as.numeric(as.character(baseline_dealer$maize.owner.agree.longe4.q63))

baseline_dealer$base_price <-  rowMeans(baseline_dealer[c("maize.owner.agree.long10h.q26","maize.owner.agree.longe7h.q38","maize.owner.agree.longe5.q51","maize.owner.agree.longe4.q63")],na.rm=T) 

baseline_dealer <- baseline_dealer %>%  mutate(maize.owner.saleprice = scale(base_price)) #standardising 


### MIDLINE 
midline_dealer$owner.agree.long10h.q26[midline_dealer$owner.agree.long10h.q26=="n/a"]<-NA
midline_dealer$owner.agree.long10h.q26<- as.numeric(as.character(midline_dealer$owner.agree.long10h.q26))

midline_dealer$owner.agree.longe7H.q38[midline_dealer$owner.agree.longe7H.q38=="n/a"]<-NA
midline_dealer$owner.agree.longe7H.q38<- as.numeric(as.character(midline_dealer$owner.agree.longe7H.q38))

midline_dealer$owner.agree.longe5.q51[midline_dealer$owner.agree.longe5.q51=="n/a"]<-NA
midline_dealer$owner.agree.longe5.q51<- as.numeric(as.character(midline_dealer$owner.agree.longe5.q51))

midline_dealer$owner.agree.longe4.q63[midline_dealer$owner.agree.longe4.q63=="n/a"]<-NA
midline_dealer$owner.agree.longe4.q63<- as.numeric(as.character(midline_dealer$owner.agree.longe4.q63))

midline_dealer$mid_price <-  rowMeans(midline_dealer[c("owner.agree.long10h.q26","owner.agree.longe7H.q38","owner.agree.longe5.q51","owner.agree.longe4.q63")],na.rm=T) 

midline_dealer <- midline_dealer %>%  mutate(owner.saleprice = scale(mid_price)) #standardising 


###average cost of buying seeds  -- 2 types of maize seeds 
#BASELINE 
baseline_dealer$maize.owner.agree.long10h.q24[baseline_dealer$maize.owner.agree.long10h.q24=="n/a"]<-NA
baseline_dealer$maize.owner.agree.long10h.q24<- as.numeric(as.character(baseline_dealer$maize.owner.agree.long10h.q24))

baseline_dealer$maize.owner.agree.longe5.q49[baseline_dealer$maize.owner.agree.longe5.q49=="n/a"]<-NA
baseline_dealer$maize.owner.agree.longe5.q49<- as.numeric(as.character(baseline_dealer$maize.owner.agree.longe5.q49))

baseline_dealer$base_cost_seed <-  rowMeans(baseline_dealer[c("maize.owner.agree.long10h.q24","maize.owner.agree.longe5.q49")],na.rm=T) 

baseline_dealer <- baseline_dealer %>%  mutate(maize.owner.costseed = scale(base_cost_seed)) #standardising 

## MIDLINE
midline_dealer$owner.agree.long10h.q24[midline_dealer$owner.agree.long10h.q24=="n/a"]<-NA
midline_dealer$owner.agree.long10h.q24<- as.numeric(as.character(midline_dealer$owner.agree.long10h.q24))

midline_dealer$owner.agree.longe5.q49[midline_dealer$owner.agree.longe5.q49=="n/a"]<-NA
midline_dealer$owner.agree.longe5.q49<- as.numeric(as.character(midline_dealer$owner.agree.longe5.q49))

midline_dealer$mid_cost_seed <-  rowMeans(midline_dealer[c("owner.agree.long10h.q24","owner.agree.longe5.q49")],na.rm=T) 

midline_dealer <- midline_dealer %>%  mutate(owner.costseed = scale(mid_cost_seed)) #standardising 

# ## how often the dealers ran out of stock --- higher it is, better it gets 
# #1 = everyday ; 2 = once a week ; 3 = once a month ; 4 = once in a season ; 5 = never ; 6 = not sure 
# 
# ##BASELINE 
# baseline_dealer$maize.owner.agree.long10h.q30[baseline_dealer$maize.owner.agree.long10h.q30=="n/a"]<-NA
# baseline_dealer$maize.owner.agree.long10h.q30[baseline_dealer$maize.owner.agree.long10h.q30=="6"]<-NA
# baseline_dealer$maize.owner.agree.long10h.q30<-as.numeric(as.character(baseline_dealer$maize.owner.agree.long10h.q30))
# 
# baseline_dealer$maize.owner.agree.longe5.q55[baseline_dealer$maize.owner.agree.longe5.q55=="n/a"]<-NA
# baseline_dealer$maize.owner.agree.longe5.q55[baseline_dealer$maize.owner.agree.longe5.q55=="6"]<-NA
# baseline_dealer$maize.owner.agree.longe5.q55<-as.numeric(as.character(baseline_dealer$maize.owner.agree.longe5.q55))
# 
# baseline_dealer$maize.owner.stockfin <-  rowMeans(baseline_dealer[c("maize.owner.agree.long10h.q30","maize.owner.agree.longe5.q55")],na.rm=T) 
# 
# #baseline_dealer <- baseline_dealer %>%  mutate(maize.owner.stockfin = scale(maize.owner.stockfin)) #standardising 
# 
# #creating dummy
# baseline_dealer$maize.owner.stockfin[baseline_dealer$maize.owner.stockfin<=3]<-0  #everyday, once a week or once a month are bad
# baseline_dealer$maize.owner.stockfin[baseline_dealer$maize.owner.stockfin>3]<-1 #once in a season or never are good 
# 
# ## MIDLINE
# midline_dealer$owner.agree.long10h.q30[midline_dealer$owner.agree.long10h.q30=="n/a"]<-NA
# midline_dealer$owner.agree.long10h.q30[midline_dealer$owner.agree.long10h.q30=="6"]<-NA
# midline_dealer$owner.agree.long10h.q30<-as.numeric(as.character(midline_dealer$owner.agree.long10h.q30))
# 
# midline_dealer$owner.agree.longe5.q55[midline_dealer$owner.agree.longe5.q55=="n/a"]<-NA
# midline_dealer$owner.agree.longe5.q55[midline_dealer$owner.agree.longe5.q55=="6"]<-NA
# midline_dealer$owner.agree.longe5.q55<-as.numeric(as.character(midline_dealer$owner.agree.longe5.q55))
# 
# midline_dealer$owner.stockfin <-  rowMeans(midline_dealer[c("owner.agree.long10h.q30","owner.agree.longe5.q55")],na.rm=T) 
# 
# #midline_dealer <- midline_dealer %>%  mutate(owner.stockfin = scale(owner.stockfin)) #standardising 
# 
# #creating dummy
# midline_dealer$owner.stockfin[midline_dealer$owner.stockfin<=3]<-0  #everyday, once a week or once a month are bad
# midline_dealer$owner.stockfin[midline_dealer$owner.stockfin>3]<-1  #once in a season or never are good 


### quantity of seeds bought from the provider by the dealer 
##BASELINE 
baseline_dealer$maize.owner.agree.long10h.q22[baseline_dealer$maize.owner.agree.long10h.q22=="n/a"]<-NA
baseline_dealer$maize.owner.agree.long10h.q22[baseline_dealer$maize.owner.agree.long10h.q22=="999"]<-NA
baseline_dealer$maize.owner.agree.long10h.q22<-as.numeric(as.character(baseline_dealer$maize.owner.agree.long10h.q22))

baseline_dealer$maize.owner.agree.longe5.q47[baseline_dealer$maize.owner.agree.longe5.q47=="n/a"]<-NA
baseline_dealer$maize.owner.agree.longe5.q47[baseline_dealer$maize.owner.agree.longe5.q47=="999"]<-NA
baseline_dealer$maize.owner.agree.longe5.q47<-as.numeric(as.character(baseline_dealer$maize.owner.agree.longe5.q47))
                                                          
baseline_dealer$maize.owner.quanprovider <-  rowMeans(baseline_dealer[c("maize.owner.agree.long10h.q22","maize.owner.agree.longe5.q47")],na.rm=T) 

baseline_dealer$maize.owner.quan_nonstan<- baseline_dealer$maize.owner.quanprovider

#standardising
baseline_dealer <- baseline_dealer %>%  mutate(maize.owner.quanprovider = scale(maize.owner.quanprovider))

## MIDLINE
midline_dealer$owner.agree.long10h.q22[midline_dealer$owner.agree.long10h.q22=="n/a"]<-NA
midline_dealer$owner.agree.long10h.q22[midline_dealer$owner.agree.long10h.q22=="999"]<-NA
midline_dealer$owner.agree.long10h.q22<-as.numeric(as.character(midline_dealer$owner.agree.long10h.q22))

midline_dealer$owner.agree.longe5.q47[midline_dealer$owner.agree.longe5.q47=="n/a"]<-NA
midline_dealer$owner.agree.longe5.q47[midline_dealer$owner.agree.longe5.q47=="999"]<-NA
midline_dealer$owner.agree.longe5.q47<-as.numeric(as.character(midline_dealer$owner.agree.longe5.q47))

midline_dealer$owner.quanprovider <-  rowMeans(midline_dealer[c("owner.agree.long10h.q22","owner.agree.longe5.q47")],na.rm=T) 

#standardising
midline_dealer <- midline_dealer %>%  mutate(owner.quanprovider = scale(owner.quanprovider))

#is the shop registered with UNADA 
baseline_dealer$maize.owner.unada<- ifelse(baseline_dealer$maize.owner.agree.inspection.q114== 'Yes', 1, 0)
midline_dealer$owner.unada<- ifelse(midline_dealer$owner.agree.inspection.q114== 'Yes', 1, 0)

### have you received a warning as a result of inspection if something was not up to standard?
baseline_dealer$maize.owner.warning <- ifelse(baseline_dealer$maize.owner.agree.inspection.q118== 'Yes', 1, 0)
midline_dealer$owner.warning<- ifelse(midline_dealer$owner.agree.inspection.q118== 'Yes', 1, 0)   


#including location variable -- distance between farmer and dealer -- calculated using GPS coordinates 
distance <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/input_dealer/distance.csv", sep = "/"), stringsAsFactors = FALSE) #reading in baseline data for location
distance_mid <- read.csv(paste(path_2,"perceptions/data_seed_systems_midline/data/input_dealer/distance.csv", sep = "/"), stringsAsFactors = FALSE) #reading in midline data for location

###baseline 
##aggregating the distance for each dealer 
distance <- aggregate(distance[c("maize.owner.dist")],list(distance$shop_ID),FUN=mean, na.rm=T)
distance$dist_km<-distance$maize.owner.dist
##getting standardized distance variable 
distance <- distance %>% mutate(maize.owner.dist = scale(maize.owner.dist))
names(distance)[1] <- "shop_ID"   #renaming variable 

###midline
##aggregating the distance for each dealer 
distance_mid <- aggregate(distance_mid[c("owner.dist")],list(distance_mid$shop_ID),FUN=mean, na.rm=T)
##getting standardized distance variable 
distance_mid <- distance_mid %>% mutate(owner.dist = scale(owner.dist))
names(distance_mid)[1] <- "shop_ID"   #renaming variable 


baseline_dealer <- merge(baseline_dealer, distance, by="shop_ID") #baseline
midline_dealer <- merge(midline_dealer, distance_mid, by="shop_ID")   #midline 

merged_dealer <- merge(baseline_dealer, midline_dealer, by="shop_ID") #merging baseline and midline 


################# DESCRIPTIVE STATISTICS ###############

mean(baseline_dealer$maize.owner.index_practices_cap, na.rm=T)
min(baseline_dealer$maize.owner.index_practices_cap , na.rm=T)
max(baseline_dealer$maize.owner.index_practices_cap , na.rm=T)
sd(baseline_dealer$maize.owner.index_practices_cap , na.rm=T)
quantile(baseline_dealer$maize.owner.index_practices_cap, na.rm=T, 0.25)
quantile(baseline_dealer$maize.owner.index_practices_cap, na.rm=T, 0.75)

sum(!is.na (baseline_dealer$maize.owner.index_practices_cap))


mean(baseline_dealer$maize.owner.index_efforts, na.rm=T)
min(baseline_dealer$maize.owner.index_efforts , na.rm=T)
max(baseline_dealer$maize.owner.index_efforts , na.rm=T)
sd(baseline_dealer$maize.owner.index_efforts , na.rm=T)
quantile(baseline_dealer$maize.owner.index_efforts, na.rm=T, 0.25)
quantile(baseline_dealer$maize.owner.index_efforts, na.rm=T, 0.75)

sum(!is.na (baseline_dealer$maize.owner.index_efforts))


mean(baseline_dealer$maize.owner.dist, na.rm=T)
min(baseline_dealer$maize.owner.dist , na.rm=T)
max(baseline_dealer$maize.owner.dist , na.rm=T)
sd(baseline_dealer$maize.owner.dist , na.rm=T)
quantile(baseline_dealer$maize.owner.dist, na.rm=T, 0.25)
quantile(baseline_dealer$maize.owner.dist, na.rm=T, 0.75)

sum(!is.na (baseline_dealer$maize.owner.dist))


mean(baseline_dealer$maize.owner.saleprice, na.rm=T)
min(baseline_dealer$maize.owner.saleprice , na.rm=T)
max(baseline_dealer$maize.owner.saleprice, na.rm=T)
sd(baseline_dealer$maize.owner.saleprice , na.rm=T)
quantile(baseline_dealer$maize.owner.saleprice, na.rm=T, 0.25)
quantile(baseline_dealer$maize.owner.saleprice, na.rm=T, 0.75)

sum(!is.na (baseline_dealer$maize.owner.saleprice))


mean(baseline_dealer$base_cost_seed, na.rm=T)
min(baseline_dealer$base_cost_seed , na.rm=T)
max(baseline_dealer$base_cost_seed, na.rm=T)
sd(baseline_dealer$base_cost_seed, na.rm=T)
quantile(baseline_dealer$base_cost_seed, na.rm=T, 0.25)
quantile(baseline_dealer$base_cost_seed, na.rm=T, 0.75)

sum(!is.na (baseline_dealer$maize.owner.costseed))


mean(baseline_dealer$maize.owner.practices_all, na.rm=T)
min(baseline_dealer$maize.owner.practices_all , na.rm=T)
max(baseline_dealer$maize.owner.practices_all, na.rm=T)
sd(baseline_dealer$maize.owner.practices_all , na.rm=T)
quantile(baseline_dealer$maize.owner.practices_all, na.rm=T, 0.25)
quantile(baseline_dealer$maize.owner.practices_all, na.rm=T, 0.75)

sum(!is.na (baseline_dealer$maize.owner.practices_all))


mean(baseline_dealer$maize.owner.quanprovider, na.rm=T)
min(baseline_dealer$maize.owner.quanprovider , na.rm=T)
max(baseline_dealer$maize.owner.quanprovider, na.rm=T)
sd(baseline_dealer$maize.owner.quanprovider , na.rm=T)
quantile(baseline_dealer$maize.owner.quanprovider, na.rm=T, 0.25)
quantile(baseline_dealer$maize.owner.quanprovider, na.rm=T, 0.75)

sum(!is.na (baseline_dealer$maize.owner.quanprovider))



############ storing descriptives

desc<- rbind(c( mean(baseline_dealer$maize.owner.index_practices_cap, na.rm=T),
                min(baseline_dealer$maize.owner.index_practices_cap , na.rm=T),
                max(baseline_dealer$maize.owner.index_practices_cap , na.rm=T),
                sd(baseline_dealer$maize.owner.index_practices_cap , na.rm=T),
                sum(!is.na (baseline_dealer$maize.owner.index_practices_cap))),
          
              c(  mean(baseline_dealer$maize.owner.index_efforts, na.rm=T),
               min(baseline_dealer$maize.owner.index_efforts , na.rm=T),
               max(baseline_dealer$maize.owner.index_efforts , na.rm=T),
               sd(baseline_dealer$maize.owner.index_efforts , na.rm=T),
               sum(!is.na (baseline_dealer$maize.owner.index_efforts))),

           c(mean(baseline_dealer$dist_km, na.rm=T),
min(baseline_dealer$dist_km , na.rm=T),
max(baseline_dealer$dist_km, na.rm=T),
sd(baseline_dealer$dist_km , na.rm=T),
sum(!is.na (baseline_dealer$dist_km))),
           
         c(  mean(baseline_dealer$base_price, na.rm=T),
           min(baseline_dealer$base_price , na.rm=T),
           max(baseline_dealer$base_price, na.rm=T),
           sd(baseline_dealer$base_price , na.rm=T),
           sum(!is.na (baseline_dealer$base_price))),
           
           
          c( mean(baseline_dealer$base_cost_seed, na.rm=T),
           min(baseline_dealer$base_cost_seed , na.rm=T),
           max(baseline_dealer$base_cost_seed, na.rm=T),
           sd(baseline_dealer$base_cost_seed , na.rm=T),
           sum(!is.na (baseline_dealer$base_cost_seed))),
           
           
         c(  mean(baseline_dealer$maize.owner.practices_all, na.rm=T),
           min(baseline_dealer$maize.owner.practices_all , na.rm=T),
           max(baseline_dealer$maize.owner.practices_all, na.rm=T),
           sd(baseline_dealer$maize.owner.practices_all , na.rm=T),
           sum(!is.na (baseline_dealer$maize.owner.practices_all))),
           
           
          c( mean(baseline_dealer$maize.owner.quan_nonstan, na.rm=T),
           min(baseline_dealer$maize.owner.quan_nonstan, na.rm=T),
           max(baseline_dealer$maize.owner.quan_nonstan, na.rm=T),
           sd(baseline_dealer$maize.owner.quan_nonstan , na.rm=T),
           sum(!is.na (baseline_dealer$maize.owner.quan_nonstan))),

c( mean(baseline_dealer$goodpractice_expired2, na.rm=T),
   min(baseline_dealer$goodpractice_expired2 , na.rm=T),
   max(baseline_dealer$goodpractice_expired2, na.rm=T),
   sd(baseline_dealer$goodpractice_expired2 , na.rm=T),
   sum(!is.na (baseline_dealer$goodpractice_expired2))))

#male dealers 
baseline_dealer$gen<-ifelse(baseline_dealer$maize.owner.agree.gender=="Male",1,0)
baseline_dealer_male <- baseline_dealer[baseline_dealer$gen==1,]

desc_male<- rbind(c( mean(baseline_dealer_male$maize.owner.index_practices_cap, na.rm=T),
                min(baseline_dealer_male$maize.owner.index_practices_cap , na.rm=T),
                max(baseline_dealer_male$maize.owner.index_practices_cap , na.rm=T),
                sd(baseline_dealer_male$maize.owner.index_practices_cap , na.rm=T),
                sum(!is.na (baseline_dealer_male$maize.owner.index_practices_cap))),
             
             c(  mean(baseline_dealer_male$maize.owner.index_efforts, na.rm=T),
                 min(baseline_dealer_male$maize.owner.index_efforts , na.rm=T),
                 max(baseline_dealer_male$maize.owner.index_efforts , na.rm=T),
                 sd(baseline_dealer_male$maize.owner.index_efforts , na.rm=T),
                 sum(!is.na (baseline_dealer_male$maize.owner.index_efforts))),
             
             c(mean(baseline_dealer_male$dist_km, na.rm=T),
               min(baseline_dealer_male$dist_km , na.rm=T),
               max(baseline_dealer_male$dist_km, na.rm=T),
               sd(baseline_dealer_male$dist_km , na.rm=T),
               sum(!is.na (baseline_dealer_male$dist_km))),
             
             c(  mean(baseline_dealer_male$base_price, na.rm=T),
                 min(baseline_dealer_male$base_price , na.rm=T),
                 max(baseline_dealer_male$base_price, na.rm=T),
                 sd(baseline_dealer_male$base_price , na.rm=T),
                 sum(!is.na (baseline_dealer_male$base_price))),
             
             
             c( mean(baseline_dealer_male$base_cost_seed, na.rm=T),
                min(baseline_dealer_male$base_cost_seed , na.rm=T),
                max(baseline_dealer_male$base_cost_seed, na.rm=T),
                sd(baseline_dealer_male$base_cost_seed , na.rm=T),
                sum(!is.na (baseline_dealer_male$base_cost_seed))),
             
             
             c(  mean(baseline_dealer_male$maize.owner.practices_all, na.rm=T),
                 min(baseline_dealer_male$maize.owner.practices_all , na.rm=T),
                 max(baseline_dealer_male$maize.owner.practices_all, na.rm=T),
                 sd(baseline_dealer_male$maize.owner.practices_all , na.rm=T),
                 sum(!is.na (baseline_dealer_male$maize.owner.practices_all))),
             
             
             c( mean(baseline_dealer_male$maize.owner.quan_nonstan, na.rm=T),
                min(baseline_dealer_male$maize.owner.quan_nonstan, na.rm=T),
                max(baseline_dealer_male$maize.owner.quan_nonstan, na.rm=T),
                sd(baseline_dealer_male$maize.owner.quan_nonstan , na.rm=T),
                sum(!is.na (baseline_dealer_male$maize.owner.quan_nonstan))),
             
             c( mean(baseline_dealer_male$goodpractice_expired2, na.rm=T),
                min(baseline_dealer_male$goodpractice_expired2 , na.rm=T),
                max(baseline_dealer_male$goodpractice_expired2, na.rm=T),
                sd(baseline_dealer_male$goodpractice_expired2 , na.rm=T),
                sum(!is.na (baseline_dealer_male$goodpractice_expired2))))

#female dealers 

baseline_dealer_female <- baseline_dealer[baseline_dealer$gen==0,]

desc_female<- rbind(c( mean(baseline_dealer_female$maize.owner.index_practices_cap, na.rm=T),
                       min(baseline_dealer_female$maize.owner.index_practices_cap , na.rm=T),
                       max(baseline_dealer_female$maize.owner.index_practices_cap , na.rm=T),
                       sd(baseline_dealer_female$maize.owner.index_practices_cap , na.rm=T),
                       sum(!is.na (baseline_dealer_female$maize.owner.index_practices_cap))),
                    
                    c(  mean(baseline_dealer_female$maize.owner.index_efforts, na.rm=T),
                        min(baseline_dealer_female$maize.owner.index_efforts , na.rm=T),
                        max(baseline_dealer_female$maize.owner.index_efforts , na.rm=T),
                        sd(baseline_dealer_female$maize.owner.index_efforts , na.rm=T),
                        sum(!is.na (baseline_dealer_female$maize.owner.index_efforts))),
                    
                    c(mean(baseline_dealer_female$dist_km, na.rm=T),
                      min(baseline_dealer_female$dist_km , na.rm=T),
                      max(baseline_dealer_female$dist_km, na.rm=T),
                      sd(baseline_dealer_female$dist_km , na.rm=T),
                      sum(!is.na (baseline_dealer_female$dist_km))),
                    
                    c(  mean(baseline_dealer_female$base_price, na.rm=T),
                        min(baseline_dealer_female$base_price , na.rm=T),
                        max(baseline_dealer_female$base_price, na.rm=T),
                        sd(baseline_dealer_female$base_price , na.rm=T),
                        sum(!is.na (baseline_dealer_female$base_price))),
                    
                    
                    c( mean(baseline_dealer_female$base_cost_seed, na.rm=T),
                       min(baseline_dealer_female$base_cost_seed , na.rm=T),
                       max(baseline_dealer_female$base_cost_seed, na.rm=T),
                       sd(baseline_dealer_female$base_cost_seed , na.rm=T),
                       sum(!is.na (baseline_dealer_female$base_cost_seed))),
                    
                    
                    c(  mean(baseline_dealer_female$maize.owner.practices_all, na.rm=T),
                        min(baseline_dealer_female$maize.owner.practices_all , na.rm=T),
                        max(baseline_dealer_female$maize.owner.practices_all, na.rm=T),
                        sd(baseline_dealer_female$maize.owner.practices_all , na.rm=T),
                        sum(!is.na (baseline_dealer_female$maize.owner.practices_all))),
                    
                    
                    c( mean(baseline_dealer_female$maize.owner.quan_nonstan, na.rm=T),
                       min(baseline_dealer_female$maize.owner.quan_nonstan, na.rm=T),
                       max(baseline_dealer_female$maize.owner.quan_nonstan, na.rm=T),
                       sd(baseline_dealer_female$maize.owner.quan_nonstan , na.rm=T),
                       sum(!is.na (baseline_dealer_female$maize.owner.quan_nonstan))),
                    
                    c( mean(baseline_dealer_female$goodpractice_expired2, na.rm=T),
                       min(baseline_dealer_female$goodpractice_expired2 , na.rm=T),
                       max(baseline_dealer_female$goodpractice_expired2, na.rm=T),
                       sd(baseline_dealer_female$goodpractice_expired2 , na.rm=T),
                       sum(!is.na (baseline_dealer_female$goodpractice_expired2))))
           
           
######################## getting descriptives of the ratings 

baserat <- merge(baseline_dealer, rating_dyads, by="shop_ID") 
           
baserat$general_rating <- as.numeric(as.character(baserat$general_rating))
baserat$location_rating <- as.numeric(as.character(baserat$location_rating))
baserat$price_rating <- as.numeric(as.character(baserat$price_rating))
baserat$quality_rating <- as.numeric(as.character(baserat$quality_rating))
baserat$stock_rating <- as.numeric(as.character(baserat$stock_rating))
baserat$reputation_rating <- as.numeric(as.character(baserat$reputation_rating))

baserat$seed_yield_rating [baserat$seed_yield_rating=="98"] <- NA
baserat$seed_drought_rating [baserat$seed_drought_rating=="98"] <- NA
baserat$seed_disease_rating [baserat$seed_disease_rating=="98"] <- NA
baserat$seed_maturing_rating [baserat$seed_maturing_rating=="98"] <- NA
baserat$seed_germinate_rating [baserat$seed_germinate_rating=="98"] <- NA
baserat$seed_quality_general_rating [baserat$seed_quality_general_rating=="98"] <- NA

baserat$seed_yield_rating <- as.numeric(as.character(baserat$seed_yield_rating))
baserat$seed_drought_rating <- as.numeric(as.character(baserat$seed_drought_rating))
baserat$seed_disease_rating <- as.numeric(as.character(baserat$seed_disease_rating))
baserat$seed_maturing_rating <- as.numeric(as.character(baserat$seed_maturing_rating))
baserat$seed_germinate_rating <- as.numeric(as.character(baserat$seed_germinate_rating))
baserat$seed_quality_general_rating <- as.numeric(as.character(baserat$seed_quality_general_rating))

desc_rat<- rbind(c(mean(baserat$general_rating, na.rm=T),
min(baserat$general_rating, na.rm=T),
max(baserat$general_rating, na.rm=T),
sd(baserat$general_rating, na.rm=T),
quantile(baserat$general_rating, na.rm=T, 0.25),
quantile(baserat$general_rating, na.rm=T, 0.75),
sum(!is.na (baserat$general_rating))),

c(mean(baserat$location_rating, na.rm=T),
  min(baserat$location_rating, na.rm=T),
  max(baserat$location_rating, na.rm=T),
  sd(baserat$location_rating, na.rm=T),
  quantile(baserat$location_rating, na.rm=T, 0.25),
  quantile(baserat$location_rating, na.rm=T, 0.75),
  sum(!is.na (baserat$location_rating))),

c(mean(baserat$price_rating, na.rm=T),
  min(baserat$price_rating, na.rm=T),
  max(baserat$price_rating, na.rm=T),
  sd(baserat$price_rating, na.rm=T),
  quantile(baserat$price_rating, na.rm=T, 0.25),
  quantile(baserat$price_rating, na.rm=T, 0.75),
  sum(!is.na (baserat$price_rating))),

c(mean(baserat$stock_rating, na.rm=T),
  min(baserat$stock_rating, na.rm=T),
  max(baserat$stock_rating, na.rm=T),
  sd(baserat$stock_rating, na.rm=T),
  quantile(baserat$stock_rating, na.rm=T, 0.25),
  quantile(baserat$stock_rating, na.rm=T, 0.75),
  sum(!is.na (baserat$stock_rating))),

c(mean(baserat$reputation_rating, na.rm=T),
  min(baserat$reputation_rating, na.rm=T),
  max(baserat$reputation_rating, na.rm=T),
  sd(baserat$reputation_rating, na.rm=T),
  quantile(baserat$reputation_rating, na.rm=T, 0.25),
  quantile(baserat$reputation_rating, na.rm=T, 0.75),
  sum(!is.na (baserat$reputation_rating))),

c(mean(baserat$seed_quality_general_rating, na.rm=T),
  min(baserat$seed_quality_general_rating, na.rm=T),
  max(baserat$seed_quality_general_rating, na.rm=T),
  sd(baserat$seed_quality_general_rating, na.rm=T),
  quantile(baserat$seed_quality_general_rating, na.rm=T, 0.25),
  quantile(baserat$seed_quality_general_rating, na.rm=T, 0.75), 
  sum(!is.na (baserat$seed_quality_general_rating))),

c(mean(baserat$seed_yield_rating, na.rm=T),
  min(baserat$seed_yield_rating, na.rm=T),
  max(baserat$seed_yield_rating, na.rm=T),
  sd(baserat$seed_yield_rating, na.rm=T),
  quantile(baserat$seed_yield_rating, na.rm=T, 0.25),
  quantile(baserat$seed_yield_rating, na.rm=T, 0.75), 
  sum(!is.na (baserat$seed_yield_rating))),

c(mean(baserat$seed_drought_rating, na.rm=T),
  min(baserat$seed_drought_rating, na.rm=T),
  max(baserat$seed_drought_rating, na.rm=T),
  sd(baserat$seed_drought_rating, na.rm=T),
  quantile(baserat$seed_drought_rating, na.rm=T, 0.25),
  quantile(baserat$seed_drought_rating, na.rm=T, 0.75),
  sum(!is.na (baserat$seed_drought_rating))),

c(mean(baserat$seed_disease_rating, na.rm=T),
  min(baserat$seed_disease_rating, na.rm=T),
  max(baserat$seed_disease_rating, na.rm=T),
  sd(baserat$seed_disease_rating, na.rm=T),
  quantile(baserat$seed_disease_rating, na.rm=T, 0.25),
  quantile(baserat$seed_disease_rating, na.rm=T, 0.75), 
  sum(!is.na (baserat$seed_disease_rating))),

c(mean(baserat$seed_maturing_rating, na.rm=T),
  min(baserat$seed_maturing_rating, na.rm=T),
  max(baserat$seed_maturing_rating, na.rm=T),
  sd(baserat$seed_maturing_rating, na.rm=T),
  quantile(baserat$seed_maturing_rating, na.rm=T, 0.25),
  quantile(baserat$seed_maturing_rating, na.rm=T, 0.75), 
  sum(!is.na (baserat$seed_maturing_rating))),

c(mean(baserat$seed_germinate_rating, na.rm=T),
  min(baserat$seed_germinate_rating, na.rm=T),
  max(baserat$seed_germinate_rating, na.rm=T),
  sd(baserat$seed_germinate_rating, na.rm=T),
  quantile(baserat$seed_germinate_rating, na.rm=T, 0.25),
  quantile(baserat$seed_germinate_rating, na.rm=T, 0.75),
  sum(!is.na (baserat$seed_germinate_rating))),

c(mean(baserat$quality_rating, na.rm=T),
  min(baserat$quality_rating, na.rm=T),
  max(baserat$quality_rating, na.rm=T),
  sd(baserat$quality_rating, na.rm=T),
  quantile(baserat$quality_rating, na.rm=T, 0.25),
  quantile(baserat$quality_rating, na.rm=T, 0.75),
sum(!is.na (baserat$quality_rating))))


#Male

baserat_male <- baserat[baserat$gen==1,]
desc_rat_male<- rbind(c(mean(baserat_male$general_rating, na.rm=T),
                   min(baserat_male$general_rating, na.rm=T),
                   max(baserat_male$general_rating, na.rm=T),
                   sd(baserat_male$general_rating, na.rm=T),
                   quantile(baserat_male$general_rating, na.rm=T, 0.25),
                   quantile(baserat_male$general_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$general_rating))),
                 
                 c(mean(baserat_male$location_rating, na.rm=T),
                   min(baserat_male$location_rating, na.rm=T),
                   max(baserat_male$location_rating, na.rm=T),
                   sd(baserat_male$location_rating, na.rm=T),
                   quantile(baserat_male$location_rating, na.rm=T, 0.25),
                   quantile(baserat_male$location_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$location_rating))),
                 
                 c(mean(baserat_male$price_rating, na.rm=T),
                   min(baserat_male$price_rating, na.rm=T),
                   max(baserat_male$price_rating, na.rm=T),
                   sd(baserat_male$price_rating, na.rm=T),
                   quantile(baserat_male$price_rating, na.rm=T, 0.25),
                   quantile(baserat_male$price_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$price_rating))),
                 
                 c(mean(baserat_male$stock_rating, na.rm=T),
                   min(baserat_male$stock_rating, na.rm=T),
                   max(baserat_male$stock_rating, na.rm=T),
                   sd(baserat_male$stock_rating, na.rm=T),
                   quantile(baserat_male$stock_rating, na.rm=T, 0.25),
                   quantile(baserat_male$stock_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$stock_rating))),
                 
                 c(mean(baserat_male$reputation_rating, na.rm=T),
                   min(baserat_male$reputation_rating, na.rm=T),
                   max(baserat_male$reputation_rating, na.rm=T),
                   sd(baserat_male$reputation_rating, na.rm=T),
                   quantile(baserat_male$reputation_rating, na.rm=T, 0.25),
                   quantile(baserat_male$reputation_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$reputation_rating))),
                 
                 c(mean(baserat_male$seed_quality_general_rating, na.rm=T),
                   min(baserat_male$seed_quality_general_rating, na.rm=T),
                   max(baserat_male$seed_quality_general_rating, na.rm=T),
                   sd(baserat_male$seed_quality_general_rating, na.rm=T),
                   quantile(baserat_male$seed_quality_general_rating, na.rm=T, 0.25),
                   quantile(baserat_male$seed_quality_general_rating, na.rm=T, 0.75), 
                   sum(!is.na (baserat_male$seed_quality_general_rating))),
                 
                 c(mean(baserat_male$seed_yield_rating, na.rm=T),
                   min(baserat_male$seed_yield_rating, na.rm=T),
                   max(baserat_male$seed_yield_rating, na.rm=T),
                   sd(baserat_male$seed_yield_rating, na.rm=T),
                   quantile(baserat_male$seed_yield_rating, na.rm=T, 0.25),
                   quantile(baserat_male$seed_yield_rating, na.rm=T, 0.75), 
                   sum(!is.na (baserat_male$seed_yield_rating))),
                 
                 c(mean(baserat_male$seed_drought_rating, na.rm=T),
                   min(baserat_male$seed_drought_rating, na.rm=T),
                   max(baserat_male$seed_drought_rating, na.rm=T),
                   sd(baserat_male$seed_drought_rating, na.rm=T),
                   quantile(baserat_male$seed_drought_rating, na.rm=T, 0.25),
                   quantile(baserat_male$seed_drought_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$seed_drought_rating))),
                 
                 c(mean(baserat_male$seed_disease_rating, na.rm=T),
                   min(baserat_male$seed_disease_rating, na.rm=T),
                   max(baserat_male$seed_disease_rating, na.rm=T),
                   sd(baserat_male$seed_disease_rating, na.rm=T),
                   quantile(baserat_male$seed_disease_rating, na.rm=T, 0.25),
                   quantile(baserat_male$seed_disease_rating, na.rm=T, 0.75), 
                   sum(!is.na (baserat_male$seed_disease_rating))),
                 
                 c(mean(baserat_male$seed_maturing_rating, na.rm=T),
                   min(baserat_male$seed_maturing_rating, na.rm=T),
                   max(baserat_male$seed_maturing_rating, na.rm=T),
                   sd(baserat_male$seed_maturing_rating, na.rm=T),
                   quantile(baserat_male$seed_maturing_rating, na.rm=T, 0.25),
                   quantile(baserat_male$seed_maturing_rating, na.rm=T, 0.75), 
                   sum(!is.na (baserat_male$seed_maturing_rating))),
                 
                 c(mean(baserat_male$seed_germinate_rating, na.rm=T),
                   min(baserat_male$seed_germinate_rating, na.rm=T),
                   max(baserat_male$seed_germinate_rating, na.rm=T),
                   sd(baserat_male$seed_germinate_rating, na.rm=T),
                   quantile(baserat_male$seed_germinate_rating, na.rm=T, 0.25),
                   quantile(baserat_male$seed_germinate_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$seed_germinate_rating))),
                 
                 c(mean(baserat_male$quality_rating, na.rm=T),
                   min(baserat_male$quality_rating, na.rm=T),
                   max(baserat_male$quality_rating, na.rm=T),
                   sd(baserat_male$quality_rating, na.rm=T),
                   quantile(baserat_male$quality_rating, na.rm=T, 0.25),
                   quantile(baserat_male$quality_rating, na.rm=T, 0.75),
                   sum(!is.na (baserat_male$quality_rating))))
           
#Female
baserat_female <- baserat[baserat$gen==0,]
desc_rat_female<- rbind(c(mean(baserat_female$general_rating, na.rm=T),
                          min(baserat_female$general_rating, na.rm=T),
                          max(baserat_female$general_rating, na.rm=T),
                          sd(baserat_female$general_rating, na.rm=T),
                          quantile(baserat_female$general_rating, na.rm=T, 0.25),
                          quantile(baserat_female$general_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$general_rating))),
                        
                        c(mean(baserat_female$location_rating, na.rm=T),
                          min(baserat_female$location_rating, na.rm=T),
                          max(baserat_female$location_rating, na.rm=T),
                          sd(baserat_female$location_rating, na.rm=T),
                          quantile(baserat_female$location_rating, na.rm=T, 0.25),
                          quantile(baserat_female$location_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$location_rating))),
                        
                        c(mean(baserat_female$price_rating, na.rm=T),
                          min(baserat_female$price_rating, na.rm=T),
                          max(baserat_female$price_rating, na.rm=T),
                          sd(baserat_female$price_rating, na.rm=T),
                          quantile(baserat_female$price_rating, na.rm=T, 0.25),
                          quantile(baserat_female$price_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$price_rating))),
                        
                        c(mean(baserat_female$stock_rating, na.rm=T),
                          min(baserat_female$stock_rating, na.rm=T),
                          max(baserat_female$stock_rating, na.rm=T),
                          sd(baserat_female$stock_rating, na.rm=T),
                          quantile(baserat_female$stock_rating, na.rm=T, 0.25),
                          quantile(baserat_female$stock_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$stock_rating))),
                        
                        c(mean(baserat_female$reputation_rating, na.rm=T),
                          min(baserat_female$reputation_rating, na.rm=T),
                          max(baserat_female$reputation_rating, na.rm=T),
                          sd(baserat_female$reputation_rating, na.rm=T),
                          quantile(baserat_female$reputation_rating, na.rm=T, 0.25),
                          quantile(baserat_female$reputation_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$reputation_rating))),
                        
                        c(mean(baserat_female$seed_quality_general_rating, na.rm=T),
                          min(baserat_female$seed_quality_general_rating, na.rm=T),
                          max(baserat_female$seed_quality_general_rating, na.rm=T),
                          sd(baserat_female$seed_quality_general_rating, na.rm=T),
                          quantile(baserat_female$seed_quality_general_rating, na.rm=T, 0.25),
                          quantile(baserat_female$seed_quality_general_rating, na.rm=T, 0.75), 
                          sum(!is.na (baserat_female$seed_quality_general_rating))),
                        
                        c(mean(baserat_female$seed_yield_rating, na.rm=T),
                          min(baserat_female$seed_yield_rating, na.rm=T),
                          max(baserat_female$seed_yield_rating, na.rm=T),
                          sd(baserat_female$seed_yield_rating, na.rm=T),
                          quantile(baserat_female$seed_yield_rating, na.rm=T, 0.25),
                          quantile(baserat_female$seed_yield_rating, na.rm=T, 0.75), 
                          sum(!is.na (baserat_female$seed_yield_rating))),
                        
                        c(mean(baserat_female$seed_drought_rating, na.rm=T),
                          min(baserat_female$seed_drought_rating, na.rm=T),
                          max(baserat_female$seed_drought_rating, na.rm=T),
                          sd(baserat_female$seed_drought_rating, na.rm=T),
                          quantile(baserat_female$seed_drought_rating, na.rm=T, 0.25),
                          quantile(baserat_female$seed_drought_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$seed_drought_rating))),
                        
                        c(mean(baserat_female$seed_disease_rating, na.rm=T),
                          min(baserat_female$seed_disease_rating, na.rm=T),
                          max(baserat_female$seed_disease_rating, na.rm=T),
                          sd(baserat_female$seed_disease_rating, na.rm=T),
                          quantile(baserat_female$seed_disease_rating, na.rm=T, 0.25),
                          quantile(baserat_female$seed_disease_rating, na.rm=T, 0.75), 
                          sum(!is.na (baserat_female$seed_disease_rating))),
                        
                        c(mean(baserat_female$seed_maturing_rating, na.rm=T),
                          min(baserat_female$seed_maturing_rating, na.rm=T),
                          max(baserat_female$seed_maturing_rating, na.rm=T),
                          sd(baserat_female$seed_maturing_rating, na.rm=T),
                          quantile(baserat_female$seed_maturing_rating, na.rm=T, 0.25),
                          quantile(baserat_female$seed_maturing_rating, na.rm=T, 0.75), 
                          sum(!is.na (baserat_female$seed_maturing_rating))),
                        
                        c(mean(baserat_female$seed_germinate_rating, na.rm=T),
                          min(baserat_female$seed_germinate_rating, na.rm=T),
                          max(baserat_female$seed_germinate_rating, na.rm=T),
                          sd(baserat_female$seed_germinate_rating, na.rm=T),
                          quantile(baserat_female$seed_germinate_rating, na.rm=T, 0.25),
                          quantile(baserat_female$seed_germinate_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$seed_germinate_rating))),
                        
                        c(mean(baserat_female$quality_rating, na.rm=T),
                          min(baserat_female$quality_rating, na.rm=T),
                          max(baserat_female$quality_rating, na.rm=T),
                          sd(baserat_female$quality_rating, na.rm=T),
                          quantile(baserat_female$quality_rating, na.rm=T, 0.25),
                          quantile(baserat_female$quality_rating, na.rm=T, 0.75),
                          sum(!is.na (baserat_female$quality_rating))))

#prepping data for FE
#subsetting baseline dealers and considering only the required variables 
base<-baseline_dealer[c("maize.owner.agree.gender", "maize.owner.agree.age","maize.owner.agree.educ", "maize.owner.agree.q3", "maize.owner.agree.q4", "maize.owner.agree.q5",
                          "maize.owner.agree.q8", "maize.owner.agree.temp.q69", "maize.owner.agree.temp.q71", "maize.owner.agree.temp.q73", "maize.owner.agree.temp.q74", "maize.owner.agree.temp.q75",
                          "maize.owner.agree.temp.q78", "maize.owner.agree.temp.q79", "maize.owner.agree.temp.q80" , "maize.owner.agree.temp.q81", "maize.owner.agree.temp.q82", "maize.owner.agree.q96",
                          "maize.owner.agree.temp.q72", "maize.owner.index_practices_cap","maize.owner.index_efforts","maize.owner.shelflife_Caro","maize.owner.moistread", "maize.owner.saleprice","maize.owner.costseed",
                          "maize.owner.agree.q19","maize.owner.quanprovider","maize.owner.unada","maize.owner.practices_all","maize.owner.warning", "maize.owner.dist","maize.owner.lot",
                        "maize.owner.complaint","shop_ID")]
basem<-baseline_dealer[c( "maize.owner.agree.q3", "maize.owner.agree.q4", "maize.owner.agree.q8","shop_ID")] #subsetting baseline to populate midline with the constant variables 
mid<-midline_dealer[c("owner.agree.gender", "owner.agree.age","owner.agree.educ", "owner.agree.q5","owner.agree.temp.q69", "owner.agree.temp.q71", "owner.agree.temp.q73", "owner.agree.temp.q74", "owner.agree.temp.q75",
                        "owner.agree.temp.q78", "owner.agree.temp.q79", "owner.agree.temp.q80" , "owner.agree.temp.q81", "owner.agree.temp.q82", "owner.agree.q96",
                        "owner.agree.temp.q72", "owner.index_practices_cap","owner.index_efforts","owner.shelflife_Caro","owner.moistread","owner.saleprice",
                      "owner.costseed","owner.agree.q19","owner.quanprovider","owner.unada","owner.practices_all","owner.warning", "owner.dist","owner.lot","owner.complaint","shop_ID")] #subsetting midline dealers and considering only the required variables
names(mid) <- gsub(x = names(mid), pattern = "owner.", replacement = "maize.owner.") #variable matching with baseline  

midm<-merge(mid, basem, by="shop_ID") #merging both midline and baseline for FE

dealer_stack <- rbind(base, midm) #stacking dealer data from both periods 


#getting controls as averages for between dealers 
merged_dealer[merged_dealer==999] <- NA

#averaging moisture reading 
merged_dealer$moist <-  rowMeans(merged_dealer[c("maize.owner.moistread","owner.moistread")],na.rm=T) 

#averaging shelflife 
merged_dealer$shelflife<-  rowMeans(merged_dealer[c("maize.owner.shelflife_Caro","owner.shelflife_Caro")],na.rm=T)

#averaging efforts by dealer
merged_dealer$dealereffort<-  rowMeans(merged_dealer[c("maize.owner.index_efforts","owner.index_efforts")],na.rm=T)

#averaging capital intensive seed handling and storage practices 
merged_dealer$index_cap<-  rowMeans(merged_dealer[c("maize.owner.index_practices_cap","owner.index_practices_cap")],na.rm=T)

#averaging all handling and storage practices 
merged_dealer$index_allprac<-  rowMeans(merged_dealer[c("maize.owner.practices_all","owner.practices_all")],na.rm=T)

#averaging sales price 
merged_dealer$saleprice <-  rowMeans(merged_dealer[c("maize.owner.saleprice","owner.saleprice")],na.rm=T) 

#averaging cost of buying maize seed for dealers 
merged_dealer$costseed <-  rowMeans(merged_dealer[c("maize.owner.costseed","owner.costseed")],na.rm=T) 

#averaging number of hybrid maize seed varieties in stock 
merged_dealer$hybridnum <-  rowMeans(merged_dealer[c("maize.owner.agree.q19","owner.agree.q19")],na.rm=T) 

# #averaging number of times dealer ran out of stock -- longe 10H and longe 5 
# merged_dealer$stockfin <-  rowMeans(merged_dealer[c("maize.owner.stockfin","owner.stockfin")],na.rm=T) 

#averaging the quantity obtained from the provider by the dealer -- longe 10H and longe 5 
merged_dealer$quanprovider <-  rowMeans(merged_dealer[c("maize.owner.quanprovider","owner.quanprovider")],na.rm=T)

#averaging unada registration 
merged_dealer$unada <-  rowMeans(merged_dealer[c("maize.owner.unada","owner.unada")],na.rm=T)

#averaging warning as a result of inspection 
merged_dealer$warning <-  rowMeans(merged_dealer[c("maize.owner.warning","owner.warning")],na.rm=T)

#averaging distance between frmer and dealer 
merged_dealer$distance <-  rowMeans(merged_dealer[c("maize.owner.dist","owner.dist")],na.rm=T)

#age
merged_dealer$maize.owner.agree.age <-  rowMeans(merged_dealer[c("maize.owner.agree.age","owner.agree.age")],na.rm=T) #averaging age 

#education -- finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
merged_dealer$primbase<-0 #baseline 
merged_dealer$primbase[merged_dealer$maize.owner.agree.educ=="e"|merged_dealer$maize.owner.agree.educ=="f"] <- 1
merged_dealer$primmid<-0 #midline 
merged_dealer$primmid[merged_dealer$owner.agree.educ=="e"|merged_dealer$owner.agree.educ=="f"] <- 1
#averaging education 
merged_dealer$prim <-  rowMeans(merged_dealer[c("primbase","primmid")],na.rm=T) 
table(merged_dealer$prim)

table(merged_dealer$maize.owner.agree.q3) #distance to tarmac road in km, only in baseline 
table(merged_dealer$maize.owner.agree.q4) #distance to murram road in km, only in baseline 

#selling only farm inputs
merged_dealer$inputsale_base<- ifelse(merged_dealer$maize.owner.agree.q5== 'Yes', 1, 0) #baseline 
merged_dealer$inputsale_mid<- ifelse(merged_dealer$owner.agree.q5== 'Yes', 1, 0) #midline 
merged_dealer$inputsale<-  rowMeans(merged_dealer[c("inputsale_base","inputsale_mid")],na.rm=T) #averaging 

#Q8. When was this agro-input shop established? (year)  ---- only in baseline 
merged_dealer$years_shop <- 2022 - as.numeric(as.character(substr(merged_dealer$maize.owner.agree.q8, start=1, stop=4)))
table(merged_dealer$years_shop)

#seed stored in dedicated area?
merged_dealer$dedicated_areabase<- ifelse(merged_dealer$maize.owner.agree.temp.q69== 'Yes', 1, 0)   #baseline
table(merged_dealer$dedicated_areabase)
merged_dealer$dedicated_areamid<- ifelse(merged_dealer$owner.agree.temp.q69== 'Yes', 1, 0)   #midline
table(merged_dealer$dedicated_areamid)
merged_dealer$dedicated_area<-  rowMeans(merged_dealer[c("dedicated_areabase","dedicated_areamid")],na.rm=T) #averaging 

#problem with rats or pests?
#we try to formulate this as a good quality variable --- So, no problem with pests is good
merged_dealer$pest_probbase<- ifelse(merged_dealer$maize.owner.agree.temp.q71== 'No', 1, 0) #baseline 
merged_dealer$pest_probmid<- ifelse(merged_dealer$owner.agree.temp.q71== 'No', 1, 0)  #midline 
merged_dealer$pest_prob<-  rowMeans(merged_dealer[c("pest_probbase","pest_probmid")],na.rm=T) #averaging 
table(merged_dealer$pest_prob)

#roof insulated?
merged_dealer$insulated_base<- ifelse(merged_dealer$maize.owner.agree.temp.q73== 'Yes', 1, 0) #baseline 
merged_dealer$insulated_mid<- ifelse(merged_dealer$owner.agree.temp.q73== 'Yes', 1, 0) #midline 
merged_dealer$insulated<-  rowMeans(merged_dealer[c("insulated_base","insulated_mid")],na.rm=T) #averaging 
table(merged_dealer$insulated)

#walls insulated?
merged_dealer$wall_heatproofbase<- ifelse(merged_dealer$maize.owner.agree.temp.q74== 'Yes', 1, 0) #baseline 
merged_dealer$wall_heatproofmid<- ifelse(merged_dealer$owner.agree.temp.q74== 'Yes', 1, 0) #midline 
merged_dealer$wall_heatproof<-  rowMeans(merged_dealer[c("wall_heatproofbase","wall_heatproofmid")],na.rm=T) #averaging 
table(merged_dealer$wall_heatproof)

#area ventilated?
merged_dealer$ventilationbase<- ifelse(merged_dealer$maize.owner.agree.temp.q75== 'Yes', 1, 0) #baseline 
merged_dealer$ventilationmid<- ifelse(merged_dealer$owner.agree.temp.q75== 'Yes', 1, 0) #midline 
merged_dealer$ventilation<-  rowMeans(merged_dealer[c("ventilationbase","ventilationmid")],na.rm=T) #averaging 
table(merged_dealer$ventilation)

#Q78. Lighting conditions in area where seed is stored?
#we try to formulate this as good quality variable --- 2 is good lighting
merged_dealer$badlighting_base <- 0
merged_dealer$badlighting_base[merged_dealer$maize.owner.agree.temp.q78=="2"]<-1 #baseline 
merged_dealer$badlighting_mid <- 0
merged_dealer$badlighting_mid[merged_dealer$owner.agree.temp.q78=="2"]<-1 #midline 
merged_dealer$badlighting<-  rowMeans(merged_dealer[c("badlighting_base","badlighting_mid")],na.rm=T) #averaging 
table(merged_dealer$badlighting)

#Q79. On what surface are seed stored?
#we try to formulate this as good quality variable ---- 3,4,5 are good storage surfaces
merged_dealer$badstored_base <- 0 #baseline 
merged_dealer$badstored_base[merged_dealer$maize.owner.agree.temp.q79=="3"|merged_dealer$maize.owner.agree.temp.q79=="4"|merged_dealer$maize.owner.agree.temp.q79=="5"]<-1
merged_dealer$badstored_mid <- 0 #midline 
merged_dealer$badstored_mid[merged_dealer$owner.agree.temp.q79=="3"|merged_dealer$owner.agree.temp.q79=="4"|merged_dealer$owner.agree.temp.q79=="5"]<-1
merged_dealer$badstored<-  rowMeans(merged_dealer[c("badstored_base","badstored_mid")],na.rm=T) #averaging 
table(merged_dealer$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
#we try to formulate this as good quality variable --- Not being stored in open containers is good
merged_dealer$open_storage_base<- ifelse(merged_dealer$maize.owner.agree.temp.q80== 'No', 1, 0) #baseline 
merged_dealer$open_storage_mid<- ifelse(merged_dealer$owner.agree.temp.q80== 'No', 1, 0) #midline 
merged_dealer$open_storage<-  rowMeans(merged_dealer[c("open_storage_base","open_storage_mid")],na.rm=T) #averaging 
table(merged_dealer$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings or that the business is registered with some association)
merged_dealer$cert_yes_base<- ifelse(merged_dealer$maize.owner.agree.temp.q81== 'Yes', 1, 0) #baseline 
merged_dealer$cert_yes_mid<- ifelse(merged_dealer$owner.agree.temp.q81== 'Yes', 1, 0) #midline 
merged_dealer$cert_yes<-  rowMeans(merged_dealer[c("cert_yes_base","cert_yes_mid")],na.rm=T) #averaging 
table(merged_dealer$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
merged_dealer$shop_rate<-  rowMeans(merged_dealer[c("maize.owner.agree.temp.q82","owner.agree.temp.q82")],na.rm=T) #averaging 
table(merged_dealer$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
#we try to formulate this as good quality variable, not getting any complaint is good
#merged_dealer$complaint_base<- ifelse(merged_dealer$maize.owner.agree.q96== 'No', 1, 0)  #baseline 
#merged_dealer$complaint_mid<- ifelse(merged_dealer$owner.agree.q96== 'No', 1, 0)  #midline 
merged_dealer$complaint<-  rowMeans(merged_dealer[c("maize.owner.complaint","owner.complaint")],na.rm=T) #averaging 
table(merged_dealer$complaint)

#roof leak proof?
merged_dealer$leakproof_base<- ifelse(merged_dealer$maize.owner.agree.temp.q72== 'Yes', 1, 0)  #baseline 
merged_dealer$leakproof_mid<- ifelse(merged_dealer$owner.agree.temp.q72== 'Yes', 1, 0)  #midline 
merged_dealer$leakproof<-  rowMeans(merged_dealer[c("leakproof_base","leakproof_mid")],na.rm=T) #averaging 
table(merged_dealer$leakproof)

#random seed bag shows lot number
merged_dealer$lotnumber<-  rowMeans(merged_dealer[c("maize.owner.lot","owner.lot")],na.rm=T) #averaging 

#select only those that have same gender in baseline and endline
to_select <- merged_dealer$shop_ID[merged_dealer$maize.owner.agree.gender == merged_dealer$owner.agree.gender]

#stack rating dyads from the two rounds
rating_dyads <- rbind(rating_dyads, rating_dyads_midline)

#keep only those that have same gender in baseline and endline
rating_dyads <- subset(rating_dyads,shop_ID  %in% to_select)

ratFE <- subset(ratFE,shop_ID  %in% to_select)

rat<-rating_dyads #prepping rating_dyads for FE

#getting dealer characteristics for controls 
rating_dyads <- merge(merged_dealer, rating_dyads, by="shop_ID")


# convert to numbers and aggregate scores at agro-input dealer level for between regression -- also including controls 

rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating"
               ) ] <- lapply(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating"
                        )], function(x) as.numeric(as.character(x)))

rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")], function(x) replace(x, x == 98,NA) )

##descriptive statistics
rating_dyads <- subset(rating_dyads, !is.na(rating_dyads$general_rating))

rating_dyads$ones <- 1
max(tapply(rating_dyads$ones,rating_dyads$shop_ID, sum))


min(tapply(rating_dyads$ones,rating_dyads$shop_ID, sum))
mean(tapply(rating_dyads$ones,rating_dyads$shop_ID, sum))
max(tapply(rating_dyads$ones,rating_dyads$farmer_ID, sum))
min(tapply(rating_dyads$ones,rating_dyads$farmer_ID, sum))
mean(tapply(rating_dyads$ones,rating_dyads$farmer_ID, sum))

#create averages for between regression--- also including controls 

#shop_av <- aggregate(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating",
 #                                   "maize.owner.agree.age","prim","maize.owner.agree.q3", "maize.owner.agree.q4",  "inputsale", "years_shop", "dedicated_area", "pest_prob", "insulated", "wall_heatproof", "ventilation", "badlighting", "badstored", "open_storage", "cert_yes", "shop_rate", "complaint", "leakproof")],list(rating_dyads$shop_ID),FUN=mean, na.rm=T)
shop_av <- aggregate(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating"
                                 )],list(rating_dyads$shop_ID),FUN=mean, na.rm=T)


names(shop_av)[1] <- "shop_ID"
#merge in dealer gender and controls 
rating_dyads <- merge(shop_av, merged_dealer[c("shop_ID","maize.owner.agree.gender", "dealereffort", "index_cap", "shelflife", "moist", "saleprice", "costseed","hybridnum","quanprovider","unada", "index_allprac","warning", "distance",
                                               "maize.owner.agree.age","prim","maize.owner.agree.q3", "maize.owner.agree.q4",  "inputsale", "years_shop", "dedicated_area", "pest_prob", "insulated", "wall_heatproof", "ventilation", "badlighting", "badstored", "open_storage", "cert_yes", "shop_rate", "complaint", "lotnumber","leakproof")],  by.x="shop_ID", by.y="shop_ID", all.x=T)

#rename
names(rating_dyads)[names(rating_dyads) == "maize.owner.agree.gender"] <- "gender"

rating_dyads$overall_rating <-  rowMeans(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating")],na.rm=T)
rating_dyads$score  <-  rowMeans(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm=T)


# rating_dyads$prim[rating_dyads$prim==0.5] <- NA
# rating_dyads$inputsale[rating_dyads$inputsale==0.5] <- NA
# rating_dyads$dedicated_area[rating_dyads$dedicated_area==0.5] <- NA
# rating_dyads$pest_prob[rating_dyads$pest_prob==0.5] <- NA
# rating_dyads$insulated[rating_dyads$insulated==0.5] <- NA
# rating_dyads$wall_heatproof[rating_dyads$wall_heatproof==0.5] <- NA
# rating_dyads$ventilation[rating_dyads$ventilation==0.5] <- NA
# rating_dyads$badlighting[rating_dyads$badlighting==0.5] <- NA
# rating_dyads$badstored[rating_dyads$badstored==0.5] <- NA
# rating_dyads$open_storage[rating_dyads$open_storage==0.5] <- NA
# rating_dyads$cert_yes[rating_dyads$cert_yes==0.5] <- NA
# 
# rating_dyads$complaint[rating_dyads$complaint==0.5] <- NA
# rating_dyads$leakproof[rating_dyads$leakproof==0.5] <- NA


#regressions without controls -- between regression 
#seed ratings 
summary(lm(score~gender,data=rating_dyads))
summary(lm(seed_quality_general_rating~gender,data=rating_dyads))
summary(lm(seed_yield_rating~gender,data=rating_dyads))
summary(lm(seed_drought_rating~gender,data=rating_dyads))
summary(lm(seed_disease_rating~gender,data=rating_dyads))
summary(lm(seed_maturing_rating~gender,data=rating_dyads))
summary(lm(seed_germinate_rating~gender,data=rating_dyads))

#storing seed ratings 
s5<- rbind(c((format(round(sum((lm(score~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3))),
           c((format(round(sqrt(diag(vcov(lm(score~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3))),
             
           c((format(round(summary(lm(score~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3))),
         
           c((format(round(sum((lm(score~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3))),
           c((format(round(sqrt(diag(vcov(lm(score~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3))),
           c((format(round(summary(lm(score~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3))),
            
           c((format(round(nobs(lm(score~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_quality_general_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_yield_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_drought_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_disease_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_maturing_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_germinate_rating~gender,data=rating_dyads)),digits=0),nsmall=0)))
)

#dealer ratings 
summary(lm(overall_rating~gender,data=rating_dyads))
summary(lm(general_rating~gender,data=rating_dyads))
summary(lm(location_rating~gender,data=rating_dyads))
summary(lm(price_rating~gender,data=rating_dyads))
summary(lm(quality_rating~gender,data=rating_dyads))
summary(lm(stock_rating~gender,data=rating_dyads))
summary(lm(reputation_rating~gender,data=rating_dyads))



#storing dealer ratings 
s7<- rbind(c((format(round(sum((lm(overall_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(location_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(price_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(quality_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(stock_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3))),
           
           c((format(round(sum((lm(overall_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(location_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(price_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(quality_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(stock_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3))),
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3))),
           
           c((format(round(nobs(lm(overall_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(general_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(location_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(price_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(quality_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(stock_rating~gender,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(reputation_rating~gender,data=rating_dyads)),digits=0),nsmall=0)))
)

#regressions with controls -- between regression 
#seed ratings


#regressions with the controls --- lot number and complaint 

#summary(lm(score~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads)) #118
#summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))    #118
#summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))    #118
#summary(lm(seed_drought_rating ~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))  #116
#summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))  #118
#summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim+moist+shelflife,data=rating_dyads))  #117
#summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))  #118

#summary(lm(score~gender+ maize.owner.agree.age +prim +lotnumber + complaint,data=rating_dyads)) 
#summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +lotnumber + complaint,data=rating_dyads))    
#summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +lotnumber + complaint,data=rating_dyads))   
#summary(lm(seed_drought_rating ~gender+ maize.owner.agree.age +prim +lotnumber + complaint,data=rating_dyads))
#summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +lotnumber + complaint,data=rating_dyads))  
#summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim+lotnumber + complaint,data=rating_dyads))  
#summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +lotnumber + complaint,data=rating_dyads))  


summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)) 
summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))    
summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))   
summary(lm(seed_drought_rating ~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))
summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))  
summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim+index_allprac,data=rating_dyads))  
summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim+index_allprac,data=rating_dyads))  



# #number of observations 
# nobs(lm(score~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads)) #118
# nobs(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))    #118
# nobs(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))    #118
# nobs(lm(seed_drought_rating ~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))  #116
# nobs(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))  #118
# nobs(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim+moist+shelflife,data=rating_dyads))  #117
# nobs(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +moist+shelflife,data=rating_dyads))  #118


# 
# summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
#              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))
# 
# summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
#              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))
# 
# summary(lm(seed_yield_rating ~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
#              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))
# 
# summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
#              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof,data=rating_dyads))
# 
# summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
#              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))
# 
# summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
#              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))
# 
# summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
#              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof,data=rating_dyads))

#storing seed ratings 
s6<- rbind(c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac, data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3))),
           
          
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac, data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac, data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac, data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac, data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3))),
           
           
    
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim+index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim+index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3))),
           
           c((format(round(nobs(lm(score~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)))
)




#dealer ratings 
#summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap +distance+saleprice +costseed +lotnumber+complaint+hybridnum+quanprovider
#           +years_shop+unada ,data=rating_dyads))

summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
           +years_shop+unada ,data=rating_dyads))



summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))
#shop rating coeff is negative here which means if the shop rating is lower, the general rating is higher 

summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))

summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed ,data=rating_dyads))
#cost of seed from provider coeff is positive here which means if the cost of seed is higher, the price rating is higher 

summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))


summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))

summary(lm(reputation_rating ~gender+ maize.owner.agree.age +prim +years_shop+unada ,data=rating_dyads))



#storing dealer ratings
s8<- rbind(c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[1]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))))[1],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))))[1],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=3))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[2]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))))[2],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))))[2],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=3))),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[3]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))))[3],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))))[3],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=3))),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[4]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))))[4],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))))[4],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=3))),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[5]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))))[5],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))))[5],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=3))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[6]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[6]),digits=3),nsmall=3)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[6]),digits=3),nsmall=3)),
             0,
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[6]),digits=3),nsmall=3)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[6]),digits=3),nsmall=3))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[6],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))))[6],digits=3),nsmall=3)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))))[6],digits=3),nsmall=3)),
             0,
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))))[6],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))))[6],digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=3)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=3)),
             0,
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=3))),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[7]),digits=3),nsmall=3)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[7]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[7],digits=3),nsmall=3)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))))[7],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[8]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[8],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[9]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[9],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[10]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[10],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[11]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[11],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[12]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[12],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada ,data=rating_dyads))$coefficients[13]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                             +years_shop+unada ,data=rating_dyads))))[13],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
           
           
           
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$r.squared,digits=3),nsmall=3))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                      +years_shop+unada,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads))$adj.r.squared,digits=3),nsmall=3))),
           
           c((format(round(nobs(lm(overall_rating~gender+ maize.owner.agree.age +prim+ +dealereffort+distance+saleprice +costseed +index_allprac+hybridnum+quanprovider
                                   +years_shop+unada,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(general_rating~gender+ maize.owner.agree.age +prim+ shop_rate+dealereffort+index_cap,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(location_rating~gender+ maize.owner.agree.age +prim +distance,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(price_rating~gender+ maize.owner.agree.age +prim+ saleprice +costseed,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(quality_rating~gender+ maize.owner.agree.age +prim +index_allprac,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(stock_rating~gender+ maize.owner.agree.age +prim +hybridnum+quanprovider,data=rating_dyads)),digits=0),nsmall=0)),
             (format(round(nobs(lm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+unada,data=rating_dyads)),digits=0),nsmall=0))))


############## FIXED EFFECTS 

ratFE<-merge(ratFE, base, by="shop_ID")
ratFEmid <- subset(rating_dyads_midline,shop_ID  %in% to_select)
ratFEmid<-merge(ratFEmid,midm, by="shop_ID")

rat <- rbind(ratFE, ratFEmid)


#merge to get gender --- merging both stacked datasets 
#rat <- merge(rat, dealer_stack, by="shop_ID")

#changing variable name 
names(rat)[names(rat) == "maize.owner.agree.gender"] <- "gender"

rat[rat=="n/a"]<- NA 
rat[rat=="98"]<- NA 
rat[rat=="999"]<- NA 

rat$dealer_ID <- 1:nrow(rat) #creating dealer ID, otherwise there will be couple duplicates of farmer and dealer IDs and plm won't run 

#creating numeric ratings 
rat$general_rating<-as.numeric(rat$general_rating)
rat$location_rating<-as.numeric(rat$location_rating)
rat$price_rating<-as.numeric(rat$price_rating)
rat$quality_rating<-as.numeric(rat$quality_rating)
rat$stock_rating<-as.numeric(rat$stock_rating)
rat$reputation_rating<-as.numeric(rat$reputation_rating)

rat$seed_quality_general_rating<-as.numeric(rat$seed_quality_general_rating)
rat$seed_yield_rating<-as.numeric(rat$seed_yield_rating)
rat$seed_drought_rating<-as.numeric(rat$seed_drought_rating)
rat$seed_disease_rating<-as.numeric(rat$seed_disease_rating)
rat$seed_maturing_rating<-as.numeric(rat$seed_maturing_rating)
rat$seed_germinate_rating<-as.numeric(rat$seed_germinate_rating)

rat$overall_rating <-  rowMeans(rat[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating")],na.rm=T) #overall dealer rating 
rat$score  <-  rowMeans(rat[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm=T)    #overall seed rating 


#checking the controls 
table(rat$maize.owner.agree.age) #age

table(rat$maize.owner.agree.educ)  #education
rat$prim<-0
#finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
rat$prim[rat$maize.owner.agree.educ=="e"|rat$maize.owner.agree.educ=="f"] <- 1
rat$prim[rat$maize.owner.agree.educ=="g"]<- NA  #since g is other 
table(rat$prim)   #finished secondary education 

table(rat$maize.owner.agree.q3) #distance to tarmac road in km

table(rat$maize.owner.agree.q4) #distance to murram road in km

#selling only farm inputs
table(rat$maize.owner.agree.q5)
rat$inputsale<- ifelse(rat$maize.owner.agree.q5== 'Yes', 1, 0)
table(rat$inputsale)

#Q8. When was this agro-input shop established? (year)
rat$years_shop <- 2020 - as.numeric(as.character(substr(rat$maize.owner.agree.q8, start=1, stop=4)))
table(rat$years_shop)

#seed stored in dedicated area?
rat$maize.owner.agree.q69
rat$dedarea<-as.character(rat$maize.owner.agree.temp.q69)
rat$dedicated_area<- ifelse(rat$dedarea== 'Yes', 1, 0)
table(rat$dedicated_area)

#problem with rats or pests?
#we try to formulate this as a good quality variable --- So, no problem with pests is good
rat$maize.owner.agree.q71
rat$pest<-as.character(rat$maize.owner.agree.temp.q71)
rat$pest_prob<- ifelse(rat$pest== 'No', 1, 0)
table(rat$pest_prob)

#roof insulated?
rat$maize.owner.agree.q73
rat$roof_insu<-as.character(rat$maize.owner.agree.temp.q73)
rat$insulated<- ifelse(rat$roof_insu== 'Yes', 1, 0)
table(rat$insulated)

#walls insulated?
rat$maize.owner.agree.q74
rat$wall_insu<-as.character(rat$maize.owner.agree.temp.q74)
rat$wall_heatproof<- ifelse(rat$wall_insu== 'Yes', 1, 0)
table(rat$wall_heatproof)

#area ventilated?
rat$maize.owner.agree.q75
rat$vent<-as.character(rat$maize.owner.agree.temp.q75)
rat$ventilation<- ifelse(rat$vent== 'Yes', 1, 0)
table(rat$ventilation)

#Q78. Lighting conditions in area where seed is stored?
#we try to formulate this as good quality variable --- 2 is good lighting
rat$badlighting <- 0
rat$badlighting[rat$maize.owner.agree.temp.q78=="2"]<-1
table(rat$badlighting)

#Q79. On what surface are seed stored?
#we try to formulate this as good quality variable ---- 3,4,5 are good storage surfaces
rat$badstored <- 0
rat$badstored[rat$maize.owner.agree.temp.q79=="3"|rat$maize.owner.agree.temp.q79=="4"|rat$maize.owner.agree.temp.q79=="5"]<-1
rat$badstored[rat$maize.owner.agree.temp.q79==96]<-NA
table(rat$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
#we try to formulate this as good quality variable --- Not being stored in open containers is good
rat$maize.owner.agree.q80
rat$open<-as.character(rat$maize.owner.agree.temp.q80)
rat$open_storage<- ifelse(rat$open== 'No', 1, 0)
table(rat$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings or that the business is registered with some association)
rat$maize.owner.agree.q81
rat$cert<-as.character(rat$maize.owner.agree.temp.q81)
rat$cert_yes<- ifelse(rat$cert== 'Yes', 1, 0)
table(rat$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
rat$shop_rate<-as.numeric(as.character(rat$maize.owner.agree.temp.q82))
table(rat$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
#we try to formulate this as good quality variable, not getting any complaint is good
table(rat$maize.owner.agree.q96)
rat$complaint<- ifelse(rat$maize.owner.agree.q96== 'No', 1, 0)
table(rat$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
table(rat$maize.owner.agree.q70)

#roof leak proof?
rat$maize.owner.agree.q72
rat$roof<-as.character(rat$maize.owner.agree.temp.q72)
rat$leakproof<- ifelse(rat$roof== 'Yes', 1, 0)
table(rat$leakproof)


###checking for shelflife and moisture for women and men
summary(rat$maize.owner.moistread[rat$gender=="Female"])
summary(rat$maize.owner.moistread[rat$gender=="Male"])

summary(rat$maize.owner.shelflife_Caro[rat$gender=="Female"])
summary(rat$maize.owner.shelflife_Caro[rat$gender=="Male"])

#FE at farmer level --- without controls 
#dealer ratings 

plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(overall_rating~gender+farmer_ID, data = rat))

plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(general_rating~gender+farmer_ID, data = rat))

plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(location_rating~gender+farmer_ID, data = rat))

plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(price_rating~gender+farmer_ID, data = rat))

plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(quality_rating~gender+farmer_ID, data = rat))

plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(stock_rating~gender+farmer_ID, data = rat))

plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(reputation_rating~gender+farmer_ID, data = rat))


fe3<- rbind( c((format(round(mean(fixef(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3))),
             c((format(round(sum((plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3))),
             c((format(round((coeftest(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=3)),
               (format(round((coeftest(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=3)),
               (format(round((coeftest(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=3)),
               (format(round((coeftest(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=3)),
               (format(round((coeftest(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=3)),
               (format(round((coeftest(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=3)),
               (format(round((coeftest(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=3))),
             
             c((format(round((coeftest(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=3)),
               (format(round((coeftest(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=3)),
               (format(round((coeftest(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=3)),
               (format(round((coeftest(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=3)),
               (format(round((coeftest(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=3)),
               (format(round((coeftest(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=3)),
               (format(round((coeftest(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=3))),
             
             
             c((format(round(summary(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3))),
             
             c((format(round(summary(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3))),
             
             c((format(round(nobs(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)))
)


#seed ratings -- without controls 

plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(score~gender+farmer_ID, data = rat))

plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_quality_general_rating~gender+farmer_ID, data = rat))

plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_yield_rating~gender+farmer_ID, data = rat))

plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_drought_rating~gender+farmer_ID, data = rat))

plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_disease_rating~gender+farmer_ID, data = rat))

plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_maturing_rating~gender+farmer_ID, data = rat))

plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_germinate_rating~gender+farmer_ID, data = rat))



fe1<- rbind( c((format(round(mean(fixef(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3)),
               (format(round(mean(fixef(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=3))),
             c((format(round(sum((plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3)),
               (format(round(sum((plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=3))),
             c((format(round((coeftest(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
[1,2],digits=3),nsmall=3)),
(format(round((coeftest(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=3)),
(format(round((coeftest(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=3)),
(format(round((coeftest(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=3)),
(format(round((coeftest(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=3)),
(format(round((coeftest(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=3)),
(format(round((coeftest(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=3))),

c((format(round((coeftest(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=3)),
  (format(round((coeftest(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=3)),
  (format(round((coeftest(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=3)),
  (format(round((coeftest(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=3)),
  (format(round((coeftest(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=3)),
  (format(round((coeftest(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=3)),
  (format(round((coeftest(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=3))),
             
             
             c((format(round(summary(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=3))),
           
  c((format(round(summary(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3)),
               (format(round(summary(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=3))),
           
  c((format(round(nobs(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)),
               (format(round(nobs(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=0),nsmall=0)))
)


#FE at farmer level --- with controls
#dealer ratings

plm1<-plm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.index_efforts+maize.owner.dist+maize.owner.saleprice+maize.owner.costseed
          +maize.owner.practices_all+maize.owner.agree.q19+maize.owner.quanprovider+years_shop+maize.owner.unada, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.index_efforts+maize.owner.dist+maize.owner.saleprice+maize.owner.costseed
            +maize.owner.practices_all+maize.owner.agree.q19+maize.owner.quanprovider+years_shop+maize.owner.unada, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))

plm2<-plm(general_rating~gender+ maize.owner.agree.age +prim +shop_rate+maize.owner.index_efforts+maize.owner.index_practices_cap, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(general_rating~gender+ maize.owner.agree.age +prim +shop_rate+maize.owner.index_efforts+maize.owner.index_practices_cap, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))

plm3<-plm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.dist, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.dist, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))

plm4<-plm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.saleprice+maize.owner.costseed, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.saleprice+maize.owner.costseed, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))

plm5<-plm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


plm6<-plm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q19+maize.owner.quanprovider, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q19+maize.owner.quanprovider, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))

plm7<-plm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+maize.owner.unada, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(reputation_rating~gender+ maize.owner.agree.age +prim +years_shop+maize.owner.unada, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


#storing dealer ratings
fe4<- rbind(c(0,
              0,
             0,
             0,
             0,
             0,
             0),
  
            c(0,
              0,
              0,
              0,
              0,
              0,
              0),
              
            c(0,
              0,
              0,
              0,
              0,
              0,
              0),
  
  
  c((format(round(sum((plm1)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm2)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm3)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm4)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm5)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm6)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm7)$coefficients[1]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3))),

            c((format(round(sum((plm1)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm2)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm3)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm4)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm5)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm6)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm7)$coefficients[2]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3))),

            c((format(round(sum((plm1)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm2)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm3)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm4)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm5)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm6)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm7)$coefficients[3]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3))),

            c((format(round(sum((plm1)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm2)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm3)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm4)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm5)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm6)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm7)$coefficients[4]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3))),

            c((format(round(sum((plm1)$coefficients[5]),digits=3),nsmall=3)),
              (format(round(sum((plm2)$coefficients[5]),digits=3),nsmall=3)),
             0,
              (format(round(sum((plm4)$coefficients[5]),digits=3),nsmall=3)),
            0,
              (format(round(sum((plm6)$coefficients[5]),digits=3),nsmall=3)),
              (format(round(sum((plm7)$coefficients[5]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
            0,
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
              0,
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3))),

            c((format(round(sum((plm1)$coefficients[6]),digits=3),nsmall=3)),
              (format(round(sum((plm2)$coefficients[6]),digits=3),nsmall=3)),
             0,
             0,
              0,
            0,
            0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=3)),
             0,
              0,
             0,
             0,
             0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=3)),
             0,
             0,
              0,
            0,
            0),

            c((format(round(sum((plm1)$coefficients[7]),digits=3),nsmall=3)),
             0,
              0,
              0,
              0,
              0,
              0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=3)),
             0,
             0,
            0,
             0,
            0,
            0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
              0,
             0),


            c((format(round(sum((plm1)$coefficients[8]),digits=3),nsmall=3)),
             0,
             0,
             0,
             0,
             0,
             0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=3)),
              0,
             0,
             0,
             0,
           0,
             0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=3)),
              0,
             0,
              0,
             0,
            0,
            0),

            c((format(round(sum((plm1)$coefficients[9]),digits=3),nsmall=3)),
             0,
             0,
              0,
             0,
              0,
             0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=3)),
              0,
             0,
              0,
             0,
            0,
            0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=3)),
              0,
             0,
              0,
             0,
              0,
             0),

            c((format(round(sum((plm1)$coefficients[10]),digits=3),nsmall=3)),
           0,
             0,
             0,
           0,
              0,
              0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=3)),
             0,
              0,
             0,
             0,
              0,
             0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=3)),
             0,
             0,
              0,
             0,
            0,
             0),

            c((format(round(sum((plm1)$coefficients[11]),digits=3),nsmall=3)),
            0,
              0,
              0,
            0,
             0,
           0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=3)),
              0,
              0,
              0,
              0,
              0,
             0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=3)),
              0,
              0,
             0,
             0,
             0,
            0),


            c((format(round(sum((plm1)$coefficients[12]),digits=3),nsmall=3)),
              0,
             0,
              0,
             0,
             0,
              0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=3)),
              0,
             0,
             0,
             0,
             0,
             0),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=3)),
              0,
              0,
             0,
             0,
             0,
            0),

        


              c((format(round(summary(plm1)$r.squared[1],digits=3),nsmall=3)),
                (format(round(summary(plm2)$r.squared[1],digits=3),nsmall=3)),
                (format(round(summary(plm3)$r.squared[1],digits=3),nsmall=3)),
                (format(round(summary(plm4)$r.squared[1],digits=3),nsmall=3)),
                (format(round(summary(plm5)$r.squared[1],digits=3),nsmall=3)),
                (format(round(summary(plm6)$r.squared[1],digits=3),nsmall=3)),
                (format(round(summary(plm7)$r.squared[1],digits=3),nsmall=3))),
            c((format(round(summary(plm1)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm2)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm3)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm4)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm5)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm6)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm7)$r.squared[2],digits=3),nsmall=3))),
              c((format(round(nobs(plm1),digits=0),nsmall=0)),
                (format(round(nobs(plm2),digits=0),nsmall=0)),
                (format(round(nobs(plm3),digits=0),nsmall=0)),
                (format(round(nobs(plm4),digits=0),nsmall=0)),
                (format(round(nobs(plm5),digits=0),nsmall=0)),
                (format(round(nobs(plm6),digits=0),nsmall=0)),
                (format(round(nobs(plm7),digits=0),nsmall=0))))



#FE at farmer level --- with controls
#seed ratings

plm8<-plm(score~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(score~gender+ maize.owner.agree.age +prim+maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


plm9<-plm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim+maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim+maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


plm10<-plm(seed_yield_rating~gender+ maize.owner.agree.age +prim+maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


plm11<-plm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


plm12<-plm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


plm13<-plm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(seed_maturing_rating~gender+ maize.owner.agree.age +prim+maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))


plm14<-plm(seed_germinate_rating~gender+ maize.owner.agree.age +prim+maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(plm(seed_germinate_rating~gender+ maize.owner.agree.age +prim+maize.owner.practices_all, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))



#storing seed ratings
fe2<- rbind(c((format(round(mean(fixef(plm8))[1],digits=3),nsmall=3)),
              (format(round(mean(fixef(plm9))[1],digits=3),nsmall=3)),
              (format(round(mean(fixef(plm10))[1],digits=3),nsmall=3)),
              (format(round(mean(fixef(plm11))[1],digits=3),nsmall=3)),
              (format(round(mean(fixef(plm12))[1],digits=3),nsmall=3)),
              (format(round(mean(fixef(plm13))[1],digits=3),nsmall=3)),
              (format(round(mean(fixef(plm14))[1],digits=3),nsmall=3))),
            c((format(round(sum((plm8)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm9)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm10)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm11)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm12)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm13)$coefficients[1]),digits=3),nsmall=3)),
              (format(round(sum((plm14)$coefficients[1]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=3))),

            c((format(round(sum((plm8)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm9)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm10)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm11)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm12)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm13)$coefficients[2]),digits=3),nsmall=3)),
              (format(round(sum((plm14)$coefficients[2]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=3))),

            c((format(round(sum((plm8)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm9)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm10)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm11)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm12)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm13)$coefficients[3]),digits=3),nsmall=3)),
              (format(round(sum((plm14)$coefficients[3]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=3))),

            c((format(round(sum((plm8)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm9)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm10)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm11)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm12)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm13)$coefficients[4]),digits=3),nsmall=3)),
              (format(round(sum((plm14)$coefficients[4]),digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=3))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=3))),

            # c((format(round(sum((plm8)$coefficients[5]),digits=3),nsmall=3)),
            #   (format(round(sum((plm9)$coefficients[5]),digits=3),nsmall=3)),
            #   (format(round(sum((plm10)$coefficients[5]),digits=3),nsmall=3)),
            #   (format(round(sum((plm11)$coefficients[5]),digits=3),nsmall=3)),
            #   (format(round(sum((plm12)$coefficients[5]),digits=3),nsmall=3)),
            #   (format(round(sum((plm13)$coefficients[5]),digits=3),nsmall=3)),
            #   (format(round(sum((plm14)$coefficients[5]),digits=3),nsmall=3))),
            # c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=3))),
            # c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3)),
            #   (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=3))),

           


            c((format(round(summary(plm8)$r.squared[1],digits=3),nsmall=3)),
              (format(round(summary(plm9)$r.squared[1],digits=3),nsmall=3)),
              (format(round(summary(plm10)$r.squared[1],digits=3),nsmall=3)),
              (format(round(summary(plm11)$r.squared[1],digits=3),nsmall=3)),
              (format(round(summary(plm12)$r.squared[1],digits=3),nsmall=3)),
              (format(round(summary(plm13)$r.squared[1],digits=3),nsmall=3)),
              (format(round(summary(plm14)$r.squared[1],digits=3),nsmall=3))),
            c((format(round(summary(plm8)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm9)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm10)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm11)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm12)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm13)$r.squared[2],digits=3),nsmall=3)),
              (format(round(summary(plm14)$r.squared[2],digits=3),nsmall=3))),
            c((format(round(nobs(plm8),digits=0),nsmall=0)),
              (format(round(nobs(plm9),digits=0),nsmall=0)),
              (format(round(nobs(plm10),digits=0),nsmall=0)),
              (format(round(nobs(plm11),digits=0),nsmall=0)),
              (format(round(nobs(plm12),digits=0),nsmall=0)),
              (format(round(nobs(plm13),digits=0),nsmall=0)),
              (format(round(nobs(plm14),digits=0),nsmall=0)))
)



##################################################################################################################
#rm(list=ls())

#baseline_deal <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv",sep="/"))
#baseline_deal=subset(baseline_dealers,clearing==T)
#write.csv(baseline_deal,"perceptions/data_seed_systems/data/input_dealer/baseline_deal.csv",row.names = F)

baseline_dealers <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv",sep="/"))
baseline_dealers=subset(baseline_dealers,clearing==T)
baseline_farmers <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/farmer/baseline_farmers.csv",sep="/"))
baseline_farmers=subset(baseline_farmers,Check2.check.maize.clearing==T)

#merge in more data

###
#A# RATINGS (only CH treatment farmers rated CH treatment dealers at baseline)
###

#BASELINE

#farmers
rating_dyads_des <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/farmer/rating_dyads.csv",sep="/"))

rating_dyads_des[rating_dyads_des=="n/a"] <- NA
rating_dyads_des[rating_dyads_des==98] <- NA

rating_dyads_des$bought_last_season <- ifelse(rating_dyads_des$bought_last_season=="Yes",1,0)

as_numeric <- c("bought_last_season","general_rating","location_rating","price_rating","quality_rating"
                ,"stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating"
                ,"seed_drought_rating","seed_disease_rating","seed_maturing_rating"
                ,"seed_germinate_rating")
rating_dyads_des[as_numeric] <- lapply(rating_dyads_des[as_numeric],function(x)as.numeric(as.character(x)))

rating_dyads_des_aggr_F <- aggregate(rating_dyads_des,by=list(rating_dyads_des$farmer_ID),FUN="mean",na.rm=T)
rating_dyads_des_aggr_F = rating_dyads_des_aggr_F[c("Group.1","bought_last_season","general_rating"
                                                    ,"location_rating","price_rating","quality_rating"
                                                    ,"stock_rating","reputation_rating"
                                                    ,"seed_quality_general_rating","seed_yield_rating"
                                                    ,"seed_drought_rating","seed_disease_rating"
                                                    ,"seed_maturing_rating","seed_germinate_rating")]

baseline_farmers <- merge(baseline_farmers,rating_dyads_des_aggr_F,by.x="farmer_ID",by.y="Group.1",all.x=TRUE)

#dealers (again)
rating_dyads_des_aggr_D <- aggregate(rating_dyads_des,by=list(rating_dyads_des$shop_ID),FUN="mean",na.rm=T)
rating_dyads_des_aggr_D = rating_dyads_des_aggr_D[c("Group.1","bought_last_season","general_rating"
                                                    ,"location_rating","price_rating","quality_rating"
                                                    ,"stock_rating","reputation_rating"
                                                    ,"seed_quality_general_rating","seed_yield_rating"
                                                    ,"seed_drought_rating","seed_disease_rating"
                                                    ,"seed_maturing_rating","seed_germinate_rating")]

baseline_dealers <- merge(baseline_dealers,rating_dyads_des_aggr_D,by.x="shop_ID",by.y="Group.1",all.x=TRUE)

baseline_dealers$general <- baseline_dealers$seed_quality_general_rating #because I've previously worked with reviews_seed by Bjorn with different variable names
baseline_dealers$yield <- baseline_dealers$seed_yield_rating
baseline_dealers$drought_resistent <- baseline_dealers$seed_drought_rating
baseline_dealers$disease_resistent <- baseline_dealers$seed_disease_rating
baseline_dealers$early_maturing <- baseline_dealers$seed_maturing_rating
baseline_dealers$germination <- baseline_dealers$seed_germinate_rating

###
#B# SERVICES (CH treatment farmers were asked during baseline, CH control farmers were asked during CH rating dissemination)
###

#BASELINE
dealer_services_dyads <- read.csv(paste(path_2,"perceptions/data_seed_systems/data/farmer/dealer_services_dyads.csv",sep="/"))

dealer_services_dyads[dealer_services_dyads=="n/a"] <- NA
dealer_services_dyads[dealer_services_dyads==98] <- NA

dealer_services_dyads$knows_dealer <- ifelse(dealer_services_dyads$knows_dealer=="Yes",1,0)
dealer_services_dyads$bought_at_dealer<-ifelse(dealer_services_dyads$bought_at_dealer=="Yes",1,0)
dealer_services_dyads$customer_years <- 2021 - as.numeric(as.character(substr(dealer_services_dyads$duration_customer,start=1,stop=4))) #dealer baseline data collection in 2020, CH rating dissemination in 2021
dealer_services_dyads$knows_other_customer<-ifelse(dealer_services_dyads$knows_other_customer=="Yes",1,0)
dealer_services_dyads$refunds<-ifelse(dealer_services_dyads$refunds=="Yes",1,0)
dealer_services_dyads$gives_credit<-ifelse(dealer_services_dyads$gives_credit=="Yes",1,0)
dealer_services_dyads$gives_advice<-ifelse(dealer_services_dyads$gives_advice=="Yes",1,0)
dealer_services_dyads$delivers<-ifelse(dealer_services_dyads$delivers=="Yes",1,0)
dealer_services_dyads$after_sales_service<-ifelse(dealer_services_dyads$after_sales_service=="Yes",1,0)
dealer_services_dyads$payment_mehtods<-ifelse(dealer_services_dyads$payment_mehtods=="Yes",1,0)
dealer_services_dyads$small_quant<-ifelse(dealer_services_dyads$small_quant=="Yes",1,0)

as_numeric <- c("knows_dealer","bought_at_dealer","customer_years","knows_other_customer","refunds"
                ,"gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods"
                ,"small_quant")
dealer_services_dyads[as_numeric] <- lapply(dealer_services_dyads[as_numeric],function(x)as.numeric(as.character(x)))

#dealers
dealer_services_dyads_aggr_D <- aggregate(dealer_services_dyads,by=list(dealer_services_dyads$shop_ID),FUN="mean",na.rm=T)
dealer_services_dyads_aggr_D = dealer_services_dyads_aggr_D[c("Group.1","knows_dealer"
                                                              ,"bought_at_dealer","customer_years"
                                                              ,"knows_other_customer","refunds"
                                                              ,"gives_credit","gives_advice"
                                                              ,"delivers","after_sales_service"
                                                              ,"payment_mehtods","small_quant")]
baseline_dealers <- merge(baseline_dealers,dealer_services_dyads_aggr_D,by.x="shop_ID",by.y="Group.1",all.x=TRUE)

#farmers
dealer_services_dyads_aggr_F <- aggregate(dealer_services_dyads,by=list(dealer_services_dyads$farmer_ID),FUN="mean",na.rm=T)
dealer_services_dyads_aggr_F = dealer_services_dyads_aggr_F[c("Group.1","knows_dealer"
                                                              ,"bought_at_dealer","customer_years"
                                                              ,"knows_other_customer","refunds"
                                                              ,"gives_credit","gives_advice"
                                                              ,"delivers","after_sales_service"
                                                              ,"payment_mehtods","small_quant")]
baseline_farmers <- merge(baseline_farmers, dealer_services_dyads_aggr_F, by.x="farmer_ID", by.y="Group.1", all.x = TRUE)

###################################################
#####DESCRIPTIVE STATISTICS + DATA EXPLORATION#####
###################################################

###################################################
#####Descriptive statistics: agro-input dealer#####
###################################################

df_descriptives_dealer <- array(NA,dim=c(120,5))

###variable transformation###
baseline_dealers$maize.owner.agree.gender<-ifelse(baseline_dealers$maize.owner.agree.gender=="Male",1,0)

baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="a"] <- 0
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="b"] <- 0
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="c"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="d"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="e"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="f"] <- 1
baseline_dealers$finished_primary[baseline_dealers$maize.owner.agree.educ=="g"] <- NA

baseline_dealers$prim <- FALSE
baseline_dealers$prim <- (baseline_dealers$maize.owner.agree.educ %in% c("c","d","e","f"))

baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="a"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="b"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="c"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="d"] <- 0
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="e"] <- 1
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="f"] <- 1
baseline_dealers$finished_secondary[baseline_dealers$maize.owner.agree.educ=="g"] <- NA

baseline_dealers$maize.owner.agree.ownership<-ifelse(baseline_dealers$maize.owner.agree.ownership=="Yes",1,0)
baseline_dealers$maize.owner.agree.q5<-ifelse(baseline_dealers$maize.owner.agree.q5=="Yes",1,0)
baseline_dealers$years_shop <- 2020 - as.numeric(as.character(substr(baseline_dealers$maize.owner.agree.q8, start=1, stop=4)))
baseline_dealers$maize.owner.agree.q9.a<-ifelse(baseline_dealers$maize.owner.agree.q9.a=="True",1,0)
baseline_dealers$maize.owner.agree.q9.b<-ifelse(baseline_dealers$maize.owner.agree.q9.b=="True",1,0)
baseline_dealers$maize.owner.agree.q9.d<-ifelse(baseline_dealers$maize.owner.agree.q9.d=="True",1,0)
baseline_dealers$maize.owner.agree.q9.e<-ifelse(baseline_dealers$maize.owner.agree.q9.e=="True",1,0)
baseline_dealers$maize.owner.agree.q10<-ifelse(baseline_dealers$maize.owner.agree.q10=="Yes",1,0)
baseline_dealers$maize.owner.agree.q11<-ifelse(baseline_dealers$maize.owner.agree.q11=="Yes",1,0)

baseline_dealers$maize.owner.agree.train_ISSD[baseline_dealers$maize.owner.agree.train_ISSD==98] <- NA #because later too late (binary)
baseline_dealers$maize.owner.agree.train_ISSD<-ifelse(baseline_dealers$maize.owner.agree.train_ISSD=="Yes",1,0)

baseline_dealers$maize.owner.agree.q20<-ifelse(baseline_dealers$maize.owner.agree.q20=="Yes",1,0)
baseline_dealers$maize.owner.agree.q32<-ifelse(baseline_dealers$maize.owner.agree.q32=="Yes",1,0)
baseline_dealers$maize.owner.agree.q45<-ifelse(baseline_dealers$maize.owner.agree.q45=="Yes",1,0)
baseline_dealers$maize.owner.agree.q57<-ifelse(baseline_dealers$maize.owner.agree.q57=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q69<-ifelse(baseline_dealers$maize.owner.agree.temp.q69=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q71<-ifelse(baseline_dealers$maize.owner.agree.temp.q71=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q72<-ifelse(baseline_dealers$maize.owner.agree.temp.q72=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q73<-ifelse(baseline_dealers$maize.owner.agree.temp.q73=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q74<-ifelse(baseline_dealers$maize.owner.agree.temp.q74=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q75<-ifelse(baseline_dealers$maize.owner.agree.temp.q75=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q76<-ifelse(baseline_dealers$maize.owner.agree.temp.q76=="Yes",1,0)

baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Cement"] <- 1
baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Tiles"] <- 1
baseline_dealers$floor[baseline_dealers$maize.owner.agree.temp.q77=="Mud"] <- 0

baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==1] <- 0
baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==2] <- 1
baseline_dealers$lighting[baseline_dealers$maize.owner.agree.temp.q78==3] <- 0

baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==1] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==2] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==3] <- 0
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==4] <- 1
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==5] <- 1
baseline_dealers$surface[baseline_dealers$maize.owner.agree.temp.q79==96] <- NA #because 2 names

baseline_dealers$maize.owner.agree.temp.q80<-ifelse(baseline_dealers$maize.owner.agree.temp.q80=="Yes",1,0)
baseline_dealers$maize.owner.agree.temp.q81<-ifelse(baseline_dealers$maize.owner.agree.temp.q81=="Yes",1,0)
baseline_dealers$maize.owner.agree.q83.a<-ifelse(baseline_dealers$maize.owner.agree.q83.a=="True",1,0)
baseline_dealers$maize.owner.agree.q83.b<-ifelse(baseline_dealers$maize.owner.agree.q83.b=="True",1,0)
baseline_dealers$maize.owner.agree.q83.c<-ifelse(baseline_dealers$maize.owner.agree.q83.c=="True",1,0)
baseline_dealers$maize.owner.agree.q83.d<-ifelse(baseline_dealers$maize.owner.agree.q83.d=="True",1,0)
baseline_dealers$maize.owner.agree.q83.e<-ifelse(baseline_dealers$maize.owner.agree.q83.e=="True",1,0)
baseline_dealers$maize.owner.agree.q83.f<-ifelse(baseline_dealers$maize.owner.agree.q83.f=="True",1,0)
baseline_dealers$maize.owner.agree.q83.g<-ifelse(baseline_dealers$maize.owner.agree.q83.g=="True",1,0)
baseline_dealers$maize.owner.agree.q83.96<-ifelse(baseline_dealers$maize.owner.agree.q83.96=="True",1,0)

baseline_dealers$badpractice_expired <- 0
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.a==1] <- NA
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.96==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.c==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.d==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.f==1] <- 1
baseline_dealers$badpractice_expired[baseline_dealers$maize.owner.agree.q83.g==1] <- 1
#there are 2 dealers who answered "This has never happened" and "Other"

baseline_dealers$goodpractice_expired[baseline_dealers$badpractice_expired==0] <- 1
baseline_dealers$goodpractice_expired[baseline_dealers$badpractice_expired==1] <- 0

baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="a"] <- 0
baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="b"] <- 0
baseline_dealers$alwaysexplains[baseline_dealers$maize.owner.agree.q85=="c"] <- 1

baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="a"] <- 0
baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="b"] <- 0
baseline_dealers$alwaysrecom[baseline_dealers$maize.owner.agree.q86=="c"] <- 1

baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="1"] <- 0
baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="2"] <- 1
baseline_dealers$extension[baseline_dealers$maize.owner.agree.q87=="3"] <- 1

baseline_dealers$maize.owner.agree.q88<-ifelse(baseline_dealers$maize.owner.agree.q88=="Yes",1,0)

baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="1"] <- 1
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="2"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="3"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="4"] <- 0
baseline_dealers$q89_bin[baseline_dealers$maize.owner.agree.q89=="other"] <- NA

baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="a"] <- 1
baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="b"] <- 1
baseline_dealers$q90_bin[baseline_dealers$maize.owner.agree.q90=="c"] <- 0

baseline_dealers$q91_bin[baseline_dealers$maize.owner.agree.q91=="No"] <- 0
baseline_dealers$q91_bin[baseline_dealers$maize.owner.agree.q91=="Yes"] <- 1

baseline_dealers$q92_bin[baseline_dealers$maize.owner.agree.q92=="No"] <- 0
baseline_dealers$q92_bin[baseline_dealers$maize.owner.agree.q92=="Yes"] <- 1

baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="1"] <- 0
baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="2"] <- 1
baseline_dealers$q93_bin[baseline_dealers$maize.owner.agree.q93=="3"] <- 1

baseline_dealers$maize.owner.agree.q94[baseline_dealers$maize.owner.agree.q94==0] <- NA
baseline_dealers$maize.owner.agree.q94[baseline_dealers$maize.owner.agree.q94=="n/a"] <- NA
baseline_dealers$maize.owner.agree.q94 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.q94))

baseline_dealers$maize.owner.agree.q95[baseline_dealers$maize.owner.agree.q95==999] <- NA
baseline_dealers$maize.owner.agree.q95[baseline_dealers$maize.owner.agree.q95=="n/a"] <- NA
baseline_dealers$maize.owner.agree.q95 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.q95))

baseline_dealers$maize.owner.agree.q96<-ifelse(baseline_dealers$maize.owner.agree.q96=="Yes",1,0)
baseline_dealers$maize.owner.agree.q97.b<-ifelse(baseline_dealers$maize.owner.agree.q97.b=="True",1,0)

baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="a"] <- 0
baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="b"] <- 1
baseline_dealers$q98_binary[baseline_dealers$maize.owner.agree.q98=="c"] <- 1

###
baseline_dealers$maize.owner.agree.inspection.q114[baseline_dealers$maize.owner.agree.inspection.q114==98] <- NA #here because binary
baseline_dealers$maize.owner.agree.inspection.q114<-as.character(baseline_dealers$maize.owner.agree.inspection.q114)
baseline_dealers$maize.owner.agree.inspection.q114<-ifelse(baseline_dealers$maize.owner.agree.inspection.q114=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q115[baseline_dealers$maize.owner.agree.inspection.q115==98] <- NA #here because binary
baseline_dealers$maize.owner.agree.inspection.q115<-as.character(baseline_dealers$maize.owner.agree.inspection.q115)
baseline_dealers$maize.owner.agree.inspection.q115<-ifelse(baseline_dealers$maize.owner.agree.inspection.q115=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q116[baseline_dealers$maize.owner.agree.inspection.q116==98] <- NA #here because binary
baseline_dealers$maize.owner.agree.inspection.q116<-as.character(baseline_dealers$maize.owner.agree.inspection.q116)
baseline_dealers$maize.owner.agree.inspection.q116<-ifelse(baseline_dealers$maize.owner.agree.inspection.q116=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q118[baseline_dealers$maize.owner.agree.inspection.q118==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q118<-as.character(baseline_dealers$maize.owner.agree.inspection.q118)
baseline_dealers$maize.owner.agree.inspection.q118<-ifelse(baseline_dealers$maize.owner.agree.inspection.q118=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q119[baseline_dealers$maize.owner.agree.inspection.q119==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q119<-as.character(baseline_dealers$maize.owner.agree.inspection.q119)
baseline_dealers$maize.owner.agree.inspection.q119<-ifelse(baseline_dealers$maize.owner.agree.inspection.q119=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q120[baseline_dealers$maize.owner.agree.inspection.q120==98] <- NA
baseline_dealers$maize.owner.agree.inspection.q120<-as.character(baseline_dealers$maize.owner.agree.inspection.q120)
baseline_dealers$maize.owner.agree.inspection.q120<-ifelse(baseline_dealers$maize.owner.agree.inspection.q120=="Yes",1,0)

baseline_dealers$maize.owner.agree.inspection.q121<-ifelse(baseline_dealers$maize.owner.agree.inspection.q121=="Yes",1,0)
baseline_dealers$maize.owner.agree.inspection.q122<-ifelse(baseline_dealers$maize.owner.agree.inspection.q122=="Yes",1,0)

###

#baseline_dealers$visible_expdate<-ifelse(baseline_dealers$exp=="n/a",0,1) #2 names
# baseline_dealers$visible_expdate<-0
# 
# baseline_dealers$seed_expired <- 0
# #baseline_dealers$seed_expired[is.na(baseline_dealers$exp)] <- NA
# #baseline_dealers$seed_expired[baseline_dealers$exp=="n/a"] <- NA
# #baseline_dealers$seed_expired[is.na(baseline_dealers$date)] <- NA
# #baseline_dealers$seed_expired[baseline_dealers$date=="n/a"] <- NA
# #1 shop has n/a for date but not for exp
# baseline_dealers$date <- as.Date(baseline_dealers$date)
# #baseline_dealers$date <- as.POSIXct(as.numeric(as.character(baseline_dealers$date)), origin = "1984-01-01")
# #baseline_dealers$exp<- as.POSIXct(as.numeric(as.character(baseline_dealers$exp)), origin = "1984-01-01")
# baseline_dealers$exp <- as.Date(baseline_dealers$exp)
# baseline_dealers$days_since_exp <- baseline_dealers$date - baseline_dealers$exp
# #baseline_dealers$seed_expired[baseline_dealers$days_since_exp > 0] <- 1
# 
# #baseline_dealers$visible_packdate<-ifelse(baseline_dealers$date_pack=="n/a",0,1)
# baseline_dealers$visible_packdate<-1
#   
# baseline_dealers$seedolderthan6m <- 0
# baseline_dealers$seedolderthan6m[is.na(baseline_dealers$date_pack)] <- NA
# baseline_dealers$seedolderthan6m[baseline_dealers$date_pack=="n/a"] <- NA
# baseline_dealers$seedolderthan6m[is.na(baseline_dealers$date)] <- NA
# baseline_dealers$date_pack <- as.Date(baseline_dealers$date_pack)
# baseline_dealers$shelflife <- baseline_dealers$date - baseline_dealers$date_pack
# baseline_dealers$seedolderthan6m[baseline_dealers$shelflife > 183] <- 1 #6x366/12
# 
# # #compare my "shelflife" with Bjorn's "age"
# # #my "shelflife"
# # summary(as.numeric(baseline_dealers$shelflife))
# # # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# # #  -15.00   32.25   58.50   62.40   77.75  261.00     194
# # #Bjorn's "age" in baseline_dealers
# # summary(baseline_dealers$age)
# # # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# # #  -15.00   33.00   52.00   66.35   79.00  259.96     159
# # #Bjorn's "age" when using only his first line of code
# # baseline_dealers$age2 <- difftime(strptime("01.10.2020", format = "%d.%m.%Y"),strptime(baseline_dealers$date_pack,format="%Y-%m-%d"),units="days")
# # summary(as.numeric(baseline_dealers$age2))
# # # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# # #  -15.00   35.00   57.00   64.97   79.00  259.96     191
# # #my "shelflife" after using 01/10/21
# # baseline_dealers$shelflife2 <- as.Date("2020-10-01") - as.Date(baseline_dealers$date_pack)
# # summary(as.numeric(baseline_dealers$shelflife2))
# # #Bjorn's "age" when using only his first line of code & my "shelflife2" after using 01/10/21 MATCH
# # sum(is.na(baseline_dealers$date_pack) & !is.na(baseline_dealers$exp))
# # #32 shop have an NA for date_pack but no NA for exp
# # #baseline_dealers$date_pack2 <- ifelse(is.na(baseline_dealers$date_pack), as.Date(baseline_dealers$exp), as.Date(baseline_dealers$date_pack))
# # baseline_dealers$date_pack_incltransformedexp<-baseline_dealers$date_pack
# # baseline_dealers$transformedexp <- baseline_dealers$exp - 180
# # baseline_dealers$date_pack_incltransformedexp[is.na(baseline_dealers$date_pack)]<-baseline_dealers$transformedexp[is.na(baseline_dealers$date_pack)]
# # baseline_dealers$shelflife3 <- as.Date("2020-10-01") - as.Date(baseline_dealers$date_pack_incltransformedexp)
# # summary(as.numeric(baseline_dealers$age))
# # summary(as.numeric(baseline_dealers$shelflife3))
# 
# baseline_dealers$date_pack_incltransformedexp <- baseline_dealers$date_pack
# baseline_dealers$transformedexp <- baseline_dealers$exp - 183 #6x366/12
# baseline_dealers$date_pack_incltransformedexp[is.na(baseline_dealers$date_pack)]<-baseline_dealers$transformedexp[is.na(baseline_dealers$date_pack)]
# baseline_dealers$shelflife_Caro <- baseline_dealers$date - as.Date(baseline_dealers$date_pack_incltransformedexp)
# baseline_dealers$shelflife_Caro[baseline_dealers$shelflife_Caro < 0] <- NA
# baseline_dealers$shelflife_Caro <- as.numeric(as.character(baseline_dealers$shelflife_Caro))
#nobs: 348 obs - 159 NA's for Bjorn's "age" in baseline_dealers or my shelflife3 - 3 sum(is.na(baseline_dealers$date)) - 3 packaging date after interview

baseline_dealers$visible_expdate<-0
baseline_dealers$seed_expired<-0
baseline_dealers$visible_packdate<-0
baseline_dealers$seedolderthan6m<-0
baseline_dealers$shelflife_Caro<-0

baseline_dealers$origin<-ifelse(baseline_dealers$origin=="Yes",1,0)
baseline_dealers$cert<-ifelse(baseline_dealers$cert=="Yes",1,0)
baseline_dealers$lot<-ifelse(baseline_dealers$lot=="Yes",1,0)
baseline_dealers$verif<-ifelse(baseline_dealers$verif=="Yes",1,0)

# Quantity of maize seed sold in kg
baseline_dealers$maize.owner.agree.q20[baseline_dealers$maize.owner.agree.q20=="Yes"] <- 1
baseline_dealers$maize.owner.agree.q20[baseline_dealers$maize.owner.agree.q20=="No"] <- 0

baseline_dealers$maize.owner.agree.q32[baseline_dealers$maize.owner.agree.q32=="Yes"] <- 1
baseline_dealers$maize.owner.agree.q32[baseline_dealers$maize.owner.agree.q32=="No"] <- 0

baseline_dealers$maize.owner.agree.q45[baseline_dealers$maize.owner.agree.q45=="Yes"] <- 1
baseline_dealers$maize.owner.agree.q45[baseline_dealers$maize.owner.agree.q45=="No"] <- 0

baseline_dealers$maize.owner.agree.q57[baseline_dealers$maize.owner.agree.q57=="Yes"] <- 1
baseline_dealers$maize.owner.agree.q57[baseline_dealers$maize.owner.agree.q57=="No"] <- 0

baseline_dealers$maize.owner.agree.long10h.q25[baseline_dealers$maize.owner.agree.long10h.q25=="n/a"] <- NA #x
baseline_dealers$maize.owner.agree.long10h.q25 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q25)) #x
baseline_dealers$maize.owner.agree.long10h.q25[baseline_dealers$maize.owner.agree.long10h.q25==999]<-NA #x
baseline_dealers$maize.owner.agree.long10h.q25[baseline_dealers$maize.owner.agree.q20=="0"] <- 0 #x

baseline_dealers$maize.owner.agree.longe7h.q37 <- baseline_dealers$maize.owner.agree.longe7h.q38 #x #different numbers in bl and ml
baseline_dealers$maize.owner.agree.longe7h.q37[baseline_dealers$maize.owner.agree.longe7h.q37=="n/a"] <- NA #x
baseline_dealers$maize.owner.agree.longe7h.q37 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe7h.q37)) #x
baseline_dealers$maize.owner.agree.longe7h.q37[baseline_dealers$maize.owner.agree.longe7h.q37==999]<-NA #x
baseline_dealers$maize.owner.agree.longe7h.q37[baseline_dealers$maize.owner.agree.q32=="0"] <- 0 #x

baseline_dealers$maize.owner.agree.longe5.q50[baseline_dealers$maize.owner.agree.longe5.q50=="n/a"] <- NA #x
baseline_dealers$maize.owner.agree.longe5.q50 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q50)) #x
baseline_dealers$maize.owner.agree.longe5.q50[baseline_dealers$maize.owner.agree.longe5.q50==999]<-NA #x
baseline_dealers$maize.owner.agree.longe5.q50[baseline_dealers$maize.owner.agree.q45=="0"] <- 0 #x

baseline_dealers$maize.owner.agree.longe4.q62[baseline_dealers$maize.owner.agree.longe4.q62=="n/a"] <- NA #x
baseline_dealers$maize.owner.agree.longe4.q62 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe4.q62)) #x
baseline_dealers$maize.owner.agree.longe4.q62[baseline_dealers$maize.owner.agree.longe4.q62==999]<-NA #x
baseline_dealers$maize.owner.agree.longe4.q62[baseline_dealers$maize.owner.agree.q57=="0"] <- 0 #x

baseline_dealers$quantitysold <- baseline_dealers$maize.owner.agree.long10h.q25+baseline_dealers$maize.owner.agree.longe7h.q37+baseline_dealers$maize.owner.agree.longe5.q50+baseline_dealers$maize.owner.agree.longe4.q62 #x

trim <- function(var,dataset,trim_perc=.02){
  
  dataset[var][dataset[var]<quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[1]|dataset[var]>quantile(dataset[var],c(trim_perc/2,1-(trim_perc/2)),na.rm=T)[2]] <- NA
  
  return(dataset)}

baseline_dealers <- trim("quantitysold",baseline_dealers,trim_perc=.02) #x

#Revenue from maize seed in mln UGX
baseline_dealers$maize.owner.agree.long10h.q26 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.long10h.q26))
baseline_dealers$revenue_long10h.q25 <- (baseline_dealers$maize.owner.agree.long10h.q25*baseline_dealers$maize.owner.agree.long10h.q26) #x
baseline_dealers$revenue_long10h.q25[baseline_dealers$maize.owner.agree.q20=="0"] <- 0 #x

baseline_dealers$maize.owner.agree.longe7h.q38 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe7h.q38))
baseline_dealers$revenue_longe7h <- (baseline_dealers$maize.owner.agree.longe7h.q37*baseline_dealers$maize.owner.agree.longe7h.q38) #x
baseline_dealers$revenue_longe7h[baseline_dealers$maize.owner.agree.q32=="0"] <- 0 #x

baseline_dealers$maize.owner.agree.longe5.q51 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe5.q51))
baseline_dealers$revenue_longe5 <- (baseline_dealers$maize.owner.agree.longe5.q50*baseline_dealers$maize.owner.agree.longe5.q51) #x
baseline_dealers$revenue_longe5[baseline_dealers$maize.owner.agree.q45=="0"] <- 0 #x

baseline_dealers$maize.owner.agree.longe4.q63 <- as.numeric(as.character(baseline_dealers$maize.owner.agree.longe4.q63))
baseline_dealers$revenue_longe4 <- (baseline_dealers$maize.owner.agree.longe4.q62*baseline_dealers$maize.owner.agree.longe4.q63) #x
baseline_dealers$revenue_longe4[baseline_dealers$maize.owner.agree.q57=="0"] <- 0 #x

baseline_dealers$revenue <- (baseline_dealers$revenue_long10h.q25+baseline_dealers$revenue_longe7h
                                 +baseline_dealers$revenue_longe5+baseline_dealers$revenue_longe4) #x
baseline_dealers$revenue <- baseline_dealers$revenue/1000000 #x

baseline_dealers <- trim("revenue",baseline_dealers,trim_perc=.02) #x

#baseline_dealers$revenue <- ihs(baseline_dealers$revenue) #x

###loop###
variables <- c("maize.owner.agree.age","maize.owner.agree.gender","finished_primary","finished_secondary","maize.owner.agree.ownership"
               ,"maize.owner.agree.q3","maize.owner.agree.q4","maize.owner.agree.q5","maize.owner.agree.q6","maize.owner.agree.q7","years_shop"
               ,"maize.owner.agree.q9.a","maize.owner.agree.q9.b","maize.owner.agree.q9.d","maize.owner.agree.q9.e","maize.owner.agree.q10"
               ,"maize.owner.agree.q11","maize.owner.agree.train_ISSD","maize.owner.agree.nr_var","maize.owner.agree.q19"
               ,"maize.owner.agree.q20","maize.owner.agree.q32","maize.owner.agree.q44","maize.owner.agree.q45","maize.owner.agree.q57"
               ,"maize.owner.agree.temp.q69","maize.owner.agree.temp.q71","maize.owner.agree.temp.q72","maize.owner.agree.temp.q73"
               ,"maize.owner.agree.temp.q74","maize.owner.agree.temp.q75","maize.owner.agree.temp.q76","floor","lighting","surface"
               ,"maize.owner.agree.temp.q80","maize.owner.agree.temp.q81","maize.owner.agree.temp.q82","maize.owner.agree.q83.a"
               ,"goodpractice_expired","alwaysexplains","alwaysrecom","extension","maize.owner.agree.q88","q89_bin","q90_bin","q91_bin"
               ,"q92_bin","q93_bin","maize.owner.agree.q94","maize.owner.agree.q95","maize.owner.agree.q96","maize.owner.agree.q97.b"
               ,"q98_binary","maize.owner.agree.q99","maize.owner.agree.q100","maize.owner.agree.q101","maize.owner.agree.q102"
               ,"maize.owner.agree.q103","maize.owner.agree.inspection.q114","maize.owner.agree.inspection.q115"
               ,"maize.owner.agree.inspection.q116","maize.owner.agree.inspection.q117","maize.owner.agree.inspection.q118"
               ,"maize.owner.agree.inspection.q119","maize.owner.agree.inspection.q120","maize.owner.agree.inspection.q121"
               ,"maize.owner.agree.inspection.q122","maize.owner.agree.q70","reading","visible_expdate","seed_expired","visible_packdate"
               ,"seedolderthan6m","shelflife_Caro","origin","cert","lot","verif"
               ,"knows_dealer","bought_at_dealer","customer_years","knows_other_customer","refunds"
               ,"gives_credit","gives_advice","delivers","after_sales_service","payment_mehtods"
               ,"small_quant"
               ,"general","yield","drought_resistent","disease_resistent","early_maturing"
               ,"germination", "quantitysold", "revenue")

baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x==999,NA))
baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x==98,NA))
baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x==96,NA))
baseline_dealers[variables] <- lapply(baseline_dealers[variables],function(x) replace(x,x=="n/a",NA))

for (i in 1:length(variables)) {
  df_descriptives_dealer[i,1] <- sum(baseline_dealers[variables[i]], na.rm=T)/(nrow(baseline_dealers)-sum(is.na(baseline_dealers[variables[i]])))
  df_descriptives_dealer[i,2] <- min(baseline_dealers[variables[i]], na.rm=T)
  df_descriptives_dealer[i,3] <- max(baseline_dealers[variables[i]], na.rm=T)
  df_descriptives_dealer[i,4] <- sqrt(var(baseline_dealers[variables[i]], na.rm=T))
  df_descriptives_dealer[i,5] <- nrow(baseline_dealers) - sum(is.na(baseline_dealers[variables[i]]))}

#Descriptive statistics by gender

#male
baseline_dealers_male <- subset(baseline_dealers, baseline_dealers$maize.owner.agree.gender==1)

baseline_dealers_male[variables] <- lapply(baseline_dealers_male[variables],function(x) replace(x,x==999,NA))
baseline_dealers_male[variables] <- lapply(baseline_dealers_male[variables],function(x) replace(x,x==98,NA))
baseline_dealers_male[variables] <- lapply(baseline_dealers_male[variables],function(x) replace(x,x==96,NA))
baseline_dealers_male[variables] <- lapply(baseline_dealers_male[variables],function(x) replace(x,x=="n/a",NA))

df_descriptives_dealer_male <- array(NA,dim=c(120,5))

for (i in 1:length(variables)) {
  df_descriptives_dealer_male[i,1] <- sum(baseline_dealers_male[variables[i]], na.rm=T)/(nrow(baseline_dealers_male)-sum(is.na(baseline_dealers_male[variables[i]])))
  df_descriptives_dealer_male[i,2] <- min(baseline_dealers_male[variables[i]], na.rm=T)
  df_descriptives_dealer_male[i,3] <- max(baseline_dealers_male[variables[i]], na.rm=T)
  df_descriptives_dealer_male[i,4] <- sqrt(var(baseline_dealers_male[variables[i]], na.rm=T))
  df_descriptives_dealer_male[i,5] <- nrow(baseline_dealers_male) - sum(is.na(baseline_dealers_male[variables[i]]))}

#female
baseline_dealers_female <- subset(baseline_dealers, baseline_dealers$maize.owner.agree.gender==0)

baseline_dealers_female[variables] <- lapply(baseline_dealers_female[variables],function(x) replace(x,x==999,NA))
baseline_dealers_female[variables] <- lapply(baseline_dealers_female[variables],function(x) replace(x,x==98,NA))
baseline_dealers_female[variables] <- lapply(baseline_dealers_female[variables],function(x) replace(x,x==96,NA))
baseline_dealers_female[variables] <- lapply(baseline_dealers_female[variables],function(x) replace(x,x=="n/a",NA))

df_descriptives_dealer_female <- array(NA,dim=c(120,5))

for (i in 1:length(variables)) {
  df_descriptives_dealer_female[i,1] <- sum(baseline_dealers_female[variables[i]], na.rm=T)/(nrow(baseline_dealers_female)-sum(is.na(baseline_dealers_female[variables[i]])))
  df_descriptives_dealer_female[i,2] <- min(baseline_dealers_female[variables[i]], na.rm=T)
  df_descriptives_dealer_female[i,3] <- max(baseline_dealers_female[variables[i]], na.rm=T)
  df_descriptives_dealer_female[i,4] <- sqrt(var(baseline_dealers_female[variables[i]], na.rm=T))
  df_descriptives_dealer_female[i,5] <- nrow(baseline_dealers_female) - sum(is.na(baseline_dealers_female[variables[i]]))}


########################################
#####Descriptive statistics: farmer#####
########################################

df_descriptives_farmer <- array(NA,dim=c(130,5))

###variable transformation###

baseline_farmers[baseline_farmers==999] <- NA
#baseline_farmers[baseline_farmers==96] <- NA
baseline_farmers[, 4:122][baseline_farmers[, 4:122] == 96] <- NA #columns 4-94 only
#baseline_farmers[baseline_farmers==98] <- NA
baseline_farmers[, 4:122][baseline_farmers[, 4:122] == 98] <- NA
baseline_farmers[baseline_farmers=="n/a"] <- NA

baseline_farmers$Check2.check.maize.q15<-ifelse(baseline_farmers$Check2.check.maize.q15=="Male",1,0)
baseline_farmers$married<-ifelse(baseline_farmers$Check2.check.maize.q16=="a",1,0)
baseline_farmers$Check2.check.maize.q17[baseline_farmers$Check2.check.maize.q17=="g"] <- NA
baseline_farmers$noformaleducation<-ifelse(baseline_farmers$Check2.check.maize.q17=="a",1,0)

baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="a"] <- 0
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="b"] <- 0
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="c"] <- 1
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="d"] <- 1
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="e"] <- 1
baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="f"] <- 1
#baseline_farmers$finishedprimary[baseline_farmers$Check2.check.maize.q17=="g"] <- 0

baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="a"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="b"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="c"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="d"] <- 0
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="e"] <- 1
baseline_farmers$finishedsecondary[baseline_farmers$Check2.check.maize.q17=="f"] <- 1

baseline_farmers$goodroof[baseline_farmers$Check2.check.maize.q21=="a"] <- 0
baseline_farmers$goodroof[baseline_farmers$Check2.check.maize.q21=="b"] <- 1
baseline_farmers$goodroof[baseline_farmers$Check2.check.maize.q21=="c"] <- 1

baseline_farmers$yearsmaize <- 2021 - as.numeric(as.character(substr(baseline_farmers$Check2.check.maize.q23, start=1, stop=4)))
baseline_farmers$Check2.check.maize.q24<-ifelse(baseline_farmers$Check2.check.maize.q24=="Yes",1,0)
baseline_farmers$Check2.check.maize.q25a<-ifelse(baseline_farmers$Check2.check.maize.q25a=="Yes",1,0)

baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="a"] <- 1
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="b"] <- 1
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="c"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="d"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="e"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="f"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="h"] <- 0
baseline_farmers$farmersavedseed[baseline_farmers$Check2.check.maize.q25b=="i"] <- 0

baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="a"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="b"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="c"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="d"] <- 1
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="e"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="f"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="h"] <- 0
baseline_farmers$boughtfromagroinputshop[baseline_farmers$Check2.check.maize.q25b=="i"] <- 0

baseline_farmers$boughtfromagroinputshop2[is.na(baseline_farmers$Check2.check.maize.q25b)] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="a"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="b"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="c"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="d"] <- 1
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="e"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="f"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="h"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25b=="i"] <- 0
baseline_farmers$boughtfromagroinputshop2[baseline_farmers$Check2.check.maize.q25a==0] <- 0

baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="a"] <- 0
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="b"] <- 0
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="c"] <- 1
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="d"] <- 1
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="e"] <- 1
baseline_farmers$thirdormore_time_used[baseline_farmers$Check2.check.maize.q25c=="f"] <- 1

baseline_farmers$Check2.check.maize.q25d <- as.numeric((as.character(baseline_farmers$Check2.check.maize.q25d)))
baseline_farmers$Check2.check.maize.q25d2 <- baseline_farmers$Check2.check.maize.q25d
baseline_farmers$Check2.check.maize.q25d2[is.na(baseline_farmers$Check2.check.maize.q25d)] = 0

baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="1"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="2"] <- 1
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="3"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="4"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="5"] <- 0
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="6"] <- NA
baseline_farmers$tooexpensive[baseline_farmers$Check2.check.maize.q25f=="n/a"] <- NA

baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="1"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="2"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="3"] <- 1
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="4"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="5"] <- 0
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="6"] <- NA
baseline_farmers$notgoodquality[baseline_farmers$Check2.check.maize.q25f=="n/a"] <- NA

baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="1"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="2"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="3"] <- 1
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="4"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="5"] <- 0
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="6"] <- NA
baseline_farmers$verygoodquality[baseline_farmers$Check2.check.maize.q25f_2=="n/a"] <- NA

baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g==1] <- 1
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="2"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="3"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="4"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="5"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="6"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="7"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="8"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="9"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="10"] <- 0
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g==96] <- NA
baseline_farmers$yieldtoolow[baseline_farmers$Check2.check.maize.q25g=="n/a"] <- NA

baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g==1] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="2"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="3"] <- 1
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="4"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="5"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="6"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="7"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="8"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="9"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="10"] <- 0
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g==96] <- NA
baseline_farmers$lesspesttolerant[baseline_farmers$Check2.check.maize.q25g=="n/a"] <- NA

baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g==1] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="2"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="3"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="4"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="5"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="6"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="7"] <- 1
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="8"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="9"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="10"] <- 0
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g==96] <- NA
baseline_farmers$lowgermination[baseline_farmers$Check2.check.maize.q25g=="n/a"] <- NA

baseline_farmers$Check2.check.maize.q25h<-ifelse(baseline_farmers$Check2.check.maize.q25h=="Yes",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_5<-ifelse(baseline_farmers$Check2.check.maize.q26.Longe_5=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go<-ifelse(baseline_farmers$Check2.check.maize.q26.Longe_7R_Kayongo.go=="True",1,0)
baseline_farmers$Check2.check.maize.q26.Wema<-ifelse(baseline_farmers$Check2.check.maize.q26.Wema=="True",1,0)
baseline_farmers$q27bin<-ifelse(baseline_farmers$Check2.check.maize.q27==1,1,0)
baseline_farmers$Check2.check.maize.q30<-ifelse(baseline_farmers$Check2.check.maize.q30=="Yes",1,0)

baseline_farmers$Check2.check.maize.q30a.1<-ifelse(baseline_farmers$Check2.check.maize.q30a.1=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.1[is.na(baseline_farmers$Check2.check.maize.q30a.1)] <- 0

baseline_farmers$Check2.check.maize.q30a.2<-ifelse(baseline_farmers$Check2.check.maize.q30a.2=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.2[is.na(baseline_farmers$Check2.check.maize.q30a.2)] <- 0

baseline_farmers$Check2.check.maize.q30a.3<-ifelse(baseline_farmers$Check2.check.maize.q30a.3=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.3[is.na(baseline_farmers$Check2.check.maize.q30a.3)] <- 0

baseline_farmers$Check2.check.maize.q30a.4<-ifelse(baseline_farmers$Check2.check.maize.q30a.4=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.4[is.na(baseline_farmers$Check2.check.maize.q30a.4)] <- 0

baseline_farmers$Check2.check.maize.q30a.5<-ifelse(baseline_farmers$Check2.check.maize.q30a.5=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.5[is.na(baseline_farmers$Check2.check.maize.q30a.5)] <- 0

baseline_farmers$Check2.check.maize.q30a.6<-ifelse(baseline_farmers$Check2.check.maize.q30a.6=="True",1,0)
baseline_farmers$Check2.check.maize.q30a.6[is.na(baseline_farmers$Check2.check.maize.q30a.6)] <- 0

baseline_farmers$Check2.check.maize.q30b <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q30b))

#hereq31
baseline_farmers$hybrid<-((baseline_farmers$Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$Check2.check.maize.q31=="Bazooka")|(baseline_farmers$Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$Check2.check.maize.q31=="Panner")|(baseline_farmers$Check2.check.maize.q31=="Wema")|(baseline_farmers$Check2.check.maize.q31=="KH_series"))
baseline_farmers$hybrid<-ifelse(baseline_farmers$hybrid=="TRUE",1,0)
baseline_farmers$hybrid[baseline_farmers$Check2.check.maize.q31=="Other_hybrid"] <- NA

baseline_farmers$OPV<-(baseline_farmers$Check2.check.maize.q31=="Longe_5")|(baseline_farmers$Check2.check.maize.q31=="Longe_4")
baseline_farmers$OPV<-ifelse(baseline_farmers$OPV=="TRUE",1,0)
baseline_farmers$OPV[baseline_farmers$Check2.check.maize.q31=="Other_hybrid"] <- NA

baseline_farmers$Land_Races<-(baseline_farmers$Check2.check.maize.q31=="Land_Races")
baseline_farmers$Land_Races<-ifelse(baseline_farmers$Land_Races=="TRUE",1,0)

baseline_farmers$improved<-((baseline_farmers$Check2.check.maize.q31=="Longe_10H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7H")|(baseline_farmers$Check2.check.maize.q31=="Longe_7R_Kayongo-go")|(baseline_farmers$Check2.check.maize.q31=="Bazooka")|(baseline_farmers$Check2.check.maize.q31=="Longe_6H")|(baseline_farmers$Check2.check.maize.q31=="Panner")|(baseline_farmers$Check2.check.maize.q31=="Wema")|(baseline_farmers$Check2.check.maize.q31=="KH_series"|baseline_farmers$Check2.check.maize.q31=="Longe_5")|(baseline_farmers$Check2.check.maize.q31=="Longe_4")|(baseline_farmers$Check2.check.maize.q31=="Other_hybrid"))
baseline_farmers$improved<-ifelse(baseline_farmers$improved=="TRUE",1,0)

baseline_farmers$farmer_saved_seed<-((baseline_farmers$Check2.check.maize.q32=="a")|(baseline_farmers$Check2.check.maize.q32=="b"))
baseline_farmers$farmer_saved_seed<-ifelse(baseline_farmers$farmer_saved_seed=="TRUE",1,0)

baseline_farmers$Bought_from_agro_input_shop<-ifelse(baseline_farmers$Check2.check.maize.q32=="d",1,0)

baseline_farmers$hybridbutsaved <- NA
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 1 & baseline_farmers$farmer_saved_seed == 1] <- 1
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 1 & baseline_farmers$farmer_saved_seed == 0] <- 0
baseline_farmers$hybridbutsaved[baseline_farmers$hybrid == 0] <- 0

baseline_farmers$fourthormore_timeused<-((baseline_farmers$Check2.check.maize.q34=="d")|(baseline_farmers$Check2.check.maize.q34=="e")|(baseline_farmers$Check2.check.maize.q34=="f"))
baseline_farmers$OPVbutfourthormore_timeused <- NA
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==1] <- 1
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==1 & baseline_farmers$fourthormore_timeused==0] <- 0
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV==1 & baseline_farmers$farmer_saved_seed==0] <- 0
baseline_farmers$OPVbutfourthormore_timeused[baseline_farmers$OPV == 0] <- 0

baseline_farmers$adoption_onfield <- baseline_farmers$improved
baseline_farmers$adoption_onfield[baseline_farmers$hybridbutsaved==1] <- 0
baseline_farmers$adoption_onfield[baseline_farmers$OPVbutfourthormore_timeused==1] <- 0
#hereq31

baseline_farmers$Check2.check.maize.q36<-ifelse(baseline_farmers$Check2.check.maize.q36=="Yes",1,0)
baseline_farmers$Check2.check.maize.q36b<-ifelse(baseline_farmers$Check2.check.maize.q36b=="Yes",1,0)
baseline_farmers$Check2.check.maize.q37<-ifelse(baseline_farmers$Check2.check.maize.q37=="Yes",1,0)
baseline_farmers$Check2.check.maize.q39 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q39))
baseline_farmers$costforseed <- baseline_farmers$Check2.check.maize.q38*(as.numeric(as.character(baseline_farmers$Check2.check.maize.q39)))

baseline_farmers$correctseedspacing<-ifelse(baseline_farmers$Check2.check.maize.q40=="c",1,0)
baseline_farmers$correctseedspacing[is.na(baseline_farmers$Check2.check.maize.q40)] <- 0

baseline_farmers$Check2.check.maize.q41 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q41))
baseline_farmers$correctnumberofseeds<-ifelse(baseline_farmers$Check2.check.maize.q41==1,1,0)
baseline_farmers$Check2.check.maize.q42<-ifelse(baseline_farmers$Check2.check.maize.q42=="Yes",1,0)
baseline_farmers$Check2.check.maize.q43<-ifelse(baseline_farmers$Check2.check.maize.q43=="Yes",1,0)
baseline_farmers$Check2.check.maize.q43a <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q43a))

baseline_farmers$Check2.check.maize.q44<-ifelse(baseline_farmers$Check2.check.maize.q44=="Yes",1,0)
baseline_farmers$Check2.check.maize.q44 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q44))

baseline_farmers$Check2.check.maize.q44a <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q44a))

baseline_farmers$correctweeding<-((baseline_farmers$Check2.check.maize.q45>=3))
baseline_farmers$correctweeding<-ifelse(baseline_farmers$correctweeding=="TRUE",1,0)

baseline_farmers$Check2.check.maize.q46 <- (as.numeric(as.character(baseline_farmers$Check2.check.maize.q46)))

baseline_farmers$correcttimeweeding <- (baseline_farmers$Check2.check.maize.q46 <= 20 & baseline_farmers$Check2.check.maize.q46 >= 18)
baseline_farmers$correcttimeweeding<-ifelse(baseline_farmers$correcttimeweeding=="TRUE",1,0)

baseline_farmers$Check2.check.maize.q47<-ifelse(baseline_farmers$Check2.check.maize.q47=="Yes",1,0)
baseline_farmers$correctplanting<-ifelse(baseline_farmers$Check2.check.maize.q48==2,1,0)
baseline_farmers$Check2.check.maize.q49<-ifelse(baseline_farmers$Check2.check.maize.q49=="Yes",1,0)

#area if intercropped
baseline_farmers$area_intercropped <- baseline_farmers$Check2.check.maize.q29*baseline_farmers$Check2.check.maize.q30b/100
#area if not intercropped
baseline_farmers$area_not_intercropped <- baseline_farmers$Check2.check.maize.q29
#area both
baseline_farmers$area<-ifelse(baseline_farmers$Check2.check.maize.q30=="Yes",baseline_farmers$area_intercropped,baseline_farmers$area_not_intercropped)
#IGNORING INTERCROPPING BECAUSE WILBER SAYS SO.
#IF MAIZE IS INTERCROPPED, MAIZE IS OFTEN (ALMOST ALWAYS) THE MAIN CROP.
#INTERCROPPED OR NOT, THERE'S AN EQUAL NUMBER OF MAIZE CROPS.
baseline_farmers$yield_inkg <- baseline_farmers$Check2.check.maize.q50*baseline_farmers$Check2.check.maize.q51 #production in kg
baseline_farmers$landproductivity <- baseline_farmers$yield_inkg/baseline_farmers$Check2.check.maize.q29 #yield in kg per acre

baseline_farmers <- trim("landproductivity",baseline_farmers,trim_perc=.02)

baseline_farmers$yield_inUGX <- baseline_farmers$Check2.check.maize.q50*baseline_farmers$Check2.check.maize.q52
baseline_farmers$yield_indollar <- baseline_farmers$yield_inUGX/3561.51
baseline_farmers$landproductivity_inUGX <- baseline_farmers$yield_inUGX/baseline_farmers$Check2.check.maize.q29

baseline_farmers$landproductivity_indollar <- baseline_farmers$landproductivity_inUGX/3561.51

baseline_farmers$Check2.check.maize.q53<-ifelse(baseline_farmers$Check2.check.maize.q53=="Yes",1,0)
baseline_farmers$Check2.check.maize.q54 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q54))
baseline_farmers$Check2.check.maize.q55 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q55))

baseline_farmers$outputprice_indollar <- baseline_farmers$Check2.check.maize.q55/3561.51
baseline_farmers$outputprice_indollar <- as.numeric(as.character(baseline_farmers$outputprice_indollar))

baseline_farmers$revenueUGX <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q54))*baseline_farmers$Check2.check.maize.q55
baseline_farmers$revenue_dollar <- baseline_farmers$revenueUGX/3561.51
baseline_farmers$Check2.check.maize.q56 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q56))
baseline_farmers$Check2.check.maize.q57 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q57))

#additional dollar measures
baseline_farmers$Check2.check.maize.q39 <- as.numeric(as.character(baseline_farmers$Check2.check.maize.q39))
baseline_farmers$priceindollar <- baseline_farmers$Check2.check.maize.q39/3561.51
baseline_farmers$costforseed_dollar <- baseline_farmers$costforseed/3561.51
baseline_farmers$marketvaluedollars <- baseline_farmers$Check2.check.maize.q52/3561.51

###loop###
variables_farmer <- c("Check2.check.maize.q8","Check2.check.maize.q9","Check2.check.maize.q10","Check2.check.maize.q11","Check2.check.maize.q12"
                      ,"Check2.check.maize.q14","Check2.check.maize.q15","married","noformaleducation","finishedprimary","finishedsecondary"
                      ,"Check2.check.maize.q18","Check2.check.maize.q20","goodroof","Check2.check.maize.q22","yearsmaize"
                      ,"Check2.check.maize.q24","Check2.check.maize.q25a","farmersavedseed","boughtfromagroinputshop2","thirdormore_time_used"
                      ,"Check2.check.maize.q25d","tooexpensive","notgoodquality","verygoodquality","yieldtoolow","lesspesttolerant"
                      ,"lowgermination","Check2.check.maize.q25h","Check2.check.maize.q26.Longe_5","Check2.check.maize.q26.Longe_7R_Kayongo.go"
                      ,"Check2.check.maize.q26.Wema","q27bin","Check2.check.maize.q27","Check2.check.maize.q29","Check2.check.maize.q30"
                      ,"Check2.check.maize.q30a.1","Check2.check.maize.q30a.2","Check2.check.maize.q30a.3","Check2.check.maize.q30a.4"
                      ,"Check2.check.maize.q30a.5","Check2.check.maize.q30a.6","Check2.check.maize.q30b","hybrid","OPV","Land_Races","improved"
                      ,"farmer_saved_seed","Bought_from_agro_input_shop","hybridbutsaved","OPVbutfourthormore_timeused","Check2.check.maize.q35a"
                      ,"Check2.check.maize.q35b","Check2.check.maize.q35c","Check2.check.maize.q35d","Check2.check.maize.q35e"
                      ,"Check2.check.maize.q35f","Check2.check.maize.q35g","Check2.check.maize.q35h","Check2.check.maize.q35i"
                      ,"Check2.check.maize.q35j","Check2.check.maize.q36","Check2.check.maize.q36b","Check2.check.maize.q37"
                      ,"Check2.check.maize.q38","Check2.check.maize.q39","costforseed","correctseedspacing","Check2.check.maize.q41"
                      ,"correctnumberofseeds","Check2.check.maize.q42","Check2.check.maize.q43","Check2.check.maize.q43a"
                      ,"Check2.check.maize.q44","Check2.check.maize.q44a","Check2.check.maize.q45","correctweeding","Check2.check.maize.q46"
                      ,"correcttimeweeding","Check2.check.maize.q47","correctplanting","Check2.check.maize.q49","Check2.check.maize.q50"
                      ,"Check2.check.maize.q51","yield_inkg","landproductivity","Check2.check.maize.q52","yield_inUGX"
                      ,"yield_indollar","landproductivity_inUGX","landproductivity_indollar","Check2.check.maize.q53","Check2.check.maize.q54"
                      ,"Check2.check.maize.q55","outputprice_indollar","revenueUGX","revenue_dollar","Check2.check.maize.q56"
                      ,"Check2.check.maize.q57","priceindollar","costforseed_dollar","marketvaluedollars"
                      ,"bought_last_season","general_rating"
                      ,"location_rating","price_rating","quality_rating"
                      ,"stock_rating","reputation_rating"
                      ,"seed_quality_general_rating","seed_yield_rating"
                      ,"seed_drought_rating","seed_disease_rating"
                      ,"seed_maturing_rating","seed_germinate_rating"
                      ,"knows_dealer","bought_at_dealer","customer_years","knows_other_customer")

for (i in 1:length(variables_farmer)) {
  df_descriptives_farmer[i,1] <- sum(baseline_farmers[variables_farmer[i]], na.rm=T)/(nrow(baseline_farmers)-sum(is.na(baseline_farmers[variables_farmer[i]])))
  df_descriptives_farmer[i,2] <- min(baseline_farmers[variables_farmer[i]], na.rm=T)
  df_descriptives_farmer[i,3] <- max(baseline_farmers[variables_farmer[i]], na.rm=T)
  df_descriptives_farmer[i,4] <- sqrt(var(baseline_farmers[variables_farmer[i]], na.rm=T))
  df_descriptives_farmer[i,5] <- nrow(baseline_farmers) - sum(is.na(baseline_farmers[variables_farmer[i]]))}

