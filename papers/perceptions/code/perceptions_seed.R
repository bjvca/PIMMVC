### run in:  ../PIMMVC/papers/perceptions
rm(list=ls())
path <- getwd()

library(miceadds)
library(texreg)
library(plyr)
library(plm)
library(lmtest)
library(lme4)
library(Rcpp)
library(sjPlot)
options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]




############################################ SEED SYSTEMS DATA ##################################################

##################################################################################################################
####################### BETWEEN FARMER --- FOCUS ON DEALER'S GENDER ##############################################

###########  MODEL 3 ###############
##################################################################################################################

rating_dyads <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"), stringsAsFactors = FALSE)[c("shop_ID", "farmer_ID",          "general_rating","location_rating","price_rating","quality_rating", "stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")]
rating_dyads_midline <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems_midline/data/farmer/midline_rating_dyads.csv", sep = "/"), stringsAsFactors = FALSE)[c("shop_ID", "farmer_ID", "general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")]

### check if gender of agro input dealer in baseline corresponds to gender in midline:
baseline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)
midline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems_midline/data/input_dealer/midline_dealer.csv", sep="/"), stringsAsFactors = FALSE)


merged_dealer <- merge(baseline_dealer, midline_dealer, by="shop_ID")

#select only those that have same gender in baseline and endline
to_select <- merged_dealer$shop_ID[merged_dealer$maize.owner.agree.gender == merged_dealer$owner.agree.gender]

#stack rating dyads from the two rounds
rating_dyads <- rbind(rating_dyads, rating_dyads_midline)

#keep only those that have same gender in baseline and endline

rating_dyads <- subset(rating_dyads,shop_ID  %in% to_select)
rat<-rating_dyads  #to be used for FE

# convert to numbers and aggregate scores at agro-input dealer level for between regression

rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")], function(x) as.numeric(as.character(x)))

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

#create averages for between regression

shop_av <- aggregate(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],list(rating_dyads$shop_ID),FUN=mean, na.rm=T)

names(shop_av)[1] <- "shop_ID"
#merge in dealer gender from baseline data
rating_dyads <- merge(shop_av, baseline_dealer[c("shop_ID","maize.owner.agree.gender")],  by.x="shop_ID", by.y="shop_ID", all.x=T)

#rename
names(rating_dyads)[names(rating_dyads) == "maize.owner.agree.gender"] <- "gender"

rating_dyads$overall_rating <-  rowMeans(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating")],na.rm=T)
rating_dyads$score  <-  rowMeans(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm=T)

#regressions without controls 
#seed ratings 
summary(lm(score~gender,data=rating_dyads))
summary(lm(seed_quality_general_rating~gender,data=rating_dyads))
summary(lm(seed_yield_rating~gender,data=rating_dyads))
summary(lm(seed_drought_rating~gender,data=rating_dyads))
summary(lm(seed_disease_rating~gender,data=rating_dyads))
summary(lm(seed_maturing_rating~gender,data=rating_dyads))
summary(lm(seed_germinate_rating~gender,data=rating_dyads))

#storing seed ratings 
s5<- rbind(c((format(round(sum((lm(score~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(sqrt(diag(vcov(lm(score~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0))),
             
           c((format(round(summary(lm(score~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0))),
         
           c((format(round(sum((lm(score~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(sqrt(diag(vcov(lm(score~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(lm(score~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0))),
            
           c((format(round(nobs(lm(score~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_quality_general_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_yield_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_drought_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_disease_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_maturing_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_germinate_rating~gender,data=rating_dyads)),digits=3),nsmall=0)))
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
s7<- rbind(c((format(round(sum((lm(overall_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender,data=rating_dyads))))[1],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender,data=rating_dyads))))[2],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$r.squared,digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0))),
           
           c((format(round(nobs(lm(overall_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(general_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(location_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(price_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(quality_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(stock_rating~gender,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(reputation_rating~gender,data=rating_dyads)),digits=3),nsmall=0)))
)



############## FIXED EFFECTS 

#merge to get gender 
rat <- merge(rat, baseline_dealer[c("shop_ID","maize.owner.agree.gender")],  by.x="shop_ID", by.y="shop_ID", all.x=T)
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
rat$quality_rating<-as.numeric(rat$stock_rating)
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

#seed ratings 

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



fe1<- rbind( c((format(round(mean(fixef(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0))),
             c((format(round(sum((plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0))),
             c((format(round((coeftest(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
[1,2],digits=3),nsmall=0)),
(format(round((coeftest(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=0)),
(format(round((coeftest(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=0)),
(format(round((coeftest(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=0)),
(format(round((coeftest(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=0)),
(format(round((coeftest(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=0)),
(format(round((coeftest(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
              [1,2],digits=3),nsmall=0))),

c((format(round((coeftest(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=0)),
  (format(round((coeftest(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=0)),
  (format(round((coeftest(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=0)),
  (format(round((coeftest(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=0)),
  (format(round((coeftest(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=0)),
  (format(round((coeftest(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=0)),
  (format(round((coeftest(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                [1,4],digits=3),nsmall=0))),
             
             
             c((format(round(summary(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0))),
           
  c((format(round(summary(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0))),
           
  c((format(round(nobs(plm(score~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(seed_quality_general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(seed_yield_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(seed_drought_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(seed_disease_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(seed_maturing_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(seed_germinate_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)))
)
