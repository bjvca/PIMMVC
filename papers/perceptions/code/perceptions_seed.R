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

