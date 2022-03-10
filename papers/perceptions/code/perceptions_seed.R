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

rat<-rating_dyads  #to be used for FE

#keep only those that have same gender in baseline and endline
rating_dyads <- subset(rating_dyads,shop_ID  %in% to_select)

#getting dealer characteristics for controls 
rating_dyads <- merge(merged_dealer, rating_dyads, by="shop_ID")

#checking the controls 
table(rating_dyads$maize.owner.agree.age) #age

table(rating_dyads$maize.owner.agree.educ)  #education
rating_dyads$prim<-0
#finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
rating_dyads$prim[rating_dyads$maize.owner.agree.educ=="e"|rating_dyads$maize.owner.agree.educ=="f"] <- 1
rating_dyads$prim[rating_dyads$maize.owner.agree.educ=="g"]<- NA  #since g is other 
table(rating_dyads$prim)   #finished secondary education 

table(rating_dyads$maize.owner.agree.q3) #distance to tarmac road in km
rating_dyads$maize.owner.agree.q3[rating_dyads$maize.owner.agree.q3==999] <- NA

table(rating_dyads$maize.owner.agree.q4) #distance to murram road in km

#selling only farm inputs
table(rating_dyads$maize.owner.agree.q5)
rating_dyads$inputsale<- ifelse(rating_dyads$maize.owner.agree.q5== 'Yes', 1, 0)

#Q8. When was this agro-input shop established? (year)
rating_dyads$years_shop <- 2020 - as.numeric(as.character(substr(rating_dyads$maize.owner.agree.q8, start=1, stop=4)))
table(rating_dyads$years_shop)

#seed stored in dedicated area?
rating_dyads$maize.owner.agree.q69
rating_dyads$dedarea<-as.character(rating_dyads$maize.owner.agree.temp.q69)
rating_dyads$dedicated_area<- ifelse(rating_dyads$dedarea== 'Yes', 1, 0)
table(rating_dyads$dedicated_area)

#problem with rats or pests?
#we try to formulate this as a good quality variable --- So, no problem with pests is good
rating_dyads$maize.owner.agree.q71
rating_dyads$pest<-as.character(rating_dyads$maize.owner.agree.temp.q71)
rating_dyads$pest_prob<- ifelse(rating_dyads$pest== 'No', 1, 0)
table(rating_dyads$pest_prob)

#roof insulated?
rating_dyads$maize.owner.agree.q73
rating_dyads$roof_insu<-as.character(rating_dyads$maize.owner.agree.temp.q73)
rating_dyads$insulated<- ifelse(rating_dyads$roof_insu== 'Yes', 1, 0)
table(rating_dyads$insulated)

#walls insulated?
rating_dyads$maize.owner.agree.q74
rating_dyads$wall_insu<-as.character(rating_dyads$maize.owner.agree.temp.q74)
rating_dyads$wall_heatproof<- ifelse(rating_dyads$wall_insu== 'Yes', 1, 0)
table(rating_dyads$wall_heatproof)

#area ventilated?
rating_dyads$maize.owner.agree.q75
rating_dyads$vent<-as.character(rating_dyads$maize.owner.agree.temp.q75)
rating_dyads$ventilation<- ifelse(rating_dyads$vent== 'Yes', 1, 0)
table(rating_dyads$ventilation)

#Q78. Lighting conditions in area where seed is stored?
#we try to formulate this as good quality variable --- 2 is good lighting
rating_dyads$badlighting <- 0
rating_dyads$badlighting[rating_dyads$maize.owner.agree.temp.q78=="2"]<-1
table(rating_dyads$badlighting)

#Q79. On what surface are seed stored?
#we try to formulate this as good quality variable ---- 3,4,5 are good storage surfaces
rating_dyads$badstored <- 0
rating_dyads$badstored[rating_dyads$maize.owner.agree.temp.q79=="3"|rating_dyads$maize.owner.agree.temp.q79=="4"|rating_dyads$maize.owner.agree.temp.q79=="5"]<-1
rating_dyads$badstored[rating_dyads$maize.owner.agree.temp.q79==96]<-NA
table(rating_dyads$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
#we try to formulate this as good quality variable --- Not being stored in open containers is good
rating_dyads$maize.owner.agree.q80
rating_dyads$open<-as.character(rating_dyads$maize.owner.agree.temp.q80)
rating_dyads$open_storage<- ifelse(rating_dyads$open== 'No', 1, 0)
table(rating_dyads$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings or that the business is registered with some association)
rating_dyads$maize.owner.agree.q81
rating_dyads$cert<-as.character(rating_dyads$maize.owner.agree.temp.q81)
rating_dyads$cert_yes<- ifelse(rating_dyads$cert== 'Yes', 1, 0)
table(rating_dyads$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
rating_dyads$shop_rate<-as.numeric(as.character(rating_dyads$maize.owner.agree.temp.q82))
table(rating_dyads$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
#we try to formulate this as good quality variable, not getting any complaint is good
table(rating_dyads$maize.owner.agree.q96)
rating_dyads$complaint<- ifelse(rating_dyads$maize.owner.agree.q96== 'No', 1, 0)
table(rating_dyads$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
table(rating_dyads$maize.owner.agree.q70)

#roof leak proof?
rating_dyads$maize.owner.agree.q72
rating_dyads$roof<-as.character(rating_dyads$maize.owner.agree.temp.q72)
rating_dyads$leakproof<- ifelse(rating_dyads$roof== 'Yes', 1, 0)
table(rating_dyads$leakproof)


# convert to numbers and aggregate scores at agro-input dealer level for between regression -- also including controls 

rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating",
               "maize.owner.agree.age","prim","maize.owner.agree.q3", "maize.owner.agree.q4",  "inputsale", "years_shop", "dedicated_area", "pest_prob", "insulated", "wall_heatproof", "ventilation", "badlighting", "badstored", "open_storage", "cert_yes", "shop_rate", "complaint", "maize.owner.agree.q70",
               "leakproof") ] <- lapply(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating",
                         "maize.owner.agree.age","prim","maize.owner.agree.q3", "maize.owner.agree.q4",  "inputsale", "years_shop", "dedicated_area", "pest_prob", "insulated", "wall_heatproof", "ventilation", "badlighting", "badstored", "open_storage", "cert_yes", "shop_rate", "complaint", "maize.owner.agree.q70", "leakproof")], function(x) as.numeric(as.character(x)))

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

shop_av <- aggregate(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating",
                                    "maize.owner.agree.age","prim","maize.owner.agree.q3", "maize.owner.agree.q4",  "inputsale", "years_shop", "dedicated_area", "pest_prob", "insulated", "wall_heatproof", "ventilation", "badlighting", "badstored", "open_storage", "cert_yes", "shop_rate", "complaint", "maize.owner.agree.q70", "leakproof")],list(rating_dyads$shop_ID),FUN=mean, na.rm=T)

names(shop_av)[1] <- "shop_ID"
#merge in dealer gender from baseline data
rating_dyads <- merge(shop_av, baseline_dealer[c("shop_ID","maize.owner.agree.gender")],  by.x="shop_ID", by.y="shop_ID", all.x=T)

#rename
names(rating_dyads)[names(rating_dyads) == "maize.owner.agree.gender"] <- "gender"

rating_dyads$overall_rating <-  rowMeans(rating_dyads[c("general_rating","location_rating","price_rating","quality_rating","stock_rating","reputation_rating")],na.rm=T)
rating_dyads$score  <-  rowMeans(rating_dyads[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm=T)

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

#regressions with controls -- between regression 
#seed ratings 
summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof,data=rating_dyads))

summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof,data=rating_dyads))

summary(lm(seed_yield_rating ~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof,data=rating_dyads))

summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof,data=rating_dyads))

summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof,data=rating_dyads))

summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof,data=rating_dyads))

summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof,data=rating_dyads))




############## FIXED EFFECTS 

#merge to get gender 
rat <- merge(rat, merged_dealer, by="shop_ID")

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


fe3<- rbind( c((format(round(mean(fixef(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0)),
               (format(round(mean(fixef(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")))[1],digits=3),nsmall=0))),
             c((format(round(sum((plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum((plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$coefficients[1]),digits=3),nsmall=0))),
             c((format(round((coeftest(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=0)),
               (format(round((coeftest(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=0)),
               (format(round((coeftest(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=0)),
               (format(round((coeftest(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=0)),
               (format(round((coeftest(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=0)),
               (format(round((coeftest(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=0)),
               (format(round((coeftest(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,2],digits=3),nsmall=0))),
             
             c((format(round((coeftest(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=0)),
               (format(round((coeftest(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=0)),
               (format(round((coeftest(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=0)),
               (format(round((coeftest(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=0)),
               (format(round((coeftest(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=0)),
               (format(round((coeftest(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=0)),
               (format(round((coeftest(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), vcovHC(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"), type = "HC0", cluster = "time")))
                             [1,4],digits=3),nsmall=0))),
             
             
             c((format(round(summary(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[1],digits=3),nsmall=0))),
             
             c((format(round(summary(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within"))$r.squared[2],digits=3),nsmall=0))),
             
             c((format(round(nobs(plm(overall_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(general_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(location_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(price_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(quality_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(stock_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)),
               (format(round(nobs(plm(reputation_rating~gender, data = rat, index=c("farmer_ID","dealer_ID"), model="within")),digits=3),nsmall=0)))
)


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
