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
library(tidyverse)
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
#baseline data 
baseline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)

#midline data 
midline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems_midline/data/input_dealer/midline_dealer.csv", sep="/"), stringsAsFactors = FALSE)

merged_dealer <- merge(baseline_dealer, midline_dealer, by="shop_ID") #merging baseline and midline 


#prepping data for FE
#subsetting baseline dealers and considering only the required variables 
base<-baseline_dealer[c("maize.owner.agree.gender", "maize.owner.agree.age","maize.owner.agree.educ", "maize.owner.agree.q3", "maize.owner.agree.q4", "maize.owner.agree.q5",
                          "maize.owner.agree.q8", "maize.owner.agree.temp.q69", "maize.owner.agree.temp.q71", "maize.owner.agree.temp.q73", "maize.owner.agree.temp.q74", "maize.owner.agree.temp.q75",
                          "maize.owner.agree.temp.q78", "maize.owner.agree.temp.q79", "maize.owner.agree.temp.q80" , "maize.owner.agree.temp.q81", "maize.owner.agree.temp.q82", "maize.owner.agree.q96",
                          "maize.owner.agree.temp.q72", "shop_ID")]
basem<-baseline_dealer[c( "maize.owner.agree.q3", "maize.owner.agree.q4", "maize.owner.agree.q8", "shop_ID")] #subsetting baseline to populate midline with the constant variables 
mid<-midline_dealer[c("owner.agree.gender", "owner.agree.age","owner.agree.educ", "owner.agree.q5","owner.agree.temp.q69", "owner.agree.temp.q71", "owner.agree.temp.q73", "owner.agree.temp.q74", "owner.agree.temp.q75",
                        "owner.agree.temp.q78", "owner.agree.temp.q79", "owner.agree.temp.q80" , "owner.agree.temp.q81", "owner.agree.temp.q82", "owner.agree.q96",
                        "owner.agree.temp.q72", "shop_ID")] #subsetting midline dealers and considering only the required variables
names(mid) <- gsub(x = names(mid), pattern = "owner.", replacement = "maize.owner.") #variable matching with baseline  

midm<-merge(mid, basem, by="shop_ID") #merging both midline and baseline for FE

dealer_stack <- rbind(base, midm) #stacking dealer data from both periods 


#getting controls as averages for between dealers 
merged_dealer[merged_dealer==999] <- NA

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
merged_dealer$complaint_base<- ifelse(merged_dealer$maize.owner.agree.q96== 'No', 1, 0)  #baseline 
merged_dealer$complaint_mid<- ifelse(merged_dealer$owner.agree.q96== 'No', 1, 0)  #midline 
merged_dealer$complaint<-  rowMeans(merged_dealer[c("complaint_base","complaint_mid")],na.rm=T) #averaging 
table(merged_dealer$complaint)

#roof leak proof?
merged_dealer$leakproof_base<- ifelse(merged_dealer$maize.owner.agree.temp.q72== 'Yes', 1, 0)  #baseline 
merged_dealer$leakproof_mid<- ifelse(merged_dealer$owner.agree.temp.q72== 'Yes', 1, 0)  #midline 
merged_dealer$leakproof<-  rowMeans(merged_dealer[c("leakproof_base","leakproof_mid")],na.rm=T) #averaging 
table(merged_dealer$leakproof)

#select only those that have same gender in baseline and endline
to_select <- merged_dealer$shop_ID[merged_dealer$maize.owner.agree.gender == merged_dealer$owner.agree.gender]

#stack rating dyads from the two rounds
rating_dyads <- rbind(rating_dyads, rating_dyads_midline)

#keep only those that have same gender in baseline and endline
rating_dyads <- subset(rating_dyads,shop_ID  %in% to_select)

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
rating_dyads <- merge(shop_av, merged_dealer[c("shop_ID","maize.owner.agree.gender",
                                               "maize.owner.agree.age","prim","maize.owner.agree.q3", "maize.owner.agree.q4",  "inputsale", "years_shop", "dedicated_area", "pest_prob", "insulated", "wall_heatproof", "ventilation", "badlighting", "badstored", "open_storage", "cert_yes", "shop_rate", "complaint", "leakproof")],  by.x="shop_ID", by.y="shop_ID", all.x=T)

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
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))

summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))

summary(lm(seed_yield_rating ~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))

summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof,data=rating_dyads))

summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))

summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))

summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof,data=rating_dyads))

#storing seed ratings 
s6<- rbind(c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[1],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[2],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[3],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[4],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[5],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[6],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[7],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[8],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0))),
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[9],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[10],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[11],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[12],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[13],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[14],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[15],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[16],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[17],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[18],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[19],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[20],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0))),
           
           # c((format(round(sum((lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21]),digits=3),nsmall=0)),
           #   (format(round(sum((lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21]),digits=3),nsmall=0)),
           #   (format(round(sum((lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21]),digits=3),nsmall=0)),
           #   (format(round(sum((lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21]),digits=3),nsmall=0)),
           #   (format(round(sum((lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21]),digits=3),nsmall=0)),
           #   (format(round(sum((lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21]),digits=3),nsmall=0)),
           #   (format(round(sum((lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21]),digits=3),nsmall=0))),
           # 
           # c((format(round(sqrt(diag(vcov(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[21],digits=3),nsmall=0)),
           #   (format(round(sqrt(diag(vcov(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[21],digits=3),nsmall=0)),
           #   (format(round(sqrt(diag(vcov(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[21],digits=3),nsmall=0)),
           #   (format(round(sqrt(diag(vcov(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[21],digits=3),nsmall=0)),
           #   (format(round(sqrt(diag(vcov(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[21],digits=3),nsmall=0)),
           #   (format(round(sqrt(diag(vcov(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[21],digits=3),nsmall=0)),
           #   (format(round(sqrt(diag(vcov(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))))[21],digits=3),nsmall=0))),
           # 
           # c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21,4],digits=3),nsmall=0)),
           #   (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21,4],digits=3),nsmall=0)),
           #   (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21,4],digits=3),nsmall=0)),
           #   (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21,4],digits=3),nsmall=0)),
           #   (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21,4],digits=3),nsmall=0)),
           #   (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21,4],digits=3),nsmall=0)),
           #   (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #                              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$coefficients[21,4],digits=3),nsmall=0))),
           
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0))),
           
           c((format(round(summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0))),
           
           c((format(round(nobs(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +    leakproof,data=rating_dyads)),digits=3),nsmall=0)))
)




#dealer ratings 
summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))

summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof,data=rating_dyads))

summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))

summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof,data=rating_dyads))

summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof,data=rating_dyads))

summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))

summary(lm(reputation_rating ~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof,data=rating_dyads))



#storing dealer ratings
s8<- rbind(c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[1],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[1],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[1,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[2],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[2],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[2,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[3],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[3],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[3,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[4],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[4],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[4,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[5],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[5],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[5,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))))[6],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[6],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[6,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[7],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[7],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[7,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[8],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[8],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[8,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[9],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[9],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[9,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[10],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[10],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[10,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[11],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[11],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[11,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[12],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[12],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[12,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[13],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[13],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[13,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[14],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[14],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[14,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[15],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[15],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[15,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[16],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[16],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[16,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[17],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[17],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[17,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[18],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[18],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[18,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[19],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[19],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[19,4],digits=3),nsmall=0))),
           
           c((format(round(sum((lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum((lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0)),
             0,
             (format(round(sum((lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20]),digits=3),nsmall=0))),
           
           c((format(round(sqrt(diag(vcov(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             (format(round(sqrt(diag(vcov(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[20],digits=3),nsmall=0)),
             0,
             (format(round(sqrt(diag(vcov(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                               badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))))[20],digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$coefficients[20,4],digits=3),nsmall=0))),
           
           
           
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$r.squared,digits=3),nsmall=0))),
           
           c((format(round(summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                        badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads))$adj.r.squared,digits=3),nsmall=0))),
           
           c((format(round(nobs(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4,data=rating_dyads)),digits=3),nsmall=0)),
             (format(round(nobs(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
                                     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +  leakproof,data=rating_dyads)),digits=3),nsmall=0)))
)


############## FIXED EFFECTS 

#merge to get gender --- merging both stacked datasets 
rat <- merge(rat, dealer_stack, by="shop_ID")

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


#FE at farmer level --- with controls
#dealer ratings

plm1<-plm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
      badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(overall_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof+farmer_ID, data = rat))

plm2<-plm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
      badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof+farmer_ID, data = rat))

plm3<-plm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(location_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+farmer_ID, data = rat))

plm4<-plm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
      badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(price_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof+farmer_ID, data = rat))

plm5<-plm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
      badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(quality_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof+farmer_ID, data = rat))

plm6<-plm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(stock_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+farmer_ID, data = rat))

plm7<-plm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
      badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(reputation_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint+leakproof+farmer_ID, data = rat))


#storing dealer ratings
fe4<- rbind(c((format(round(mean(fixef(plm1))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm2))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm3))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm4))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm5))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm6))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm7))[1],digits=3),nsmall=0))),
            c((format(round(sum((plm1)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm3)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm4)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm6)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm7)$coefficients[1]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm3)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm4)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm6)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm7)$coefficients[2]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm3)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm4)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm6)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm7)$coefficients[3]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm3)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm4)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm6)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm7)$coefficients[4]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm3)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm4)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm6)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm7)$coefficients[5]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm3), vcovHC((plm3), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm6), vcovHC((plm6), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[6]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm4)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[6]),digits=3),nsmall=0)),
            0,
              (format(round(sum((plm7)$coefficients[6]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
          0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[7]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm4)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[7]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm7)$coefficients[7]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0))),


            c((format(round(sum((plm1)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[8]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm4)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[8]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm7)$coefficients[8]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
           0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[9]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm4)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[9]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm7)$coefficients[9]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[10]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm4)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[10]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm7)$coefficients[10]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[11]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm4)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[11]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm7)$coefficients[11]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0))),


            c((format(round(sum((plm1)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[12]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm4)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[12]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm7)$coefficients[12]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[13]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm4)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[13]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm7)$coefficients[13]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[14]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm4)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[14]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm7)$coefficients[14]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
           0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[15]),digits=3),nsmall=0)),
            0,
              (format(round(sum((plm4)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[15]),digits=3),nsmall=0)),
            0,
              (format(round(sum((plm7)$coefficients[15]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[16]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm4)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[16]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm7)$coefficients[16]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[17]),digits=3),nsmall=0)),
          0,
              (format(round(sum((plm4)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[17]),digits=3),nsmall=0)),
             0,
              (format(round(sum((plm7)$coefficients[17]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[18]),digits=3),nsmall=0)),
        0,
              (format(round(sum((plm4)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[18]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm7)$coefficients[18]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0))),

            c((format(round(sum((plm1)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm2)$coefficients[19]),digits=3),nsmall=0)),
              0,
              (format(round(sum((plm4)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm5)$coefficients[19]),digits=3),nsmall=0)),
           0,
              (format(round(sum((plm7)$coefficients[19]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
            0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              0,
              (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
             0,
              (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0))),

        # c((format(round(sum((plm1)$coefficients[20]),digits=3),nsmall=0)),
        #   (format(round(sum((plm2)$coefficients[20]),digits=3),nsmall=0)),
        #   0,
        #   (format(round(sum((plm4)$coefficients[20]),digits=3),nsmall=0)),
        #   (format(round(sum((plm5)$coefficients[20]),digits=3),nsmall=0)),
        #   0,
        #   (format(round(sum((plm7)$coefficients[20]),digits=3),nsmall=0))),
        # c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[20,2],digits=3),nsmall=0)),
        #   (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[20,2],digits=3),nsmall=0)),
        #   0,
        #   (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[20,2],digits=3),nsmall=0)),
        #   (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[20,2],digits=3),nsmall=0)),
        #   0,
        #   (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[20,2],digits=3),nsmall=0))),
        # c((format(round(coeftest((plm1), vcovHC((plm1), type = "HC0",cluster = "time"))[20,4],digits=3),nsmall=0)),
        #   (format(round(coeftest((plm2), vcovHC((plm2), type = "HC0",cluster = "time"))[20,4],digits=3),nsmall=0)),
        #   0,
        #   (format(round(coeftest((plm4), vcovHC((plm4), type = "HC0",cluster = "time"))[20,4],digits=3),nsmall=0)),
        #   (format(round(coeftest((plm5), vcovHC((plm5), type = "HC0",cluster = "time"))[20,4],digits=3),nsmall=0)),
        #   0,
        #   (format(round(coeftest((plm7), vcovHC((plm7), type = "HC0",cluster = "time"))[20,4],digits=3),nsmall=0))),



              c((format(round(summary(plm1)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plm2)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plm3)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plm4)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plm5)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plm6)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plm7)$r.squared[1],digits=3),nsmall=0))),
            c((format(round(summary(plm1)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm2)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm3)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm4)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm5)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm6)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm7)$r.squared[2],digits=3),nsmall=0))),
              c((format(round(nobs(plm1),digits=3),nsmall=0)),
                (format(round(nobs(plm2),digits=3),nsmall=0)),
                (format(round(nobs(plm3),digits=3),nsmall=0)),
                (format(round(nobs(plm4),digits=3),nsmall=0)),
                (format(round(nobs(plm5),digits=3),nsmall=0)),
                (format(round(nobs(plm6),digits=3),nsmall=0)),
                (format(round(nobs(plm7),digits=3),nsmall=0)))
            )



#FE at farmer level --- with controls
#seed ratings

plm8<-plm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
            badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(score~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof+farmer_ID, data = rat))

plm9<-plm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
            badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_quality_general_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof+farmer_ID, data = rat))

plm10<-plm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_yield_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof+farmer_ID, data = rat))

plm11<-plm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
            badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_drought_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof+farmer_ID, data = rat))

plm12<-plm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
            badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_disease_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint +leakproof+farmer_ID, data = rat))

plm13<-plm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_maturing_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof+farmer_ID, data = rat))

plm14<-plm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
            badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint  +leakproof, data = rat, index=c("farmer_ID","dealer_ID"), model="within")
summary(lm(seed_germinate_rating~gender+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint+leakproof+farmer_ID, data = rat))


#storing seed ratings
fe2<- rbind(c((format(round(mean(fixef(plm8))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm9))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm10))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm11))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm12))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm13))[1],digits=3),nsmall=0)),
              (format(round(mean(fixef(plm14))[1],digits=3),nsmall=0))),
            c((format(round(sum((plm8)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[1]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[1,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[1,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[2]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[2,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[2,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[3]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[3,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[3,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[4]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[4,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[4,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[5]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[5,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[5,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[6]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[6,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[6,4],digits=3),nsmall=0))),
            
            
            c((format(round(sum((plm8)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[7]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[7,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[7,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[8]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[8,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[8,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[9]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[9,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[9,4],digits=3),nsmall=0))),
            
            
            c((format(round(sum((plm8)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[10]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[10,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[10,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[11]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[11,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[11,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[12]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[12,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[12,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[13]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[13,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[13,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[14]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[14,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[14,4],digits=3),nsmall=0))),
            
            
            c((format(round(sum((plm8)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[15]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[15,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[15,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[16]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[16,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[16,4],digits=3),nsmall=0))),
            
            c((format(round(sum((plm8)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[17]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[17,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[17,4],digits=3),nsmall=0))),
            
            
            c((format(round(sum((plm8)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[18]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[18,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[18,4],digits=3),nsmall=0))),
            
            
            c((format(round(sum((plm8)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm9)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm10)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm11)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm12)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm13)$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum((plm14)$coefficients[19]),digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[19,2],digits=3),nsmall=0))),
            c((format(round(coeftest((plm8), vcovHC((plm8), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm9), vcovHC((plm9), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm10), vcovHC((plm10), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm11), vcovHC((plm11), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm12), vcovHC((plm12), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm13), vcovHC((plm13), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0)),
              (format(round(coeftest((plm14), vcovHC((plm14), type = "HC0",cluster = "time"))[19,4],digits=3),nsmall=0))),
            
            
            
            
            c((format(round(summary(plm8)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plm9)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plm10)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plm11)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plm12)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plm13)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plm14)$r.squared[1],digits=3),nsmall=0))),
            c((format(round(summary(plm8)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm9)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm10)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm11)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm12)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm13)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plm14)$r.squared[2],digits=3),nsmall=0))),
            c((format(round(nobs(plm8),digits=3),nsmall=0)),
              (format(round(nobs(plm9),digits=3),nsmall=0)),
              (format(round(nobs(plm10),digits=3),nsmall=0)),
              (format(round(nobs(plm11),digits=3),nsmall=0)),
              (format(round(nobs(plm12),digits=3),nsmall=0)),
              (format(round(nobs(plm13),digits=3),nsmall=0)),
              (format(round(nobs(plm14),digits=3),nsmall=0)))
)

