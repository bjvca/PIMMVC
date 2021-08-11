### run in:  ../PIMMVC/papers/perceptions
#rm(list=ls())
path <- getwd()

library(miceadds)
library(texreg)
library(plyr)
library(plm)

options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]




############################################ SEED SYSTEMS DATA ##################################################

##################################################################################################################
####################### BETWEEN FARMER --- FOCUS ON DEALER'S GENDER ##############################################

###########  MODEL 3 ###############
##################################################################################################################


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
                               tapply(as.numeric(between_farmer$maize.owner.agree.q70), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$general_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$location_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$price_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$stock_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_farmer$reputation_rating), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               tapply(between_farmer$bought_at_dealer=="Yes" | between_farmer$knows_other_customer=="Yes", between_farmer$farmer_ID,sum)))


names(reviews_bf) <- c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination","gender_avg","dealer_age","dealer_educ","tarmac_dealer",
                       "murram_dealer","farm_inputs","years_shop","dedicatedarea","pestprob","roof_insu","wall_heatproof","ventilation","plasterwall","goodfloor",
                       "badlighting","badstored","open_storage","cert","shop_rate","complaint","leakproof","temp","general_rating_nonseed","location","price",
                       "stock","reputation","nr_reviews")

reviews_bf$farmer_ID <- rownames(reviews_bf)

reviews_bf$score <-  rowMeans(reviews_bf[c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
reviews_bf$overall_rating <-  rowMeans(reviews_bf[c("general_rating_nonseed", "location","price","quality","stock","reputation")],na.rm=T)

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

#### SEED RELATED RATINGS ####

#### regressions without controls - seed related ratings 

m1<-lm(score~gender_avg , data = bfm)
se1 <- sqrt(diag(vcov(m1)))

m2<-lm(quality~gender_avg , data = bfm)
se2 <- sqrt(diag(vcov(m2)))

m3<-lm(general~gender_avg , data = bfm)
se3<- sqrt(diag(vcov(m3)))

m4<-lm(yield~gender_avg , data = bfm)
se4 <- sqrt(diag(vcov(m4)))

m5<-lm(drought_resistent~gender_avg , data = bfm)
se5<- sqrt(diag(vcov(m5)))

m6<-lm(disease_resistent~gender_avg , data = bfm)
se6<- sqrt(diag(vcov(m6)))

m7<-lm(early_maturing~gender_avg , data = bfm)
se7<- sqrt(diag(vcov(m7)))

m8<-lm(germination~gender_avg , data = bfm)
se8<- sqrt(diag(vcov(m8)))

summary(lm(score~gender_avg , data = bfm))
summary(lm(quality~gender_avg , data = bfm))
summary(lm(general~gender_avg , data = bfm))
summary(lm(yield~gender_avg , data = bfm))
summary(lm(drought_resistent~gender_avg , data = bfm))
summary(lm(disease_resistent~gender_avg , data = bfm))
summary(lm(early_maturing~gender_avg , data = bfm))
summary(lm(germination~gender_avg , data = bfm))


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

m9<-lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
         murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
         badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se9 <- sqrt(diag(vcov(m9)))

m10<-lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
         murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
         badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se10 <- sqrt(diag(vcov(m10)))

m11<-lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se11 <- sqrt(diag(vcov(m11)))

m12<-lm(yield~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se12 <- sqrt(diag(vcov(m12)))

m13<-lm(drought_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se13<- sqrt(diag(vcov(m13)))

m14<-lm(disease_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se14 <- sqrt(diag(vcov(m14)))

m15<-lm(early_maturing ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se15<- sqrt(diag(vcov(m15)))

m16<-lm(germination ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se16<- sqrt(diag(vcov(m16)))



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


#### NON-SEED RELATED RATINGS ####

#### regressions without controls - non-seed related ratings 

summary(lm(overall_rating~gender_avg , data = bfm))
summary(lm(general_rating_nonseed~gender_avg , data = bfm))
summary(lm(location~gender_avg , data = bfm))
summary(lm(price~gender_avg , data = bfm))
summary(lm(quality~gender_avg , data = bfm))
summary(lm(stock ~gender_avg , data = bfm))
summary(lm(reputation ~gender_avg , data = bfm))


#### regressions with dealer's gender (averaged) and farmer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(price~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 , data = bfm))
summary(lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(stock~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(reputation~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))


#### regressions with dealer's gender (averaged) and farmer + dealer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(price~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof , data = bfm))

summary(lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(stock~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))

summary(lm(reputation~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
             murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +plasterwall +goodfloor+
             badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp+leakproof, data = bfm))




################### MODEL 7 #########################

avg <- data.frame(cbind(tapply(as.numeric(between_farmer$quality_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$seed_quality_general_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$seed_yield_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$seed_drought_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$seed_disease_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$seed_maturing_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$seed_germinate_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$general_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$location_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$price_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$stock_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(as.numeric(between_farmer$reputation_rating), between_farmer$shop_ID,mean,na.rm=TRUE),
                        tapply(between_farmer$bought_at_dealer=="Yes" | between_farmer$knows_other_customer=="Yes", between_farmer$shop_ID,sum)))

names(avg) <- c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination","general_rating_nonseed","location","price",
                       "stock","reputation","nr_reviews")

avg$shop_ID <- rownames(avg)

avg$score <-  rowMeans(avg[c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
avg$overall_rating <-  rowMeans(avg[c("general_rating_nonseed", "location","price","quality","stock","reputation")],na.rm=T)

avg <- merge(avg, baseline_dealer, by="shop_ID") #merging with baseline dealer data to get the controls and gender 

#create gender dummy
avg$genderdummy <- ifelse(avg$maize.owner.agree.gender == "Male", 1, 0)

mean(avg$score[avg$genderdummy==1], na.rm=TRUE) #3.462431 -- seed avg quality, male 
mean(avg$score[avg$genderdummy==0], na.rm=TRUE) #3.377216 -- seed avg quality, female 

mean(avg$overall_rating[avg$genderdummy==1], na.rm=TRUE) #3.802201 -- non-seed avg rating , male 
mean(avg$overall_rating[avg$genderdummy==0], na.rm=TRUE) #3.674769 -- non-seed avg rating , female 


###dealer characteristics for  controls

#education of dealers 
avg$prim <- 0
avg$prim[avg$maize.owner.agree.educ=="c"|avg$maize.owner.agree.educ=="d"|avg$maize.owner.agree.educ=="e"|
           avg$maize.owner.agree.educ=="f"] <- 1
table(avg$prim)

#distance of shop to nearest tarmac road
table(avg$maize.owner.agree.q3)
avg$maize.owner.agree.q3[avg$maize.owner.agree.q3==999] <- NA

#distance of shop to nearest murram road 
table(avg$maize.owner.agree.q4)

#selling only farm inputs 
table(avg$maize.owner.agree.q5)
avg$inputsale<- ifelse(avg$maize.owner.agree.q5== 'Yes', 1, 0)  

#Q8. When was this agro-input shop established? (year)
avg$years_shop <- 2020 - as.numeric(as.character(substr(avg$maize.owner.agree.q8, start=1, stop=4)))

#seed stored in dedicated area?
avg$maize.owner.agree.q69
avg$dedarea<-as.character(avg$maize.owner.agree.temp.q69)
avg$dedicated_area<- ifelse(avg$dedarea== 'Yes', 1, 0)  
table(avg$dedicated_area)

#problem with rats or pests?
avg$maize.owner.agree.q71
avg$pest<-as.character(avg$maize.owner.agree.temp.q71)
avg$pest_prob<- ifelse(avg$pest== 'Yes', 1, 0)  
table(avg$pest_prob)

#roof leak proof?  
avg$maize.owner.agree.q72
avg$roof<-as.character(avg$maize.owner.agree.temp.q72)
avg$leakproof<- ifelse(avg$roof== 'Yes', 1, 0)  
table(avg$leakproof)

#roof insulated?
avg$maize.owner.agree.q73
avg$roof_insu<-as.character(avg$maize.owner.agree.temp.q73)
avg$insulated<- ifelse(avg$roof_insu== 'Yes', 1, 0)  
table(avg$insulated)

#walls insulated?
avg$maize.owner.agree.q74
avg$wall_insu<-as.character(avg$maize.owner.agree.temp.q74)
avg$wall_heatproof<- ifelse(avg$wall_insu== 'Yes', 1, 0)  
table(avg$wall_heatproof)

#area ventilated?
avg$maize.owner.agree.q75
avg$vent<-as.character(avg$maize.owner.agree.temp.q75)
avg$ventilation<- ifelse(avg$vent== 'Yes', 1, 0)  
table(avg$ventilation)

#plastered walls?
avg$maize.owner.agree.q76
avg$plas<-as.character(avg$maize.owner.agree.temp.q76)
avg$wall_plastered<- ifelse(avg$plas== 'Yes', 1, 0)  
table(avg$wall_plastered)

#Q77. Material of floor in areas where seed is stored?
avg$goodfloor <- 0
avg$goodfloor[avg$maize.owner.agree.temp.q77=="Cement"|avg$maize.owner.agree.temp.q77=="Tiles"] <-1
table(avg$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
avg$badlighting <- 0
avg$badlighting[avg$maize.owner.agree.temp.q78=="1"]<-1
table(avg$badlighting)

#Q79. On what surface are seed stored?
avg$badstored <- 0
avg$badstored[avg$maize.owner.agree.temp.q79=="1"|avg$maize.owner.agree.temp.q79=="2"| avg$maize.owner.agree.temp.q79=="96"]<-1
table(avg$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
avg$maize.owner.agree.q80
avg$open<-as.character(avg$maize.owner.agree.temp.q80)
avg$open_storage<- ifelse(avg$open== 'Yes', 1, 0)  
table(avg$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings 
#or that the business is registered with some association)
avg$maize.owner.agree.q81
avg$cert<-as.character(avg$maize.owner.agree.temp.q81)
avg$cert_yes<- ifelse(avg$cert== 'Yes', 1, 0)  
table(avg$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
avg$shop_rate<-as.numeric(as.character(avg$maize.owner.agree.temp.q82))
table(avg$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
avg$maize.owner.agree.q96
avg$complaint<- ifelse(avg$maize.owner.agree.q96== 'Yes', 1, 0)  
table(avg$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
avg$maize.owner.agree.q70


## SEED RELATED RATINGS ##

#### regressions without controls - seed related ratings 

summary(lm(score~genderdummy , data = avg))
summary(lm(quality~genderdummy , data = avg))
summary(lm(general~genderdummy , data = avg))
summary(lm(yield~genderdummy , data = avg))
summary(lm(drought_resistent~genderdummy , data = avg))
summary(lm(disease_resistent~genderdummy , data = avg))
summary(lm(early_maturing~genderdummy , data = avg))
summary(lm(germination~genderdummy , data = avg))

#### regressions with dealer's gender + dealer characteristics  --- seed related ratings 

summary(lm(score~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(general~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(yield~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(drought_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(disease_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(early_maturing~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(germination~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))


## NON SEED RELATED RATINGS ##

#### regressions without controls - non-seed related ratings 

summary(lm(overall_rating~genderdummy , data = avg))
summary(lm(general_rating_nonseed~genderdummy , data = avg))
summary(lm(location~genderdummy , data = avg))
summary(lm(price~genderdummy , data = avg))
summary(lm(quality~genderdummy , data = avg))
summary(lm(stock ~genderdummy , data = avg))
summary(lm(reputation ~genderdummy , data = avg))


#### regressions with dealer's gender + dealer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(general_rating_nonseed~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(location~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(price~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(stock~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(reputation~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))




################### MODEL 5 (FE) #########################

fedata <- merge(rating_dyads, baseline_dealer, by="shop_ID")

#create gender dummy
fedata$genderdummy <- ifelse(fedata$maize.owner.agree.gender == "Male", 1, 0)

###dealer characteristics for  controls

#education of dealers 
fedata$prim <- 0
fedata$prim[fedata$maize.owner.agree.educ=="c"|fedata$maize.owner.agree.educ=="d"|fedata$maize.owner.agree.educ=="e"|
              fedata$maize.owner.agree.educ=="f"] <- 1
table(fedata$prim)

#distance of shop to nearest tarmac road
table(fedata$maize.owner.agree.q3)
fedata$maize.owner.agree.q3[fedata$maize.owner.agree.q3==999] <- NA

#distance of shop to nearest murram road 
table(fedata$maize.owner.agree.q4)

#selling only farm inputs 
table(fedata$maize.owner.agree.q5)
fedata$inputsale<- ifelse(fedata$maize.owner.agree.q5== 'Yes', 1, 0)  

#Q8. When was this agro-input shop established? (year)
fedata$years_shop <- 2020 - as.numeric(as.character(substr(fedata$maize.owner.agree.q8, start=1, stop=4)))

#seed stored in dedicated area?
fedata$maize.owner.agree.q69
fedata$dedarea<-as.character(fedata$maize.owner.agree.temp.q69)
fedata$dedicated_area<- ifelse(fedata$dedarea== 'Yes', 1, 0)  
table(fedata$dedicated_area)

#problem with rats or pests?
fedata$maize.owner.agree.q71
fedata$pest<-as.character(fedata$maize.owner.agree.temp.q71)
fedata$pest_prob<- ifelse(fedata$pest== 'Yes', 1, 0)  
table(fedata$pest_prob)

#roof leak proof?  
fedata$maize.owner.agree.q72
fedata$roof<-as.character(fedata$maize.owner.agree.temp.q72)
fedata$leakproof<- ifelse(fedata$roof== 'Yes', 1, 0)  
table(fedata$leakproof)

#roof insulated?
fedata$maize.owner.agree.q73
fedata$roof_insu<-as.character(fedata$maize.owner.agree.temp.q73)
fedata$insulated<- ifelse(fedata$roof_insu== 'Yes', 1, 0)  
table(fedata$insulated)

#walls insulated?
fedata$maize.owner.agree.q74
fedata$wall_insu<-as.character(fedata$maize.owner.agree.temp.q74)
fedata$wall_heatproof<- ifelse(fedata$wall_insu== 'Yes', 1, 0)  
table(fedata$wall_heatproof)

#area ventilated?
fedata$maize.owner.agree.q75
fedata$vent<-as.character(fedata$maize.owner.agree.temp.q75)
fedata$ventilation<- ifelse(fedata$vent== 'Yes', 1, 0)  
table(fedata$ventilation)

#plastered walls?
fedata$maize.owner.agree.q76
fedata$plas<-as.character(fedata$maize.owner.agree.temp.q76)
fedata$wall_plastered<- ifelse(fedata$plas== 'Yes', 1, 0)  
table(fedata$wall_plastered)

#Q77. Material of floor in areas where seed is stored?
fedata$goodfloor <- 0
fedata$goodfloor[fedata$maize.owner.agree.temp.q77=="Cement"|fedata$maize.owner.agree.temp.q77=="Tiles"] <-1
table(fedata$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
fedata$badlighting <- 0
fedata$badlighting[fedata$maize.owner.agree.temp.q78=="1"]<-1
table(fedata$badlighting)

#Q79. On what surface are seed stored?
fedata$badstored <- 0
fedata$badstored[fedata$maize.owner.agree.temp.q79=="1"|fedata$maize.owner.agree.temp.q79=="2"| fedata$maize.owner.agree.temp.q79=="96"]<-1
table(fedata$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
fedata$maize.owner.agree.q80
fedata$open<-as.character(fedata$maize.owner.agree.temp.q80)
fedata$open_storage<- ifelse(fedata$open== 'Yes', 1, 0)  
table(fedata$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings 
#or that the business is registered with some association)
fedata$maize.owner.agree.q81
fedata$cert<-as.character(fedata$maize.owner.agree.temp.q81)
fedata$cert_yes<- ifelse(fedata$cert== 'Yes', 1, 0)  
table(fedata$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
fedata$shop_rate<-as.numeric(as.character(fedata$maize.owner.agree.temp.q82))
table(fedata$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
fedata$maize.owner.agree.q96
fedata$complaint<- ifelse(fedata$maize.owner.agree.q96== 'Yes', 1, 0)  
table(fedata$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
fedata$maize.owner.agree.q70

###AVERAGE RATINGS 
fedata$quality<- as.numeric(fedata$quality_rating)
fedata$general<- as.numeric(fedata$seed_quality_general_rating)
fedata$yield <- as.numeric(fedata$seed_yield_rating)
fedata$drought_resistent <- as.numeric(fedata$seed_drought_rating)
fedata$disease_resistent <- as.numeric(fedata$seed_disease_rating)
fedata$early_maturing<- as.numeric(fedata$seed_maturing_rating)
fedata$germination<- as.numeric(fedata$seed_germinate_rating)

fedata$general_rating_nonseed<- as.numeric(fedata$general_rating)
fedata$location<- as.numeric(fedata$location_rating)
fedata$price <- as.numeric(fedata$price_rating)
fedata$stock <- as.numeric(fedata$stock_rating)
fedata$reputation<- as.numeric(fedata$reputation_rating)


fedata$score <-  rowMeans(fedata[c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
fedata$overall_rating <-  rowMeans(fedata[c("general_rating_nonseed", "location","price","quality","stock","reputation")],na.rm=T)


## SEED RELATED RATINGS ##

#### regressions without controls - seed related ratings 

summary(lm(score~genderdummy +farmer_ID, data = fedata))
summary(lm(quality~genderdummy+farmer_ID , data = fedata))
summary(lm(general~genderdummy +farmer_ID, data = fedata))
summary(lm(yield~genderdummy +farmer_ID, data = fedata))
summary(lm(drought_resistent~genderdummy+farmer_ID , data = fedata))
summary(lm(disease_resistent~genderdummy+farmer_ID , data = fedata))
summary(lm(early_maturing~genderdummy+farmer_ID , data = fedata))
summary(lm(germination~genderdummy+farmer_ID , data = fedata))

#### regressions with dealer's gender + dealer characteristics  --- seed related ratings 

summary(lm(score~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(general~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(yield~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(drought_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(disease_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(early_maturing~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(germination~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))


## NON SEED RELATED RATINGS ##

#### regressions without controls - non-seed related ratings 

summary(lm(overall_rating~genderdummy +farmer_ID, data = fedata))
summary(lm(general_rating_nonseed~genderdummy +farmer_ID, data = fedata))
summary(lm(location~genderdummy +farmer_ID, data = fedata))
summary(lm(price~genderdummy +farmer_ID, data = fedata))
summary(lm(quality~genderdummy +farmer_ID, data = fedata))
summary(lm(stock ~genderdummy+farmer_ID , data = fedata))
summary(lm(reputation ~genderdummy+farmer_ID , data = fedata))


#### regressions with dealer's gender + dealer characteristics  --- non-seed related ratings 

summary(lm(overall_rating~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(general_rating_nonseed~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(location~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(price~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(stock~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(reputation~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +wall_plastered +goodfloor+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))






##################################################################################################################
####################### BETWEEN DEALER --- FOCUS ON FARMER'S GENDER ##############################################

rating_dyads <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))
##Farmers' dataset
farmers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_farmers.csv", sep = "/"))

#how many dealers have been rated (i.e. how many unique shopIDs are in the dyads dataset)?
print(length(table(rating_dyads$shop_ID)))

#MERGED_DATASET
between_dealer <- merge(rating_dyads, farmers_seed, by="shop_ID")

#create gender dummy
between_dealer$farmergen <- ifelse(between_dealer$Check2.check.maize.q15 == "Male", 1, 0)

###farmer characteristics for controls 
between_dealer$educ_f <- 0
between_dealer$educ_f[between_dealer$Check2.check.maize.q17=="b" |between_dealer$Check2.check.maize.q17=="c" | between_dealer$Check2.check.maize.q17=="d" | between_dealer$Check2.check.maize.q17=="e" | 
                        between_dealer$Check2.check.maize.q17=="f" |between_dealer$Check2.check.maize.q17=="g" ] <- 1 #educated farmers
between_dealer$married <- ifelse(between_dealer$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers

between_dealer[between_dealer=="n/a"]<- NA

### seed related ratings:

between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x) as.numeric(as.character(x)) )

between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x)replace(x, x == 98,NA) )

between_dealer$quality_rating[between_farmer$shop_ID == "AD_99"]

reviews_bd <- data.frame(cbind(tapply(as.numeric(between_dealer$quality_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$seed_quality_general_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$seed_yield_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$seed_drought_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$seed_disease_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$seed_maturing_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$seed_germinate_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$farmergen), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$educ_f), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$married), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$Check2.check.maize.q14), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$Check2.check.maize.q8), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$general_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$location_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$price_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$stock_rating), between_dealer$shop_ID,mean,na.rm=TRUE),
                               tapply(as.numeric(between_dealer$reputation_rating), between_dealer$shop_ID,mean,na.rm=TRUE)))

names(reviews_bd) <- c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination","gender_avgf","farmer_educ","farmer_married","farmer_age",
                       "farmer_tarmac","general_nonseed","location","price","stock","reputation")

reviews_bd$shop_ID <- rownames(reviews_bd)

reviews_bd$score_bd <-  rowMeans(reviews_bd[c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
reviews_bd$avg_rating <-  rowMeans(reviews_bd[c("quality","general_nonseed","location","price","stock","reputation")],na.rm=T)



###dealer characteristics for  controls
baseline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)

betwd <- merge(reviews_bd, baseline_dealer, by="shop_ID")

#education of dealers 
betwd$prim <- 0
betwd$prim[betwd$maize.owner.agree.educ=="c"|betwd$maize.owner.agree.educ=="d"|betwd$maize.owner.agree.educ=="e"|
                      betwd$maize.owner.agree.educ=="f"] <- 1
table(betwd$prim)

#distance of shop to nearest tarmac road
table(betwd$maize.owner.agree.q3)
betwd$maize.owner.agree.q3[betwd$maize.owner.agree.q3==999] <- NA

#distance of shop to nearest murram road 
table(betwd$maize.owner.agree.q4)

#selling only farm inputs 
table(betwd$maize.owner.agree.q5)
betwd$inputsale<- ifelse(betwd$maize.owner.agree.q5== 'Yes', 1, 0)  

#Q8. When was this agro-input shop established? (year)
betwd$years_shop <- 2020 - as.numeric(as.character(substr(betwd$maize.owner.agree.q8, start=1, stop=4)))

#seed stored in dedicated area?
betwd$maize.owner.agree.q69
betwd$dedarea<-as.character(betwd$maize.owner.agree.temp.q69)
betwd$dedicated_area<- ifelse(betwd$dedarea== 'Yes', 1, 0)  
table(betwd$dedicated_area)

#problem with rats or pests?
betwd$maize.owner.agree.q71
betwd$pest<-as.character(betwd$maize.owner.agree.temp.q71)
betwd$pest_prob<- ifelse(betwd$pest== 'Yes', 1, 0)  
table(betwd$pest_prob)

#roof leak proof?  ------ NOT USED AS WHILE AVERAGING HAVING SOME ISSUES 
betwd$maize.owner.agree.q72
betwd$roof<-as.character(betwd$maize.owner.agree.temp.q72)
betwd$leakproof<- ifelse(betwd$roof== 'Yes', 1, 0)  
table(betwd$leakproof)

#roof insulated?
betwd$maize.owner.agree.q73
betwd$roof_insu<-as.character(betwd$maize.owner.agree.temp.q73)
betwd$insulated<- ifelse(betwd$roof_insu== 'Yes', 1, 0)  
table(betwd$insulated)

#walls insulated?
betwd$maize.owner.agree.q74
betwd$wall_insu<-as.character(betwd$maize.owner.agree.temp.q74)
betwd$wall_heatproof<- ifelse(betwd$wall_insu== 'Yes', 1, 0)  
table(betwd$wall_heatproof)

#area ventilated?
betwd$maize.owner.agree.q75
betwd$vent<-as.character(betwd$maize.owner.agree.temp.q75)
betwd$ventilation<- ifelse(betwd$vent== 'Yes', 1, 0)  
table(betwd$ventilation)

#plastered walls?
betwd$maize.owner.agree.q76
betwd$plas<-as.character(betwd$maize.owner.agree.temp.q76)
betwd$wall_plastered<- ifelse(betwd$plas== 'Yes', 1, 0)  
table(betwd$wall_plastered)

#Q77. Material of floor in areas where seed is stored?
betwd$goodfloor <- 0
betwd$goodfloor[betwd$maize.owner.agree.temp.q77=="Cement"|betwd$maize.owner.agree.temp.q77=="Tiles"] <-1
table(betwd$goodfloor)

#Q78. Lighting conditions in area where seed is stored?
betwd$badlighting <- 0
betwd$badlighting[betwd$maize.owner.agree.temp.q78=="1"]<-1
table(betwd$badlighting)

#Q79. On what surface are seed stored?
betwd$badstored <- 0
betwd$badstored[betwd$maize.owner.agree.temp.q79=="1"|betwd$maize.owner.agree.temp.q79=="2"| betwd$maize.owner.agree.temp.q79=="96"]<-1
table(betwd$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
betwd$maize.owner.agree.q80
betwd$open<-as.character(betwd$maize.owner.agree.temp.q80)
betwd$open_storage<- ifelse(betwd$open== 'Yes', 1, 0)  
table(betwd$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings 
#or that the business is registered with some association)
betwd$maize.owner.agree.q81
betwd$cert<-as.character(betwd$maize.owner.agree.temp.q81)
betwd$cert_yes<- ifelse(betwd$cert== 'Yes', 1, 0)  
table(betwd$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
betwd$shop_rate<-as.numeric(as.character(betwd$maize.owner.agree.temp.q82))
table(betwd$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
betwd$maize.owner.agree.q96
betwd$complaint<- ifelse(betwd$maize.owner.agree.q96== 'Yes', 1, 0)  
table(betwd$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
betwd$temp<-betwd$maize.owner.agree.q70 


#### regressions without controls - seed related ratings 

summary(lm(score_bd~gender_avgf , data = betwd))
summary(lm(quality~gender_avgf , data = betwd))
summary(lm(general~gender_avgf, data = betwd))
summary(lm(yield~gender_avgf , data = betwd))
summary(lm(drought_resistent~gender_avgf , data = betwd))
summary(lm(disease_resistent~gender_avgf , data = betwd))
summary(lm(early_maturing~gender_avgf , data = betwd))
summary(lm(germination~gender_avgf , data = betwd))

#### regressions with farmer's gender (averaged) and dealer characteristics  --- seed related ratings 

summary(lm(score_bd~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(quality~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(general~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(yield~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(drought_resistent~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(disease_resistent~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(early_maturing~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(germination~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))


#### regressions with farmer's gender (averaged) and farmer+dealer characteristics  --- seed related ratings 

summary(lm(score_bd~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(quality~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(general~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(yield~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(drought_resistent~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(disease_resistent~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(early_maturing~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(germination~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))


#NON-SEED RELATED RATINGS 
##########################################


#### regressions without controls - non-seed related ratings 

summary(lm(avg_rating~gender_avgf , data = betwd))
summary(lm(general_nonseed~gender_avgf, data = betwd))
summary(lm(location~gender_avgf , data = betwd))
summary(lm(price~gender_avgf , data = betwd))
summary(lm(quality~gender_avgf , data = betwd))
summary(lm(stock~gender_avgf , data = betwd))
summary(lm(reputation~gender_avgf , data = betwd))


#### regressions with farmer's gender (averaged) and dealer characteristics  --- non-seed related ratings 

summary(lm(avg_rating~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(general_nonseed~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(location~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(price~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(quality~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(stock~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(reputation~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))


#### regressions with farmer's gender (averaged) and farmer+dealer characteristics  --- non-seed related ratings 

summary(lm(avg_rating~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(general_nonseed~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(location~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(price~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(quality~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(stock~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(reputation~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +wall_plastered +goodfloor+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))


