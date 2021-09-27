### run in:  ../PIMMVC/papers/perceptions
#rm(list=ls())
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


rating_dyads <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))
baseline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)

#how many farmers have rated at least one agro-input dealers (i.e. how many unique farmerIDs are in the dyads dataset)?
#print(length(table(rating_dyads$farmer_ID)))


#MERGED_DATASET
between_farmer <- merge(rating_dyads, baseline_dealer, by="shop_ID")

#create gender dummy
between_farmer$genderdummy <- ifelse(between_farmer$maize.owner.agree.gender == "Male", 1, 0)


###dealer characteristics for  controls

#education of dealers
table(between_farmer$maize.owner.agree.educ) #430 are g which is Other
between_farmer$prim <- 0
#between_farmer$prim[between_farmer$maize.owner.agree.educ=="c"|between_farmer$maize.owner.agree.educ=="d"|between_farmer$maize.owner.agree.educ=="e"|between_farmer$maize.owner.agree.educ=="f"]<- 1

#finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
between_farmer$prim[between_farmer$maize.owner.agree.educ=="e"|between_farmer$maize.owner.agree.educ=="f"] <- 1
between_farmer$prim[between_farmer$maize.owner.agree.educ=="g"]<- NA
table(between_farmer$prim)
#5208 ARE 1 --- 45 percent


#age of dealer
summary(between_farmer$maize.owner.agree.age)
between_farmer$maize.owner.agree.age[between_farmer$maize.owner.agree.age==999] <- NA
table(between_farmer$maize.owner.agree.age)

#distance of shop to nearest tarmac road
table(between_farmer$maize.owner.agree.q3)
between_farmer$maize.owner.agree.q3[between_farmer$maize.owner.agree.q3==999] <- NA
summary(between_farmer$maize.owner.agree.q3)

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
#we try to formulate this as a good quality variable --- So, no problem with pests is good
between_farmer$maize.owner.agree.q71
between_farmer$pest<-as.character(between_farmer$maize.owner.agree.temp.q71)
between_farmer$pest_prob<- ifelse(between_farmer$pest== 'No', 1, 0)
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

#plastered walls? ---- DROP
#between_farmer$plas<-as.character(between_farmer$maize.owner.agree.temp.q76)
#between_farmer$wall_plastered<- ifelse(between_farmer$plas== 'Yes', 1, 0)
#table(between_farmer$wall_plastered)

#Q77. Material of floor in areas where seed is stored? ---- DROP
#between_farmer$goodfloor<- ifelse(between_farmer$maize.owner.agree.temp.q77=="Tiles", 1, 0)
#between_farmer$goodfloor <- 0
#between_farmer$goodfloor[between_farmer$maize.owner.agree.temp.q77=="Cement"|between_farmer$maize.owner.agree.temp.q77=="Tiles"] <-1
#between_farmer$goodfloor[between_farmer$maize.owner.agree.temp.q77==96]<- NA
#between_farmer$goodfloor[between_farmer$maize.owner.agree.temp.q77=="Tiles"] <-1
#table(between_farmer$goodfloor)
#99 PERCENT OF THE DEALERS HAVE CEMENT OR TILES AS THEIR FLOOR

#Q78. Lighting conditions in area where seed is stored?
#we try to formulate this as good quality variable --- 2 is good lighting
between_farmer$badlighting <- 0
between_farmer$badlighting[between_farmer$maize.owner.agree.temp.q78=="2"]<-1
table(between_farmer$badlighting)

#Q79. On what surface are seed stored?
#we try to formulate this as good quality variable ---- 3,4,5 are good storage surfaces
between_farmer$badstored <- 0
between_farmer$badstored[between_farmer$maize.owner.agree.temp.q79=="3"|between_farmer$maize.owner.agree.temp.q79=="4"|between_farmer$maize.owner.agree.temp.q79=="5"]<-1
between_farmer$badstored[between_farmer$maize.owner.agree.temp.q79==96]<-NA
table(between_farmer$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
#we try to formulate this as good quality variable --- Not being stored in open containers is good
between_farmer$maize.owner.agree.q80
between_farmer$open<-as.character(between_farmer$maize.owner.agree.temp.q80)
between_farmer$open_storage<- ifelse(between_farmer$open== 'No', 1, 0)
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
#we try to formulate this as good quality variable, not getting any complaint is good
table(between_farmer$maize.owner.agree.q96)
between_farmer$complaint<- ifelse(between_farmer$maize.owner.agree.q96== 'No', 1, 0)
table(between_farmer$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
table(between_farmer$maize.owner.agree.q70)
between_farmer$maize.owner.agree.q70[between_farmer$maize.owner.agree.q70==999] <- NA

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
                               #tapply(as.numeric(between_farmer$wall_plastered), between_farmer$farmer_ID,mean,na.rm=TRUE),
                               #tapply(as.numeric(between_farmer$goodfloor), between_farmer$farmer_ID,mean,na.rm=TRUE),
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
                       "murram_dealer","farm_inputs","years_shop","dedicatedarea","pestprob","roof_insu","wall_heatproof","ventilation",
                       "badlighting","badstored","open_storage","cert","shop_rate","complaint","leakproof","temp","general_rating_nonseed","location","price",
                       "stock","reputation","nr_reviews")

reviews_bf$farmer_ID <- rownames(reviews_bf)

reviews_bf$score <-  rowMeans(reviews_bf[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
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
bfm$educ_f[bfm$Check2.check.maize.q17=="c" | bfm$Check2.check.maize.q17=="d" | bfm$Check2.check.maize.q17=="e" |
         bfm$Check2.check.maize.q17=="f"  ] <- 1 #educated farmers -- finished primary educ
bfm$educ_f[ bfm$Check2.check.maize.q17=="g" ] <- NA
table(bfm$educ_f) #52.5 percent have finished primary educ

bfm$married <- ifelse(bfm$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers -- 88 percent

#age of farmers
bfm$Check2.check.maize.q14[ bfm$Check2.check.maize.q14==999 ] <- NA
summary(bfm$Check2.check.maize.q14 )

#distance from tarmac road
bfm$Check2.check.maize.q8[ bfm$Check2.check.maize.q8==999 ] <- NA
summary(bfm$Check2.check.maize.q8 )

#### SEED RELATED RATINGS ####

#### regressions without controls - seed related ratings

m1<-lm(score~gender_avg , data = bfm)
se1 <- sqrt(diag(vcov(m1)))

#m2<-lm(quality~gender_avg , data = bfm)
#se2 <- sqrt(diag(vcov(m2)))

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


s1<- rbind(c((format(round(sum(m1$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m3$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m4$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m5$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m6$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m7$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m8$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(se1[1],digits=3),nsmall=0)),
             (format(round(se3[1],digits=3),nsmall=0)),
             (format(round(se4[1],digits=3),nsmall=0)),
             (format(round(se5[1],digits=3),nsmall=0)),
             (format(round(se6[1],digits=3),nsmall=0)),
             (format(round(se7[1],digits=3),nsmall=0)),
             (format(round(se8[1],digits=3),nsmall=0))),
           c((format(round(summary(m1)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m3)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m4)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m5)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m6)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m7)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m8)$coefficients[1,4],digits=3),nsmall=0))),

           c((format(round(sum(m1$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m3$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m4$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m5$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m6$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m7$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m8$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(se1[2],digits=3),nsmall=0)),
             (format(round(se3[2],digits=3),nsmall=0)),
             (format(round(se4[2],digits=3),nsmall=0)),
             (format(round(se5[2],digits=3),nsmall=0)),
             (format(round(se6[2],digits=3),nsmall=0)),
             (format(round(se7[2],digits=3),nsmall=0)),
             (format(round(se8[2],digits=3),nsmall=0))),
           c((format(round(summary(m1)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m3)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m4)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m5)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m6)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m7)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m8)$coefficients[2,4],digits=3),nsmall=0))),

           c((format(round(summary(m1)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m3)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m4)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m5)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m6)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m7)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m8)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(m1)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m3)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m4)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m5)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m6)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m7)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m8)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(m1),digits=3),nsmall=0)),
             (format(round(nobs(m3),digits=3),nsmall=0)),
             (format(round(nobs(m4),digits=3),nsmall=0)),
             (format(round(nobs(m5),digits=3),nsmall=0)),
             (format(round(nobs(m6),digits=3),nsmall=0)),
             (format(round(nobs(m7),digits=3),nsmall=0)),
             (format(round(nobs(m8),digits=3),nsmall=0)))
           )




summary(lm(score~gender_avg , data = bfm))
#summary(lm(quality~gender_avg , data = bfm))
summary(lm(general~gender_avg , data = bfm))
summary(lm(yield~gender_avg , data = bfm))
summary(lm(drought_resistent~gender_avg , data = bfm))
summary(lm(disease_resistent~gender_avg , data = bfm))
summary(lm(early_maturing~gender_avg , data = bfm))
summary(lm(germination~gender_avg , data = bfm))


#### regressions with dealer's gender (averaged) and farmer characteristics  --- seed related ratings

summary(lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
#summary(lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(yield~gender_avg+ educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14 , data = bfm))
summary(lm(drought_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(disease_resistent~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(early_maturing~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))
summary(lm(germination~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm))


#### regressions with dealer's gender (averaged) and farmer characteristics  --- seed related ratings

#m9<-lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#se9 <- sqrt(diag(vcov(m9)))

#m10<-lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
 #        murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
  #       badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
#se10 <- sqrt(diag(vcov(m10)))

#m11<-lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#se11 <- sqrt(diag(vcov(m11)))

#m12<-lm(yield~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#se12 <- sqrt(diag(vcov(m12)))

#m13<-lm(drought_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#se13<- sqrt(diag(vcov(m13)))

#m14<-lm(disease_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#se14 <- sqrt(diag(vcov(m14)))

#m15<-lm(early_maturing ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#se15<- sqrt(diag(vcov(m15)))

#m16<-lm(germination ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#se16<- sqrt(diag(vcov(m16)))

#### regressions with dealer's gender (averaged) and farmer+dealer characteristics  --- seed related ratings 

m9<-lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
         murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
         badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se9 <- sqrt(diag(vcov(m9)))

#m10<-lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
#        murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
#       badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
#se10 <- sqrt(diag(vcov(m10)))

m11<-lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se11 <- sqrt(diag(vcov(m11)))

m12<-lm(yield~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se12 <- sqrt(diag(vcov(m12)))

m13<-lm(drought_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se13<- sqrt(diag(vcov(m13)))

m14<-lm(disease_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se14 <- sqrt(diag(vcov(m14)))

m15<-lm(early_maturing ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se15<- sqrt(diag(vcov(m15)))

m16<-lm(germination ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
se16<- sqrt(diag(vcov(m16)))



s2<- rbind(c((format(round(sum(m9$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(se9[1],digits=3),nsmall=0)),
             (format(round(se11[1],digits=3),nsmall=0)),
             (format(round(se12[1],digits=3),nsmall=0)),
             (format(round(se13[1],digits=3),nsmall=0)),
             (format(round(se14[1],digits=3),nsmall=0)),
             (format(round(se15[1],digits=3),nsmall=0)),
             (format(round(se16[1],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[1,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[2]),digits=3),nsmall=0)),
             # (format(round(sum(m10$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(se9[2],digits=3),nsmall=0)),
             # (format(round(se10[2],digits=3),nsmall=0)),
             (format(round(se11[2],digits=3),nsmall=0)),
             (format(round(se12[2],digits=3),nsmall=0)),
             (format(round(se13[2],digits=3),nsmall=0)),
             (format(round(se14[2],digits=3),nsmall=0)),
             (format(round(se15[2],digits=3),nsmall=0)),
             (format(round(se16[2],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[2,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[2,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[3]),digits=3),nsmall=0)),
             # (format(round(sum(m10$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[3]),digits=3),nsmall=0))),
           c((format(round(se9[3],digits=3),nsmall=0)),
             #(format(round(se10[3],digits=3),nsmall=0)),
             (format(round(se11[3],digits=3),nsmall=0)),
             (format(round(se12[3],digits=3),nsmall=0)),
             (format(round(se13[3],digits=3),nsmall=0)),
             (format(round(se14[3],digits=3),nsmall=0)),
             (format(round(se15[3],digits=3),nsmall=0)),
             (format(round(se16[3],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[3,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[3,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[4]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[4]),digits=3),nsmall=0))),
           c((format(round(se9[4],digits=3),nsmall=0)),
             # (format(round(se10[4],digits=3),nsmall=0)),
             (format(round(se11[4],digits=3),nsmall=0)),
             (format(round(se12[4],digits=3),nsmall=0)),
             (format(round(se13[4],digits=3),nsmall=0)),
             (format(round(se14[4],digits=3),nsmall=0)),
             (format(round(se15[4],digits=3),nsmall=0)),
             (format(round(se16[4],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[4,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[4,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[5]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[5]),digits=3),nsmall=0))),
           c((format(round(se9[5],digits=3),nsmall=0)),
             # (format(round(se10[5],digits=3),nsmall=0)),
             (format(round(se11[5],digits=3),nsmall=0)),
             (format(round(se12[5],digits=3),nsmall=0)),
             (format(round(se13[5],digits=3),nsmall=0)),
             (format(round(se14[5],digits=3),nsmall=0)),
             (format(round(se15[5],digits=3),nsmall=0)),
             (format(round(se16[5],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[5,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[5,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[6]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[6]),digits=3),nsmall=0))),
           c((format(round(se9[6],digits=3),nsmall=0)),
             # (format(round(se10[6],digits=3),nsmall=0)),
             (format(round(se11[6],digits=3),nsmall=0)),
             (format(round(se12[6],digits=3),nsmall=0)),
             (format(round(se13[6],digits=3),nsmall=0)),
             (format(round(se14[6],digits=3),nsmall=0)),
             (format(round(se15[6],digits=3),nsmall=0)),
             (format(round(se16[6],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[6,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[6,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[7]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[7]),digits=3),nsmall=0))),
           c((format(round(se9[7],digits=3),nsmall=0)),
             #(format(round(se10[7],digits=3),nsmall=0)),
             (format(round(se11[7],digits=3),nsmall=0)),
             (format(round(se12[7],digits=3),nsmall=0)),
             (format(round(se13[7],digits=3),nsmall=0)),
             (format(round(se14[7],digits=3),nsmall=0)),
             (format(round(se15[7],digits=3),nsmall=0)),
             (format(round(se16[7],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[7,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[7,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[8]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[8]),digits=3),nsmall=0))),
           c((format(round(se9[8],digits=3),nsmall=0)),
             #(format(round(se10[8],digits=3),nsmall=0)),
             (format(round(se11[8],digits=3),nsmall=0)),
             (format(round(se12[8],digits=3),nsmall=0)),
             (format(round(se13[8],digits=3),nsmall=0)),
             (format(round(se14[8],digits=3),nsmall=0)),
             (format(round(se15[8],digits=3),nsmall=0)),
             (format(round(se16[8],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[8,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[8,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[9]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[9]),digits=3),nsmall=0))),
           c((format(round(se9[9],digits=3),nsmall=0)),
             #(format(round(se10[9],digits=3),nsmall=0)),
             (format(round(se11[9],digits=3),nsmall=0)),
             (format(round(se12[9],digits=3),nsmall=0)),
             (format(round(se13[9],digits=3),nsmall=0)),
             (format(round(se14[9],digits=3),nsmall=0)),
             (format(round(se15[9],digits=3),nsmall=0)),
             (format(round(se16[9],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[9,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[9,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[10]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[10]),digits=3),nsmall=0))),
           c((format(round(se9[10],digits=3),nsmall=0)),
             #(format(round(se10[10],digits=3),nsmall=0)),
             (format(round(se11[10],digits=3),nsmall=0)),
             (format(round(se12[10],digits=3),nsmall=0)),
             (format(round(se13[10],digits=3),nsmall=0)),
             (format(round(se14[10],digits=3),nsmall=0)),
             (format(round(se15[10],digits=3),nsmall=0)),
             (format(round(se16[10],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[10,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[10,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[11]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[11]),digits=3),nsmall=0))),
           c((format(round(se9[11],digits=3),nsmall=0)),
             #(format(round(se10[11],digits=3),nsmall=0)),
             (format(round(se11[11],digits=3),nsmall=0)),
             (format(round(se12[11],digits=3),nsmall=0)),
             (format(round(se13[11],digits=3),nsmall=0)),
             (format(round(se14[11],digits=3),nsmall=0)),
             (format(round(se15[11],digits=3),nsmall=0)),
             (format(round(se16[11],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[11,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[11,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[12]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[12]),digits=3),nsmall=0))),
           c((format(round(se9[12],digits=3),nsmall=0)),
             #(format(round(se10[12],digits=3),nsmall=0)),
             (format(round(se11[12],digits=3),nsmall=0)),
             (format(round(se12[12],digits=3),nsmall=0)),
             (format(round(se13[12],digits=3),nsmall=0)),
             (format(round(se14[12],digits=3),nsmall=0)),
             (format(round(se15[12],digits=3),nsmall=0)),
             (format(round(se16[12],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[12,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[12,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[13]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[13]),digits=3),nsmall=0))),
           c((format(round(se9[13],digits=3),nsmall=0)),
             #(format(round(se10[13],digits=3),nsmall=0)),
             (format(round(se11[13],digits=3),nsmall=0)),
             (format(round(se12[13],digits=3),nsmall=0)),
             (format(round(se13[13],digits=3),nsmall=0)),
             (format(round(se14[13],digits=3),nsmall=0)),
             (format(round(se15[13],digits=3),nsmall=0)),
             (format(round(se16[13],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[13,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[13,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[14]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[14]),digits=3),nsmall=0))),
           c((format(round(se9[14],digits=3),nsmall=0)),
             #(format(round(se10[14],digits=3),nsmall=0)),
             (format(round(se11[14],digits=3),nsmall=0)),
             (format(round(se12[14],digits=3),nsmall=0)),
             (format(round(se13[14],digits=3),nsmall=0)),
             (format(round(se14[14],digits=3),nsmall=0)),
             (format(round(se15[14],digits=3),nsmall=0)),
             (format(round(se16[14],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[14,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[14,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[15]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[15]),digits=3),nsmall=0))),
           c((format(round(se9[15],digits=3),nsmall=0)),
             #(format(round(se10[15],digits=3),nsmall=0)),
             (format(round(se11[15],digits=3),nsmall=0)),
             (format(round(se12[15],digits=3),nsmall=0)),
             (format(round(se13[15],digits=3),nsmall=0)),
             (format(round(se14[15],digits=3),nsmall=0)),
             (format(round(se15[15],digits=3),nsmall=0)),
             (format(round(se16[15],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[15,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[15,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[16]),digits=3),nsmall=0)),
             # (format(round(sum(m10$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[16]),digits=3),nsmall=0))),
           c((format(round(se9[16],digits=3),nsmall=0)),
             #(format(round(se10[16],digits=3),nsmall=0)),
             (format(round(se11[16],digits=3),nsmall=0)),
             (format(round(se12[16],digits=3),nsmall=0)),
             (format(round(se13[16],digits=3),nsmall=0)),
             (format(round(se14[16],digits=3),nsmall=0)),
             (format(round(se15[16],digits=3),nsmall=0)),
             (format(round(se16[16],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[16,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[16,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[17]),digits=3),nsmall=0)),
             # (format(round(sum(m10$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[17]),digits=3),nsmall=0))),
           c((format(round(se9[17],digits=3),nsmall=0)),
             #(format(round(se10[17],digits=3),nsmall=0)),
             (format(round(se11[17],digits=3),nsmall=0)),
             (format(round(se12[17],digits=3),nsmall=0)),
             (format(round(se13[17],digits=3),nsmall=0)),
             (format(round(se14[17],digits=3),nsmall=0)),
             (format(round(se15[17],digits=3),nsmall=0)),
             (format(round(se16[17],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[17,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[17,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[18]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[18]),digits=3),nsmall=0))),
           c((format(round(se9[18],digits=3),nsmall=0)),
             #(format(round(se10[18],digits=3),nsmall=0)),
             (format(round(se11[18],digits=3),nsmall=0)),
             (format(round(se12[18],digits=3),nsmall=0)),
             (format(round(se13[18],digits=3),nsmall=0)),
             (format(round(se14[18],digits=3),nsmall=0)),
             (format(round(se15[18],digits=3),nsmall=0)),
             (format(round(se16[18],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[18,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[18,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[19]),digits=3),nsmall=0)),
             # (format(round(sum(m10$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[19]),digits=3),nsmall=0))),
           c((format(round(se9[19],digits=3),nsmall=0)),
             #(format(round(se10[19],digits=3),nsmall=0)),
             (format(round(se11[19],digits=3),nsmall=0)),
             (format(round(se12[19],digits=3),nsmall=0)),
             (format(round(se13[19],digits=3),nsmall=0)),
             (format(round(se14[19],digits=3),nsmall=0)),
             (format(round(se15[19],digits=3),nsmall=0)),
             (format(round(se16[19],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[19,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[19,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[20]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[20]),digits=3),nsmall=0))),
           c((format(round(se9[20],digits=3),nsmall=0)),
             # (format(round(se10[20],digits=3),nsmall=0)),
             (format(round(se11[20],digits=3),nsmall=0)),
             (format(round(se12[20],digits=3),nsmall=0)),
             (format(round(se13[20],digits=3),nsmall=0)),
             (format(round(se14[20],digits=3),nsmall=0)),
             (format(round(se15[20],digits=3),nsmall=0)),
             (format(round(se16[20],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[20,4],digits=3),nsmall=0)),
             #(format(round(summary(m10)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[20,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[21]),digits=3),nsmall=0)),
             #  (format(round(sum(m10$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[21]),digits=3),nsmall=0))),
           c((format(round(se9[21],digits=3),nsmall=0)),
             #  (format(round(se10[21],digits=3),nsmall=0)),
             (format(round(se11[21],digits=3),nsmall=0)),
             (format(round(se12[21],digits=3),nsmall=0)),
             (format(round(se13[21],digits=3),nsmall=0)),
             (format(round(se14[21],digits=3),nsmall=0)),
             (format(round(se15[21],digits=3),nsmall=0)),
             (format(round(se16[21],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[21,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[21,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[22]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[22]),digits=3),nsmall=0))),
           c((format(round(se9[22],digits=3),nsmall=0)),
             #  (format(round(se10[22],digits=3),nsmall=0)),
             (format(round(se11[22],digits=3),nsmall=0)),
             (format(round(se12[22],digits=3),nsmall=0)),
             (format(round(se13[22],digits=3),nsmall=0)),
             (format(round(se14[22],digits=3),nsmall=0)),
             (format(round(se15[22],digits=3),nsmall=0)),
             (format(round(se16[22],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[22,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[22,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[23]),digits=3),nsmall=0)),
             # (format(round(sum(m10$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[23]),digits=3),nsmall=0))),
           c((format(round(se9[23],digits=3),nsmall=0)),
             #  (format(round(se10[23],digits=3),nsmall=0)),
             (format(round(se11[23],digits=3),nsmall=0)),
             (format(round(se12[23],digits=3),nsmall=0)),
             (format(round(se13[23],digits=3),nsmall=0)),
             (format(round(se14[23],digits=3),nsmall=0)),
             (format(round(se15[23],digits=3),nsmall=0)),
             (format(round(se16[23],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[23,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[23,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[24]),digits=3),nsmall=0)),
             #  (format(round(sum(m10$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[24]),digits=3),nsmall=0))),
           c((format(round(se9[24],digits=3),nsmall=0)),
             # (format(round(se10[24],digits=3),nsmall=0)),
             (format(round(se11[24],digits=3),nsmall=0)),
             (format(round(se12[24],digits=3),nsmall=0)),
             (format(round(se13[24],digits=3),nsmall=0)),
             (format(round(se14[24],digits=3),nsmall=0)),
             (format(round(se15[24],digits=3),nsmall=0)),
             (format(round(se16[24],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[24,4],digits=3),nsmall=0)),
             #  (format(round(summary(m10)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[24,4],digits=3),nsmall=0))),
           
           c((format(round(sum(m9$coefficients[25]),digits=3),nsmall=0)),
             #(format(round(sum(m10$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(m11$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(m12$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(m13$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(m14$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(m15$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(m16$coefficients[25]),digits=3),nsmall=0))),
           c((format(round(se9[25],digits=3),nsmall=0)),
             # (format(round(se10[25],digits=3),nsmall=0)),
             (format(round(se11[25],digits=3),nsmall=0)),
             (format(round(se12[25],digits=3),nsmall=0)),
             (format(round(se13[25],digits=3),nsmall=0)),
             (format(round(se14[25],digits=3),nsmall=0)),
             (format(round(se15[25],digits=3),nsmall=0)),
             (format(round(se16[25],digits=3),nsmall=0))),
           c((format(round(summary(m9)$coefficients[25,4],digits=3),nsmall=0)),
             # (format(round(summary(m10)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(m11)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(m12)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(m13)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(m14)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(m15)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(m16)$coefficients[25,4],digits=3),nsmall=0))),
           
           
           c((format(round(summary(m9)$r.squared,digits=3),nsmall=0)),
             #   (format(round(summary(m10)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m11)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m12)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m13)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m14)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m15)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(m16)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(m9)$adj.r.squared,digits=3),nsmall=0)),
             # (format(round(summary(m10)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m11)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m12)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m13)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m14)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m15)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(m16)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(m9),digits=3),nsmall=0)),
             # (format(round(nobs(m10),digits=3),nsmall=0)),
             (format(round(nobs(m11),digits=3),nsmall=0)),
             (format(round(nobs(m12),digits=3),nsmall=0)),
             (format(round(nobs(m13),digits=3),nsmall=0)),
             (format(round(nobs(m14),digits=3),nsmall=0)),
             (format(round(nobs(m15),digits=3),nsmall=0)),
             (format(round(nobs(m16),digits=3),nsmall=0)))
)




#### NON-SEED RELATED RATINGS ####

#### regressions without controls - non-seed related ratings

n1 <- lm(overall_rating~gender_avg , data = bfm)
sen1<- sqrt(diag(vcov(n1)))
n2 <- lm(general_rating_nonseed~gender_avg , data = bfm)
sen2<- sqrt(diag(vcov(n2)))
n3 <- lm(location~gender_avg , data = bfm)
sen3<- sqrt(diag(vcov(n3)))
n4 <- lm(price~gender_avg , data = bfm)
sen4<- sqrt(diag(vcov(n4)))
n5 <- lm(quality~gender_avg , data = bfm)
sen5<- sqrt(diag(vcov(n5)))
n6 <- lm(stock~gender_avg , data = bfm)
sen6<- sqrt(diag(vcov(n6)))
n7 <- lm(reputation~gender_avg , data = bfm)
sen7<- sqrt(diag(vcov(n7)))



s3<- rbind(c((format(round(sum(n1$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n2$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n3$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n4$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n5$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n6$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n7$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(sen1[1],digits=3),nsmall=0)),
             (format(round(sen2[1],digits=3),nsmall=0)),
             (format(round(sen3[1],digits=3),nsmall=0)),
             (format(round(sen4[1],digits=3),nsmall=0)),
             (format(round(sen5[1],digits=3),nsmall=0)),
             (format(round(sen6[1],digits=3),nsmall=0)),
             (format(round(sen7[1],digits=3),nsmall=0))),
           c((format(round(summary(n1)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n2)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n3)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n4)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n5)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n6)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n7)$coefficients[1,4],digits=3),nsmall=0))),
           c((format(round(sum(n1$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n2$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n3$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n4$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n5$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n6$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n7$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(sen1[2],digits=3),nsmall=0)),
             (format(round(sen2[2],digits=3),nsmall=0)),
             (format(round(sen3[2],digits=3),nsmall=0)),
             (format(round(sen4[2],digits=3),nsmall=0)),
             (format(round(sen5[2],digits=3),nsmall=0)),
             (format(round(sen6[2],digits=3),nsmall=0)),
             (format(round(sen7[2],digits=3),nsmall=0))),
           c((format(round(summary(n1)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n2)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n3)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n4)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n5)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n6)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n7)$coefficients[2,4],digits=3),nsmall=0))),

           c((format(round(summary(n1)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n2)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n3)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n4)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n5)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n6)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n7)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(n1)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n2)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n3)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n4)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n5)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n6)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n7)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(n1),digits=3),nsmall=0)),
             (format(round(nobs(n2),digits=3),nsmall=0)),
             (format(round(nobs(n3),digits=3),nsmall=0)),
             (format(round(nobs(n4),digits=3),nsmall=0)),
             (format(round(nobs(n5),digits=3),nsmall=0)),
             (format(round(nobs(n6),digits=3),nsmall=0)),
             (format(round(nobs(n7),digits=3),nsmall=0)))
)



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


#### regressions with dealer's gender (averaged) and farmer characteristics  --- non-seed related ratings

#n9<-lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#sen9 <- sqrt(diag(vcov(n9)))

#n10<-lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#sen10 <- sqrt(diag(vcov(n10)))

#n11<-lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#sen11 <- sqrt(diag(vcov(n11)))

#n12<-lm(price~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#sen12 <- sqrt(diag(vcov(n12)))

#n13<-lm(quality ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#sen13<- sqrt(diag(vcov(n13)))

#n14<-lm(stock ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#sen14 <- sqrt(diag(vcov(n14)))

#n15<-lm(reputation ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
#sen15<- sqrt(diag(vcov(n15)))


#### regressions with dealer's gender (averaged) and farmer + dealer characteristics  --- non-seed related ratings 

n9<-lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
         murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
         badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
sen9 <- sqrt(diag(vcov(n9)))

n10<-lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
sen10 <- sqrt(diag(vcov(n10)))

n11<-lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
sen11 <- sqrt(diag(vcov(n11)))

n12<-lm(price~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
sen12 <- sqrt(diag(vcov(n12)))

n13<-lm(quality ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
sen13<- sqrt(diag(vcov(n13)))

n14<-lm(stock ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
sen14 <- sqrt(diag(vcov(n14)))

n15<-lm(reputation ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
          murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
          badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
sen15<- sqrt(diag(vcov(n15)))



s4<- rbind(c((format(round(sum(n9$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(sen9[1],digits=3),nsmall=0)),
             (format(round(sen10[1],digits=3),nsmall=0)),
             (format(round(sen11[1],digits=3),nsmall=0)),
             (format(round(sen12[1],digits=3),nsmall=0)),
             (format(round(sen13[1],digits=3),nsmall=0)),
             (format(round(sen14[1],digits=3),nsmall=0)),
             (format(round(sen15[1],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[1,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(sen9[2],digits=3),nsmall=0)),
             (format(round(sen10[2],digits=3),nsmall=0)),
             (format(round(sen11[2],digits=3),nsmall=0)),
             (format(round(sen12[2],digits=3),nsmall=0)),
             (format(round(sen13[2],digits=3),nsmall=0)),
             (format(round(sen14[2],digits=3),nsmall=0)),
             (format(round(sen15[2],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[2,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[3]),digits=3),nsmall=0))),
           c((format(round(sen9[3],digits=3),nsmall=0)),
             (format(round(sen10[3],digits=3),nsmall=0)),
             (format(round(sen11[3],digits=3),nsmall=0)),
             (format(round(sen12[3],digits=3),nsmall=0)),
             (format(round(sen13[3],digits=3),nsmall=0)),
             (format(round(sen14[3],digits=3),nsmall=0)),
             (format(round(sen15[3],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[3,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[4]),digits=3),nsmall=0))),
           c((format(round(sen9[4],digits=3),nsmall=0)),
             (format(round(sen10[4],digits=3),nsmall=0)),
             (format(round(sen11[4],digits=3),nsmall=0)),
             (format(round(sen12[4],digits=3),nsmall=0)),
             (format(round(sen13[4],digits=3),nsmall=0)),
             (format(round(sen14[4],digits=3),nsmall=0)),
             (format(round(sen15[4],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[4,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[5]),digits=3),nsmall=0))),
           c((format(round(sen9[5],digits=3),nsmall=0)),
             (format(round(sen10[5],digits=3),nsmall=0)),
             (format(round(sen11[5],digits=3),nsmall=0)),
             (format(round(sen12[5],digits=3),nsmall=0)),
             (format(round(sen13[5],digits=3),nsmall=0)),
             (format(round(sen14[5],digits=3),nsmall=0)),
             (format(round(sen15[5],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[5,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[6]),digits=3),nsmall=0))),
           c((format(round(sen9[6],digits=3),nsmall=0)),
             (format(round(sen10[6],digits=3),nsmall=0)),
             (format(round(sen11[6],digits=3),nsmall=0)),
             (format(round(sen12[6],digits=3),nsmall=0)),
             (format(round(sen13[6],digits=3),nsmall=0)),
             (format(round(sen14[6],digits=3),nsmall=0)),
             (format(round(sen15[6],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[6,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[7]),digits=3),nsmall=0))),
           c((format(round(sen9[7],digits=3),nsmall=0)),
             (format(round(sen10[7],digits=3),nsmall=0)),
             (format(round(sen11[7],digits=3),nsmall=0)),
             (format(round(sen12[7],digits=3),nsmall=0)),
             (format(round(sen13[7],digits=3),nsmall=0)),
             (format(round(sen14[7],digits=3),nsmall=0)),
             (format(round(sen15[7],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[7,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[8]),digits=3),nsmall=0))),
           c((format(round(sen9[8],digits=3),nsmall=0)),
             (format(round(sen10[8],digits=3),nsmall=0)),
             (format(round(sen11[8],digits=3),nsmall=0)),
             (format(round(sen12[8],digits=3),nsmall=0)),
             (format(round(sen13[8],digits=3),nsmall=0)),
             (format(round(sen14[8],digits=3),nsmall=0)),
             (format(round(sen15[8],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[8,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[9]),digits=3),nsmall=0))),
           c((format(round(sen9[9],digits=3),nsmall=0)),
             (format(round(sen10[9],digits=3),nsmall=0)),
             (format(round(sen11[9],digits=3),nsmall=0)),
             (format(round(sen12[9],digits=3),nsmall=0)),
             (format(round(sen13[9],digits=3),nsmall=0)),
             (format(round(sen14[9],digits=3),nsmall=0)),
             (format(round(sen15[9],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[9,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(n11$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(n12$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(n14$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(n15$coefficients[10]),digits=3),nsmall=0))),
           c((format(round(sen9[10],digits=3),nsmall=0)),
             (format(round(sen10[10],digits=3),nsmall=0)),
             (format(round(sen11[10],digits=3),nsmall=0)),
             (format(round(sen12[10],digits=3),nsmall=0)),
             (format(round(sen13[10],digits=3),nsmall=0)),
             (format(round(sen14[10],digits=3),nsmall=0)),
             (format(round(sen15[10],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(n11)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(n12)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(n14)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(n15)$coefficients[10,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[11]),digits=3),nsmall=0)),
           0,
             (format(round(sum(n12$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[11]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n15$coefficients[11]),digits=3),nsmall=0))),
           c((format(round(sen9[11],digits=3),nsmall=0)),
             (format(round(sen10[11],digits=3),nsmall=0)),
            0,
             (format(round(sen12[11],digits=3),nsmall=0)),
             (format(round(sen13[11],digits=3),nsmall=0)),
            0,
             (format(round(sen15[11],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[11,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[11,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(n15)$coefficients[11,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[12]),digits=3),nsmall=0)),
          0,
             (format(round(sum(n12$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[12]),digits=3),nsmall=0)),
         0,
             (format(round(sum(n15$coefficients[12]),digits=3),nsmall=0))),
           c((format(round(sen9[12],digits=3),nsmall=0)),
             (format(round(sen10[12],digits=3),nsmall=0)),
           0,
             (format(round(sen12[12],digits=3),nsmall=0)),
             (format(round(sen13[12],digits=3),nsmall=0)),
           0,
             (format(round(sen15[12],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[12,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(n12)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[12,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(n15)$coefficients[12,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[13]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[13]),digits=3),nsmall=0)),
             0,
             (format(round(sum(n15$coefficients[13]),digits=3),nsmall=0))),
           c((format(round(sen9[13],digits=3),nsmall=0)),
             (format(round(sen10[13],digits=3),nsmall=0)),
           0,
             (format(round(sen12[13],digits=3),nsmall=0)),
             (format(round(sen13[13],digits=3),nsmall=0)),
            0,
             (format(round(sen15[13],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[13,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(n12)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[13,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n15)$coefficients[13,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[14]),digits=3),nsmall=0)),
           0,
             (format(round(sum(n12$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[14]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n15$coefficients[14]),digits=3),nsmall=0))),
           c((format(round(sen9[14],digits=3),nsmall=0)),
             (format(round(sen10[14],digits=3),nsmall=0)),
            0,
             (format(round(sen12[14],digits=3),nsmall=0)),
             (format(round(sen13[14],digits=3),nsmall=0)),
            0,
             (format(round(sen15[14],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[14,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[14,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n15)$coefficients[14,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[15]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[15]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n15$coefficients[15]),digits=3),nsmall=0))),
           c((format(round(sen9[15],digits=3),nsmall=0)),
             (format(round(sen10[15],digits=3),nsmall=0)),
            0,
             (format(round(sen12[15],digits=3),nsmall=0)),
             (format(round(sen13[15],digits=3),nsmall=0)),
            0,
             (format(round(sen15[15],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[15,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[15,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n15)$coefficients[15,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[16]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[16]),digits=3),nsmall=0)),
           0,
             (format(round(sum(n15$coefficients[16]),digits=3),nsmall=0))),
           c((format(round(sen9[16],digits=3),nsmall=0)),
             (format(round(sen10[16],digits=3),nsmall=0)),
            0,
             (format(round(sen12[16],digits=3),nsmall=0)),
             (format(round(sen13[16],digits=3),nsmall=0)),
            0,
             (format(round(sen15[16],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[16,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(n12)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[16,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n15)$coefficients[16,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[17]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[17]),digits=3),nsmall=0)),
           0,
             (format(round(sum(n15$coefficients[17]),digits=3),nsmall=0))),
           c((format(round(sen9[17],digits=3),nsmall=0)),
             (format(round(sen10[17],digits=3),nsmall=0)),
            0,
             (format(round(sen12[17],digits=3),nsmall=0)),
             (format(round(sen13[17],digits=3),nsmall=0)),
            0,
             (format(round(sen15[17],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[17,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[17,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n15)$coefficients[17,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[18]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[18]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n15$coefficients[18]),digits=3),nsmall=0))),
           c((format(round(sen9[18],digits=3),nsmall=0)),
             (format(round(sen10[18],digits=3),nsmall=0)),
           0,
             (format(round(sen12[18],digits=3),nsmall=0)),
             (format(round(sen13[18],digits=3),nsmall=0)),
            0,
             (format(round(sen15[18],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[18,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[18,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n15)$coefficients[18,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[19]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[19]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n15$coefficients[19]),digits=3),nsmall=0))),
           c((format(round(sen9[19],digits=3),nsmall=0)),
             (format(round(sen10[19],digits=3),nsmall=0)),
           0,
             (format(round(sen12[19],digits=3),nsmall=0)),
             (format(round(sen13[19],digits=3),nsmall=0)),
            0,
             (format(round(sen15[19],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[19,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[19,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(n15)$coefficients[19,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[20]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[20]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n15$coefficients[20]),digits=3),nsmall=0))),
           c((format(round(sen9[20],digits=3),nsmall=0)),
             (format(round(sen10[20],digits=3),nsmall=0)),
           0,
             (format(round(sen12[20],digits=3),nsmall=0)),
             (format(round(sen13[20],digits=3),nsmall=0)),
             0,
             (format(round(sen15[20],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[20,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n12)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[20,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n15)$coefficients[20,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[21]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[21]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n15$coefficients[21]),digits=3),nsmall=0))),
           c((format(round(sen9[21],digits=3),nsmall=0)),
             (format(round(sen10[21],digits=3),nsmall=0)),
            0,
             (format(round(sen12[21],digits=3),nsmall=0)),
             (format(round(sen13[21],digits=3),nsmall=0)),
            0,
             (format(round(sen15[21],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[21,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n12)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[21,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n15)$coefficients[21,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[22]),digits=3),nsmall=0)),
             0,
             (format(round(sum(n12$coefficients[22]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[22]),digits=3),nsmall=0)),
             0,
             (format(round(sum(n15$coefficients[22]),digits=3),nsmall=0))),
           c((format(round(sen9[22],digits=3),nsmall=0)),
             (format(round(sen10[22],digits=3),nsmall=0)),
            0,
             (format(round(sen12[22],digits=3),nsmall=0)),
             (format(round(sen13[22],digits=3),nsmall=0)),
             0,
             (format(round(sen15[22],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[22,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[22,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[22,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n15)$coefficients[22,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[23]),digits=3),nsmall=0)),
             0,
             (format(round(sum(n12$coefficients[23]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[23]),digits=3),nsmall=0)),
           0,
             (format(round(sum(n15$coefficients[23]),digits=3),nsmall=0))),
           c((format(round(sen9[23],digits=3),nsmall=0)),
             (format(round(sen10[23],digits=3),nsmall=0)),
            0,
             (format(round(sen12[23],digits=3),nsmall=0)),
             (format(round(sen13[23],digits=3),nsmall=0)),
             0,
             (format(round(sen15[23],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[23,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[23,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[23,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n15)$coefficients[23,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[24]),digits=3),nsmall=0)),
           0,
             (format(round(sum(n12$coefficients[24]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[24]),digits=3),nsmall=0)),
           0,
             (format(round(sum(n15$coefficients[24]),digits=3),nsmall=0))),
           c((format(round(sen9[24],digits=3),nsmall=0)),
             (format(round(sen10[24],digits=3),nsmall=0)),
            0,
             (format(round(sen12[24],digits=3),nsmall=0)),
             (format(round(sen13[24],digits=3),nsmall=0)),
            0,
             (format(round(sen15[24],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[24,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n12)$coefficients[24,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[24,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(n15)$coefficients[24,4],digits=3),nsmall=0))),
           
           c((format(round(sum(n9$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(n10$coefficients[25]),digits=3),nsmall=0)),
            0,
             (format(round(sum(n12$coefficients[25]),digits=3),nsmall=0)),
             (format(round(sum(n13$coefficients[25]),digits=3),nsmall=0)),
          0,
             (format(round(sum(n15$coefficients[25]),digits=3),nsmall=0))),
           c((format(round(sen9[25],digits=3),nsmall=0)),
             (format(round(sen10[25],digits=3),nsmall=0)),
            0,
             (format(round(sen12[25],digits=3),nsmall=0)),
             (format(round(sen13[25],digits=3),nsmall=0)),
            0,
             (format(round(sen15[25],digits=3),nsmall=0))),
           c((format(round(summary(n9)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(n10)$coefficients[25,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n12)$coefficients[25,4],digits=3),nsmall=0)),
             (format(round(summary(n13)$coefficients[25,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(n15)$coefficients[25,4],digits=3),nsmall=0))),
           
           
           c((format(round(summary(n9)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n10)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n11)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n12)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n13)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n14)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(n15)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(n9)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n10)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n11)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n12)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n13)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n14)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(n15)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(n9),digits=3),nsmall=0)),
             (format(round(nobs(n10),digits=3),nsmall=0)),
             (format(round(nobs(n11),digits=3),nsmall=0)),
             (format(round(nobs(n12),digits=3),nsmall=0)),
             (format(round(nobs(n13),digits=3),nsmall=0)),
             (format(round(nobs(n14),digits=3),nsmall=0)),
             (format(round(nobs(n15),digits=3),nsmall=0))))





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

avg$score <-  rowMeans(avg[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
avg$overall_rating <-  rowMeans(avg[c("general_rating_nonseed", "location","price","quality","stock","reputation")],na.rm=T)

avg <- merge(avg, baseline_dealer, by="shop_ID") #merging with baseline dealer data to get the controls and gender

#create gender dummy
avg$genderdummy <- ifelse(avg$maize.owner.agree.gender == "Male", 1, 0)

mean(avg$score[avg$genderdummy==1], na.rm=TRUE) #3.40848 -- seed avg quality, male
mean(avg$score[avg$genderdummy==0], na.rm=TRUE) #3.326955 -- seed avg quality, female

mean(avg$overall_rating[avg$genderdummy==1], na.rm=TRUE) #3.802201 -- non-seed avg rating , male
mean(avg$overall_rating[avg$genderdummy==0], na.rm=TRUE) #3.674769 -- non-seed avg rating , female



###dealer characteristics for  controls

#education of dealers
table(avg$maize.owner.agree.educ) #5 are g which is Other
avg$prim <- 0
#avg$prim[avg$maize.owner.agree.educ=="c"|avg$maize.owner.agree.educ=="d"|avg$maize.owner.agree.educ=="e"|avg$maize.owner.agree.educ=="f"]<- 1

#finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
avg$prim[avg$maize.owner.agree.educ=="e"|avg$maize.owner.agree.educ=="f"] <- 1
avg$prim[avg$maize.owner.agree.educ=="g"]<- NA
table(avg$prim)
#73 ARE 1 --- 38.83 percent


#age of dealer
summary(avg$maize.owner.agree.age)
avg$maize.owner.agree.age[avg$maize.owner.agree.age==999] <- NA
table(avg$maize.owner.agree.age)

#distance of shop to nearest tarmac road
table(avg$maize.owner.agree.q3)
avg$maize.owner.agree.q3[avg$maize.owner.agree.q3==999] <- NA
summary(avg$maize.owner.agree.q3)

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
avg$pest_prob<- ifelse(avg$pest== 'No', 1, 0)
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

#Q78. Lighting conditions in area where seed is stored?
avg$badlighting <- 0
avg$badlighting[avg$maize.owner.agree.temp.q78=="2"]<-1
table(avg$badlighting)

#Q79. On what surface are seed stored?
avg$badstored <- 0
avg$badstored[avg$maize.owner.agree.temp.q79=="3"|avg$maize.owner.agree.temp.q79=="4"|avg$maize.owner.agree.temp.q79=="5"]<-1
avg$badstored[avg$maize.owner.agree.temp.q79==96]<-NA
table(avg$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
avg$maize.owner.agree.q80
avg$open<-as.character(avg$maize.owner.agree.temp.q80)
avg$open_storage<- ifelse(avg$open== 'No', 1, 0)
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
table(avg$maize.owner.agree.q96)
avg$complaint<- ifelse(avg$maize.owner.agree.q96== 'No', 1, 0)
table(avg$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
table(avg$maize.owner.agree.q70)
avg$maize.owner.agree.q70[avg$maize.owner.agree.q70==999] <- NA




## SEED RELATED RATINGS ##

#### regressions without controls - seed related ratings

mm1<-lm(score~genderdummy, data = avg)
sem1<- sqrt(diag(vcov(mm1)))
#mm2<-lm(quality~genderdummy, data = avg)
#sem2<- sqrt(diag(vcov(mm2)))
mm3<-lm(general~genderdummy, data = avg)
sem3<- sqrt(diag(vcov(mm3)))
mm4<-lm(yield~genderdummy, data = avg)
sem4<- sqrt(diag(vcov(mm4)))
mm5<-lm(drought_resistent~genderdummy, data = avg)
sem5<- sqrt(diag(vcov(mm5)))
mm6<-lm(disease_resistent~genderdummy, data = avg)
sem6<- sqrt(diag(vcov(mm6)))
mm7<-lm(early_maturing~genderdummy, data = avg)
sem7<- sqrt(diag(vcov(mm7)))
mm8<-lm(germination~genderdummy, data = avg)
sem8<- sqrt(diag(vcov(mm8)))

s5<- rbind(c((format(round(sum(mm1$coefficients[1]),digits=3),nsmall=0)),
             #(format(round(sum(mm2$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(mm3$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(mm4$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(mm5$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(mm6$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(mm7$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(mm8$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(sem1[1],digits=3),nsmall=0)),
             #(format(round(sem2[1],digits=3),nsmall=0)),
             (format(round(sem3[1],digits=3),nsmall=0)),
             (format(round(sem4[1],digits=3),nsmall=0)),
             (format(round(sem5[1],digits=3),nsmall=0)),
             (format(round(sem6[1],digits=3),nsmall=0)),
             (format(round(sem7[1],digits=3),nsmall=0)),
             (format(round(sem8[1],digits=3),nsmall=0))),
           c((format(round(summary(mm1)$coefficients[1,4],digits=3),nsmall=0)),
           #  (format(round(summary(mm2)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(mm3)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(mm4)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(mm5)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(mm6)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(mm7)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(mm8)$coefficients[1,4],digits=3),nsmall=0))),

           c((format(round(sum(mm1$coefficients[2]),digits=3),nsmall=0)),
            # (format(round(sum(mm2$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(mm3$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(mm4$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(mm5$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(mm6$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(mm7$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(mm8$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(sem1[2],digits=3),nsmall=0)),
            # (format(round(sem2[2],digits=3),nsmall=0)),
             (format(round(sem3[2],digits=3),nsmall=0)),
             (format(round(sem4[2],digits=3),nsmall=0)),
             (format(round(sem5[2],digits=3),nsmall=0)),
             (format(round(sem6[2],digits=3),nsmall=0)),
             (format(round(sem7[2],digits=3),nsmall=0)),
             (format(round(sem8[2],digits=3),nsmall=0))),
           c((format(round(summary(mm1)$coefficients[2,4],digits=3),nsmall=0)),
            # (format(round(summary(mm2)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(mm3)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(mm4)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(mm5)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(mm6)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(mm7)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(mm8)$coefficients[2,4],digits=3),nsmall=0))),

           c((format(round(summary(mm1)$r.squared,digits=3),nsmall=0)),
            # (format(round(summary(mm2)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm3)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm4)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm5)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm6)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm7)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm8)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(mm1)$adj.r.squared,digits=3),nsmall=0)),
            # (format(round(summary(mm2)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm3)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm4)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm5)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm6)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm7)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(mm8)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(mm1),digits=3),nsmall=0)),
            # (format(round(nobs(mm2),digits=3),nsmall=0)),
             (format(round(nobs(mm3),digits=3),nsmall=0)),
             (format(round(nobs(mm4),digits=3),nsmall=0)),
             (format(round(nobs(mm5),digits=3),nsmall=0)),
             (format(round(nobs(mm6),digits=3),nsmall=0)),
             (format(round(nobs(mm7),digits=3),nsmall=0)),
             (format(round(nobs(mm8),digits=3),nsmall=0)))
)


summary(lm(score~genderdummy , data = avg))
#summary(lm(quality~genderdummy , data = avg))
summary(lm(general~genderdummy , data = avg))
summary(lm(yield~genderdummy , data = avg))
summary(lm(drought_resistent~genderdummy , data = avg))
summary(lm(disease_resistent~genderdummy , data = avg))
summary(lm(early_maturing~genderdummy , data = avg))
summary(lm(germination~genderdummy , data = avg))

#### regressions with dealer's gender + dealer characteristics  --- seed related ratings

mm9<- lm(score~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
sem9<- sqrt(diag(vcov(mm9)))

#### TO CHECK THE COEFF WITH QUALITY CONTROLS AND WITHOUT ANY CONTROL
#myvars <- names(avg) %in% c("score", "genderdummy", "inputsale",
           #                   "years_shop", "dedicated_area", "pest_prob", "ventilation", "insulated", "wall_heatproof",
              #                "badlighting", "badstored",  "open_storage", "cert_yes", "shop_rate", "complaint", "maize.owner.agree.q70", "leakproof")
#newdata <- avg[myvars]
#new <- na.omit(newdata)
#summary(lm(score~genderdummy , data = new)) # 0.08674
#summary(lm(score~genderdummy + inputsale+
         #    +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
         #    badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = new)) #  0.1189104**

#test1<- lm(score~genderdummy , data = new)
#test2<- lm(score~genderdummy + inputsale+
         #    +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
         #    badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = new)
#waldtest(test2, test1)
#anova(test2, test1)


#mm10<- lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
           #  +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #  badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
#sem10<- sqrt(diag(vcov(mm10)))

mm10<-lm(general~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
sem10<- sqrt(diag(vcov(mm10)))

mm11<-lm(yield~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg )
sem11<- sqrt(diag(vcov(mm11)))

mm12<-lm(drought_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
sem12<- sqrt(diag(vcov(mm12)))

mm13<-lm(disease_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
sem13<- sqrt(diag(vcov(mm13)))

mm14<-lm(early_maturing~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
sem14<- sqrt(diag(vcov(mm14)))

mm15<-lm(germination~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
sem15<- sqrt(diag(vcov(mm15)))


            s6<- rbind(c((format(round(sum(mm9$coefficients[1]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[1]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[1]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[1]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[1]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[1]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[1]),digits=3),nsmall=0))),
                       c((format(round(sem9[1],digits=3),nsmall=0)),
                         (format(round(sem10[1],digits=3),nsmall=0)),
                         (format(round(sem11[1],digits=3),nsmall=0)),
                         (format(round(sem12[1],digits=3),nsmall=0)),
                         (format(round(sem13[1],digits=3),nsmall=0)),
                         (format(round(sem14[1],digits=3),nsmall=0)),
                         (format(round(sem15[1],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[1,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[1,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[1,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[1,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[1,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[1,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[1,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[2]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[2]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[2]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[2]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[2]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[2]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[2]),digits=3),nsmall=0))),
                       c((format(round(sem9[2],digits=3),nsmall=0)),
                         (format(round(sem10[2],digits=3),nsmall=0)),
                         (format(round(sem11[2],digits=3),nsmall=0)),
                         (format(round(sem12[2],digits=3),nsmall=0)),
                         (format(round(sem13[2],digits=3),nsmall=0)),
                         (format(round(sem14[2],digits=3),nsmall=0)),
                         (format(round(sem15[2],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[2,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[2,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[2,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[2,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[2,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[2,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[2,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[3]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[3]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[3]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[3]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[3]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[3]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[3]),digits=3),nsmall=0))),
                       c((format(round(sem9[3],digits=3),nsmall=0)),
                         (format(round(sem10[3],digits=3),nsmall=0)),
                         (format(round(sem11[3],digits=3),nsmall=0)),
                         (format(round(sem12[3],digits=3),nsmall=0)),
                         (format(round(sem13[3],digits=3),nsmall=0)),
                         (format(round(sem14[3],digits=3),nsmall=0)),
                         (format(round(sem15[3],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[3,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[3,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[3,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[3,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[3,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[3,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[3,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[4]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[4]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[4]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[4]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[4]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[4]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[4]),digits=3),nsmall=0))),
                       c((format(round(sem9[4],digits=3),nsmall=0)),
                         (format(round(sem10[4],digits=3),nsmall=0)),
                         (format(round(sem11[4],digits=3),nsmall=0)),
                         (format(round(sem12[4],digits=3),nsmall=0)),
                         (format(round(sem13[4],digits=3),nsmall=0)),
                         (format(round(sem14[4],digits=3),nsmall=0)),
                         (format(round(sem15[4],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[4,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[4,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[4,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[4,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[4,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[4,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[4,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[5]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[5]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[5]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[5]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[5]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[5]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[5]),digits=3),nsmall=0))),
                       c((format(round(sem9[5],digits=3),nsmall=0)),
                         (format(round(sem10[5],digits=3),nsmall=0)),
                         (format(round(sem11[5],digits=3),nsmall=0)),
                         (format(round(sem12[5],digits=3),nsmall=0)),
                         (format(round(sem13[5],digits=3),nsmall=0)),
                         (format(round(sem14[5],digits=3),nsmall=0)),
                         (format(round(sem15[5],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[5,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[5,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[5,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[5,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[5,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[5,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[5,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[6]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[6]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[6]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[6]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[6]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[6]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[6]),digits=3),nsmall=0))),
                       c((format(round(sem9[6],digits=3),nsmall=0)),
                         (format(round(sem10[6],digits=3),nsmall=0)),
                         (format(round(sem11[6],digits=3),nsmall=0)),
                         (format(round(sem12[6],digits=3),nsmall=0)),
                         (format(round(sem13[6],digits=3),nsmall=0)),
                         (format(round(sem14[6],digits=3),nsmall=0)),
                         (format(round(sem15[6],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[6,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[6,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[6,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[6,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[6,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[6,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[6,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[7]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[7]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[7]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[7]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[7]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[7]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[7]),digits=3),nsmall=0))),
                       c((format(round(sem9[7],digits=3),nsmall=0)),
                         (format(round(sem10[7],digits=3),nsmall=0)),
                         (format(round(sem11[7],digits=3),nsmall=0)),
                         (format(round(sem12[7],digits=3),nsmall=0)),
                         (format(round(sem13[7],digits=3),nsmall=0)),
                         (format(round(sem14[7],digits=3),nsmall=0)),
                         (format(round(sem15[7],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[7,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[7,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[7,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[7,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[7,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[7,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[7,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[8]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[8]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[8]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[8]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[8]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[8]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[8]),digits=3),nsmall=0))),
                       c((format(round(sem9[8],digits=3),nsmall=0)),
                         (format(round(sem10[8],digits=3),nsmall=0)),
                         (format(round(sem11[8],digits=3),nsmall=0)),
                         (format(round(sem12[8],digits=3),nsmall=0)),
                         (format(round(sem13[8],digits=3),nsmall=0)),
                         (format(round(sem14[8],digits=3),nsmall=0)),
                         (format(round(sem15[8],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[8,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[8,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[8,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[8,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[8,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[8,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[8,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[9]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[9]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[9]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[9]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[9]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[9]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[9]),digits=3),nsmall=0))),
                       c((format(round(sem9[9],digits=3),nsmall=0)),
                         (format(round(sem10[9],digits=3),nsmall=0)),
                         (format(round(sem11[9],digits=3),nsmall=0)),
                         (format(round(sem12[9],digits=3),nsmall=0)),
                         (format(round(sem13[9],digits=3),nsmall=0)),
                         (format(round(sem14[9],digits=3),nsmall=0)),
                         (format(round(sem15[9],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[9,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[9,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[9,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[9,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[9,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[9,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[9,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[10]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[10]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[10]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[10]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[10]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[10]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[10]),digits=3),nsmall=0))),
                       c((format(round(sem9[10],digits=3),nsmall=0)),
                         (format(round(sem10[10],digits=3),nsmall=0)),
                         (format(round(sem11[10],digits=3),nsmall=0)),
                         (format(round(sem12[10],digits=3),nsmall=0)),
                         (format(round(sem13[10],digits=3),nsmall=0)),
                         (format(round(sem14[10],digits=3),nsmall=0)),
                         (format(round(sem15[10],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[10,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[10,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[10,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[10,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[10,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[10,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[10,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[11]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[11]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[11]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[11]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[11]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[11]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[11]),digits=3),nsmall=0))),
                       c((format(round(sem9[11],digits=3),nsmall=0)),
                         (format(round(sem10[11],digits=3),nsmall=0)),
                         (format(round(sem11[11],digits=3),nsmall=0)),
                         (format(round(sem12[11],digits=3),nsmall=0)),
                         (format(round(sem13[11],digits=3),nsmall=0)),
                         (format(round(sem14[11],digits=3),nsmall=0)),
                         (format(round(sem15[11],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[11,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[11,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[11,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[11,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[11,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[11,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[11,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[12]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[12]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[12]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[12]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[12]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[12]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[12]),digits=3),nsmall=0))),
                       c((format(round(sem9[12],digits=3),nsmall=0)),
                         (format(round(sem10[12],digits=3),nsmall=0)),
                         (format(round(sem11[12],digits=3),nsmall=0)),
                         (format(round(sem12[12],digits=3),nsmall=0)),
                         (format(round(sem13[12],digits=3),nsmall=0)),
                         (format(round(sem14[12],digits=3),nsmall=0)),
                         (format(round(sem15[12],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[12,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[12,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[12,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[12,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[12,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[12,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[12,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[13]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[13]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[13]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[13]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[13]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[13]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[13]),digits=3),nsmall=0))),
                       c((format(round(sem9[13],digits=3),nsmall=0)),
                         (format(round(sem10[13],digits=3),nsmall=0)),
                         (format(round(sem11[13],digits=3),nsmall=0)),
                         (format(round(sem12[13],digits=3),nsmall=0)),
                         (format(round(sem13[13],digits=3),nsmall=0)),
                         (format(round(sem14[13],digits=3),nsmall=0)),
                         (format(round(sem15[13],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[13,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[13,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[13,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[13,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[13,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[13,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[13,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[14]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[14]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[14]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[14]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[14]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[14]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[14]),digits=3),nsmall=0))),
                       c((format(round(sem9[14],digits=3),nsmall=0)),
                         (format(round(sem10[14],digits=3),nsmall=0)),
                         (format(round(sem11[14],digits=3),nsmall=0)),
                         (format(round(sem12[14],digits=3),nsmall=0)),
                         (format(round(sem13[14],digits=3),nsmall=0)),
                         (format(round(sem14[14],digits=3),nsmall=0)),
                         (format(round(sem15[14],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[14,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[14,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[14,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[14,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[14,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[14,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[14,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[15]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[15]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[15]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[15]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[15]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[15]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[15]),digits=3),nsmall=0))),
                       c((format(round(sem9[15],digits=3),nsmall=0)),
                         (format(round(sem10[15],digits=3),nsmall=0)),
                         (format(round(sem11[15],digits=3),nsmall=0)),
                         (format(round(sem12[15],digits=3),nsmall=0)),
                         (format(round(sem13[15],digits=3),nsmall=0)),
                         (format(round(sem14[15],digits=3),nsmall=0)),
                         (format(round(sem15[15],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[15,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[15,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[15,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[15,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[15,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[15,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[15,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[16]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[16]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[16]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[16]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[16]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[16]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[16]),digits=3),nsmall=0))),
                       c((format(round(sem9[16],digits=3),nsmall=0)),
                         (format(round(sem10[16],digits=3),nsmall=0)),
                         (format(round(sem11[16],digits=3),nsmall=0)),
                         (format(round(sem12[16],digits=3),nsmall=0)),
                         (format(round(sem13[16],digits=3),nsmall=0)),
                         (format(round(sem14[16],digits=3),nsmall=0)),
                         (format(round(sem15[16],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[16,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[16,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[16,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[16,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[16,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[16,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[16,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[17]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[17]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[17]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[17]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[17]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[17]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[17]),digits=3),nsmall=0))),
                       c((format(round(sem9[17],digits=3),nsmall=0)),
                         (format(round(sem10[17],digits=3),nsmall=0)),
                         (format(round(sem11[17],digits=3),nsmall=0)),
                         (format(round(sem12[17],digits=3),nsmall=0)),
                         (format(round(sem13[17],digits=3),nsmall=0)),
                         (format(round(sem14[17],digits=3),nsmall=0)),
                         (format(round(sem15[17],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[17,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[17,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[17,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[17,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[17,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[17,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[17,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[18]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[18]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[18]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[18]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[18]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[18]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[18]),digits=3),nsmall=0))),
                       c((format(round(sem9[18],digits=3),nsmall=0)),
                         (format(round(sem10[18],digits=3),nsmall=0)),
                         (format(round(sem11[18],digits=3),nsmall=0)),
                         (format(round(sem12[18],digits=3),nsmall=0)),
                         (format(round(sem13[18],digits=3),nsmall=0)),
                         (format(round(sem14[18],digits=3),nsmall=0)),
                         (format(round(sem15[18],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[18,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[18,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[18,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[18,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[18,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[18,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[18,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[19]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[19]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[19]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[19]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[19]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[19]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[19]),digits=3),nsmall=0))),
                       c((format(round(sem9[19],digits=3),nsmall=0)),
                         (format(round(sem10[19],digits=3),nsmall=0)),
                         (format(round(sem11[19],digits=3),nsmall=0)),
                         (format(round(sem12[19],digits=3),nsmall=0)),
                         (format(round(sem13[19],digits=3),nsmall=0)),
                         (format(round(sem14[19],digits=3),nsmall=0)),
                         (format(round(sem15[19],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[19,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[19,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[19,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[19,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[19,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[19,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[19,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[20]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[20]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[20]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[20]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[20]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[20]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[20]),digits=3),nsmall=0))),
                       c((format(round(sem9[20],digits=3),nsmall=0)),
                         (format(round(sem10[20],digits=3),nsmall=0)),
                         (format(round(sem11[20],digits=3),nsmall=0)),
                         (format(round(sem12[20],digits=3),nsmall=0)),
                         (format(round(sem13[20],digits=3),nsmall=0)),
                         (format(round(sem14[20],digits=3),nsmall=0)),
                         (format(round(sem15[20],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[20,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[20,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[20,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[20,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[20,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[20,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[20,4],digits=3),nsmall=0))),

                       c((format(round(sum(mm9$coefficients[21]),digits=3),nsmall=0)),
                         (format(round(sum(mm10$coefficients[21]),digits=3),nsmall=0)),
                         (format(round(sum(mm11$coefficients[21]),digits=3),nsmall=0)),
                         (format(round(sum(mm12$coefficients[21]),digits=3),nsmall=0)),
                         (format(round(sum(mm13$coefficients[21]),digits=3),nsmall=0)),
                         (format(round(sum(mm14$coefficients[21]),digits=3),nsmall=0)),
                         (format(round(sum(mm15$coefficients[21]),digits=3),nsmall=0))),
                       c((format(round(sem9[21],digits=3),nsmall=0)),
                         (format(round(sem10[21],digits=3),nsmall=0)),
                         (format(round(sem11[21],digits=3),nsmall=0)),
                         (format(round(sem12[21],digits=3),nsmall=0)),
                         (format(round(sem13[21],digits=3),nsmall=0)),
                         (format(round(sem14[21],digits=3),nsmall=0)),
                         (format(round(sem15[21],digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$coefficients[21,4],digits=3),nsmall=0)),
                         (format(round(summary(mm10)$coefficients[21,4],digits=3),nsmall=0)),
                         (format(round(summary(mm11)$coefficients[21,4],digits=3),nsmall=0)),
                         (format(round(summary(mm12)$coefficients[21,4],digits=3),nsmall=0)),
                         (format(round(summary(mm13)$coefficients[21,4],digits=3),nsmall=0)),
                         (format(round(summary(mm14)$coefficients[21,4],digits=3),nsmall=0)),
                         (format(round(summary(mm15)$coefficients[21,4],digits=3),nsmall=0))),


                       c((format(round(summary(mm9)$r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm10)$r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm11)$r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm12)$r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm13)$r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm14)$r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm15)$r.squared,digits=3),nsmall=0))),
                       c((format(round(summary(mm9)$adj.r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm10)$adj.r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm11)$adj.r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm12)$adj.r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm13)$adj.r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm14)$adj.r.squared,digits=3),nsmall=0)),
                         (format(round(summary(mm15)$adj.r.squared,digits=3),nsmall=0))),
                       c((format(round(nobs(mm9),digits=3),nsmall=0)),
                         (format(round(nobs(mm10),digits=3),nsmall=0)),
                         (format(round(nobs(mm11),digits=3),nsmall=0)),
                         (format(round(nobs(mm12),digits=3),nsmall=0)),
                         (format(round(nobs(mm13),digits=3),nsmall=0)),
                         (format(round(nobs(mm14),digits=3),nsmall=0)),
                         (format(round(nobs(mm15),digits=3),nsmall=0)))
            )




summary(lm(score~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

#summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
            # +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
          #   badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(general~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(yield~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(drought_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(disease_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(early_maturing~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(germination~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))



## NON SEED RELATED RATINGS ##

#### regressions without controls - non-seed related ratings

nn1<- lm(overall_rating~genderdummy , data = avg)
senn1<- sqrt(diag(vcov(nn1)))
nn2<- lm(general_rating_nonseed~genderdummy , data = avg)
senn2<- sqrt(diag(vcov(nn2)))
nn3<- lm(location~genderdummy , data = avg)
senn3<- sqrt(diag(vcov(nn3)))
nn4<- lm(price~genderdummy , data = avg)
senn4<- sqrt(diag(vcov(nn4)))
nn5<- lm(quality~genderdummy , data = avg)
senn5<- sqrt(diag(vcov(nn5)))
nn6<- lm(stock~genderdummy , data = avg)
senn6<- sqrt(diag(vcov(nn6)))
nn7<- lm(reputation~genderdummy , data = avg)
senn7<- sqrt(diag(vcov(nn7)))

s7<- rbind(c((format(round(sum(nn1$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn2$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn3$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn4$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn5$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn6$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn7$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(senn1[1],digits=3),nsmall=0)),
             (format(round(senn2[1],digits=3),nsmall=0)),
             (format(round(senn3[1],digits=3),nsmall=0)),
             (format(round(senn4[1],digits=3),nsmall=0)),
             (format(round(senn5[1],digits=3),nsmall=0)),
             (format(round(senn6[1],digits=3),nsmall=0)),
             (format(round(senn7[1],digits=3),nsmall=0))),
           c((format(round(summary(nn1)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn2)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn3)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn4)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn5)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn6)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn7)$coefficients[1,4],digits=3),nsmall=0))),

           c((format(round(sum(nn1$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn2$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn3$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn4$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn5$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn6$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn7$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(senn1[2],digits=3),nsmall=0)),
             (format(round(senn2[2],digits=3),nsmall=0)),
             (format(round(senn3[2],digits=3),nsmall=0)),
             (format(round(senn4[2],digits=3),nsmall=0)),
             (format(round(senn5[2],digits=3),nsmall=0)),
             (format(round(senn6[2],digits=3),nsmall=0)),
             (format(round(senn7[2],digits=3),nsmall=0))),
           c((format(round(summary(nn1)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn2)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn3)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn4)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn5)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn6)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn7)$coefficients[2,4],digits=3),nsmall=0))),

           c((format(round(summary(nn1)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn2)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn3)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn4)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn5)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn6)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn7)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(nn1)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn2)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn3)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn4)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn5)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn6)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn7)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(nn1),digits=3),nsmall=0)),
             (format(round(nobs(nn2),digits=3),nsmall=0)),
             (format(round(nobs(nn3),digits=3),nsmall=0)),
             (format(round(nobs(nn4),digits=3),nsmall=0)),
             (format(round(nobs(nn5),digits=3),nsmall=0)),
             (format(round(nobs(nn6),digits=3),nsmall=0)),
             (format(round(nobs(nn7),digits=3),nsmall=0)))
)


summary(lm(overall_rating~genderdummy , data = avg))
summary(lm(general_rating_nonseed~genderdummy , data = avg))
summary(lm(location~genderdummy , data = avg))
summary(lm(price~genderdummy , data = avg))
summary(lm(quality~genderdummy , data = avg))
summary(lm(stock ~genderdummy , data = avg))
summary(lm(reputation ~genderdummy , data = avg))

#### regressions with dealer's gender + dealer characteristics  --- non-seed related ratings

nn9<- lm(overall_rating~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
           +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
senn9<- sqrt(diag(vcov(nn9)))

nn10<-lm(general_rating_nonseed~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
senn10<- sqrt(diag(vcov(nn10)))

nn11<-lm(location~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = avg)
senn11<- sqrt(diag(vcov(nn11)))

nn12<-lm(price~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
senn12<- sqrt(diag(vcov(nn12)))

nn13<-lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
senn13<- sqrt(diag(vcov(nn13)))

nn14<-lm(stock~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = avg)
senn14<- sqrt(diag(vcov(nn14)))

nn15<-lm(reputation~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg)
senn15<- sqrt(diag(vcov(nn15)))


s8<- rbind(c((format(round(sum(nn9$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn11$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn12$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn14$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(nn15$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(senn9[1],digits=3),nsmall=0)),
             (format(round(senn10[1],digits=3),nsmall=0)),
             (format(round(senn11[1],digits=3),nsmall=0)),
             (format(round(senn12[1],digits=3),nsmall=0)),
             (format(round(senn13[1],digits=3),nsmall=0)),
             (format(round(senn14[1],digits=3),nsmall=0)),
             (format(round(senn15[1],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn11)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn12)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn14)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(nn15)$coefficients[1,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn11$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn12$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn14$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(nn15$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(senn9[2],digits=3),nsmall=0)),
             (format(round(senn10[2],digits=3),nsmall=0)),
             (format(round(senn11[2],digits=3),nsmall=0)),
             (format(round(senn12[2],digits=3),nsmall=0)),
             (format(round(senn13[2],digits=3),nsmall=0)),
             (format(round(senn14[2],digits=3),nsmall=0)),
             (format(round(senn15[2],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn11)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn12)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn14)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(nn15)$coefficients[2,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(nn11$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(nn12$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(nn14$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(nn15$coefficients[3]),digits=3),nsmall=0))),
           c((format(round(senn9[3],digits=3),nsmall=0)),
             (format(round(senn10[3],digits=3),nsmall=0)),
             (format(round(senn11[3],digits=3),nsmall=0)),
             (format(round(senn12[3],digits=3),nsmall=0)),
             (format(round(senn13[3],digits=3),nsmall=0)),
             (format(round(senn14[3],digits=3),nsmall=0)),
             (format(round(senn15[3],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(nn11)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(nn12)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(nn14)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(nn15)$coefficients[3,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(nn11$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(nn12$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(nn14$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(nn15$coefficients[4]),digits=3),nsmall=0))),
           c((format(round(senn9[4],digits=3),nsmall=0)),
             (format(round(senn10[4],digits=3),nsmall=0)),
             (format(round(senn11[4],digits=3),nsmall=0)),
             (format(round(senn12[4],digits=3),nsmall=0)),
             (format(round(senn13[4],digits=3),nsmall=0)),
             (format(round(senn14[4],digits=3),nsmall=0)),
             (format(round(senn15[4],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(nn11)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(nn12)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(nn14)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(nn15)$coefficients[4,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(nn11$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(nn12$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(nn14$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(nn15$coefficients[5]),digits=3),nsmall=0))),
           c((format(round(senn9[5],digits=3),nsmall=0)),
             (format(round(senn10[5],digits=3),nsmall=0)),
             (format(round(senn11[5],digits=3),nsmall=0)),
             (format(round(senn12[5],digits=3),nsmall=0)),
             (format(round(senn13[5],digits=3),nsmall=0)),
             (format(round(senn14[5],digits=3),nsmall=0)),
             (format(round(senn15[5],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(nn11)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(nn12)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(nn14)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(nn15)$coefficients[5,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(nn11$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(nn12$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(nn14$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(nn15$coefficients[6]),digits=3),nsmall=0))),
           c((format(round(senn9[6],digits=3),nsmall=0)),
             (format(round(senn10[6],digits=3),nsmall=0)),
             (format(round(senn11[6],digits=3),nsmall=0)),
             (format(round(senn12[6],digits=3),nsmall=0)),
             (format(round(senn13[6],digits=3),nsmall=0)),
             (format(round(senn14[6],digits=3),nsmall=0)),
             (format(round(senn15[6],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(nn11)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(nn12)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(nn14)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(nn15)$coefficients[6,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[7]),digits=3),nsmall=0)),
             0,
             (format(round(sum(nn12$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[7]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn15$coefficients[7]),digits=3),nsmall=0))),
           c((format(round(senn9[7],digits=3),nsmall=0)),
             (format(round(senn10[7],digits=3),nsmall=0)),
            0,
             (format(round(senn12[7],digits=3),nsmall=0)),
             (format(round(senn13[7],digits=3),nsmall=0)),
            0,
             (format(round(senn15[7],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[7,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[7,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[7,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[8]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[8]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn15$coefficients[8]),digits=3),nsmall=0))),
           c((format(round(senn9[8],digits=3),nsmall=0)),
             (format(round(senn10[8],digits=3),nsmall=0)),
            0,
             (format(round(senn12[8],digits=3),nsmall=0)),
             (format(round(senn13[8],digits=3),nsmall=0)),
             0,
             (format(round(senn15[8],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[8,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[8,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[8,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[9]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[9]),digits=3),nsmall=0)),
           0,
             (format(round(sum(nn15$coefficients[9]),digits=3),nsmall=0))),
           c((format(round(senn9[9],digits=3),nsmall=0)),
             (format(round(senn10[9],digits=3),nsmall=0)),
            0,
             (format(round(senn12[9],digits=3),nsmall=0)),
             (format(round(senn13[9],digits=3),nsmall=0)),
           0,
             (format(round(senn15[9],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[9,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[9,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(nn15)$coefficients[9,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[10]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[10]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn15$coefficients[10]),digits=3),nsmall=0))),
           c((format(round(senn9[10],digits=3),nsmall=0)),
             (format(round(senn10[10],digits=3),nsmall=0)),
            0,
             (format(round(senn12[10],digits=3),nsmall=0)),
             (format(round(senn13[10],digits=3),nsmall=0)),
            0,
             (format(round(senn15[10],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[10,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[10,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(nn15)$coefficients[10,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[11]),digits=3),nsmall=0)),
           0,
             (format(round(sum(nn12$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[11]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn15$coefficients[11]),digits=3),nsmall=0))),
           c((format(round(senn9[11],digits=3),nsmall=0)),
             (format(round(senn10[11],digits=3),nsmall=0)),
            0,
             (format(round(senn12[11],digits=3),nsmall=0)),
             (format(round(senn13[11],digits=3),nsmall=0)),
            0,
             (format(round(senn15[11],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[11,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(nn12)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[11,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(nn15)$coefficients[11,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[12]),digits=3),nsmall=0)),
          0,
             (format(round(sum(nn12$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[12]),digits=3),nsmall=0)),
             0,
             (format(round(sum(nn15$coefficients[12]),digits=3),nsmall=0))),
           c((format(round(senn9[12],digits=3),nsmall=0)),
             (format(round(senn10[12],digits=3),nsmall=0)),
            0,
             (format(round(senn12[12],digits=3),nsmall=0)),
             (format(round(senn13[12],digits=3),nsmall=0)),
             0,
             (format(round(senn15[12],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[12,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[12,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[12,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[13]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[13]),digits=3),nsmall=0)),
              0,
             (format(round(sum(nn15$coefficients[13]),digits=3),nsmall=0))),
           c((format(round(senn9[13],digits=3),nsmall=0)),
             (format(round(senn10[13],digits=3),nsmall=0)),
            0,
             (format(round(senn12[13],digits=3),nsmall=0)),
             (format(round(senn13[13],digits=3),nsmall=0)),
            0,
             (format(round(senn15[13],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[13,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[13,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[13,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[14]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[14]),digits=3),nsmall=0)),
             0,
             (format(round(sum(nn15$coefficients[14]),digits=3),nsmall=0))),
           c((format(round(senn9[14],digits=3),nsmall=0)),
             (format(round(senn10[14],digits=3),nsmall=0)),
            0,
             (format(round(senn12[14],digits=3),nsmall=0)),
             (format(round(senn13[14],digits=3),nsmall=0)),
            0,
             (format(round(senn15[14],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[14,4],digits=3),nsmall=0)),
             0,
             (format(round(summary(nn12)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[14,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(nn15)$coefficients[14,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[15]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[15]),digits=3),nsmall=0)),
             0,
             (format(round(sum(nn15$coefficients[15]),digits=3),nsmall=0))),
           c((format(round(senn9[15],digits=3),nsmall=0)),
             (format(round(senn10[15],digits=3),nsmall=0)),
            0,
             (format(round(senn12[15],digits=3),nsmall=0)),
             (format(round(senn13[15],digits=3),nsmall=0)),
            0,
             (format(round(senn15[15],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[15,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[15,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[15,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[16]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[16]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn15$coefficients[16]),digits=3),nsmall=0))),
           c((format(round(senn9[16],digits=3),nsmall=0)),
             (format(round(senn10[16],digits=3),nsmall=0)),
            0,
             (format(round(senn12[16],digits=3),nsmall=0)),
             (format(round(senn13[16],digits=3),nsmall=0)),
            0,
             (format(round(senn15[16],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[16,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[16,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[16,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[17]),digits=3),nsmall=0)),
             0,
             (format(round(sum(nn12$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[17]),digits=3),nsmall=0)),
             0,
             (format(round(sum(nn15$coefficients[17]),digits=3),nsmall=0))),
           c((format(round(senn9[17],digits=3),nsmall=0)),
             (format(round(senn10[17],digits=3),nsmall=0)),
            0,
             (format(round(senn12[17],digits=3),nsmall=0)),
             (format(round(senn13[17],digits=3),nsmall=0)),
            0,
             (format(round(senn15[17],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[17,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(nn12)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[17,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[17,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[18]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[18]),digits=3),nsmall=0)),
             0,
             (format(round(sum(nn15$coefficients[18]),digits=3),nsmall=0))),
           c((format(round(senn9[18],digits=3),nsmall=0)),
             (format(round(senn10[18],digits=3),nsmall=0)),
            0,
             (format(round(senn12[18],digits=3),nsmall=0)),
             (format(round(senn13[18],digits=3),nsmall=0)),
            0,
             (format(round(senn15[18],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[18,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[18,4],digits=3),nsmall=0)),
           0,
             (format(round(summary(nn15)$coefficients[18,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[19]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[19]),digits=3),nsmall=0)),
           0,
             (format(round(sum(nn15$coefficients[19]),digits=3),nsmall=0))),
           c((format(round(senn9[19],digits=3),nsmall=0)),
             (format(round(senn10[19],digits=3),nsmall=0)),
            0,
             (format(round(senn12[19],digits=3),nsmall=0)),
             (format(round(senn13[19],digits=3),nsmall=0)),
           0,
             (format(round(senn15[19],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[19,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[19,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[19,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[20]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[20]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn15$coefficients[20]),digits=3),nsmall=0))),
           c((format(round(senn9[20],digits=3),nsmall=0)),
             (format(round(senn10[20],digits=3),nsmall=0)),
            0,
             (format(round(senn12[20],digits=3),nsmall=0)),
             (format(round(senn13[20],digits=3),nsmall=0)),
            0,
             (format(round(senn15[20],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[20,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[20,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[20,4],digits=3),nsmall=0))),

           c((format(round(sum(nn9$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(nn10$coefficients[21]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn12$coefficients[21]),digits=3),nsmall=0)),
             (format(round(sum(nn13$coefficients[21]),digits=3),nsmall=0)),
            0,
             (format(round(sum(nn15$coefficients[21]),digits=3),nsmall=0))),
           c((format(round(senn9[21],digits=3),nsmall=0)),
             (format(round(senn10[21],digits=3),nsmall=0)),
            0,
             (format(round(senn12[21],digits=3),nsmall=0)),
             (format(round(senn13[21],digits=3),nsmall=0)),
            0,
             (format(round(senn15[21],digits=3),nsmall=0))),
           c((format(round(summary(nn9)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(nn10)$coefficients[21,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn12)$coefficients[21,4],digits=3),nsmall=0)),
             (format(round(summary(nn13)$coefficients[21,4],digits=3),nsmall=0)),
            0,
             (format(round(summary(nn15)$coefficients[21,4],digits=3),nsmall=0))),


           c((format(round(summary(nn9)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn10)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn11)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn12)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn13)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn14)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn15)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(nn9)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn10)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn11)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn12)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn13)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn14)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(nn15)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(nn9),digits=3),nsmall=0)),
             (format(round(nobs(nn10),digits=3),nsmall=0)),
             (format(round(nobs(nn11),digits=3),nsmall=0)),
             (format(round(nobs(nn12),digits=3),nsmall=0)),
             (format(round(nobs(nn13),digits=3),nsmall=0)),
             (format(round(nobs(nn14),digits=3),nsmall=0)),
             (format(round(nobs(nn15),digits=3),nsmall=0)))
)



summary(lm(overall_rating~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(general_rating_nonseed~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(location~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(price~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(stock~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))

summary(lm(reputation~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = avg))



################### MODEL 5 (FE) #########################
fedata_base <- merge(farmers_seedsub, rating_dyads, by="farmer_ID")

fedata <- merge(fedata_base, baseline_dealer, by="shop_ID")

fedata[fedata=="n/a"]<- NA
fedata[fedata=="98"]<- NA
fedata[fedata=="999"]<- NA

#create gender dummy
fedata$genderdummy <- ifelse(fedata$maize.owner.agree.gender == "Male", 1, 0)


###dealer characteristics for  controls

#education of dealers
table(fedata$maize.owner.agree.educ) #430 are g which is Other
fedata$prim <- 0
#fedata$prim[fedata$maize.owner.agree.educ=="c"|fedata$maize.owner.agree.educ=="d"|fedata$maize.owner.agree.educ=="e"|fedata$maize.owner.agree.educ=="f"]<- 1

#finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
fedata$prim[fedata$maize.owner.agree.educ=="e"|fedata$maize.owner.agree.educ=="f"] <- 1
fedata$prim[fedata$maize.owner.agree.educ=="g"]<- NA
table(fedata$prim)
#5208 ARE 1 --- 45 percent


#age of dealer
summary(fedata$maize.owner.agree.age)
fedata$maize.owner.agree.age[fedata$maize.owner.agree.age==999] <- NA
table(fedata$maize.owner.agree.age)

#distance of shop to nearest tarmac road
table(fedata$maize.owner.agree.q3)
fedata$maize.owner.agree.q3[fedata$maize.owner.agree.q3==999] <- NA
summary(fedata$maize.owner.agree.q3)

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
fedata$pest_prob<- ifelse(fedata$pest== 'No', 1, 0)
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

#Q78. Lighting conditions in area where seed is stored?
fedata$badlighting <- 0
fedata$badlighting[fedata$maize.owner.agree.temp.q78=="2"]<-1
table(fedata$badlighting)

#Q79. On what surface are seed stored?
fedata$badstored <- 0
fedata$badstored[fedata$maize.owner.agree.temp.q79=="3"|fedata$maize.owner.agree.temp.q79=="4"|fedata$maize.owner.agree.temp.q79=="5"]<-1
fedata$badstored[fedata$maize.owner.agree.temp.q79==96]<-NA
table(fedata$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
fedata$maize.owner.agree.q80
fedata$open<-as.character(fedata$maize.owner.agree.temp.q80)
fedata$open_storage<- ifelse(fedata$open== 'No', 1, 0)
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
table(fedata$maize.owner.agree.q96)
fedata$complaint<- ifelse(fedata$maize.owner.agree.q96== 'No', 1, 0)
table(fedata$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
table(fedata$maize.owner.agree.q70)
fedata$maize.owner.agree.q70[fedata$maize.owner.agree.q70==999] <- NA

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


fedata$score <-  rowMeans(fedata[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
fedata$overall_rating <-  rowMeans(fedata[c("general_rating_nonseed", "location","price","quality","stock","reputation")],na.rm=T)

#fedata$score <-  rowMeans(fedata[c("quality_rating","seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating")],na.rm=T)
#fedata$overall_rating <-  rowMeans(fedata[c("general_rating", "location_rating","price_rating","quality_rating","stock_rating","reputation_rating")],na.rm=T)


###farmer characteristics for controls
fedata$educ_f <- 0
fedata$educ_f[fedata$Check2.check.maize.q17=="c" | fedata$Check2.check.maize.q17=="d" |fedata$Check2.check.maize.q17=="e" |
                        fedata$Check2.check.maize.q17=="f"  ] <- 1 #educated farmers -- finished primary educ
fedata$educ_f[ fedata$Check2.check.maize.q17=="g" ] <- NA
table(fedata$educ_f)

fedata$married <- ifelse(fedata$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers

#age of farmers
fedata$Check2.check.maize.q14[fedata$Check2.check.maize.q14==999 ] <- NA
summary(fedata$Check2.check.maize.q14 )

#distance from tarmac road
fedata$Check2.check.maize.q8[ fedata$Check2.check.maize.q8==999 ] <- NA
summary(fedata$Check2.check.maize.q8 )



write.csv(fedata, file = "fedata.csv")

## SEED RELATED RATINGS ##

#### regressions without controls - seed related ratings

summary(lm(score~genderdummy +farmer_ID, data = fedata))
#if you want to remove intercept
#summary(lm(score~genderdummy-1 +farmer_ID, data = fedata))
#the intercept without -1 is the intercept of the first farmerID

#reg1<-summary(lm(quality~genderdummy+farmer_ID , data = fedata))
#intercept --- 5.066
#with(fedata, plot(genderdummy,quality))
#abline(reg1)
summary(lm(yield~genderdummy +farmer_ID, data = fedata))
summary(lm(drought_resistent~genderdummy+farmer_ID , data = fedata))
summary(lm(disease_resistent~genderdummy+farmer_ID , data = fedata))
summary(lm(early_maturing~genderdummy+farmer_ID , data = fedata))
summary(lm(germination~genderdummy+farmer_ID , data = fedata))

############# plm method

plm1<-plm(score~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i1<-mean(fixef(plm1))
seplm1<- sqrt(diag(vcov(plm1)))

coef1<-coeftest(plm1, vcovHC(plm1, type = "HC0", cluster = "time"))


#plm2<-plm(quality~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
#i2<-mean(fixef(plm2))
#seplm2<- sqrt(diag(vcov(plm2)))

plm3<-plm(general~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i3<-mean(fixef(plm3))
seplm3<- sqrt(diag(vcov(plm3)))

coef3<-coeftest(plm3, vcovHC(plm3, type = "HC0", cluster = "time"))


plm4<-plm(yield~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i4<-mean(fixef(plm4))
seplm4<- sqrt(diag(vcov(plm4)))

coef4<-coeftest(plm4, vcovHC(plm4, type = "HC0", cluster = "time"))


plm5<-plm(drought_resistent~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i5<-mean(fixef(plm5))
seplm5<- sqrt(diag(vcov(plm5)))

coef5<-coeftest(plm5, vcovHC(plm5, type = "HC0", cluster = "time"))

plm6<-plm(disease_resistent~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i6<-mean(fixef(plm6))
seplm6<- sqrt(diag(vcov(plm6)))

coef6<-coeftest(plm6, vcovHC(plm6, type = "HC0", cluster = "time"))

plm7<-plm(early_maturing~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i7<-mean(fixef(plm7))
seplm7<- sqrt(diag(vcov(plm7)))

coef7<-coeftest(plm7, vcovHC(plm7, type = "HC0", cluster = "time"))

plm8<-plm(germination~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i8<-mean(fixef(plm8))
seplm8<- sqrt(diag(vcov(plm8)))

coef8<-coeftest(plm8, vcovHC(plm8, type = "HC0", cluster = "time"))


fe1<- rbind( c((format(round(i1[1],digits=3),nsmall=0)),
              # (format(round(i2[1],digits=3),nsmall=0)),
               (format(round(i3[1],digits=3),nsmall=0)),
               (format(round(i4[1],digits=3),nsmall=0)),
               (format(round(i5[1],digits=3),nsmall=0)),
               (format(round(i6[1],digits=3),nsmall=0)),
               (format(round(i7[1],digits=3),nsmall=0)),
               (format(round(i8[1],digits=3),nsmall=0))),
            c((format(round(sum(plm1$coefficients[1]),digits=3),nsmall=0)),
            # (format(round(sum(plm2$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm3$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm4$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm5$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm6$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm7$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm8$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(coef1[1,2],digits=3),nsmall=0)),
             (format(round(coef3[1,2],digits=3),nsmall=0)),
             (format(round(coef4[1,2],digits=3),nsmall=0)),
             (format(round(coef5[1,2],digits=3),nsmall=0)),
             (format(round(coef6[1,2],digits=3),nsmall=0)),
             (format(round(coef7[1,2],digits=3),nsmall=0)),
             (format(round(coef8[1,2],digits=3),nsmall=0))),
           c((format(round(coef1[1,4],digits=3),nsmall=0)),
             (format(round(coef3[1,4],digits=3),nsmall=0)),
             (format(round(coef4[1,4],digits=3),nsmall=0)),
             (format(round(coef5[1,4],digits=3),nsmall=0)),
             (format(round(coef6[1,4],digits=3),nsmall=0)),
             (format(round(coef7[1,4],digits=3),nsmall=0)),
             (format(round(coef8[1,4],digits=3),nsmall=0))),


           c((format(round(summary(plm1)$r.squared[1],digits=3),nsmall=0)),
          #   (format(round(summary(plm2)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm3)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm4)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm5)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm6)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm7)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm8)$r.squared[1],digits=3),nsmall=0))),
           c((format(round(summary(plm1)$r.squared[2],digits=3),nsmall=0)),
            # (format(round(summary(plm2)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm3)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm4)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm5)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm6)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm7)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm8)$r.squared[2],digits=3),nsmall=0))),
           c((format(round(nobs(plm1),digits=3),nsmall=0)),
            # (format(round(nobs(plm2),digits=3),nsmall=0)),
             (format(round(nobs(plm3),digits=3),nsmall=0)),
             (format(round(nobs(plm4),digits=3),nsmall=0)),
             (format(round(nobs(plm5),digits=3),nsmall=0)),
             (format(round(nobs(plm6),digits=3),nsmall=0)),
             (format(round(nobs(plm7),digits=3),nsmall=0)),
             (format(round(nobs(plm8),digits=3),nsmall=0)))
)





#### regressions with dealer's gender + dealer characteristics  --- seed related ratings

summary(lm(score~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

#summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
           #  +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
           #  badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))
#reg2<-lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
     #      +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
      #     badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata)
#with(fedata, plot(genderdummy,quality))
#abline(reg2)

summary(lm(general~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(yield~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(drought_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(disease_resistent~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(early_maturing~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation+
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(germination~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))


####### plm method

plm9<-plm(score~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
            badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i9<-mean(fixef(plm9))
seplm9<- sqrt(diag(vcov(plm9)))

coef9<-coeftest(plm9, vcovHC(plm9, type = "HC0", cluster = "time"))

############## if we want to check the overall intercept --- only works when controls are also included
#fx_level <- fixef(plm9, type = "level")
#fx_dmean <- fixef(plm9, type = "dmean")
#overallint <- within_intercept(plm9)
#all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE)


#plm10<-plm(quality~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
          #   +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
          #   badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
#i10<-mean(fixef(plm10))
#seplm10<- sqrt(diag(vcov(plm10)))

plm11<-plm(general~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i11<-mean(fixef(plm11))
seplm11<- sqrt(diag(vcov(plm11)))

coef11<-coeftest(plm11, vcovHC(plm11, type = "HC0", cluster = "time"))

plm12<-plm(yield~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i12<-mean(fixef(plm12))
seplm12<- sqrt(diag(vcov(plm12)))

coef12<-coeftest(plm12, vcovHC(plm12, type = "HC0", cluster = "time"))

plm13<-plm(drought_resistent~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i13<-mean(fixef(plm13))
seplm13<- sqrt(diag(vcov(plm13)))

coef13<-coeftest(plm13, vcovHC(plm13, type = "HC0", cluster = "time"))

plm14<-plm(disease_resistent~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i14<-mean(fixef(plm14))
seplm14<- sqrt(diag(vcov(plm14)))

coef14<-coeftest(plm14, vcovHC(plm14, type = "HC0", cluster = "time"))


plm15<-plm(early_maturing~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i15<-mean(fixef(plm15))
seplm15<- sqrt(diag(vcov(plm15)))

coef15<-coeftest(plm15, vcovHC(plm15, type = "HC0", cluster = "time"))


plm16<-plm(germination~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i16<-mean(fixef(plm16))
seplm16<- sqrt(diag(vcov(plm16)))

coef16<-coeftest(plm16, vcovHC(plm16, type = "HC0", cluster = "time"))



fe2<- rbind(c((format(round(i9[1],digits=3),nsmall=0)),
            #  (format(round(i10[1],digits=3),nsmall=0)),
              (format(round(i11[1],digits=3),nsmall=0)),
              (format(round(i12[1],digits=3),nsmall=0)),
              (format(round(i13[1],digits=3),nsmall=0)),
              (format(round(i14[1],digits=3),nsmall=0)),
              (format(round(i15[1],digits=3),nsmall=0)),
              (format(round(i16[1],digits=3),nsmall=0))),

            c((format(round(sum(plm9$coefficients[1]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(coef9[1,2],digits=3),nsmall=0)),
             (format(round(coef11[1,2],digits=3),nsmall=0)),
             (format(round(coef12[1,2],digits=3),nsmall=0)),
             (format(round(coef13[1,2],digits=3),nsmall=0)),
             (format(round(coef14[1,2],digits=3),nsmall=0)),
             (format(round(coef15[1,2],digits=3),nsmall=0)),
             (format(round(coef16[1,2],digits=3),nsmall=0))),
           c((format(round(coef9[1,4],digits=3),nsmall=0)),
             (format(round(coef11[1,4],digits=3),nsmall=0)),
             (format(round(coef12[1,4],digits=3),nsmall=0)),
             (format(round(coef13[1,4],digits=3),nsmall=0)),
             (format(round(coef14[1,4],digits=3),nsmall=0)),
             (format(round(coef15[1,4],digits=3),nsmall=0)),
             (format(round(coef16[1,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[2]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(coef9[2,2],digits=3),nsmall=0)),
             (format(round(coef11[2,2],digits=3),nsmall=0)),
             (format(round(coef12[2,2],digits=3),nsmall=0)),
             (format(round(coef13[2,2],digits=3),nsmall=0)),
             (format(round(coef14[2,2],digits=3),nsmall=0)),
             (format(round(coef15[2,2],digits=3),nsmall=0)),
             (format(round(coef16[2,2],digits=3),nsmall=0))),
           c((format(round(coef9[2,4],digits=3),nsmall=0)),
             (format(round(coef11[2,4],digits=3),nsmall=0)),
             (format(round(coef12[2,4],digits=3),nsmall=0)),
             (format(round(coef13[2,4],digits=3),nsmall=0)),
             (format(round(coef14[2,4],digits=3),nsmall=0)),
             (format(round(coef15[2,4],digits=3),nsmall=0)),
             (format(round(coef16[2,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[3]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[3]),digits=3),nsmall=0))),
           c((format(round(coef9[3,2],digits=3),nsmall=0)),
             (format(round(coef11[3,2],digits=3),nsmall=0)),
             (format(round(coef12[3,2],digits=3),nsmall=0)),
             (format(round(coef13[3,2],digits=3),nsmall=0)),
             (format(round(coef14[3,2],digits=3),nsmall=0)),
             (format(round(coef15[3,2],digits=3),nsmall=0)),
             (format(round(coef16[3,2],digits=3),nsmall=0))),
           c((format(round(coef9[3,4],digits=3),nsmall=0)),
             (format(round(coef11[3,4],digits=3),nsmall=0)),
             (format(round(coef12[3,4],digits=3),nsmall=0)),
             (format(round(coef13[3,4],digits=3),nsmall=0)),
             (format(round(coef14[3,4],digits=3),nsmall=0)),
             (format(round(coef15[3,4],digits=3),nsmall=0)),
             (format(round(coef16[3,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[4]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[4]),digits=3),nsmall=0))),
           c((format(round(coef9[4,2],digits=3),nsmall=0)),
             (format(round(coef11[4,2],digits=3),nsmall=0)),
             (format(round(coef12[4,2],digits=3),nsmall=0)),
             (format(round(coef13[4,2],digits=3),nsmall=0)),
             (format(round(coef14[4,2],digits=3),nsmall=0)),
             (format(round(coef15[4,2],digits=3),nsmall=0)),
             (format(round(coef16[4,2],digits=3),nsmall=0))),
           c((format(round(coef9[4,4],digits=3),nsmall=0)),
             (format(round(coef11[4,4],digits=3),nsmall=0)),
             (format(round(coef12[4,4],digits=3),nsmall=0)),
             (format(round(coef13[4,4],digits=3),nsmall=0)),
             (format(round(coef14[4,4],digits=3),nsmall=0)),
             (format(round(coef15[4,4],digits=3),nsmall=0)),
             (format(round(coef16[4,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[5]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[5]),digits=3),nsmall=0))),
           c((format(round(coef9[5,2],digits=3),nsmall=0)),
             (format(round(coef11[5,2],digits=3),nsmall=0)),
             (format(round(coef12[5,2],digits=3),nsmall=0)),
             (format(round(coef13[5,2],digits=3),nsmall=0)),
             (format(round(coef14[5,2],digits=3),nsmall=0)),
             (format(round(coef15[5,2],digits=3),nsmall=0)),
             (format(round(coef16[5,2],digits=3),nsmall=0))),
           c((format(round(coef9[5,4],digits=3),nsmall=0)),
             (format(round(coef11[5,4],digits=3),nsmall=0)),
             (format(round(coef12[5,4],digits=3),nsmall=0)),
             (format(round(coef13[5,4],digits=3),nsmall=0)),
             (format(round(coef14[5,4],digits=3),nsmall=0)),
             (format(round(coef15[5,4],digits=3),nsmall=0)),
             (format(round(coef16[5,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[6]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[6]),digits=3),nsmall=0))),
           c((format(round(coef9[6,2],digits=3),nsmall=0)),
             (format(round(coef11[6,2],digits=3),nsmall=0)),
             (format(round(coef12[6,2],digits=3),nsmall=0)),
             (format(round(coef13[6,2],digits=3),nsmall=0)),
             (format(round(coef14[6,2],digits=3),nsmall=0)),
             (format(round(coef15[6,2],digits=3),nsmall=0)),
             (format(round(coef16[6,2],digits=3),nsmall=0))),
           c((format(round(coef9[6,4],digits=3),nsmall=0)),
             (format(round(coef11[6,4],digits=3),nsmall=0)),
             (format(round(coef12[6,4],digits=3),nsmall=0)),
             (format(round(coef13[6,4],digits=3),nsmall=0)),
             (format(round(coef14[6,4],digits=3),nsmall=0)),
             (format(round(coef15[6,4],digits=3),nsmall=0)),
             (format(round(coef16[6,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[7]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[7]),digits=3),nsmall=0))),
           c((format(round(coef9[7,2],digits=3),nsmall=0)),
             (format(round(coef11[7,2],digits=3),nsmall=0)),
             (format(round(coef12[7,2],digits=3),nsmall=0)),
             (format(round(coef13[7,2],digits=3),nsmall=0)),
             (format(round(coef14[7,2],digits=3),nsmall=0)),
             (format(round(coef15[7,2],digits=3),nsmall=0)),
             (format(round(coef16[7,2],digits=3),nsmall=0))),
           c((format(round(coef9[7,4],digits=3),nsmall=0)),
             (format(round(coef11[7,4],digits=3),nsmall=0)),
             (format(round(coef12[7,4],digits=3),nsmall=0)),
             (format(round(coef13[7,4],digits=3),nsmall=0)),
             (format(round(coef14[7,4],digits=3),nsmall=0)),
             (format(round(coef15[7,4],digits=3),nsmall=0)),
             (format(round(coef16[7,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[8]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[8]),digits=3),nsmall=0))),
           c((format(round(coef9[8,2],digits=3),nsmall=0)),
             (format(round(coef11[8,2],digits=3),nsmall=0)),
             (format(round(coef12[8,2],digits=3),nsmall=0)),
             (format(round(coef13[8,2],digits=3),nsmall=0)),
             (format(round(coef14[8,2],digits=3),nsmall=0)),
             (format(round(coef15[8,2],digits=3),nsmall=0)),
             (format(round(coef16[8,2],digits=3),nsmall=0))),
           c((format(round(coef9[8,4],digits=3),nsmall=0)),
             (format(round(coef11[8,4],digits=3),nsmall=0)),
             (format(round(coef12[8,4],digits=3),nsmall=0)),
             (format(round(coef13[8,4],digits=3),nsmall=0)),
             (format(round(coef14[8,4],digits=3),nsmall=0)),
             (format(round(coef15[8,4],digits=3),nsmall=0)),
             (format(round(coef16[8,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[9]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[9]),digits=3),nsmall=0))),
           c((format(round(coef9[9,2],digits=3),nsmall=0)),
             (format(round(coef11[9,2],digits=3),nsmall=0)),
             (format(round(coef12[9,2],digits=3),nsmall=0)),
             (format(round(coef13[9,2],digits=3),nsmall=0)),
             (format(round(coef14[9,2],digits=3),nsmall=0)),
             (format(round(coef15[9,2],digits=3),nsmall=0)),
             (format(round(coef16[9,2],digits=3),nsmall=0))),
           c((format(round(coef9[9,4],digits=3),nsmall=0)),
             (format(round(coef11[9,4],digits=3),nsmall=0)),
             (format(round(coef12[9,4],digits=3),nsmall=0)),
             (format(round(coef13[9,4],digits=3),nsmall=0)),
             (format(round(coef14[9,4],digits=3),nsmall=0)),
             (format(round(coef15[9,4],digits=3),nsmall=0)),
             (format(round(coef16[9,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[10]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[10]),digits=3),nsmall=0))),
           c((format(round(coef9[10,2],digits=3),nsmall=0)),
             (format(round(coef11[10,2],digits=3),nsmall=0)),
             (format(round(coef12[10,2],digits=3),nsmall=0)),
             (format(round(coef13[10,2],digits=3),nsmall=0)),
             (format(round(coef14[10,2],digits=3),nsmall=0)),
             (format(round(coef15[10,2],digits=3),nsmall=0)),
             (format(round(coef16[10,2],digits=3),nsmall=0))),
           c((format(round(coef9[10,4],digits=3),nsmall=0)),
             (format(round(coef11[10,4],digits=3),nsmall=0)),
             (format(round(coef12[10,4],digits=3),nsmall=0)),
             (format(round(coef13[10,4],digits=3),nsmall=0)),
             (format(round(coef14[10,4],digits=3),nsmall=0)),
             (format(round(coef15[10,4],digits=3),nsmall=0)),
             (format(round(coef16[10,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[11]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[11]),digits=3),nsmall=0))),
           c((format(round(coef9[11,2],digits=3),nsmall=0)),
             (format(round(coef11[11,2],digits=3),nsmall=0)),
             (format(round(coef12[11,2],digits=3),nsmall=0)),
             (format(round(coef13[11,2],digits=3),nsmall=0)),
             (format(round(coef14[11,2],digits=3),nsmall=0)),
             (format(round(coef15[11,2],digits=3),nsmall=0)),
             (format(round(coef16[11,2],digits=3),nsmall=0))),
           c((format(round(coef9[11,4],digits=3),nsmall=0)),
             (format(round(coef11[11,4],digits=3),nsmall=0)),
             (format(round(coef12[11,4],digits=3),nsmall=0)),
             (format(round(coef13[11,4],digits=3),nsmall=0)),
             (format(round(coef14[11,4],digits=3),nsmall=0)),
             (format(round(coef15[11,4],digits=3),nsmall=0)),
             (format(round(coef16[11,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[12]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[12]),digits=3),nsmall=0))),
           c((format(round(coef9[12,2],digits=3),nsmall=0)),
             (format(round(coef11[12,2],digits=3),nsmall=0)),
             (format(round(coef12[12,2],digits=3),nsmall=0)),
             (format(round(coef13[12,2],digits=3),nsmall=0)),
             (format(round(coef14[12,2],digits=3),nsmall=0)),
             (format(round(coef15[12,2],digits=3),nsmall=0)),
             (format(round(coef16[12,2],digits=3),nsmall=0))),
           c((format(round(coef9[12,4],digits=3),nsmall=0)),
             (format(round(coef11[12,4],digits=3),nsmall=0)),
             (format(round(coef12[12,4],digits=3),nsmall=0)),
             (format(round(coef13[12,4],digits=3),nsmall=0)),
             (format(round(coef14[12,4],digits=3),nsmall=0)),
             (format(round(coef15[12,4],digits=3),nsmall=0)),
             (format(round(coef16[12,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[13]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[13]),digits=3),nsmall=0))),
           c((format(round(coef9[13,2],digits=3),nsmall=0)),
             (format(round(coef11[13,2],digits=3),nsmall=0)),
             (format(round(coef12[13,2],digits=3),nsmall=0)),
             (format(round(coef13[13,2],digits=3),nsmall=0)),
             (format(round(coef14[13,2],digits=3),nsmall=0)),
             (format(round(coef15[13,2],digits=3),nsmall=0)),
             (format(round(coef16[13,2],digits=3),nsmall=0))),
           c((format(round(coef9[13,4],digits=3),nsmall=0)),
             (format(round(coef11[13,4],digits=3),nsmall=0)),
             (format(round(coef12[13,4],digits=3),nsmall=0)),
             (format(round(coef13[13,4],digits=3),nsmall=0)),
             (format(round(coef14[13,4],digits=3),nsmall=0)),
             (format(round(coef15[13,4],digits=3),nsmall=0)),
             (format(round(coef16[13,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[14]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[14]),digits=3),nsmall=0))),
           c((format(round(coef9[14,2],digits=3),nsmall=0)),
             (format(round(coef11[14,2],digits=3),nsmall=0)),
             (format(round(coef12[14,2],digits=3),nsmall=0)),
             (format(round(coef13[14,2],digits=3),nsmall=0)),
             (format(round(coef14[14,2],digits=3),nsmall=0)),
             (format(round(coef15[14,2],digits=3),nsmall=0)),
             (format(round(coef16[14,2],digits=3),nsmall=0))),
           c((format(round(coef9[14,4],digits=3),nsmall=0)),
             (format(round(coef11[14,4],digits=3),nsmall=0)),
             (format(round(coef12[14,4],digits=3),nsmall=0)),
             (format(round(coef13[14,4],digits=3),nsmall=0)),
             (format(round(coef14[14,4],digits=3),nsmall=0)),
             (format(round(coef15[14,4],digits=3),nsmall=0)),
             (format(round(coef16[14,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[15]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[15]),digits=3),nsmall=0))),
           c((format(round(coef9[15,2],digits=3),nsmall=0)),
             (format(round(coef11[15,2],digits=3),nsmall=0)),
             (format(round(coef12[15,2],digits=3),nsmall=0)),
             (format(round(coef13[15,2],digits=3),nsmall=0)),
             (format(round(coef14[15,2],digits=3),nsmall=0)),
             (format(round(coef15[15,2],digits=3),nsmall=0)),
             (format(round(coef16[15,2],digits=3),nsmall=0))),
           c((format(round(coef9[15,4],digits=3),nsmall=0)),
             (format(round(coef11[15,4],digits=3),nsmall=0)),
             (format(round(coef12[15,4],digits=3),nsmall=0)),
             (format(round(coef13[15,4],digits=3),nsmall=0)),
             (format(round(coef14[15,4],digits=3),nsmall=0)),
             (format(round(coef15[15,4],digits=3),nsmall=0)),
             (format(round(coef16[15,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[16]),digits=3),nsmall=0)),
          #   (format(round(sum(plm10$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[16]),digits=3),nsmall=0))),
          c((format(round(coef9[16,2],digits=3),nsmall=0)),
            (format(round(coef11[16,2],digits=3),nsmall=0)),
            (format(round(coef12[16,2],digits=3),nsmall=0)),
            (format(round(coef13[16,2],digits=3),nsmall=0)),
            (format(round(coef14[16,2],digits=3),nsmall=0)),
            (format(round(coef15[16,2],digits=3),nsmall=0)),
            (format(round(coef16[16,2],digits=3),nsmall=0))),
          c((format(round(coef9[16,4],digits=3),nsmall=0)),
            (format(round(coef11[16,4],digits=3),nsmall=0)),
            (format(round(coef12[16,4],digits=3),nsmall=0)),
            (format(round(coef13[16,4],digits=3),nsmall=0)),
            (format(round(coef14[16,4],digits=3),nsmall=0)),
            (format(round(coef15[16,4],digits=3),nsmall=0)),
            (format(round(coef16[16,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[17]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[17]),digits=3),nsmall=0))),
          c((format(round(coef9[17,2],digits=3),nsmall=0)),
            (format(round(coef11[17,2],digits=3),nsmall=0)),
            (format(round(coef12[17,2],digits=3),nsmall=0)),
            (format(round(coef13[17,2],digits=3),nsmall=0)),
            (format(round(coef14[17,2],digits=3),nsmall=0)),
            (format(round(coef15[17,2],digits=3),nsmall=0)),
            (format(round(coef16[17,2],digits=3),nsmall=0))),
          c((format(round(coef9[17,4],digits=3),nsmall=0)),
            (format(round(coef11[17,4],digits=3),nsmall=0)),
            (format(round(coef12[17,4],digits=3),nsmall=0)),
            (format(round(coef13[17,4],digits=3),nsmall=0)),
            (format(round(coef14[17,4],digits=3),nsmall=0)),
            (format(round(coef15[17,4],digits=3),nsmall=0)),
            (format(round(coef16[17,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[18]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[18]),digits=3),nsmall=0))),
          c((format(round(coef9[18,2],digits=3),nsmall=0)),
            (format(round(coef11[18,2],digits=3),nsmall=0)),
            (format(round(coef12[18,2],digits=3),nsmall=0)),
            (format(round(coef13[18,2],digits=3),nsmall=0)),
            (format(round(coef14[18,2],digits=3),nsmall=0)),
            (format(round(coef15[18,2],digits=3),nsmall=0)),
            (format(round(coef16[18,2],digits=3),nsmall=0))),
          c((format(round(coef9[18,4],digits=3),nsmall=0)),
            (format(round(coef11[18,4],digits=3),nsmall=0)),
            (format(round(coef12[18,4],digits=3),nsmall=0)),
            (format(round(coef13[18,4],digits=3),nsmall=0)),
            (format(round(coef14[18,4],digits=3),nsmall=0)),
            (format(round(coef15[18,4],digits=3),nsmall=0)),
            (format(round(coef16[18,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[19]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[19]),digits=3),nsmall=0))),
          c((format(round(coef9[19,2],digits=3),nsmall=0)),
            (format(round(coef11[19,2],digits=3),nsmall=0)),
            (format(round(coef12[19,2],digits=3),nsmall=0)),
            (format(round(coef13[19,2],digits=3),nsmall=0)),
            (format(round(coef14[19,2],digits=3),nsmall=0)),
            (format(round(coef15[19,2],digits=3),nsmall=0)),
            (format(round(coef16[19,2],digits=3),nsmall=0))),
          c((format(round(coef9[19,4],digits=3),nsmall=0)),
            (format(round(coef11[19,4],digits=3),nsmall=0)),
            (format(round(coef12[19,4],digits=3),nsmall=0)),
            (format(round(coef13[19,4],digits=3),nsmall=0)),
            (format(round(coef14[19,4],digits=3),nsmall=0)),
            (format(round(coef15[19,4],digits=3),nsmall=0)),
            (format(round(coef16[19,4],digits=3),nsmall=0))),

           c((format(round(sum(plm9$coefficients[20]),digits=3),nsmall=0)),
         #    (format(round(sum(plm10$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[20]),digits=3),nsmall=0))),
         c((format(round(coef9[20,2],digits=3),nsmall=0)),
           (format(round(coef11[20,2],digits=3),nsmall=0)),
           (format(round(coef12[20,2],digits=3),nsmall=0)),
           (format(round(coef13[20,2],digits=3),nsmall=0)),
           (format(round(coef14[20,2],digits=3),nsmall=0)),
           (format(round(coef15[20,2],digits=3),nsmall=0)),
           (format(round(coef16[20,2],digits=3),nsmall=0))),
         c((format(round(coef9[20,4],digits=3),nsmall=0)),
           (format(round(coef11[20,4],digits=3),nsmall=0)),
           (format(round(coef12[20,4],digits=3),nsmall=0)),
           (format(round(coef13[20,4],digits=3),nsmall=0)),
           (format(round(coef14[20,4],digits=3),nsmall=0)),
           (format(round(coef15[20,4],digits=3),nsmall=0)),
           (format(round(coef16[20,4],digits=3),nsmall=0))),

           c((format(round(summary(plm9)$r.squared[1],digits=3),nsmall=0)),
           #  (format(round(summary(plm10)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm11)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm12)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm13)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm14)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm15)$r.squared[1],digits=3),nsmall=0)),
             (format(round(summary(plm16)$r.squared[1],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$r.squared[2],digits=3),nsmall=0)),
           #  (format(round(summary(plm10)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm11)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm12)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm13)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm14)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm15)$r.squared[2],digits=3),nsmall=0)),
             (format(round(summary(plm16)$r.squared[2],digits=3),nsmall=0))),
           c((format(round(nobs(plm9),digits=3),nsmall=0)),
            # (format(round(nobs(plm10),digits=3),nsmall=0)),
             (format(round(nobs(plm11),digits=3),nsmall=0)),
             (format(round(nobs(plm12),digits=3),nsmall=0)),
             (format(round(nobs(plm13),digits=3),nsmall=0)),
             (format(round(nobs(plm14),digits=3),nsmall=0)),
             (format(round(nobs(plm15),digits=3),nsmall=0)),
             (format(round(nobs(plm16),digits=3),nsmall=0)))
)





## NON SEED RELATED RATINGS ##

#### regressions without controls - non-seed related ratings

summary(lm(overall_rating~genderdummy +farmer_ID, data = fedata))
summary(lm(general_rating_nonseed~genderdummy +farmer_ID, data = fedata))
summary(lm(location~genderdummy +farmer_ID, data = fedata))
summary(lm(price~genderdummy +farmer_ID, data = fedata))
summary(lm(quality~genderdummy +farmer_ID, data = fedata))
summary(lm(stock ~genderdummy+farmer_ID , data = fedata))
summary(lm(reputation ~genderdummy+farmer_ID , data = fedata))


plmm1<-plm(overall_rating~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im1<- mean(fixef(plmm1))
seplmm1<- sqrt(diag(vcov(plmm1)))

coefm1<-coeftest(plmm1, vcovHC(plmm1, type = "HC0", cluster = "time"))

plmm2<-plm(general_rating_nonseed~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im2<-mean(fixef(plmm2))
seplmm2<- sqrt(diag(vcov(plmm2)))

coefm2<-coeftest(plmm2, vcovHC(plmm2, type = "HC0", cluster = "time"))

plmm3<-plm(location~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im3<-mean(fixef(plmm3))
seplmm3<- sqrt(diag(vcov(plmm3)))

coefm3<-coeftest(plmm3, vcovHC(plmm3, type = "HC0", cluster = "time"))

plmm4<-plm(price~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im4<-mean(fixef(plmm4))
seplmm4<- sqrt(diag(vcov(plmm4)))

coefm4<-coeftest(plmm4, vcovHC(plmm4, type = "HC0", cluster = "time"))

plmm5<-plm(quality~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im5<-mean(fixef(plmm5))
seplmm5<- sqrt(diag(vcov(plmm5)))

coefm5<-coeftest(plmm5, vcovHC(plmm5, type = "HC0", cluster = "time"))

plmm6<-plm(stock~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im6<-mean(fixef(plmm6))
seplmm6<- sqrt(diag(vcov(plmm6)))

coefm6<-coeftest(plmm6, vcovHC(plmm6, type = "HC0", cluster = "time"))

plmm7<-plm(reputation~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im7<-mean(fixef(plmm7))
seplmm7<- sqrt(diag(vcov(plmm7)))

coefm7<-coeftest(plmm7, vcovHC(plmm7, type = "HC0", cluster = "time"))

fe3<- rbind( c((format(round(im1[1],digits=3),nsmall=0)),
               (format(round(im2[1],digits=3),nsmall=0)),
               (format(round(im3[1],digits=3),nsmall=0)),
               (format(round(im4[1],digits=3),nsmall=0)),
               (format(round(im5[1],digits=3),nsmall=0)),
               (format(round(im6[1],digits=3),nsmall=0)),
               (format(round(im7[1],digits=3),nsmall=0))),
             c((format(round(sum(plmm1$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum(plmm2$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum(plmm3$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum(plmm4$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum(plmm5$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum(plmm6$coefficients[1]),digits=3),nsmall=0)),
               (format(round(sum(plmm7$coefficients[1]),digits=3),nsmall=0))),
             c((format(round(coefm1[1,2],digits=3),nsmall=0)),
               (format(round(coefm2[1,2],digits=3),nsmall=0)),
               (format(round(coefm3[1,2],digits=3),nsmall=0)),
               (format(round(coefm4[1,2],digits=3),nsmall=0)),
               (format(round(coefm5[1,2],digits=3),nsmall=0)),
               (format(round(coefm6[1,2],digits=3),nsmall=0)),
               (format(round(coefm7[1,2],digits=3),nsmall=0))),
             c((format(round(coefm1[1,4],digits=3),nsmall=0)),
               (format(round(coefm2[1,4],digits=3),nsmall=0)),
               (format(round(coefm3[1,4],digits=3),nsmall=0)),
               (format(round(coefm4[1,4],digits=3),nsmall=0)),
               (format(round(coefm5[1,4],digits=3),nsmall=0)),
               (format(round(coefm6[1,4],digits=3),nsmall=0)),
               (format(round(coefm7[1,4],digits=3),nsmall=0))),


             c((format(round(summary(plmm1)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plmm2)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plmm3)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plmm4)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plmm5)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plmm6)$r.squared[1],digits=3),nsmall=0)),
               (format(round(summary(plmm7)$r.squared[1],digits=3),nsmall=0))),
             c((format(round(summary(plmm1)$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plmm2)$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plmm3)$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plmm4)$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plmm5)$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plmm6)$r.squared[2],digits=3),nsmall=0)),
               (format(round(summary(plmm7)$r.squared[2],digits=3),nsmall=0))),
             c((format(round(nobs(plmm1),digits=3),nsmall=0)),
               (format(round(nobs(plmm2),digits=3),nsmall=0)),
               (format(round(nobs(plmm3),digits=3),nsmall=0)),
               (format(round(nobs(plmm4),digits=3),nsmall=0)),
               (format(round(nobs(plmm5),digits=3),nsmall=0)),
               (format(round(nobs(plmm6),digits=3),nsmall=0)),
               (format(round(nobs(plmm7),digits=3),nsmall=0)))
)




#### regressions with dealer's gender + dealer characteristics  --- non-seed related ratings

summary(lm(overall_rating~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

plm1<-plm(overall_rating~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
mean(fixef(plm1))
within_intercept(plm1)
#https://stat.ethz.ch/pipermail/r-help/2012-December/344338.html

summary(lm(general_rating_nonseed~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(location~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(price~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(quality~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(stock~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))

summary(lm(reputation~genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof+farmer_ID, data = fedata))


########## plm method

plmm8<-plm(overall_rating~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im8<-mean(fixef(plmm8))
seplmm8<- sqrt(diag(vcov(plmm8)))

coefm8<-coeftest(plmm8, vcovHC(plmm8, type = "HC0", cluster = "time"))

plmm9<-plm(general_rating_nonseed~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im9<-mean(fixef(plmm9))
seplmm9<- sqrt(diag(vcov(plmm9)))

coefm9<-coeftest(plmm9, vcovHC(plmm9, type = "HC0", cluster = "time"))

plmm10<-plm(location~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im10<-mean(fixef(plmm10))
seplmm10<- sqrt(diag(vcov(plmm10)))

coefm10<-coeftest(plmm10, vcovHC(plmm10, type = "HC0", cluster = "time"))


plmm11<-plm(price~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im11<-mean(fixef(plmm11))
seplmm11<- sqrt(diag(vcov(plmm11)))

coefm11<-coeftest(plmm11, vcovHC(plmm11, type = "HC0", cluster = "time"))


plmm12<-plm(quality~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im12<-mean(fixef(plmm12))
seplmm12<- sqrt(diag(vcov(plmm12)))

coefm12<-coeftest(plmm12, vcovHC(plmm12, type = "HC0", cluster = "time"))


plmm13<-plm(stock~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im13<-mean(fixef(plmm13))
#4.332454
seplmm13<- sqrt(diag(vcov(plmm13)))

coefm13<-coeftest(plmm13, vcovHC(plmm13, type = "HC0", cluster = "time"))


plmm14<-plm(reputation~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im14<-mean(fixef(plmm14))
seplmm14<- sqrt(diag(vcov(plmm14)))

coefm14<-coeftest(plmm14, vcovHC(plmm14, type = "HC0", cluster = "time"))




fe4<- rbind(c((format(round(im8[1],digits=3),nsmall=0)),
              (format(round(im9[1],digits=3),nsmall=0)),
              (format(round(im10[1],digits=3),nsmall=0)),
              (format(round(im11[1],digits=3),nsmall=0)),
              (format(round(im12[1],digits=3),nsmall=0)),
              (format(round(im13[1],digits=3),nsmall=0)),
              (format(round(im14[1],digits=3),nsmall=0))),
            c((format(round(sum(plmm8$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[1]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[1]),digits=3),nsmall=0))),
            c((format(round(coefm8[1,2],digits=3),nsmall=0)),
              (format(round(coefm9[1,2],digits=3),nsmall=0)),
              (format(round(coefm10[1,2],digits=3),nsmall=0)),
              (format(round(coefm11[1,2],digits=3),nsmall=0)),
              (format(round(coefm12[1,2],digits=3),nsmall=0)),
              (format(round(coefm13[1,2],digits=3),nsmall=0)),
              (format(round(coefm14[1,2],digits=3),nsmall=0))),
            c((format(round(coefm8[1,4],digits=3),nsmall=0)),
              (format(round(coefm9[1,4],digits=3),nsmall=0)),
              (format(round(coefm10[1,4],digits=3),nsmall=0)),
              (format(round(coefm11[1,4],digits=3),nsmall=0)),
              (format(round(coefm12[1,4],digits=3),nsmall=0)),
              (format(round(coefm13[1,4],digits=3),nsmall=0)),
              (format(round(coefm14[1,4],digits=3),nsmall=0))),

            c((format(round(sum(plmm8$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[2]),digits=3),nsmall=0))),
            c((format(round(coefm8[2,2],digits=3),nsmall=0)),
              (format(round(coefm9[2,2],digits=3),nsmall=0)),
              (format(round(coefm10[2,2],digits=3),nsmall=0)),
              (format(round(coefm11[2,2],digits=3),nsmall=0)),
              (format(round(coefm12[2,2],digits=3),nsmall=0)),
              (format(round(coefm13[2,2],digits=3),nsmall=0)),
              (format(round(coefm14[2,2],digits=3),nsmall=0))),
            c((format(round(coefm8[2,4],digits=3),nsmall=0)),
              (format(round(coefm9[2,4],digits=3),nsmall=0)),
              (format(round(coefm10[2,4],digits=3),nsmall=0)),
              (format(round(coefm11[2,4],digits=3),nsmall=0)),
              (format(round(coefm12[2,4],digits=3),nsmall=0)),
              (format(round(coefm13[2,4],digits=3),nsmall=0)),
              (format(round(coefm14[2,4],digits=3),nsmall=0))),

            c((format(round(sum(plmm8$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[3]),digits=3),nsmall=0))),
            c((format(round(coefm8[3,2],digits=3),nsmall=0)),
              (format(round(coefm9[3,2],digits=3),nsmall=0)),
              (format(round(coefm10[3,2],digits=3),nsmall=0)),
              (format(round(coefm11[3,2],digits=3),nsmall=0)),
              (format(round(coefm12[3,2],digits=3),nsmall=0)),
              (format(round(coefm13[3,2],digits=3),nsmall=0)),
              (format(round(coefm14[3,2],digits=3),nsmall=0))),
            c((format(round(coefm8[3,4],digits=3),nsmall=0)),
              (format(round(coefm9[3,4],digits=3),nsmall=0)),
              (format(round(coefm10[3,4],digits=3),nsmall=0)),
              (format(round(coefm11[3,4],digits=3),nsmall=0)),
              (format(round(coefm12[3,4],digits=3),nsmall=0)),
              (format(round(coefm13[3,4],digits=3),nsmall=0)),
              (format(round(coefm14[3,4],digits=3),nsmall=0))),

            c((format(round(sum(plmm8$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[4]),digits=3),nsmall=0))),
            c((format(round(coefm8[4,2],digits=3),nsmall=0)),
              (format(round(coefm9[4,2],digits=3),nsmall=0)),
              (format(round(coefm10[4,2],digits=3),nsmall=0)),
              (format(round(coefm11[4,2],digits=3),nsmall=0)),
              (format(round(coefm12[4,2],digits=3),nsmall=0)),
              (format(round(coefm13[4,2],digits=3),nsmall=0)),
              (format(round(coefm14[4,2],digits=3),nsmall=0))),
            c((format(round(coefm8[4,4],digits=3),nsmall=0)),
              (format(round(coefm9[4,4],digits=3),nsmall=0)),
              (format(round(coefm10[4,4],digits=3),nsmall=0)),
              (format(round(coefm11[4,4],digits=3),nsmall=0)),
              (format(round(coefm12[4,4],digits=3),nsmall=0)),
              (format(round(coefm13[4,4],digits=3),nsmall=0)),
              (format(round(coefm14[4,4],digits=3),nsmall=0))),

            c((format(round(sum(plmm8$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[5]),digits=3),nsmall=0))),
            c((format(round(coefm8[5,2],digits=3),nsmall=0)),
              (format(round(coefm9[5,2],digits=3),nsmall=0)),
              (format(round(coefm10[5,2],digits=3),nsmall=0)),
              (format(round(coefm11[5,2],digits=3),nsmall=0)),
              (format(round(coefm12[5,2],digits=3),nsmall=0)),
              (format(round(coefm13[5,2],digits=3),nsmall=0)),
              (format(round(coefm14[5,2],digits=3),nsmall=0))),
            c((format(round(coefm8[5,4],digits=3),nsmall=0)),
              (format(round(coefm9[5,4],digits=3),nsmall=0)),
              (format(round(coefm10[5,4],digits=3),nsmall=0)),
              (format(round(coefm11[5,4],digits=3),nsmall=0)),
              (format(round(coefm12[5,4],digits=3),nsmall=0)),
              (format(round(coefm13[5,4],digits=3),nsmall=0)),
              (format(round(coefm14[5,4],digits=3),nsmall=0))),

            c((format(round(sum(plmm8$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[6]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[6]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm14$coefficients[6]),digits=3),nsmall=0))),
            c((format(round(coefm8[6,2],digits=3),nsmall=0)),
              (format(round(coefm9[6,2],digits=3),nsmall=0)),
              0,
              (format(round(coefm11[6,2],digits=3),nsmall=0)),
              (format(round(coefm12[6,2],digits=3),nsmall=0)),
              0,
              (format(round(coefm14[6,2],digits=3),nsmall=0))),
            c((format(round(coefm8[6,4],digits=3),nsmall=0)),
              (format(round(coefm9[6,4],digits=3),nsmall=0)),
              0,
              (format(round(coefm11[6,4],digits=3),nsmall=0)),
              (format(round(coefm12[6,4],digits=3),nsmall=0)),
              0,
              (format(round(coefm14[6,4],digits=3),nsmall=0))),
            
           
            c((format(round(sum(plmm8$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[7]),digits=3),nsmall=0)),
           0,
              (format(round(sum(plmm11$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[7]),digits=3),nsmall=0)),
            0,
              (format(round(sum(plmm14$coefficients[7]),digits=3),nsmall=0))),
           c((format(round(coefm8[7,2],digits=3),nsmall=0)),
             (format(round(coefm9[7,2],digits=3),nsmall=0)),
             0,
             (format(round(coefm11[7,2],digits=3),nsmall=0)),
             (format(round(coefm12[7,2],digits=3),nsmall=0)),
             0,
             (format(round(coefm14[7,2],digits=3),nsmall=0))),
           c((format(round(coefm8[7,4],digits=3),nsmall=0)),
             (format(round(coefm9[7,4],digits=3),nsmall=0)),
             0,
             (format(round(coefm11[7,4],digits=3),nsmall=0)),
             (format(round(coefm12[7,4],digits=3),nsmall=0)),
             0,
             (format(round(coefm14[7,4],digits=3),nsmall=0))),
           

            c((format(round(sum(plmm8$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[8]),digits=3),nsmall=0)),
              0,
              (format(round(sum(plmm11$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[8]),digits=3),nsmall=0)),
         0,
              (format(round(sum(plmm14$coefficients[8]),digits=3),nsmall=0))),
         c((format(round(coefm8[8,2],digits=3),nsmall=0)),
           (format(round(coefm9[8,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[8,2],digits=3),nsmall=0)),
           (format(round(coefm12[8,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[8,2],digits=3),nsmall=0))),
         c((format(round(coefm8[8,4],digits=3),nsmall=0)),
           (format(round(coefm9[8,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[8,4],digits=3),nsmall=0)),
           (format(round(coefm12[8,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[8,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[9]),digits=3),nsmall=0)),
            0,
              (format(round(sum(plmm11$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[9]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm14$coefficients[9]),digits=3),nsmall=0))),
         c((format(round(coefm8[9,2],digits=3),nsmall=0)),
           (format(round(coefm9[9,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[9,2],digits=3),nsmall=0)),
           (format(round(coefm12[9,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[9,2],digits=3),nsmall=0))),
         c((format(round(coefm8[9,4],digits=3),nsmall=0)),
           (format(round(coefm9[9,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[9,4],digits=3),nsmall=0)),
           (format(round(coefm12[9,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[9,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[10]),digits=3),nsmall=0)),
            0,
              (format(round(sum(plmm11$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[10]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm14$coefficients[10]),digits=3),nsmall=0))),
         c((format(round(coefm8[10,2],digits=3),nsmall=0)),
           (format(round(coefm9[10,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[10,2],digits=3),nsmall=0)),
           (format(round(coefm12[10,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[10,2],digits=3),nsmall=0))),
         c((format(round(coefm8[10,4],digits=3),nsmall=0)),
           (format(round(coefm9[10,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[10,4],digits=3),nsmall=0)),
           (format(round(coefm12[10,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[10,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[11]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[11]),digits=3),nsmall=0)),
              0,
              (format(round(sum(plmm14$coefficients[11]),digits=3),nsmall=0))),
         c((format(round(coefm8[11,2],digits=3),nsmall=0)),
           (format(round(coefm9[11,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[11,2],digits=3),nsmall=0)),
           (format(round(coefm12[11,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[11,2],digits=3),nsmall=0))),
         c((format(round(coefm8[11,4],digits=3),nsmall=0)),
           (format(round(coefm9[11,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[11,4],digits=3),nsmall=0)),
           (format(round(coefm12[11,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[11,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[12]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[12]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm14$coefficients[12]),digits=3),nsmall=0))),
         c((format(round(coefm8[12,2],digits=3),nsmall=0)),
           (format(round(coefm9[12,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[12,2],digits=3),nsmall=0)),
           (format(round(coefm12[12,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[12,2],digits=3),nsmall=0))),
         c((format(round(coefm8[12,4],digits=3),nsmall=0)),
           (format(round(coefm9[12,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[12,4],digits=3),nsmall=0)),
           (format(round(coefm12[12,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[12,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[13]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[13]),digits=3),nsmall=0)),
            0,
              (format(round(sum(plmm14$coefficients[13]),digits=3),nsmall=0))),
         c((format(round(coefm8[13,2],digits=3),nsmall=0)),
           (format(round(coefm9[13,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[13,2],digits=3),nsmall=0)),
           (format(round(coefm12[13,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[13,2],digits=3),nsmall=0))),
         c((format(round(coefm8[13,4],digits=3),nsmall=0)),
           (format(round(coefm9[13,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[13,4],digits=3),nsmall=0)),
           (format(round(coefm12[13,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[13,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[14]),digits=3),nsmall=0)),
            0,
              (format(round(sum(plmm11$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[14]),digits=3),nsmall=0)),
              0,
              (format(round(sum(plmm14$coefficients[14]),digits=3),nsmall=0))),
         c((format(round(coefm8[14,2],digits=3),nsmall=0)),
           (format(round(coefm9[14,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[14,2],digits=3),nsmall=0)),
           (format(round(coefm12[14,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[14,2],digits=3),nsmall=0))),
         c((format(round(coefm8[14,4],digits=3),nsmall=0)),
           (format(round(coefm9[14,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[14,4],digits=3),nsmall=0)),
           (format(round(coefm12[14,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[14,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[15]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[15]),digits=3),nsmall=0)),
            0,
              (format(round(sum(plmm14$coefficients[15]),digits=3),nsmall=0))),
         c((format(round(coefm8[15,2],digits=3),nsmall=0)),
           (format(round(coefm9[15,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[15,2],digits=3),nsmall=0)),
           (format(round(coefm12[15,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[15,2],digits=3),nsmall=0))),
         c((format(round(coefm8[15,4],digits=3),nsmall=0)),
           (format(round(coefm9[15,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[15,4],digits=3),nsmall=0)),
           (format(round(coefm12[15,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[15,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[16]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[16]),digits=3),nsmall=0)),
            0,
              (format(round(sum(plmm14$coefficients[16]),digits=3),nsmall=0))),
         c((format(round(coefm8[16,2],digits=3),nsmall=0)),
           (format(round(coefm9[16,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[16,2],digits=3),nsmall=0)),
           (format(round(coefm12[16,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[16,2],digits=3),nsmall=0))),
         c((format(round(coefm8[16,4],digits=3),nsmall=0)),
           (format(round(coefm9[16,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[16,4],digits=3),nsmall=0)),
           (format(round(coefm12[16,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[16,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[17]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[17]),digits=3),nsmall=0)),
           0,
              (format(round(sum(plmm14$coefficients[17]),digits=3),nsmall=0))),
         c((format(round(coefm8[17,2],digits=3),nsmall=0)),
           (format(round(coefm9[17,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[17,2],digits=3),nsmall=0)),
           (format(round(coefm12[17,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[17,2],digits=3),nsmall=0))),
         c((format(round(coefm8[17,4],digits=3),nsmall=0)),
           (format(round(coefm9[17,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[17,4],digits=3),nsmall=0)),
           (format(round(coefm12[17,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[17,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[18]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[18]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm14$coefficients[18]),digits=3),nsmall=0))),
         c((format(round(coefm8[18,2],digits=3),nsmall=0)),
           (format(round(coefm9[18,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[18,2],digits=3),nsmall=0)),
           (format(round(coefm12[18,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[18,2],digits=3),nsmall=0))),
         c((format(round(coefm8[18,4],digits=3),nsmall=0)),
           (format(round(coefm9[18,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[18,4],digits=3),nsmall=0)),
           (format(round(coefm12[18,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[18,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[19]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[19]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm14$coefficients[19]),digits=3),nsmall=0))),
         c((format(round(coefm8[19,2],digits=3),nsmall=0)),
           (format(round(coefm9[19,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[19,2],digits=3),nsmall=0)),
           (format(round(coefm12[19,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[19,2],digits=3),nsmall=0))),
         c((format(round(coefm8[19,4],digits=3),nsmall=0)),
           (format(round(coefm9[19,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[19,4],digits=3),nsmall=0)),
           (format(round(coefm12[19,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[19,4],digits=3),nsmall=0))),
         

            c((format(round(sum(plmm8$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[20]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm11$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[20]),digits=3),nsmall=0)),
             0,
              (format(round(sum(plmm14$coefficients[20]),digits=3),nsmall=0))),
         c((format(round(coefm8[20,2],digits=3),nsmall=0)),
           (format(round(coefm9[20,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[20,2],digits=3),nsmall=0)),
           (format(round(coefm12[20,2],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[20,2],digits=3),nsmall=0))),
         c((format(round(coefm8[20,4],digits=3),nsmall=0)),
           (format(round(coefm9[20,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm11[20,4],digits=3),nsmall=0)),
           (format(round(coefm12[20,4],digits=3),nsmall=0)),
           0,
           (format(round(coefm14[20,4],digits=3),nsmall=0))),
         


            c((format(round(summary(plmm8)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$r.squared[1],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$r.squared[1],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$r.squared[2],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$r.squared[2],digits=3),nsmall=0))),
            c((format(round(nobs(plmm8),digits=3),nsmall=0)),
              (format(round(nobs(plmm9),digits=3),nsmall=0)),
              (format(round(nobs(plmm10),digits=3),nsmall=0)),
              (format(round(nobs(plmm11),digits=3),nsmall=0)),
              (format(round(nobs(plmm12),digits=3),nsmall=0)),
              (format(round(nobs(plmm13),digits=3),nsmall=0)),
              (format(round(nobs(plmm14),digits=3),nsmall=0)))
)



#################################################################################################################
#################################################################################################################
###################                             RANDOM EFFECTS                    ###############################
#################################################################################################################
#################################################################################################################

### SEED RATINGS 


#### One way random effect at the farmer level - no controls 
plmr1<-plm(score ~ genderdummy, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefr1<-coeftest(plmr1, vcovHC(plmr1, type = "HC0", cluster = "time"))
summary(plmr1)
coefr1

plmr2<-plm(general ~ genderdummy, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefr2<-coeftest(plmr2, vcovHC(plmr2, type = "HC0", cluster = "time"))
summary(plmr2)
coefr2

plmr3<-plm(yield ~ genderdummy, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefr3<-coeftest(plmr3, vcovHC(plmr3, type = "HC0", cluster = "time"))
summary(plmr3)
coefr3

plmr4<-plm(drought_resistent ~ genderdummy, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefr4<-coeftest(plmr4, vcovHC(plmr4, type = "HC0", cluster = "time"))
summary(plmr4)
coefr4

plmr5<-plm(disease_resistent ~ genderdummy, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefr5<-coeftest(plmr5, vcovHC(plmr5, type = "HC0", cluster = "time"))
summary(plmr5)
coefr5

plmr6<-plm(early_maturing ~ genderdummy, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefr6<-coeftest(plmr6, vcovHC(plmr6, type = "HC0", cluster = "time"))
summary(plmr6)
coefr6

plmr7<-plm(germination ~ genderdummy, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefr7<-coeftest(plmr7, vcovHC(plmr7, type = "HC0", cluster = "time"))
summary(plmr7)
coefr7

tab_model(plmr1, plmr2,plmr3, plmr4, plmr5, plmr6, plmr7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)

ranr1<- rbind( c((format(round(sum(coefr1[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr2[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr3[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr4[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr5[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr6[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr7[1,1]),digits=3),nsmall=0))),
               c((format(round(coefr1[1,2],digits=3),nsmall=0)),
                 (format(round(coefr2[1,2],digits=3),nsmall=0)),
                 (format(round(coefr3[1,2],digits=3),nsmall=0)),
                 (format(round(coefr4[1,2],digits=3),nsmall=0)),
                 (format(round(coefr5[1,2],digits=3),nsmall=0)),
                 (format(round(coefr6[1,2],digits=3),nsmall=0)),
                 (format(round(coefr7[1,2],digits=3),nsmall=0))),
               c((format(round(coefr1[1,4],digits=3),nsmall=0)),
                 (format(round(coefr2[1,4],digits=3),nsmall=0)),
                 (format(round(coefr3[1,4],digits=3),nsmall=0)),
                 (format(round(coefr4[1,4],digits=3),nsmall=0)),
                 (format(round(coefr5[1,4],digits=3),nsmall=0)),
                 (format(round(coefr6[1,4],digits=3),nsmall=0)),
                 (format(round(coefr7[1,4],digits=3),nsmall=0))),
               
               c((format(round(sum(coefr1[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr2[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr3[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr4[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr5[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr6[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefr7[2,1]),digits=3),nsmall=0))),
               c((format(round(coefr1[2,2],digits=3),nsmall=0)),
                 (format(round(coefr2[2,2],digits=3),nsmall=0)),
                 (format(round(coefr3[2,2],digits=3),nsmall=0)),
                 (format(round(coefr4[2,2],digits=3),nsmall=0)),
                 (format(round(coefr5[2,2],digits=3),nsmall=0)),
                 (format(round(coefr6[2,2],digits=3),nsmall=0)),
                 (format(round(coefr7[2,2],digits=3),nsmall=0))),
               c((format(round(coefr1[2,4],digits=3),nsmall=0)),
                 (format(round(coefr2[2,4],digits=3),nsmall=0)),
                 (format(round(coefr3[2,4],digits=3),nsmall=0)),
                 (format(round(coefr4[2,4],digits=3),nsmall=0)),
                 (format(round(coefr5[2,4],digits=3),nsmall=0)),
                 (format(round(coefr6[2,4],digits=3),nsmall=0)),
                 (format(round(coefr7[2,4],digits=3),nsmall=0))),
               
               
               c((format(round(summary(plmr1)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmr2)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmr3)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmr4)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmr5)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmr6)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmr7)$r.squared[1],digits=3),nsmall=0))),
               c((format(round(summary(plmr1)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmr2)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmr3)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmr4)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmr5)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmr6)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmr7)$r.squared[2],digits=3),nsmall=0))),
               c((format(round(nobs(plmr1),digits=3),nsmall=0)),
                 (format(round(nobs(plmr2),digits=3),nsmall=0)),
                 (format(round(nobs(plmr3),digits=3),nsmall=0)),
                 (format(round(nobs(plmr4),digits=3),nsmall=0)),
                 (format(round(nobs(plmr5),digits=3),nsmall=0)),
                 (format(round(nobs(plmr6),digits=3),nsmall=0)),
                 (format(round(nobs(plmr7),digits=3),nsmall=0)))
)



#### One way random effect at the dealer level - no controls 
#plmr8<-plm(score ~ genderdummy, data =fedata,  model = "random", index = c("shop_ID","farmer_ID"),
     #      effect = "individual")
#coefr8<-coeftest(plmr8, vcovHC(plmr8, type = "HC0", cluster = "time"))
#summary(plmr8)
#coefr8


# #### Two way random effect at the farmer level and dealer level -- no controls 
# 
lmer8<-lmer(score ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) , data = fedata)
lmer9<-lmer(general ~ genderdummy  + (1 | farmer_ID) +(1 | shop_ID), data = fedata)
lmer10<-lmer(yield ~ genderdummy  + (1 | farmer_ID) +(1 | shop_ID), data = fedata)
lmer11<-lmer(drought_resistent ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID), data = fedata)
lmer12<-lmer(disease_resistent ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) , data = fedata)
lmer13<-lmer(early_maturing ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) , data = fedata)
lmer14<-lmer(germination ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) , data = fedata)
# 
# #tab_model(lmer8,lmer9,lmer10,lmer11,lmer12,lmer13,lmer14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# 
# 
# lmer_two1<- rbind( c((format(round(sum(coef(summary(lmer8))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer9))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer10))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer11))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer12))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer13))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer14))[1,1]),digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmer8))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer9))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer10))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer11))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer12))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer13))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer14))[1,2],digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmer8))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer9))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer10))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer11))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer12))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer13))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer14))[1,3],digits=3),nsmall=0))),
# 
#                c((format(round(sum(coef(summary(lmer8))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer9))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer10))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer11))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer12))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer13))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmer14))[2,1]),digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmer8))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer9))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer10))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer11))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer12))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer13))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer14))[2,2],digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmer8))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer9))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer10))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer11))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer12))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer13))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmer14))[2,3],digits=3),nsmall=0))),
# 
# 
#                c((format(round(summary(lmer8)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmer9)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmer10)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmer11)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmer12)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmer13)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmer14)$r.squared[1],digits=3),nsmall=0))),
#                c((format(round(summary(lmer8)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmer9)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmer10)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmer11)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmer12)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmer13)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmer14)$r.squared[2],digits=3),nsmall=0))),
#                c((format(round(nobs(lmer8),digits=3),nsmall=0)),
#                  (format(round(nobs(lmer9),digits=3),nsmall=0)),
#                  (format(round(nobs(lmer10),digits=3),nsmall=0)),
#                  (format(round(nobs(lmer11),digits=3),nsmall=0)),
#                  (format(round(nobs(lmer12),digits=3),nsmall=0)),
#                  (format(round(nobs(lmer13),digits=3),nsmall=0)),
#                  (format(round(nobs(lmer14),digits=3),nsmall=0)))
# )
# 
# #### Two way random effect at the farmer level and dealer level -- with controls -- controls are both farmer and dealer characteristics 
# 
lmerc8<-lmer(score ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
 +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
 badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof  , data = fedata)

lmerc9<-lmer(general ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof  , data = fedata)

lmerc10<-lmer(yield ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
 +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
 badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof  , data = fedata)

lmerc11<-lmer(drought_resistent ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
 +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof  , data = fedata)

lmerc12<-lmer(disease_resistent ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof  , data = fedata)

lmerc13<-lmer(early_maturing ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof  , data = fedata)

lmerc14<-lmer(germination ~ genderdummy  + (1 | farmer_ID)+(1 | shop_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof  , data = fedata)

# #tab_model(lmerc8,lmerc9,lmerc10,lmerc11,lmerc12,lmerc13,lmerc14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# 
# lmer_two2<- rbind( c((format(round(sum(coef(summary(lmerc8))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[1,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[1,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[1,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[2,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[2,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[2,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[3,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[3,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[3,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[4,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[4,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[4,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[5,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[5,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[5,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[6,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[6,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[6,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[7,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[7,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[7,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[8,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[8,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[8,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[9,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[9,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[9,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[10,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[10,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[10,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[11,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[11,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[11,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[12,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[12,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[12,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[13,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[13,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[13,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[14,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[14,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[14,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[15,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[15,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[15,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[16,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[16,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[16,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[17,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[17,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[17,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[18,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[18,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[18,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[19,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[19,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[19,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[20,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[20,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[20,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[21,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[21,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[21,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[22,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[22,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[22,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[23,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[23,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[23,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[24,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[24,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[24,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerc8))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc9))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc10))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc11))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc12))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc13))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerc14))[25,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[25,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerc8))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc9))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc10))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc11))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc12))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc13))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerc14))[25,3],digits=3),nsmall=0))),
# 
# 
#                    c((format(round(summary(lmerc8)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc9)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc10)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc11)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc12)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc13)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc14)$r.squared[1],digits=3),nsmall=0))),
#                    c((format(round(summary(lmerc8)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc9)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc10)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc11)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc12)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc13)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerc14)$r.squared[2],digits=3),nsmall=0))),
#                    c((format(round(nobs(lmerc8),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerc9),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerc10),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerc11),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerc12),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerc13),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerc14),digits=3),nsmall=0)))
# )


#### One way random effect at the farmer level -- with controls --- controls are only dealer characteristics 

plmrc1<-plm(score ~ genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefrc1<-coeftest(plmrc1, vcovHC(plmrc1, type = "HC0", cluster = "time"))
summary(plmrc1)
coefrc1

plmrc2<-plm(general ~ genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefrc2<-coeftest(plmrc2, vcovHC(plmrc2, type = "HC0", cluster = "time"))
summary(plmrc2)
coefrc2

plmrc3<-plm(yield ~ genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefrc3<-coeftest(plmrc3, vcovHC(plmrc3, type = "HC0", cluster = "time"))
summary(plmrc3)
coefrc3

plmrc4<-plm(drought_resistent ~ genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefrc4<-coeftest(plmrc4, vcovHC(plmrc4, type = "HC0", cluster = "time"))
summary(plmrc4)
coefrc4

plmrc5<-plm(disease_resistent ~ genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefrc5<-coeftest(plmrc5, vcovHC(plmrc5, type = "HC0", cluster = "time"))
summary(plmrc5)
coefrc5

plmrc6<-plm(early_maturing ~ genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefrc6<-coeftest(plmrc6, vcovHC(plmrc6, type = "HC0", cluster = "time"))
summary(plmrc6)
coefrc6

plmrc7<-plm(germination ~ genderdummy + maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefrc7<-coeftest(plmrc7, vcovHC(plmrc7, type = "HC0", cluster = "time"))
summary(plmrc7)
coefrc7

tab_model(plmrc1, plmrc2,plmrc3, plmrc4, plmrc5, plmrc6, plmrc7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)

ranrc1<- rbind( c((format(round(sum(coefrc1[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[1,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[1,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[1,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[1,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[1,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[1,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[1,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[1,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[1,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[1,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[1,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[1,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[1,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[1,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[1,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[2,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[2,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[2,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[2,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[2,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[2,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[2,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[2,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[2,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[2,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[2,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[2,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[2,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[2,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[2,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[3,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[3,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[3,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[3,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[3,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[3,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[3,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[3,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[3,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[3,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[3,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[3,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[3,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[3,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[3,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[4,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[4,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[4,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[4,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[4,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[4,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[4,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[4,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[4,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[4,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[4,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[4,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[4,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[4,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[4,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[5,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[5,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[5,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[5,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[5,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[5,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[5,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[5,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[5,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[5,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[5,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[5,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[5,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[5,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[5,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[6,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[6,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[6,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[6,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[6,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[6,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[6,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[6,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[6,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[6,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[6,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[6,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[6,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[6,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[6,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[7,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[7,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[7,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[7,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[7,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[7,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[7,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[7,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[7,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[7,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[7,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[7,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[7,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[7,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[7,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[8,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[8,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[8,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[8,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[8,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[8,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[8,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[8,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[8,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[8,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[8,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[8,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[8,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[8,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[8,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[9,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[9,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[9,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[9,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[9,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[9,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[9,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[9,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[9,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[9,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[9,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[9,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[9,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[9,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[9,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[10,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[10,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[10,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[10,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[10,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[10,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[10,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[10,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[10,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[10,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[10,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[10,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[10,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[10,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[10,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[11,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[11,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[11,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[11,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[11,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[11,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[11,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[11,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[11,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[11,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[11,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[11,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[11,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[11,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[11,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[12,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[12,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[12,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[12,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[12,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[12,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[12,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[12,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[12,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[12,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[12,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[12,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[12,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[12,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[12,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[13,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[13,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[13,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[13,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[13,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[13,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[13,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[13,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[13,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[13,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[13,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[13,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[13,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[13,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[13,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[14,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[14,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[14,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[14,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[14,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[14,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[14,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[14,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[14,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[14,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[14,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[14,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[14,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[14,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[14,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[15,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[15,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[15,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[15,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[15,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[15,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[15,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[15,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[15,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[15,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[15,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[15,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[15,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[15,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[15,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[16,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[16,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[16,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[16,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[16,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[16,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[16,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[16,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[16,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[16,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[16,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[16,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[16,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[16,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[16,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[17,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[17,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[17,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[17,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[17,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[17,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[17,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[17,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[17,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[17,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[17,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[17,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[17,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[17,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[17,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[18,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[18,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[18,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[18,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[18,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[18,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[18,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[18,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[18,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[18,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[18,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[18,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[18,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[18,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[18,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[19,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[19,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[19,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[19,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[19,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[19,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[19,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[19,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[19,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[19,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[19,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[19,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[19,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[19,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[19,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[20,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[20,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[20,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[20,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[20,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[20,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[20,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[20,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[20,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[20,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[20,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[20,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[20,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[20,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[20,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefrc1[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc2[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc3[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc4[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc5[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc6[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefrc7[21,1]),digits=3),nsmall=0))),
                c((format(round(coefrc1[21,2],digits=3),nsmall=0)),
                  (format(round(coefrc2[21,2],digits=3),nsmall=0)),
                  (format(round(coefrc3[21,2],digits=3),nsmall=0)),
                  (format(round(coefrc4[21,2],digits=3),nsmall=0)),
                  (format(round(coefrc5[21,2],digits=3),nsmall=0)),
                  (format(round(coefrc6[21,2],digits=3),nsmall=0)),
                  (format(round(coefrc7[21,2],digits=3),nsmall=0))),
                c((format(round(coefrc1[21,4],digits=3),nsmall=0)),
                  (format(round(coefrc2[21,4],digits=3),nsmall=0)),
                  (format(round(coefrc3[21,4],digits=3),nsmall=0)),
                  (format(round(coefrc4[21,4],digits=3),nsmall=0)),
                  (format(round(coefrc5[21,4],digits=3),nsmall=0)),
                  (format(round(coefrc6[21,4],digits=3),nsmall=0)),
                  (format(round(coefrc7[21,4],digits=3),nsmall=0))),
                
                
                c((format(round(summary(plmrc1)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmrc2)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmrc3)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmrc4)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmrc5)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmrc6)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmrc7)$r.squared[1],digits=3),nsmall=0))),
                c((format(round(summary(plmrc1)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmrc2)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmrc3)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmrc4)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmrc5)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmrc6)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmrc7)$r.squared[2],digits=3),nsmall=0))),
                c((format(round(nobs(plmrc1),digits=3),nsmall=0)),
                  (format(round(nobs(plmrc2),digits=3),nsmall=0)),
                  (format(round(nobs(plmrc3),digits=3),nsmall=0)),
                  (format(round(nobs(plmrc4),digits=3),nsmall=0)),
                  (format(round(nobs(plmrc5),digits=3),nsmall=0)),
                  (format(round(nobs(plmrc6),digits=3),nsmall=0)),
                  (format(round(nobs(plmrc7),digits=3),nsmall=0)))
)



### NON SEED RATINGS

#### One way random effect at the farmer level  -- no controls 

plmn1<-plm(overall_rating ~ genderdummy , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefn1<-coeftest(plmn1, vcovHC(plmn1, type = "HC0", cluster = "time"))
summary(plmn1)
coefn1

plmn2<-plm(general_rating_nonseed ~ genderdummy , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefn2<-coeftest(plmn2, vcovHC(plmn2, type = "HC0", cluster = "time"))
summary(plmn2)
coefn2

plmn3<-plm(location ~ genderdummy , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefn3<-coeftest(plmn3, vcovHC(plmn3, type = "HC0", cluster = "time"))
summary(plmn3)
coefn3

plmn4<-plm(price ~ genderdummy , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefn4<-coeftest(plmn4, vcovHC(plmn4, type = "HC0", cluster = "time"))
summary(plmn4)
coefn4

plmn5<-plm(quality ~ genderdummy , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefn5<-coeftest(plmn5, vcovHC(plmn5, type = "HC0", cluster = "time"))
summary(plmn5)
coefn5

plmn6<-plm(stock ~ genderdummy , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefn6<-coeftest(plmn6, vcovHC(plmn6, type = "HC0", cluster = "time"))
summary(plmn6)
coefn6

plmn7<-plm(reputation ~ genderdummy , data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
           effect = "individual")
coefn7<-coeftest(plmn7, vcovHC(plmn7, type = "HC0", cluster = "time"))
summary(plmn7)
coefn7

tab_model(plmn1, plmn2,plmn3, plmn4, plmn5, plmn6, plmn7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)

rann1<- rbind( c((format(round(sum(coefn1[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn2[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn3[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn4[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn5[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn6[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn7[1,1]),digits=3),nsmall=0))),
               c((format(round(coefn1[1,2],digits=3),nsmall=0)),
                 (format(round(coefn2[1,2],digits=3),nsmall=0)),
                 (format(round(coefn3[1,2],digits=3),nsmall=0)),
                 (format(round(coefn4[1,2],digits=3),nsmall=0)),
                 (format(round(coefn5[1,2],digits=3),nsmall=0)),
                 (format(round(coefn6[1,2],digits=3),nsmall=0)),
                 (format(round(coefn7[1,2],digits=3),nsmall=0))),
               c((format(round(coefn1[1,4],digits=3),nsmall=0)),
                 (format(round(coefn2[1,4],digits=3),nsmall=0)),
                 (format(round(coefn3[1,4],digits=3),nsmall=0)),
                 (format(round(coefn4[1,4],digits=3),nsmall=0)),
                 (format(round(coefn5[1,4],digits=3),nsmall=0)),
                 (format(round(coefn6[1,4],digits=3),nsmall=0)),
                 (format(round(coefn7[1,4],digits=3),nsmall=0))),
               
               c((format(round(sum(coefn1[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn2[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn3[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn4[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn5[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn6[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefn7[2,1]),digits=3),nsmall=0))),
               c((format(round(coefn1[2,2],digits=3),nsmall=0)),
                 (format(round(coefn2[2,2],digits=3),nsmall=0)),
                 (format(round(coefn3[2,2],digits=3),nsmall=0)),
                 (format(round(coefn4[2,2],digits=3),nsmall=0)),
                 (format(round(coefn5[2,2],digits=3),nsmall=0)),
                 (format(round(coefn6[2,2],digits=3),nsmall=0)),
                 (format(round(coefn7[2,2],digits=3),nsmall=0))),
               c((format(round(coefn1[2,4],digits=3),nsmall=0)),
                 (format(round(coefn2[2,4],digits=3),nsmall=0)),
                 (format(round(coefn3[2,4],digits=3),nsmall=0)),
                 (format(round(coefn4[2,4],digits=3),nsmall=0)),
                 (format(round(coefn5[2,4],digits=3),nsmall=0)),
                 (format(round(coefn6[2,4],digits=3),nsmall=0)),
                 (format(round(coefn7[2,4],digits=3),nsmall=0))),
               
               
               c((format(round(summary(plmn1)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmn2)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmn3)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmn4)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmn5)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmn6)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmn7)$r.squared[1],digits=3),nsmall=0))),
               c((format(round(summary(plmn1)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmn2)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmn3)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmn4)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmn5)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmn6)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmn7)$r.squared[2],digits=3),nsmall=0))),
               c((format(round(nobs(plmn1),digits=3),nsmall=0)),
                 (format(round(nobs(plmn2),digits=3),nsmall=0)),
                 (format(round(nobs(plmn3),digits=3),nsmall=0)),
                 (format(round(nobs(plmn4),digits=3),nsmall=0)),
                 (format(round(nobs(plmn5),digits=3),nsmall=0)),
                 (format(round(nobs(plmn6),digits=3),nsmall=0)),
                 (format(round(nobs(plmn7),digits=3),nsmall=0)))
)




# #### two way random effects at the farmer level and dealer level  -- no controls 
# 
lmern8<-lmer(overall_rating ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID) , data = fedata)
lmern9<-lmer(general_rating_nonseed ~ genderdummy  + (1 | farmer_ID) + (1 | shop_ID), data = fedata)
lmern10<-lmer(location ~ genderdummy  + (1 | farmer_ID) + (1 | shop_ID), data = fedata)
lmern11<-lmer(price ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID), data = fedata)
lmern12<-lmer(quality~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID), data = fedata)
lmern13<-lmer(stock ~ genderdummy  + (1 | farmer_ID) + (1 | shop_ID), data = fedata)
lmern14<-lmer(reputation~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID) , data = fedata)

# #tab_model(lmern8,lmern9,lmern10,lmern11,lmern12,lmern13,lmern14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# 
# lmer_two3<- rbind( c((format(round(sum(coef(summary(lmern8))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern9))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern10))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern11))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern12))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern13))[1,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern14))[1,1]),digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmern8))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern9))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern10))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern11))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern12))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern13))[1,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern14))[1,2],digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmern8))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern9))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern10))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern11))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern12))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern13))[1,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern14))[1,3],digits=3),nsmall=0))),
# 
#                c((format(round(sum(coef(summary(lmern8))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern9))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern10))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern11))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern12))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern13))[2,1]),digits=3),nsmall=0)),
#                  (format(round(sum(coef(summary(lmern14))[2,1]),digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmern8))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern9))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern10))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern11))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern12))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern13))[2,2],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern14))[2,2],digits=3),nsmall=0))),
#                c((format(round(coef(summary(lmern8))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern9))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern10))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern11))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern12))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern13))[2,3],digits=3),nsmall=0)),
#                  (format(round(coef(summary(lmern14))[2,3],digits=3),nsmall=0))),
# 
# 
#                c((format(round(summary(lmern8)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmern9)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmern10)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmern11)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmern12)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmern13)$r.squared[1],digits=3),nsmall=0)),
#                  (format(round(summary(lmern14)$r.squared[1],digits=3),nsmall=0))),
#                c((format(round(summary(lmern8)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmern9)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmern10)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmern11)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmern12)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmern13)$r.squared[2],digits=3),nsmall=0)),
#                  (format(round(summary(lmern14)$r.squared[2],digits=3),nsmall=0))),
#                c((format(round(nobs(lmern8),digits=3),nsmall=0)),
#                  (format(round(nobs(lmern9),digits=3),nsmall=0)),
#                  (format(round(nobs(lmern10),digits=3),nsmall=0)),
#                  (format(round(nobs(lmern11),digits=3),nsmall=0)),
#                  (format(round(nobs(lmern12),digits=3),nsmall=0)),
#                  (format(round(nobs(lmern13),digits=3),nsmall=0)),
#                  (format(round(nobs(lmern14),digits=3),nsmall=0)))
# )
# 
# 
# 
# #### two way random effects at the farmer level and dealer level  -- with controls -- controls are farmer and dealer characteristics 
# 
lmernc8<-lmer(overall_rating ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID)  +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata)

lmernc9<-lmer(general_rating_nonseed ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID)  +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata)

lmernc10<-lmer(location ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID)  +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = fedata)

lmernc11<-lmer(price ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID)  +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata)

lmernc12<-lmer(quality ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID)  +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata)

lmernc13<-lmer(stock ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID)  +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = fedata)

lmernc14<-lmer(reputation ~ genderdummy  + (1 | farmer_ID)+ (1 | shop_ID)  +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8  +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata)

# #tab_model(lmernc8,lmernc9,lmernc10,lmernc11,lmernc12,lmernc13,lmernc14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# 
# lmer_two4<- rbind( c((format(round(sum(coef(summary(lmernc8))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[1,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[1,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[1,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[2,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[2,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[2,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[3,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[3,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[3,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[4,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[4,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[4,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[5,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[5,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[5,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[6,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[6,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[6,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[7,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[7,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[7,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[8,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[8,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[8,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[9,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[9,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[9,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc10))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc11))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc13))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc14))[10,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[10,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc10))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc11))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc13))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc14))[10,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[11,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[11,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[11,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[11,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[11,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[11,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[11,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc11))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[11,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[11,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[12,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[12,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmernc14))[12,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[12,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc11))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[12,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[12,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[12,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[12,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[12,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[13,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[13,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmernc14))[13,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[13,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[13,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc14))[13,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[13,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[13,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[13,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[14,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[14,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[14,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[14,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[14,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[14,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[14,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc11))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[14,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[14,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[15,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmernc11))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[15,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmernc14))[15,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[15,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[15,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc14))[15,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[15,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[15,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc14))[15,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[16,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmernc11))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[16,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmernc14))[16,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[16,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[16,2],digits=3),nsmall=0)),
#                       0,
#                      (format(round(coef(summary(lmernc14))[16,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[16,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[16,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[16,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[17,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[17,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[17,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[17,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[17,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[17,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[17,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc11))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[17,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[17,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[18,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[18,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[18,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[18,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[18,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[18,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[18,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[18,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[18,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[19,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[19,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[19,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[19,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[19,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[19,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[19,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmernc11))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[19,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[19,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[20,1]),digits=3),nsmall=0)),
#                     0),
#                      (format(round(sum(coef(summary(lmernc11))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[20,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmernc14))[20,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[20,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[20,2],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmernc14))[20,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[20,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[20,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmernc14))[20,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[21,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[21,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmernc14))[21,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[21,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc11))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[21,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[21,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[21,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[21,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmernc14))[21,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[22,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[22,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[22,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[22,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[22,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[22,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[22,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc11))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[22,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[22,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[23,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc11))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[23,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[23,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[23,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[23,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[23,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[23,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[23,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[23,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[24,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmernc11))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[24,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmernc14))[24,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[24,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmernc11))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[24,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[24,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[24,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[24,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[24,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmernc8))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc9))[25,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmernc11))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmernc12))[25,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmernc14))[25,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[25,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[25,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc14))[25,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmernc8))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc9))[25,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmernc11))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmernc12))[25,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmernc14))[25,3],digits=3),nsmall=0))),
# 
# 
#                    c((format(round(summary(lmernc8)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc9)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc10)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc11)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc12)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc13)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc14)$r.squared[1],digits=3),nsmall=0))),
#                    c((format(round(summary(lmernc8)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc9)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc10)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc11)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc12)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc13)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmernc14)$r.squared[2],digits=3),nsmall=0))),
#                    c((format(round(nobs(lmernc8),digits=3),nsmall=0)),
#                      (format(round(nobs(lmernc9),digits=3),nsmall=0)),
#                      (format(round(nobs(lmernc10),digits=3),nsmall=0)),
#                      (format(round(nobs(lmernc11),digits=3),nsmall=0)),
#                      (format(round(nobs(lmernc12),digits=3),nsmall=0)),
#                      (format(round(nobs(lmernc13),digits=3),nsmall=0)),
#                      (format(round(nobs(lmernc14),digits=3),nsmall=0)))
# )
# 


#### One way random effect at the farmer level  -- with controls -- controls are only dealer characteristics 

plmnc1<-plm(overall_rating ~ genderdummy +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefnc1<-coeftest(plmnc1, vcovHC(plmnc1, type = "HC0", cluster = "time"))
summary(plmnc1)
coefnc1

plmnc2<-plm(general_rating_nonseed ~ genderdummy +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefnc2<-coeftest(plmnc2, vcovHC(plmnc2, type = "HC0", cluster = "time"))
summary(plmnc2)
coefnc2

plmnc3<-plm(location ~ genderdummy +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefnc3<-coeftest(plmnc3, vcovHC(plmnc3, type = "HC0", cluster = "time"))
summary(plmnc3)
coefnc3

plmnc4<-plm(price ~ genderdummy +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefnc4<-coeftest(plmnc4, vcovHC(plmnc4, type = "HC0", cluster = "time"))
summary(plmnc4)
coefnc4

plmnc5<-plm(quality ~ genderdummy +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefnc5<-coeftest(plmnc5, vcovHC(plmnc5, type = "HC0", cluster = "time"))
summary(plmnc5)
coefnc5

plmnc6<-plm(stock ~ genderdummy +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefnc6<-coeftest(plmnc6, vcovHC(plmnc6, type = "HC0", cluster = "time"))
summary(plmnc6)
coefnc6

plmnc7<-plm(reputation ~ genderdummy +maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale
            +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
              badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data =fedata,  model = "random", index = c( "farmer_ID", "shop_ID"),
            effect = "individual")
coefnc7<-coeftest(plmnc7, vcovHC(plmnc7, type = "HC0", cluster = "time"))
summary(plmnc7)
coefnc7

tab_model(plmnc1, plmnc2,plmnc3, plmnc4, plmnc5, plmnc6, plmnc7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)

rannc1<- rbind( c((format(round(sum(coefnc1[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc3[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc4[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc6[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc7[1,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[1,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[1,2],digits=3),nsmall=0)),
                  (format(round(coefnc3[1,2],digits=3),nsmall=0)),
                  (format(round(coefnc4[1,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[1,2],digits=3),nsmall=0)),
                  (format(round(coefnc6[1,2],digits=3),nsmall=0)),
                  (format(round(coefnc7[1,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[1,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[1,4],digits=3),nsmall=0)),
                  (format(round(coefnc3[1,4],digits=3),nsmall=0)),
                  (format(round(coefnc4[1,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[1,4],digits=3),nsmall=0)),
                  (format(round(coefnc6[1,4],digits=3),nsmall=0)),
                  (format(round(coefnc7[1,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc3[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc4[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc6[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc7[2,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[2,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[2,2],digits=3),nsmall=0)),
                  (format(round(coefnc3[2,2],digits=3),nsmall=0)),
                  (format(round(coefnc4[2,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[2,2],digits=3),nsmall=0)),
                  (format(round(coefnc6[2,2],digits=3),nsmall=0)),
                  (format(round(coefnc7[2,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[2,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[2,4],digits=3),nsmall=0)),
                  (format(round(coefnc3[2,4],digits=3),nsmall=0)),
                  (format(round(coefnc4[2,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[2,4],digits=3),nsmall=0)),
                  (format(round(coefnc6[2,4],digits=3),nsmall=0)),
                  (format(round(coefnc7[2,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc3[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc4[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc6[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc7[3,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[3,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[3,2],digits=3),nsmall=0)),
                  (format(round(coefnc3[3,2],digits=3),nsmall=0)),
                  (format(round(coefnc4[3,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[3,2],digits=3),nsmall=0)),
                  (format(round(coefnc6[3,2],digits=3),nsmall=0)),
                  (format(round(coefnc7[3,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[3,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[3,4],digits=3),nsmall=0)),
                  (format(round(coefnc3[3,4],digits=3),nsmall=0)),
                  (format(round(coefnc4[3,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[3,4],digits=3),nsmall=0)),
                  (format(round(coefnc6[3,4],digits=3),nsmall=0)),
                  (format(round(coefnc7[3,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc3[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc4[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc6[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc7[4,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[4,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[4,2],digits=3),nsmall=0)),
                  (format(round(coefnc3[4,2],digits=3),nsmall=0)),
                  (format(round(coefnc4[4,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[4,2],digits=3),nsmall=0)),
                  (format(round(coefnc6[4,2],digits=3),nsmall=0)),
                  (format(round(coefnc7[4,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[4,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[4,4],digits=3),nsmall=0)),
                  (format(round(coefnc3[4,4],digits=3),nsmall=0)),
                  (format(round(coefnc4[4,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[4,4],digits=3),nsmall=0)),
                  (format(round(coefnc6[4,4],digits=3),nsmall=0)),
                  (format(round(coefnc7[4,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc3[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc4[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc6[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc7[5,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[5,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[5,2],digits=3),nsmall=0)),
                  (format(round(coefnc3[5,2],digits=3),nsmall=0)),
                  (format(round(coefnc4[5,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[5,2],digits=3),nsmall=0)),
                  (format(round(coefnc6[5,2],digits=3),nsmall=0)),
                  (format(round(coefnc7[5,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[5,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[5,4],digits=3),nsmall=0)),
                  (format(round(coefnc3[5,4],digits=3),nsmall=0)),
                  (format(round(coefnc4[5,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[5,4],digits=3),nsmall=0)),
                  (format(round(coefnc6[5,4],digits=3),nsmall=0)),
                  (format(round(coefnc7[5,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc3[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc4[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc6[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc7[6,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[6,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[6,2],digits=3),nsmall=0)),
                  (format(round(coefnc3[6,2],digits=3),nsmall=0)),
                  (format(round(coefnc4[6,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[6,2],digits=3),nsmall=0)),
                  (format(round(coefnc6[6,2],digits=3),nsmall=0)),
                  (format(round(coefnc7[6,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[6,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[6,4],digits=3),nsmall=0)),
                  (format(round(coefnc3[6,4],digits=3),nsmall=0)),
                  (format(round(coefnc4[6,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[6,4],digits=3),nsmall=0)),
                  (format(round(coefnc6[6,4],digits=3),nsmall=0)),
                  (format(round(coefnc7[6,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[7,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[7,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[7,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[7,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[7,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[7,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[7,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[7,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[7,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[7,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[7,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[7,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[7,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[7,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[8,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[8,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[8,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[8,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[8,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[8,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[8,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[8,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[8,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[8,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[8,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[8,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[8,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[8,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[9,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[9,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[9,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[9,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[9,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[9,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[9,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[9,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[9,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[9,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[9,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[9,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[9,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[9,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[10,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[10,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[10,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[10,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[10,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[10,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[10,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[10,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[10,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[10,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[10,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[10,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[10,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[10,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[11,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[11,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[11,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[11,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[11,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[11,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[11,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[11,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[11,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[11,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[11,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[11,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[11,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[11,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[12,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[12,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[12,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[12,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[12,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[12,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[12,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[12,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[12,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[12,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[12,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[12,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[12,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[12,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[13,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[13,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[13,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[13,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[13,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[13,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[13,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[13,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[13,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[13,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[13,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[13,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[13,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[13,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[14,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[14,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[14,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[14,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[14,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[14,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[14,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[14,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[14,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[14,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[14,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[14,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[14,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[14,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[15,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[15,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[15,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[15,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[15,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[15,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[15,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[15,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[15,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[15,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[15,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[15,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[15,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[15,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[16,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[16,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[16,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[16,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[16,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[16,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[16,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[16,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[16,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[16,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[16,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[16,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[16,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[16,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[17,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[17,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[17,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[17,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[17,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[17,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[17,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[17,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[17,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[17,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[17,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[17,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[17,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[17,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[18,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[18,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[18,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[18,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[18,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[18,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[18,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[18,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[18,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[18,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[18,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[18,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[18,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[18,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[19,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[19,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[19,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[19,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[19,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[19,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[19,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[19,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[19,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[19,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[19,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[19,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[19,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[19,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[20,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[20,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[20,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[20,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[20,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[20,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[20,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[20,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[20,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[20,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[20,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[20,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[20,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[20,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefnc1[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc2[21,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc4[21,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefnc5[21,1]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(coefnc7[21,1]),digits=3),nsmall=0))),
                c((format(round(coefnc1[21,2],digits=3),nsmall=0)),
                  (format(round(coefnc2[21,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[21,2],digits=3),nsmall=0)),
                  (format(round(coefnc5[21,2],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[21,2],digits=3),nsmall=0))),
                c((format(round(coefnc1[21,4],digits=3),nsmall=0)),
                  (format(round(coefnc2[21,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc4[21,4],digits=3),nsmall=0)),
                  (format(round(coefnc5[21,4],digits=3),nsmall=0)),
                  0,
                  (format(round(coefnc7[21,4],digits=3),nsmall=0))),
                
                
                c((format(round(summary(plmnc1)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmnc2)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmnc3)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmnc4)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmnc5)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmnc6)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmnc7)$r.squared[1],digits=3),nsmall=0))),
                c((format(round(summary(plmnc1)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmnc2)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmnc3)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmnc4)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmnc5)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmnc6)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmnc7)$r.squared[2],digits=3),nsmall=0))),
                c((format(round(nobs(plmnc1),digits=3),nsmall=0)),
                  (format(round(nobs(plmnc2),digits=3),nsmall=0)),
                  (format(round(nobs(plmnc3),digits=3),nsmall=0)),
                  (format(round(nobs(plmnc4),digits=3),nsmall=0)),
                  (format(round(nobs(plmnc5),digits=3),nsmall=0)),
                  (format(round(nobs(plmnc6),digits=3),nsmall=0)),
                  (format(round(nobs(plmnc7),digits=3),nsmall=0)))
)



############################################################################################################################################






##################################################################################################################
####################### BETWEEN DEALER --- FOCUS ON FARMER'S GENDER ##############################################
###  Question 2

rating_dyads <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"))
##Farmers' dataset
farmers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_farmers.csv", sep = "/"))

#how many dealers have been rated (i.e. how many unique shopIDs are in the dyads dataset)?
#print(length(table(rating_dyads$shop_ID)))

#MERGED_DATASET
between_dealer <- merge(rating_dyads, farmers_seed, by="shop_ID")

#create gender dummy
between_dealer$farmergen <- ifelse(between_dealer$Check2.check.maize.q15 == "Male", 1, 0)

###farmer characteristics for controls
between_dealer$educ_f <- 0
between_dealer$educ_f[between_dealer$Check2.check.maize.q17=="c" | between_dealer$Check2.check.maize.q17=="d" | between_dealer$Check2.check.maize.q17=="e" |
                        between_dealer$Check2.check.maize.q17=="f"  ] <- 1 #educated farmers -- finished primary educ
between_dealer$educ_f[ between_dealer$Check2.check.maize.q17=="g" ] <- NA
table(between_dealer$educ_f)

between_dealer$married <- ifelse(between_dealer$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers

#age of farmers
between_dealer$Check2.check.maize.q14[ between_dealer$Check2.check.maize.q14==999 ] <- NA
summary(between_dealer$Check2.check.maize.q14 )

#distance from tarmac road
between_dealer$Check2.check.maize.q8[ between_dealer$Check2.check.maize.q8==999 ] <- NA
summary(between_dealer$Check2.check.maize.q8 )

between_dealer[between_dealer=="n/a"]<- NA

### seed related ratings:

between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x) as.numeric(as.character(x)) )

between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ] <- lapply(between_dealer[c("seed_quality_general_rating","seed_yield_rating","seed_drought_rating","seed_disease_rating","seed_maturing_rating","seed_germinate_rating") ], function(x)replace(x, x == 98,NA) )

between_dealer$quality_rating[between_dealer$shop_ID == "AD_99"]

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

reviews_bd$score_bd <-  rowMeans(reviews_bd[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
reviews_bd$avg_rating <-  rowMeans(reviews_bd[c("quality","general_nonseed","location","price","stock","reputation")],na.rm=T)



###dealer characteristics for  controls
baseline_dealer <- read.csv(paste(path_2,"papers/perceptions/data_seed_systems/data/input_dealer/baseline_dealer.csv", sep="/"), stringsAsFactors = FALSE)

betwd <- merge(reviews_bd, baseline_dealer, by="shop_ID")

###dealer characteristics for  controls

#education of dealers
table(betwd$maize.owner.agree.educ) #5 are g which is Other
betwd$prim <- 0
#betwd$prim[betwd$maize.owner.agree.educ=="c"|betwd$maize.owner.agree.educ=="d"|betwd$maize.owner.agree.educ=="e"|betwd$maize.owner.agree.educ=="f"]<- 1

#finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
betwd$prim[betwd$maize.owner.agree.educ=="e"|betwd$maize.owner.agree.educ=="f"] <- 1
betwd$prim[betwd$maize.owner.agree.educ=="g"]<- NA
table(betwd$prim)
#73 ARE 1 --- 38.8 percent

#age of dealer
summary(betwd$maize.owner.agree.age)
betwd$maize.owner.agree.age[betwd$maize.owner.agree.age==999] <- NA
table(betwd$maize.owner.agree.age)

#distance of shop to nearest tarmac road
table(betwd$maize.owner.agree.q3)
betwd$maize.owner.agree.q3[betwd$maize.owner.agree.q3==999] <- NA
summary(betwd$maize.owner.agree.q3)

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
betwd$pest_prob<- ifelse(betwd$pest== 'No', 1, 0)
table(betwd$pest_prob)

#roof leak proof?
betwd$maize.owner.agree.q72
betwd$roof<-as.character(betwd$maize.owner.agree.temp.q72)
betwd$leakproof<- ifelse(betwd$roof== 'Yes', 1, 0)
table(betwd$leakproof)

#roof insulated?
betwd$maize.owner.agree.q73
betwd$roof_insu<-as.character(betwd$maize.owner.agree.temp.q73)
betwd$roof_insu<- ifelse(betwd$roof_insu== 'Yes', 1, 0)
table(betwd$roof_insu)

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

#Q78. Lighting conditions in area where seed is stored?
betwd$badlighting <- 0
betwd$badlighting[betwd$maize.owner.agree.temp.q78=="2"]<-1
table(betwd$badlighting)

#Q79. On what surface are seed stored?
betwd$badstored <- 0
betwd$badstored[betwd$maize.owner.agree.temp.q79=="3"|betwd$maize.owner.agree.temp.q79=="4"|betwd$maize.owner.agree.temp.q79=="5"]<-1
betwd$badstored[betwd$maize.owner.agree.temp.q79==96]<-NA
table(betwd$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
betwd$maize.owner.agree.q80
betwd$open<-as.character(betwd$maize.owner.agree.temp.q80)
betwd$open_storage<- ifelse(betwd$open== 'No', 1, 0)
table(betwd$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings
#or that the business is registered with some association)
betwd$maize.owner.agree.q81
betwd$cert<-as.character(betwd$maize.owner.agree.temp.q81)
betwd$cert<- ifelse(betwd$cert== 'Yes', 1, 0)
table(betwd$cert)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
betwd$shop_rate<-as.numeric(as.character(betwd$maize.owner.agree.temp.q82))
table(betwd$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
table(betwd$maize.owner.agree.q96)
betwd$complaint<- ifelse(betwd$maize.owner.agree.q96== 'No', 1, 0)
table(betwd$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
table(betwd$maize.owner.agree.q70)
betwd$maize.owner.agree.q70[betwd$maize.owner.agree.q70==999] <- NA
betwd$temp<-betwd$maize.owner.agree.q70


#### regressions without controls - seed related ratings

bd1<-lm(score_bd~gender_avgf , data = betwd)
sebd1<- sqrt(diag(vcov(bd1)))

#bd2<- lm(quality~gender_avgf , data = betwd)
#sebd2<- sqrt(diag(vcov(bd2)))

bd3<- lm(general~gender_avgf, data = betwd)
sebd3<- sqrt(diag(vcov(bd3)))

bd4<- lm(yield~gender_avgf , data = betwd)
sebd4<- sqrt(diag(vcov(bd4)))

bd5<- lm(drought_resistent~gender_avgf , data = betwd)
sebd5<- sqrt(diag(vcov(bd5)))

bd6<- lm(disease_resistent~gender_avgf , data = betwd)
sebd6<- sqrt(diag(vcov(bd6)))

bd7<-lm(early_maturing~gender_avgf , data = betwd)
sebd7<- sqrt(diag(vcov(bd7)))

bd8<-lm(germination~gender_avgf , data = betwd)
sebd8<- sqrt(diag(vcov(bd8)))


s_deal1<- rbind(c((format(round(sum(bd1$coefficients[1]),digits=3),nsmall=0)),
          #   (format(round(sum(bd2$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(bd3$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(bd4$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(bd5$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(bd6$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(bd7$coefficients[1]),digits=3),nsmall=0)),
             (format(round(sum(bd8$coefficients[1]),digits=3),nsmall=0))),
           c((format(round(sebd1[1],digits=3),nsmall=0)),
           #  (format(round(sebd2[1],digits=3),nsmall=0)),
             (format(round(sebd3[1],digits=3),nsmall=0)),
             (format(round(sebd4[1],digits=3),nsmall=0)),
             (format(round(sebd5[1],digits=3),nsmall=0)),
             (format(round(sebd6[1],digits=3),nsmall=0)),
             (format(round(sebd7[1],digits=3),nsmall=0)),
             (format(round(sebd8[1],digits=3),nsmall=0))),
           c((format(round(summary(bd1)$coefficients[1,4],digits=3),nsmall=0)),
            # (format(round(summary(bd2)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(bd3)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(bd4)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(bd5)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(bd6)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(bd7)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(bd8)$coefficients[1,4],digits=3),nsmall=0))),

           c((format(round(sum(bd1$coefficients[2]),digits=3),nsmall=0)),
             #(format(round(sum(bd2$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(bd3$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(bd4$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(bd5$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(bd6$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(bd7$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(bd8$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(sebd1[2],digits=3),nsmall=0)),
             #(format(round(sebd2[2],digits=3),nsmall=0)),
             (format(round(sebd3[2],digits=3),nsmall=0)),
             (format(round(sebd4[2],digits=3),nsmall=0)),
             (format(round(sebd5[2],digits=3),nsmall=0)),
             (format(round(sebd6[2],digits=3),nsmall=0)),
             (format(round(sebd7[2],digits=3),nsmall=0)),
             (format(round(sebd8[2],digits=3),nsmall=0))),
           c((format(round(summary(bd1)$coefficients[2,4],digits=3),nsmall=0)),
             #(format(round(summary(bd2)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(bd3)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(bd4)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(bd5)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(bd6)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(bd7)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(bd8)$coefficients[2,4],digits=3),nsmall=0))),

           c((format(round(summary(bd1)$r.squared,digits=3),nsmall=0)),
             #(format(round(summary(bd2)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd3)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd4)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd5)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd6)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd7)$r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd8)$r.squared,digits=3),nsmall=0))),
           c((format(round(summary(bd1)$adj.r.squared,digits=3),nsmall=0)),
             #(format(round(summary(bd2)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd3)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd4)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd5)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd6)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd7)$adj.r.squared,digits=3),nsmall=0)),
             (format(round(summary(bd8)$adj.r.squared,digits=3),nsmall=0))),
           c((format(round(nobs(bd1),digits=3),nsmall=0)),
             #(format(round(nobs(bd2),digits=3),nsmall=0)),
             (format(round(nobs(bd3),digits=3),nsmall=0)),
             (format(round(nobs(bd4),digits=3),nsmall=0)),
             (format(round(nobs(bd5),digits=3),nsmall=0)),
             (format(round(nobs(bd6),digits=3),nsmall=0)),
             (format(round(nobs(bd7),digits=3),nsmall=0)),
             (format(round(nobs(bd8),digits=3),nsmall=0)))
)



summary(lm(score_bd~gender_avgf , data = betwd))
#summary(lm(quality~gender_avgf , data = betwd))
summary(lm(general~gender_avgf, data = betwd))
summary(lm(yield~gender_avgf , data = betwd))
summary(lm(drought_resistent~gender_avgf , data = betwd))
summary(lm(disease_resistent~gender_avgf , data = betwd))
summary(lm(early_maturing~gender_avgf , data = betwd))
summary(lm(germination~gender_avgf , data = betwd))

#### regressions with farmer's gender (averaged) and dealer characteristics  --- seed related ratings

summary(lm(score_bd~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

#summary(lm(quality~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
 #          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
  #         + complaint + temp+leakproof, data = betwd))

summary(lm(general~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(yield~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(drought_resistent~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(disease_resistent~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(early_maturing~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(germination~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))


#### regressions with farmer's gender (averaged) + dealer characteristics  --- seed related ratings

#bd9<-lm(score_bd~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
 #       +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
  #      + complaint + temp+leakproof, data = betwd)
#sebd9<- sqrt(diag(vcov(bd9)))

#bd10<-lm(quality~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
 #          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
  #         + complaint + temp+leakproof, data = betwd)
#sebd10<- sqrt(diag(vcov(bd10)))

#bd11<- lm(general~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
  #         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
   #        + complaint + temp+leakproof, data = betwd)
#sebd11<- sqrt(diag(vcov(bd11)))

#bd12<-lm(yield~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
#          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
  #         + complaint + temp+leakproof, data = betwd)
#sebd12<- sqrt(diag(vcov(bd12)))

#bd13<-lm(drought_resistent~gender_avgf +  maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
  #         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
   #        + complaint + temp+leakproof, data = betwd)
#sebd13<- sqrt(diag(vcov(bd13)))

#bd14<-lm(disease_resistent~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
  #         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
   #        + complaint + temp+leakproof, data = betwd)
#sebd14<- sqrt(diag(vcov(bd14)))

#bd15<-lm(early_maturing~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
   #        +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
      #     + complaint + temp+leakproof, data = betwd)
#sebd15<- sqrt(diag(vcov(bd15)))

#bd16<-lm(germination~gender_avgf+maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
    #       +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
     #      + complaint + temp+leakproof, data = betwd)
#sebd16<- sqrt(diag(vcov(bd16)))



#### regressions with farmer's gender (averaged) and farmer+dealer characteristics  --- seed related ratings 

bd9<-lm(score_bd~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
        +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
        + complaint + temp+leakproof, data = betwd)
sebd9<- sqrt(diag(vcov(bd9)))

#bd10<-lm(quality~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
#          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
#         + complaint + temp+leakproof, data = betwd)
#sebd10<- sqrt(diag(vcov(bd10)))

bd11<- lm(general~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
          + complaint + temp+leakproof, data = betwd)
sebd11<- sqrt(diag(vcov(bd11)))

bd12<-lm(yield~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
         + complaint + temp+leakproof, data = betwd)
sebd12<- sqrt(diag(vcov(bd12)))

bd13<-lm(drought_resistent~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
         + complaint + temp+leakproof, data = betwd)
sebd13<- sqrt(diag(vcov(bd13)))

bd14<-lm(disease_resistent~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
         + complaint + temp+leakproof, data = betwd)
sebd14<- sqrt(diag(vcov(bd14)))

bd15<-lm(early_maturing~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
         + complaint + temp+leakproof, data = betwd)
sebd15<- sqrt(diag(vcov(bd15)))

bd16<-lm(germination~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
         + complaint + temp+leakproof, data = betwd)
sebd16<- sqrt(diag(vcov(bd16)))


s_deal2<- rbind(c((format(round(sum(bd9$coefficients[1]),digits=3),nsmall=0)),
                  #(format(round(sum(bd10$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[1]),digits=3),nsmall=0))),
                c((format(round(sebd9[1],digits=3),nsmall=0)),
                  #(format(round(sebd10[1],digits=3),nsmall=0)),
                  (format(round(sebd11[1],digits=3),nsmall=0)),
                  (format(round(sebd12[1],digits=3),nsmall=0)),
                  (format(round(sebd13[1],digits=3),nsmall=0)),
                  (format(round(sebd14[1],digits=3),nsmall=0)),
                  (format(round(sebd15[1],digits=3),nsmall=0)),
                  (format(round(sebd16[1],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[1,4],digits=3),nsmall=0)),
                  #(format(round(summary(bd10)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[1,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[2]),digits=3),nsmall=0)),
                  #  (format(round(sum(bd10$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[2]),digits=3),nsmall=0))),
                c((format(round(sebd9[2],digits=3),nsmall=0)),
                  #  (format(round(sebd10[2],digits=3),nsmall=0)),
                  (format(round(sebd11[2],digits=3),nsmall=0)),
                  (format(round(sebd12[2],digits=3),nsmall=0)),
                  (format(round(sebd13[2],digits=3),nsmall=0)),
                  (format(round(sebd14[2],digits=3),nsmall=0)),
                  (format(round(sebd15[2],digits=3),nsmall=0)),
                  (format(round(sebd16[2],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[2,4],digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[2,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[3]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[3]),digits=3),nsmall=0))),
                c((format(round(sebd9[3],digits=3),nsmall=0)),
                  #  (format(round(sebd10[3],digits=3),nsmall=0)),
                  (format(round(sebd11[3],digits=3),nsmall=0)),
                  (format(round(sebd12[3],digits=3),nsmall=0)),
                  (format(round(sebd13[3],digits=3),nsmall=0)),
                  (format(round(sebd14[3],digits=3),nsmall=0)),
                  (format(round(sebd15[3],digits=3),nsmall=0)),
                  (format(round(sebd16[3],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[3,4],digits=3),nsmall=0)),
                  #   (format(round(summary(bd10)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[3,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[4]),digits=3),nsmall=0)),
                  #(format(round(sum(bd10$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[4]),digits=3),nsmall=0))),
                c((format(round(sebd9[4],digits=3),nsmall=0)),
                  #  (format(round(sebd10[4],digits=3),nsmall=0)),
                  (format(round(sebd11[4],digits=3),nsmall=0)),
                  (format(round(sebd12[4],digits=3),nsmall=0)),
                  (format(round(sebd13[4],digits=3),nsmall=0)),
                  (format(round(sebd14[4],digits=3),nsmall=0)),
                  (format(round(sebd15[4],digits=3),nsmall=0)),
                  (format(round(sebd16[4],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[4,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[4,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[5]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[5]),digits=3),nsmall=0))),
                c((format(round(sebd9[5],digits=3),nsmall=0)),
                  #    (format(round(sebd10[5],digits=3),nsmall=0)),
                  (format(round(sebd11[5],digits=3),nsmall=0)),
                  (format(round(sebd12[5],digits=3),nsmall=0)),
                  (format(round(sebd13[5],digits=3),nsmall=0)),
                  (format(round(sebd14[5],digits=3),nsmall=0)),
                  (format(round(sebd15[5],digits=3),nsmall=0)),
                  (format(round(sebd16[5],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[5,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[5,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[6]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[6]),digits=3),nsmall=0))),
                c((format(round(sebd9[6],digits=3),nsmall=0)),
                  #(format(round(sebd10[6],digits=3),nsmall=0)),
                  (format(round(sebd11[6],digits=3),nsmall=0)),
                  (format(round(sebd12[6],digits=3),nsmall=0)),
                  (format(round(sebd13[6],digits=3),nsmall=0)),
                  (format(round(sebd14[6],digits=3),nsmall=0)),
                  (format(round(sebd15[6],digits=3),nsmall=0)),
                  (format(round(sebd16[6],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[6,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[6,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[7]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[7]),digits=3),nsmall=0))),
                c((format(round(sebd9[7],digits=3),nsmall=0)),
                  # (format(round(sebd10[7],digits=3),nsmall=0)),
                  (format(round(sebd11[7],digits=3),nsmall=0)),
                  (format(round(sebd12[7],digits=3),nsmall=0)),
                  (format(round(sebd13[7],digits=3),nsmall=0)),
                  (format(round(sebd14[7],digits=3),nsmall=0)),
                  (format(round(sebd15[7],digits=3),nsmall=0)),
                  (format(round(sebd16[7],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[7,4],digits=3),nsmall=0)),
                  #(format(round(summary(bd10)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[7,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[8]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[8]),digits=3),nsmall=0))),
                c((format(round(sebd9[8],digits=3),nsmall=0)),
                  #  (format(round(sebd10[8],digits=3),nsmall=0)),
                  (format(round(sebd11[8],digits=3),nsmall=0)),
                  (format(round(sebd12[8],digits=3),nsmall=0)),
                  (format(round(sebd13[8],digits=3),nsmall=0)),
                  (format(round(sebd14[8],digits=3),nsmall=0)),
                  (format(round(sebd15[8],digits=3),nsmall=0)),
                  (format(round(sebd16[8],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[8,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[8,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[9]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[9]),digits=3),nsmall=0))),
                c((format(round(sebd9[9],digits=3),nsmall=0)),
                  # (format(round(sebd10[9],digits=3),nsmall=0)),
                  (format(round(sebd11[9],digits=3),nsmall=0)),
                  (format(round(sebd12[9],digits=3),nsmall=0)),
                  (format(round(sebd13[9],digits=3),nsmall=0)),
                  (format(round(sebd14[9],digits=3),nsmall=0)),
                  (format(round(sebd15[9],digits=3),nsmall=0)),
                  (format(round(sebd16[9],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[9,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[9,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[10]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[10]),digits=3),nsmall=0))),
                c((format(round(sebd9[10],digits=3),nsmall=0)),
                  # (format(round(sebd10[10],digits=3),nsmall=0)),
                  (format(round(sebd11[10],digits=3),nsmall=0)),
                  (format(round(sebd12[10],digits=3),nsmall=0)),
                  (format(round(sebd13[10],digits=3),nsmall=0)),
                  (format(round(sebd14[10],digits=3),nsmall=0)),
                  (format(round(sebd15[10],digits=3),nsmall=0)),
                  (format(round(sebd16[10],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[10,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[10,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[11]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[11]),digits=3),nsmall=0))),
                c((format(round(sebd9[11],digits=3),nsmall=0)),
                  # (format(round(sebd10[11],digits=3),nsmall=0)),
                  (format(round(sebd11[11],digits=3),nsmall=0)),
                  (format(round(sebd12[11],digits=3),nsmall=0)),
                  (format(round(sebd13[11],digits=3),nsmall=0)),
                  (format(round(sebd14[11],digits=3),nsmall=0)),
                  (format(round(sebd15[11],digits=3),nsmall=0)),
                  (format(round(sebd16[11],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[11,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[11,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[12]),digits=3),nsmall=0)),
                  #  (format(round(sum(bd10$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[12]),digits=3),nsmall=0))),
                c((format(round(sebd9[12],digits=3),nsmall=0)),
                  #   (format(round(sebd10[12],digits=3),nsmall=0)),
                  (format(round(sebd11[12],digits=3),nsmall=0)),
                  (format(round(sebd12[12],digits=3),nsmall=0)),
                  (format(round(sebd13[12],digits=3),nsmall=0)),
                  (format(round(sebd14[12],digits=3),nsmall=0)),
                  (format(round(sebd15[12],digits=3),nsmall=0)),
                  (format(round(sebd16[12],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[12,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[12,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[13]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[13]),digits=3),nsmall=0))),
                c((format(round(sebd9[13],digits=3),nsmall=0)),
                  # (format(round(sebd10[13],digits=3),nsmall=0)),
                  (format(round(sebd11[13],digits=3),nsmall=0)),
                  (format(round(sebd12[13],digits=3),nsmall=0)),
                  (format(round(sebd13[13],digits=3),nsmall=0)),
                  (format(round(sebd14[13],digits=3),nsmall=0)),
                  (format(round(sebd15[13],digits=3),nsmall=0)),
                  (format(round(sebd16[13],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[13,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[13,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[14]),digits=3),nsmall=0)),
                  #  (format(round(sum(bd10$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[14]),digits=3),nsmall=0))),
                c((format(round(sebd9[14],digits=3),nsmall=0)),
                  #  (format(round(sebd10[14],digits=3),nsmall=0)),
                  (format(round(sebd11[14],digits=3),nsmall=0)),
                  (format(round(sebd12[14],digits=3),nsmall=0)),
                  (format(round(sebd13[14],digits=3),nsmall=0)),
                  (format(round(sebd14[14],digits=3),nsmall=0)),
                  (format(round(sebd15[14],digits=3),nsmall=0)),
                  (format(round(sebd16[14],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[14,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[14,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[15]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[15]),digits=3),nsmall=0))),
                c((format(round(sebd9[15],digits=3),nsmall=0)),
                  #   (format(round(sebd10[15],digits=3),nsmall=0)),
                  (format(round(sebd11[15],digits=3),nsmall=0)),
                  (format(round(sebd12[15],digits=3),nsmall=0)),
                  (format(round(sebd13[15],digits=3),nsmall=0)),
                  (format(round(sebd14[15],digits=3),nsmall=0)),
                  (format(round(sebd15[15],digits=3),nsmall=0)),
                  (format(round(sebd16[15],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[15,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[15,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[16]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[16]),digits=3),nsmall=0))),
                c((format(round(sebd9[16],digits=3),nsmall=0)),
                  #  (format(round(sebd10[16],digits=3),nsmall=0)),
                  (format(round(sebd11[16],digits=3),nsmall=0)),
                  (format(round(sebd12[16],digits=3),nsmall=0)),
                  (format(round(sebd13[16],digits=3),nsmall=0)),
                  (format(round(sebd14[16],digits=3),nsmall=0)),
                  (format(round(sebd15[16],digits=3),nsmall=0)),
                  (format(round(sebd16[16],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[16,4],digits=3),nsmall=0)),
                  #   (format(round(summary(bd10)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[16,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[17]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[17]),digits=3),nsmall=0))),
                c((format(round(sebd9[17],digits=3),nsmall=0)),
                  # (format(round(sebd10[17],digits=3),nsmall=0)),
                  (format(round(sebd11[17],digits=3),nsmall=0)),
                  (format(round(sebd12[17],digits=3),nsmall=0)),
                  (format(round(sebd13[17],digits=3),nsmall=0)),
                  (format(round(sebd14[17],digits=3),nsmall=0)),
                  (format(round(sebd15[17],digits=3),nsmall=0)),
                  (format(round(sebd16[17],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[17,4],digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[17,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[18]),digits=3),nsmall=0)),
                  #  (format(round(sum(bd10$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[18]),digits=3),nsmall=0))),
                c((format(round(sebd9[18],digits=3),nsmall=0)),
                  #  (format(round(sebd10[18],digits=3),nsmall=0)),
                  (format(round(sebd11[18],digits=3),nsmall=0)),
                  (format(round(sebd12[18],digits=3),nsmall=0)),
                  (format(round(sebd13[18],digits=3),nsmall=0)),
                  (format(round(sebd14[18],digits=3),nsmall=0)),
                  (format(round(sebd15[18],digits=3),nsmall=0)),
                  (format(round(sebd16[18],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[18,4],digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[18,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[19]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[19]),digits=3),nsmall=0))),
                c((format(round(sebd9[19],digits=3),nsmall=0)),
                  #  (format(round(sebd10[19],digits=3),nsmall=0)),
                  (format(round(sebd11[19],digits=3),nsmall=0)),
                  (format(round(sebd12[19],digits=3),nsmall=0)),
                  (format(round(sebd13[19],digits=3),nsmall=0)),
                  (format(round(sebd14[19],digits=3),nsmall=0)),
                  (format(round(sebd15[19],digits=3),nsmall=0)),
                  (format(round(sebd16[19],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[19,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[19,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[20]),digits=3),nsmall=0)),
                  #  (format(round(sum(bd10$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[20]),digits=3),nsmall=0))),
                c((format(round(sebd9[20],digits=3),nsmall=0)),
                  # (format(round(sebd10[20],digits=3),nsmall=0)),
                  (format(round(sebd11[20],digits=3),nsmall=0)),
                  (format(round(sebd12[20],digits=3),nsmall=0)),
                  (format(round(sebd13[20],digits=3),nsmall=0)),
                  (format(round(sebd14[20],digits=3),nsmall=0)),
                  (format(round(sebd15[20],digits=3),nsmall=0)),
                  (format(round(sebd16[20],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[20,4],digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[20,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[21]),digits=3),nsmall=0)),
                  # (format(round(sum(bd10$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[21]),digits=3),nsmall=0))),
                c((format(round(sebd9[21],digits=3),nsmall=0)),
                  # (format(round(sebd10[21],digits=3),nsmall=0)),
                  (format(round(sebd11[21],digits=3),nsmall=0)),
                  (format(round(sebd12[21],digits=3),nsmall=0)),
                  (format(round(sebd13[21],digits=3),nsmall=0)),
                  (format(round(sebd14[21],digits=3),nsmall=0)),
                  (format(round(sebd15[21],digits=3),nsmall=0)),
                  (format(round(sebd16[21],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[21,4],digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[21,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[22]),digits=3),nsmall=0)),
                  #  (format(round(sum(bd10$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[22]),digits=3),nsmall=0))),
                c((format(round(sebd9[22],digits=3),nsmall=0)),
                  #  (format(round(sebd10[22],digits=3),nsmall=0)),
                  (format(round(sebd11[22],digits=3),nsmall=0)),
                  (format(round(sebd12[22],digits=3),nsmall=0)),
                  (format(round(sebd13[22],digits=3),nsmall=0)),
                  (format(round(sebd14[22],digits=3),nsmall=0)),
                  (format(round(sebd15[22],digits=3),nsmall=0)),
                  (format(round(sebd16[22],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[22,4],digits=3),nsmall=0)),
                  #   (format(round(summary(bd10)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[22,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[23]),digits=3),nsmall=0)),
                  #  (format(round(sum(bd10$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[23]),digits=3),nsmall=0))),
                c((format(round(sebd9[23],digits=3),nsmall=0)),
                  # (format(round(sebd10[23],digits=3),nsmall=0)),
                  (format(round(sebd11[23],digits=3),nsmall=0)),
                  (format(round(sebd12[23],digits=3),nsmall=0)),
                  (format(round(sebd13[23],digits=3),nsmall=0)),
                  (format(round(sebd14[23],digits=3),nsmall=0)),
                  (format(round(sebd15[23],digits=3),nsmall=0)),
                  (format(round(sebd16[23],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[23,4],digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[23,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[24]),digits=3),nsmall=0)),
                  #    (format(round(sum(bd10$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[24]),digits=3),nsmall=0))),
                c((format(round(sebd9[24],digits=3),nsmall=0)),
                  #   (format(round(sebd10[24],digits=3),nsmall=0)),
                  (format(round(sebd11[24],digits=3),nsmall=0)),
                  (format(round(sebd12[24],digits=3),nsmall=0)),
                  (format(round(sebd13[24],digits=3),nsmall=0)),
                  (format(round(sebd14[24],digits=3),nsmall=0)),
                  (format(round(sebd15[24],digits=3),nsmall=0)),
                  (format(round(sebd16[24],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[24,4],digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[24,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bd9$coefficients[25]),digits=3),nsmall=0)),
                  #    (format(round(sum(bd10$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bd11$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bd12$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bd13$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bd14$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bd15$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bd16$coefficients[25]),digits=3),nsmall=0))),
                c((format(round(sebd9[25],digits=3),nsmall=0)),
                  #  (format(round(sebd10[25],digits=3),nsmall=0)),
                  (format(round(sebd11[25],digits=3),nsmall=0)),
                  (format(round(sebd12[25],digits=3),nsmall=0)),
                  (format(round(sebd13[25],digits=3),nsmall=0)),
                  (format(round(sebd14[25],digits=3),nsmall=0)),
                  (format(round(sebd15[25],digits=3),nsmall=0)),
                  (format(round(sebd16[25],digits=3),nsmall=0))),
                c((format(round(summary(bd9)$coefficients[25,4],digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bd11)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bd12)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bd13)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bd14)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bd15)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bd16)$coefficients[25,4],digits=3),nsmall=0))),
                
                
                c((format(round(summary(bd9)$r.squared,digits=3),nsmall=0)),
                  #  (format(round(summary(bd10)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd11)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd12)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd13)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd14)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd15)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd16)$r.squared,digits=3),nsmall=0))),
                c((format(round(summary(bd9)$adj.r.squared,digits=3),nsmall=0)),
                  # (format(round(summary(bd10)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd11)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd12)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd13)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd14)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd15)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bd16)$adj.r.squared,digits=3),nsmall=0))),
                c((format(round(nobs(bd9),digits=3),nsmall=0)),
                  #  (format(round(nobs(bd10),digits=3),nsmall=0)),
                  (format(round(nobs(bd11),digits=3),nsmall=0)),
                  (format(round(nobs(bd12),digits=3),nsmall=0)),
                  (format(round(nobs(bd13),digits=3),nsmall=0)),
                  (format(round(nobs(bd14),digits=3),nsmall=0)),
                  (format(round(nobs(bd15),digits=3),nsmall=0)),
                  (format(round(nobs(bd16),digits=3),nsmall=0)))
)


#NON-SEED RELATED RATINGS
##########################################


#### regressions without controls - non-seed related ratings
bdn1<-lm(avg_rating~gender_avgf , data = betwd)
sebdn1<- sqrt(diag(vcov(bdn1)))

bdn2<-lm(general_nonseed~gender_avgf, data = betwd)
sebdn2<- sqrt(diag(vcov(bdn2)))

bdn3<-lm(location~gender_avgf , data = betwd)
sebdn3<- sqrt(diag(vcov(bdn3)))

bdn4<-lm(price~gender_avgf , data = betwd)
sebdn4<- sqrt(diag(vcov(bdn4)))

bdn5<-lm(quality~gender_avgf , data = betwd)
sebdn5<- sqrt(diag(vcov(bdn5)))

bdn6<-lm(stock~gender_avgf , data = betwd)
sebdn6<- sqrt(diag(vcov(bdn6)))

bdn7<-lm(reputation~gender_avgf , data = betwd)
sebdn7<- sqrt(diag(vcov(bdn7)))






s_deal3<- rbind(c((format(round(sum(bdn1$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn2$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn3$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn4$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn5$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn6$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn7$coefficients[1]),digits=3),nsmall=0))),
                c((format(round(sebdn1[1],digits=3),nsmall=0)),
                  (format(round(sebdn2[1],digits=3),nsmall=0)),
                  (format(round(sebdn3[1],digits=3),nsmall=0)),
                  (format(round(sebdn4[1],digits=3),nsmall=0)),
                  (format(round(sebdn5[1],digits=3),nsmall=0)),
                  (format(round(sebdn6[1],digits=3),nsmall=0)),
                  (format(round(sebdn7[1],digits=3),nsmall=0))),
                c((format(round(summary(bdn1)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn2)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn3)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn4)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn5)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn6)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn7)$coefficients[1,4],digits=3),nsmall=0))),
                c((format(round(sum(bdn1$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn2$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn3$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn4$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn5$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn6$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn7$coefficients[2]),digits=3),nsmall=0))),
                c((format(round(sebdn1[2],digits=3),nsmall=0)),
                  (format(round(sebdn2[2],digits=3),nsmall=0)),
                  (format(round(sebdn3[2],digits=3),nsmall=0)),
                  (format(round(sebdn4[2],digits=3),nsmall=0)),
                  (format(round(sebdn5[2],digits=3),nsmall=0)),
                  (format(round(sebdn6[2],digits=3),nsmall=0)),
                  (format(round(sebdn7[2],digits=3),nsmall=0))),
                c((format(round(summary(bdn1)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn2)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn3)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn4)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn5)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn6)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn7)$coefficients[2,4],digits=3),nsmall=0))),

                c((format(round(summary(bdn1)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn2)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn3)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn4)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn5)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn6)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn7)$r.squared,digits=3),nsmall=0))),
                c((format(round(summary(bdn1)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn2)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn3)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn4)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn5)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn6)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn7)$adj.r.squared,digits=3),nsmall=0))),
                c((format(round(nobs(bdn1),digits=3),nsmall=0)),
                  (format(round(nobs(bdn2),digits=3),nsmall=0)),
                  (format(round(nobs(bdn3),digits=3),nsmall=0)),
                  (format(round(nobs(bdn4),digits=3),nsmall=0)),
                  (format(round(nobs(bdn5),digits=3),nsmall=0)),
                  (format(round(nobs(bdn6),digits=3),nsmall=0)),
                  (format(round(nobs(bdn7),digits=3),nsmall=0)))
)


summary(lm(avg_rating~gender_avgf , data = betwd))
summary(lm(general_nonseed~gender_avgf, data = betwd))
summary(lm(location~gender_avgf , data = betwd))
summary(lm(price~gender_avgf , data = betwd))
summary(lm(quality~gender_avgf , data = betwd))
summary(lm(stock~gender_avgf , data = betwd))
summary(lm(reputation~gender_avgf , data = betwd))


#### regressions with farmer's gender (averaged) and dealer characteristics  --- non-seed related ratings

summary(lm(avg_rating~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(general_nonseed~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(location~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(price~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(quality~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(stock~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))

summary(lm(reputation~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
           +roof_insu+ wall_heatproof +ventilation+badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd))


#### regressions with farmer's gender (averaged) +dealer characteristics  --- non-seed related ratings

#bdn9<-lm(avg_rating~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
  #         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
    #       + complaint + temp+leakproof, data = betwd)
#sebdn9<- sqrt(diag(vcov(bdn9)))

#bdn10<-lm(general_nonseed~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
      #     +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
       #    + complaint + temp+leakproof, data = betwd)
#sebdn10<- sqrt(diag(vcov(bdn10)))

#bdn11<-lm(location~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4, data = betwd)
#sebdn11<- sqrt(diag(vcov(bdn11)))

#bdn12<-lm(price~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
     #      +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
      #     + complaint + temp+leakproof, data = betwd)
#sebdn12<- sqrt(diag(vcov(bdn12)))

#bdn13<-lm(quality~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
       #    +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
       #    + complaint + temp+leakproof, data = betwd)
#sebdn13<- sqrt(diag(vcov(bdn13)))

#bdn14<-lm(stock~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4, data = betwd)
#sebdn14<- sqrt(diag(vcov(bdn14)))

#bdn15<-lm(reputation~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob
       #    +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
       #    + complaint + temp+leakproof, data = betwd)
#sebdn15<- sqrt(diag(vcov(bdn15)))


#### regressions with farmer's gender (averaged) and farmer+dealer characteristics  --- non-seed related ratings 

bdn9<-lm(avg_rating~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
         +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
         + complaint + temp+leakproof, data = betwd)
sebdn9<- sqrt(diag(vcov(bdn9)))

bdn10<-lm(general_nonseed~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
          + complaint + temp+leakproof, data = betwd)
sebdn10<- sqrt(diag(vcov(bdn10)))

bdn11<-lm(location~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
          + complaint + temp+leakproof, data = betwd)
sebdn11<- sqrt(diag(vcov(bdn11)))

bdn12<-lm(price~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
          + complaint + temp+leakproof, data = betwd)
sebdn12<- sqrt(diag(vcov(bdn12)))

bdn13<-lm(quality~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
          + complaint + temp+leakproof, data = betwd)
sebdn13<- sqrt(diag(vcov(bdn13)))

bdn14<-lm(stock~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
          + complaint + temp+leakproof, data = betwd)
sebdn14<- sqrt(diag(vcov(bdn14)))

bdn15<-lm(reputation~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
          + complaint + temp+leakproof, data = betwd)
sebdn15<- sqrt(diag(vcov(bdn15)))


s_deal4<- rbind(c((format(round(sum(bdn9$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[1]),digits=3),nsmall=0))),
                c((format(round(sebdn9[1],digits=3),nsmall=0)),
                  (format(round(sebdn10[1],digits=3),nsmall=0)),
                  (format(round(sebdn11[1],digits=3),nsmall=0)),
                  (format(round(sebdn12[1],digits=3),nsmall=0)),
                  (format(round(sebdn13[1],digits=3),nsmall=0)),
                  (format(round(sebdn14[1],digits=3),nsmall=0)),
                  (format(round(sebdn15[1],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[1,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[2]),digits=3),nsmall=0))),
                c((format(round(sebdn9[2],digits=3),nsmall=0)),
                  (format(round(sebdn10[2],digits=3),nsmall=0)),
                  (format(round(sebdn11[2],digits=3),nsmall=0)),
                  (format(round(sebdn12[2],digits=3),nsmall=0)),
                  (format(round(sebdn13[2],digits=3),nsmall=0)),
                  (format(round(sebdn14[2],digits=3),nsmall=0)),
                  (format(round(sebdn15[2],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[2,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[3]),digits=3),nsmall=0))),
                c((format(round(sebdn9[3],digits=3),nsmall=0)),
                  (format(round(sebdn10[3],digits=3),nsmall=0)),
                  (format(round(sebdn11[3],digits=3),nsmall=0)),
                  (format(round(sebdn12[3],digits=3),nsmall=0)),
                  (format(round(sebdn13[3],digits=3),nsmall=0)),
                  (format(round(sebdn14[3],digits=3),nsmall=0)),
                  (format(round(sebdn15[3],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[3,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[4]),digits=3),nsmall=0))),
                c((format(round(sebdn9[4],digits=3),nsmall=0)),
                  (format(round(sebdn10[4],digits=3),nsmall=0)),
                  (format(round(sebdn11[4],digits=3),nsmall=0)),
                  (format(round(sebdn12[4],digits=3),nsmall=0)),
                  (format(round(sebdn13[4],digits=3),nsmall=0)),
                  (format(round(sebdn14[4],digits=3),nsmall=0)),
                  (format(round(sebdn15[4],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[4,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[5]),digits=3),nsmall=0))),
                c((format(round(sebdn9[5],digits=3),nsmall=0)),
                  (format(round(sebdn10[5],digits=3),nsmall=0)),
                  (format(round(sebdn11[5],digits=3),nsmall=0)),
                  (format(round(sebdn12[5],digits=3),nsmall=0)),
                  (format(round(sebdn13[5],digits=3),nsmall=0)),
                  (format(round(sebdn14[5],digits=3),nsmall=0)),
                  (format(round(sebdn15[5],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[5,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[6]),digits=3),nsmall=0))),
                c((format(round(sebdn9[6],digits=3),nsmall=0)),
                  (format(round(sebdn10[6],digits=3),nsmall=0)),
                  (format(round(sebdn11[6],digits=3),nsmall=0)),
                  (format(round(sebdn12[6],digits=3),nsmall=0)),
                  (format(round(sebdn13[6],digits=3),nsmall=0)),
                  (format(round(sebdn14[6],digits=3),nsmall=0)),
                  (format(round(sebdn15[6],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[6,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[7]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[7]),digits=3),nsmall=0))),
                c((format(round(sebdn9[7],digits=3),nsmall=0)),
                  (format(round(sebdn10[7],digits=3),nsmall=0)),
                  (format(round(sebdn11[7],digits=3),nsmall=0)),
                  (format(round(sebdn12[7],digits=3),nsmall=0)),
                  (format(round(sebdn13[7],digits=3),nsmall=0)),
                  (format(round(sebdn14[7],digits=3),nsmall=0)),
                  (format(round(sebdn15[7],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[7,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[7,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[8]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[8]),digits=3),nsmall=0))),
                c((format(round(sebdn9[8],digits=3),nsmall=0)),
                  (format(round(sebdn10[8],digits=3),nsmall=0)),
                  (format(round(sebdn11[8],digits=3),nsmall=0)),
                  (format(round(sebdn12[8],digits=3),nsmall=0)),
                  (format(round(sebdn13[8],digits=3),nsmall=0)),
                  (format(round(sebdn14[8],digits=3),nsmall=0)),
                  (format(round(sebdn15[8],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[8,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[8,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[9]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[9]),digits=3),nsmall=0))),
                c((format(round(sebdn9[9],digits=3),nsmall=0)),
                  (format(round(sebdn10[9],digits=3),nsmall=0)),
                  (format(round(sebdn11[9],digits=3),nsmall=0)),
                  (format(round(sebdn12[9],digits=3),nsmall=0)),
                  (format(round(sebdn13[9],digits=3),nsmall=0)),
                  (format(round(sebdn14[9],digits=3),nsmall=0)),
                  (format(round(sebdn15[9],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[9,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[9,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[10]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[10]),digits=3),nsmall=0))),
                c((format(round(sebdn9[10],digits=3),nsmall=0)),
                  (format(round(sebdn10[10],digits=3),nsmall=0)),
                  (format(round(sebdn11[10],digits=3),nsmall=0)),
                  (format(round(sebdn12[10],digits=3),nsmall=0)),
                  (format(round(sebdn13[10],digits=3),nsmall=0)),
                  (format(round(sebdn14[10],digits=3),nsmall=0)),
                  (format(round(sebdn15[10],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[10,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[10,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[11]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[11]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[11]),digits=3),nsmall=0))),
                c((format(round(sebdn9[11],digits=3),nsmall=0)),
                  (format(round(sebdn10[11],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[11],digits=3),nsmall=0)),
                  (format(round(sebdn13[11],digits=3),nsmall=0)),
                0,
                  (format(round(sebdn15[11],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[11,4],digits=3),nsmall=0)),
                0,
                  (format(round(summary(bdn12)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[11,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[11,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[12]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[12]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[12]),digits=3),nsmall=0))),
                c((format(round(sebdn9[12],digits=3),nsmall=0)),
                  (format(round(sebdn10[12],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn12[12],digits=3),nsmall=0)),
                  (format(round(sebdn13[12],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[12],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[12,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[12,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[12,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[13]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[13]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[13]),digits=3),nsmall=0))),
                c((format(round(sebdn9[13],digits=3),nsmall=0)),
                  (format(round(sebdn10[13],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[13],digits=3),nsmall=0)),
                  (format(round(sebdn13[13],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[13],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[13,4],digits=3),nsmall=0)),
                0,
                  (format(round(summary(bdn12)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[13,4],digits=3),nsmall=0)),
                0,
                  (format(round(summary(bdn15)$coefficients[13,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[14]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[14]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[14]),digits=3),nsmall=0))),
                c((format(round(sebdn9[14],digits=3),nsmall=0)),
                  (format(round(sebdn10[14],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn12[14],digits=3),nsmall=0)),
                  (format(round(sebdn13[14],digits=3),nsmall=0)),
                0,
                  (format(round(sebdn15[14],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[14,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[14,4],digits=3),nsmall=0)),
                  0,
                  (format(round(summary(bdn15)$coefficients[14,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[15]),digits=3),nsmall=0)),
                0,
                  (format(round(sum(bdn12$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[15]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(bdn15$coefficients[15]),digits=3),nsmall=0))),
                c((format(round(sebdn9[15],digits=3),nsmall=0)),
                  (format(round(sebdn10[15],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[15],digits=3),nsmall=0)),
                  (format(round(sebdn13[15],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[15],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[15,4],digits=3),nsmall=0)),
                0,
                  (format(round(summary(bdn12)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[15,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[15,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[16]),digits=3),nsmall=0)),
                0,
                  (format(round(sum(bdn12$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[16]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(bdn15$coefficients[16]),digits=3),nsmall=0))),
                c((format(round(sebdn9[16],digits=3),nsmall=0)),
                  (format(round(sebdn10[16],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[16],digits=3),nsmall=0)),
                  (format(round(sebdn13[16],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn15[16],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[16,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[16,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[16,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[17]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[17]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[17]),digits=3),nsmall=0))),
                c((format(round(sebdn9[17],digits=3),nsmall=0)),
                  (format(round(sebdn10[17],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[17],digits=3),nsmall=0)),
                  (format(round(sebdn13[17],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[17],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[17,4],digits=3),nsmall=0)),
                0,
                  (format(round(summary(bdn12)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[17,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[17,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[18]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[18]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(bdn15$coefficients[18]),digits=3),nsmall=0))),
                c((format(round(sebdn9[18],digits=3),nsmall=0)),
                  (format(round(sebdn10[18],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn12[18],digits=3),nsmall=0)),
                  (format(round(sebdn13[18],digits=3),nsmall=0)),
              0,
                  (format(round(sebdn15[18],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[18,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[18,4],digits=3),nsmall=0)),
                0,
                  (format(round(summary(bdn15)$coefficients[18,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[19]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[19]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[19]),digits=3),nsmall=0))),
                c((format(round(sebdn9[19],digits=3),nsmall=0)),
                  (format(round(sebdn10[19],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[19],digits=3),nsmall=0)),
                  (format(round(sebdn13[19],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[19],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[19,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[19,4],digits=3),nsmall=0)),
                0,
                  (format(round(summary(bdn15)$coefficients[19,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[20]),digits=3),nsmall=0)),
               0,
                  (format(round(sum(bdn12$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[20]),digits=3),nsmall=0)),
                0,
                  (format(round(sum(bdn15$coefficients[20]),digits=3),nsmall=0))),
                c((format(round(sebdn9[20],digits=3),nsmall=0)),
                  (format(round(sebdn10[20],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[20],digits=3),nsmall=0)),
                  (format(round(sebdn13[20],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[20],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[20,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[20,4],digits=3),nsmall=0)),
                  0,
                  (format(round(summary(bdn15)$coefficients[20,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[21]),digits=3),nsmall=0)),
                0,
                  (format(round(sum(bdn12$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[21]),digits=3),nsmall=0)),
                0,
                  (format(round(sum(bdn15$coefficients[21]),digits=3),nsmall=0))),
                c((format(round(sebdn9[21],digits=3),nsmall=0)),
                  (format(round(sebdn10[21],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn12[21],digits=3),nsmall=0)),
                  (format(round(sebdn13[21],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[21],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[21,4],digits=3),nsmall=0)),
                  0,
                  (format(round(summary(bdn12)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[21,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[21,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[22]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[22]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[22]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[22]),digits=3),nsmall=0))),
                c((format(round(sebdn9[22],digits=3),nsmall=0)),
                  (format(round(sebdn10[22],digits=3),nsmall=0)),
                0,
                  (format(round(sebdn12[22],digits=3),nsmall=0)),
                  (format(round(sebdn13[22],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn15[22],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[22,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[22,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[22,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[22,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[23]),digits=3),nsmall=0)),
                  0,
                  (format(round(sum(bdn12$coefficients[23]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[23]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[23]),digits=3),nsmall=0))),
                c((format(round(sebdn9[23],digits=3),nsmall=0)),
                  (format(round(sebdn10[23],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn12[23],digits=3),nsmall=0)),
                  (format(round(sebdn13[23],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[23],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[23,4],digits=3),nsmall=0)),
                  0,
                  (format(round(summary(bdn12)$coefficients[23,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[23,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[23,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[24]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[24]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[24]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[24]),digits=3),nsmall=0))),
                c((format(round(sebdn9[24],digits=3),nsmall=0)),
                  (format(round(sebdn10[24],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn12[24],digits=3),nsmall=0)),
                  (format(round(sebdn13[24],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn15[24],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[24,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[24,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[24,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[24,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[25]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn12$coefficients[25]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[25]),digits=3),nsmall=0)),
                 0,
                  (format(round(sum(bdn15$coefficients[25]),digits=3),nsmall=0))),
                c((format(round(sebdn9[25],digits=3),nsmall=0)),
                  (format(round(sebdn10[25],digits=3),nsmall=0)),
                 0,
                  (format(round(sebdn12[25],digits=3),nsmall=0)),
                  (format(round(sebdn13[25],digits=3),nsmall=0)),
                  0,
                  (format(round(sebdn15[25],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[25,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn12)$coefficients[25,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[25,4],digits=3),nsmall=0)),
                 0,
                  (format(round(summary(bdn15)$coefficients[25,4],digits=3),nsmall=0))),
                
                
                c((format(round(summary(bdn9)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$r.squared,digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$adj.r.squared,digits=3),nsmall=0))),
                c((format(round(nobs(bdn9),digits=3),nsmall=0)),
                  (format(round(nobs(bdn10),digits=3),nsmall=0)),
                  (format(round(nobs(bdn11),digits=3),nsmall=0)),
                  (format(round(nobs(bdn12),digits=3),nsmall=0)),
                  (format(round(nobs(bdn13),digits=3),nsmall=0)),
                  (format(round(nobs(bdn14),digits=3),nsmall=0)),
                  (format(round(nobs(bdn15),digits=3),nsmall=0))))



################### BETWEEN FARMERS MODEL (CARO'S APPROACH) --- FOCUS ON FARMER'S GENDER #########################

avg_q <- data.frame(cbind(tapply(as.numeric(between_dealer$quality_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$seed_quality_general_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$seed_yield_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$seed_drought_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$seed_disease_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$seed_maturing_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$seed_germinate_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$general_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$location_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$price_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$stock_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(as.numeric(between_dealer$reputation_rating), between_dealer$farmer_ID.x,mean,na.rm=TRUE),
                           tapply(between_dealer$bought_at_dealer=="Yes" | between_dealer$knows_other_customer=="Yes", between_dealer$farmer_ID.x,sum)))

names(avg_q) <- c("quality","general","yield","drought_resistent","disease_resistent","early_maturing","germination","general_rating_nonseed","location","price",
                "stock","reputation","nr_reviews")

avg_q$farmer_ID.x <- rownames(avg_q)

avg_q$score <-  rowMeans(avg_q[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
avg_q$overall_rating <-  rowMeans(avg_q[c("general_rating_nonseed", "location","price","quality","stock","reputation")],na.rm=T)

colnames(avg_q)[14] <- "farmer_ID"
avg_q <- merge(avg_q, farmers_seedsub, by="farmer_ID") #merging with baseline farmers data to get the controls and gender

#create gender dummy
avg_q$farmergen <- ifelse(avg_q$Check2.check.maize.q15 == "Male", 1, 0)


###farmer characteristics for  controls
avg_q$educ_f <- 0
avg_q$educ_f[avg_q$Check2.check.maize.q17=="c" | avg_q$Check2.check.maize.q17=="d" | avg_q$Check2.check.maize.q17=="e" |
               avg_q$Check2.check.maize.q17=="f"  ] <- 1 #educated farmers -- finished primary educ
avg_q$educ_f[ avg_q$Check2.check.maize.q17=="g" ] <- NA
table(avg_q$educ_f)

avg_q$married <- ifelse(avg_q$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers

#age of farmers
avg_q$Check2.check.maize.q14[ avg_q$Check2.check.maize.q14==999 ] <- NA
summary(avg_q$Check2.check.maize.q14 )

#distance from tarmac road
avg_q$Check2.check.maize.q8[ avg_q$Check2.check.maize.q8==999 ] <- NA
summary(avg_q$Check2.check.maize.q8 )


## SEED RELATED RATINGS ##

#### regressions without controls - seed related ratings

mm_q1<-lm(score~farmergen, data = avg_q)
semq1<- sqrt(diag(vcov(mm_q1)))
#mm_q2<-lm(quality~farmergen, data = avg_q)
#semq2<- sqrt(diag(vcov(mm_q2)))
mm_q3<-lm(general~farmergen, data = avg_q)
semq3<- sqrt(diag(vcov(mm_q3)))
mm_q4<-lm(yield~farmergen, data = avg_q)
semq4<- sqrt(diag(vcov(mm_q4)))
mm_q5<-lm(drought_resistent~farmergen, data = avg_q)
semq5<- sqrt(diag(vcov(mm_q5)))
mm_q6<-lm(disease_resistent~farmergen, data = avg_q)
semq6<- sqrt(diag(vcov(mm_q6)))
mm_q7<-lm(early_maturing~farmergen, data = avg_q)
semq7<- sqrt(diag(vcov(mm_q7)))
mm_q8<-lm(germination~farmergen, data = avg_q)
semq8<- sqrt(diag(vcov(mm_q8)))

s_deal5<- rbind(c((format(round(sum(mm_q1$coefficients[1]),digits=3),nsmall=0)),
               #   (format(round(sum(mm_q2$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q3$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q4$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q5$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q6$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q7$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q8$coefficients[1]),digits=3),nsmall=0))),
                c((format(round(semq1[1],digits=3),nsmall=0)),
               #   (format(round(semq2[1],digits=3),nsmall=0)),
                  (format(round(semq3[1],digits=3),nsmall=0)),
                  (format(round(semq4[1],digits=3),nsmall=0)),
                  (format(round(semq5[1],digits=3),nsmall=0)),
                  (format(round(semq6[1],digits=3),nsmall=0)),
                  (format(round(semq7[1],digits=3),nsmall=0)),
                  (format(round(semq8[1],digits=3),nsmall=0))),
                c((format(round(summary(mm_q1)$coefficients[1,4],digits=3),nsmall=0)),
             #     (format(round(summary(mm_q2)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q3)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q4)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q5)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q6)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q7)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q8)$coefficients[1,4],digits=3),nsmall=0))),

                c((format(round(sum(mm_q1$coefficients[2]),digits=3),nsmall=0)),
                 # (format(round(sum(mm_q2$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q3$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q4$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q5$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q6$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q7$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q8$coefficients[2]),digits=3),nsmall=0))),
                c((format(round(semq1[2],digits=3),nsmall=0)),
              #    (format(round(semq2[2],digits=3),nsmall=0)),
                  (format(round(semq3[2],digits=3),nsmall=0)),
                  (format(round(semq4[2],digits=3),nsmall=0)),
                  (format(round(semq5[2],digits=3),nsmall=0)),
                  (format(round(semq6[2],digits=3),nsmall=0)),
                  (format(round(semq7[2],digits=3),nsmall=0)),
                  (format(round(semq8[2],digits=3),nsmall=0))),
                c((format(round(summary(mm_q1)$coefficients[2,4],digits=3),nsmall=0)),
              #    (format(round(summary(mm_q2)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q3)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q4)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q5)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q6)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q7)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q8)$coefficients[2,4],digits=3),nsmall=0))),

                c((format(round(summary(mm_q1)$r.squared,digits=3),nsmall=0)),
              #    (format(round(summary(mm_q2)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q3)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q4)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q5)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q6)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q7)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q8)$r.squared,digits=3),nsmall=0))),
                c((format(round(summary(mm_q1)$adj.r.squared,digits=3),nsmall=0)),
                #  (format(round(summary(mm_q2)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q3)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q4)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q5)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q6)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q7)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q8)$adj.r.squared,digits=3),nsmall=0))),
                c((format(round(nobs(mm_q1),digits=3),nsmall=0)),
                 # (format(round(nobs(mm_q2),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q3),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q4),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q5),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q6),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q7),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q8),digits=3),nsmall=0)))
)




#### regressions with FARMER's gender + FARMER characteristics  --- seed related ratings

mm_q9<- lm(score~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
semq9<- sqrt(diag(vcov(mm_q9)))
#mm_q10<- lm(quality~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
#semq10<- sqrt(diag(vcov(mm_q10)))
mm_q11<- lm(general~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
semq11<- sqrt(diag(vcov(mm_q11)))
mm_q12<- lm(yield~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
semq12<- sqrt(diag(vcov(mm_q12)))
mm_q13<- lm(drought_resistent~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
semq13<- sqrt(diag(vcov(mm_q13)))
mm_q14<- lm(disease_resistent~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
semq14<- sqrt(diag(vcov(mm_q14)))
mm_q15<- lm(early_maturing~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
semq15<- sqrt(diag(vcov(mm_q15)))
mm_q16<- lm(germination~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = avg_q)
semq16<- sqrt(diag(vcov(mm_q16)))

s_deal6<- rbind(c((format(round(sum(mm_q9$coefficients[1]),digits=3),nsmall=0)),
                #  (format(round(sum(mm_q10$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q11$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q12$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q13$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q14$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q15$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q16$coefficients[1]),digits=3),nsmall=0))),
                c((format(round(semq9[1],digits=3),nsmall=0)),
               #   (format(round(semq10[1],digits=3),nsmall=0)),
                  (format(round(semq11[1],digits=3),nsmall=0)),
                  (format(round(semq12[1],digits=3),nsmall=0)),
                  (format(round(semq13[1],digits=3),nsmall=0)),
                  (format(round(semq14[1],digits=3),nsmall=0)),
                  (format(round(semq15[1],digits=3),nsmall=0)),
                  (format(round(semq16[1],digits=3),nsmall=0))),
                c((format(round(summary(mm_q9)$coefficients[1,4],digits=3),nsmall=0)),
              #    (format(round(summary(mm_q10)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$coefficients[1,4],digits=3),nsmall=0))),

                c((format(round(sum(mm_q9$coefficients[2]),digits=3),nsmall=0)),
             #     (format(round(sum(mm_q10$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q11$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q12$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q13$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q14$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q15$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q16$coefficients[2]),digits=3),nsmall=0))),
                c((format(round(semq9[2],digits=3),nsmall=0)),
                #  (format(round(semq10[2],digits=3),nsmall=0)),
                  (format(round(semq11[2],digits=3),nsmall=0)),
                  (format(round(semq12[2],digits=3),nsmall=0)),
                  (format(round(semq13[2],digits=3),nsmall=0)),
                  (format(round(semq14[2],digits=3),nsmall=0)),
                  (format(round(semq15[2],digits=3),nsmall=0)),
                  (format(round(semq16[2],digits=3),nsmall=0))),
                c((format(round(summary(mm_q9)$coefficients[2,4],digits=3),nsmall=0)),
                #  (format(round(summary(mm_q10)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$coefficients[2,4],digits=3),nsmall=0))),

                c((format(round(sum(mm_q9$coefficients[3]),digits=3),nsmall=0)),
                 # (format(round(sum(mm_q10$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q11$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q12$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q13$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q14$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q15$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q16$coefficients[3]),digits=3),nsmall=0))),
                c((format(round(semq9[3],digits=3),nsmall=0)),
                #  (format(round(semq10[3],digits=3),nsmall=0)),
                  (format(round(semq11[3],digits=3),nsmall=0)),
                  (format(round(semq12[3],digits=3),nsmall=0)),
                  (format(round(semq13[3],digits=3),nsmall=0)),
                  (format(round(semq14[3],digits=3),nsmall=0)),
                  (format(round(semq15[3],digits=3),nsmall=0)),
                  (format(round(semq16[3],digits=3),nsmall=0))),
                c((format(round(summary(mm_q9)$coefficients[3,4],digits=3),nsmall=0)),
                  #(format(round(summary(mm_q10)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$coefficients[3,4],digits=3),nsmall=0))),

                c((format(round(sum(mm_q9$coefficients[4]),digits=3),nsmall=0)),
                 # (format(round(sum(mm_q10$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q11$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q12$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q13$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q14$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q15$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q16$coefficients[4]),digits=3),nsmall=0))),
                c((format(round(semq9[4],digits=3),nsmall=0)),
                #  (format(round(semq10[4],digits=3),nsmall=0)),
                  (format(round(semq11[4],digits=3),nsmall=0)),
                  (format(round(semq12[4],digits=3),nsmall=0)),
                  (format(round(semq13[4],digits=3),nsmall=0)),
                  (format(round(semq14[4],digits=3),nsmall=0)),
                  (format(round(semq15[4],digits=3),nsmall=0)),
                  (format(round(semq16[4],digits=3),nsmall=0))),
                c((format(round(summary(mm_q9)$coefficients[4,4],digits=3),nsmall=0)),
               #   (format(round(summary(mm_q10)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$coefficients[4,4],digits=3),nsmall=0))),

                c((format(round(sum(mm_q9$coefficients[5]),digits=3),nsmall=0)),
                 # (format(round(sum(mm_q10$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q11$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q12$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q13$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q14$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q15$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q16$coefficients[5]),digits=3),nsmall=0))),
                c((format(round(semq9[5],digits=3),nsmall=0)),
                #  (format(round(semq10[5],digits=3),nsmall=0)),
                  (format(round(semq11[5],digits=3),nsmall=0)),
                  (format(round(semq12[5],digits=3),nsmall=0)),
                  (format(round(semq13[5],digits=3),nsmall=0)),
                  (format(round(semq14[5],digits=3),nsmall=0)),
                  (format(round(semq15[5],digits=3),nsmall=0)),
                  (format(round(semq16[5],digits=3),nsmall=0))),
                c((format(round(summary(mm_q9)$coefficients[5,4],digits=3),nsmall=0)),
               #   (format(round(summary(mm_q10)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$coefficients[5,4],digits=3),nsmall=0))),

                c((format(round(sum(mm_q9$coefficients[6]),digits=3),nsmall=0)),
                 # (format(round(sum(mm_q10$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q11$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q12$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q13$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q14$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q15$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mm_q16$coefficients[6]),digits=3),nsmall=0))),
                c((format(round(semq9[6],digits=3),nsmall=0)),
                 # (format(round(semq10[6],digits=3),nsmall=0)),
                  (format(round(semq11[6],digits=3),nsmall=0)),
                  (format(round(semq12[6],digits=3),nsmall=0)),
                  (format(round(semq13[6],digits=3),nsmall=0)),
                  (format(round(semq14[6],digits=3),nsmall=0)),
                  (format(round(semq15[6],digits=3),nsmall=0)),
                  (format(round(semq16[6],digits=3),nsmall=0))),
                c((format(round(summary(mm_q9)$coefficients[6,4],digits=3),nsmall=0)),
                #  (format(round(summary(mm_q10)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$coefficients[6,4],digits=3),nsmall=0))),


                c((format(round(summary(mm_q9)$r.squared,digits=3),nsmall=0)),
             #     (format(round(summary(mm_q10)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$r.squared,digits=3),nsmall=0))),
                c((format(round(summary(mm_q9)$adj.r.squared,digits=3),nsmall=0)),
             #     (format(round(summary(mm_q10)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q11)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q12)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q13)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q14)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q15)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mm_q16)$adj.r.squared,digits=3),nsmall=0))),
                c((format(round(nobs(mm_q9),digits=3),nsmall=0)),
                #  (format(round(nobs(mm_q10),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q11),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q12),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q13),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q14),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q15),digits=3),nsmall=0)),
                  (format(round(nobs(mm_q16),digits=3),nsmall=0)))
)



#### NON-SEED RELATED RATINGS ####

#### regressions without controls - non-seed related ratings

mn_q1 <- lm(overall_rating~farmergen , data =avg_q)
senm1<- sqrt(diag(vcov(mn_q1)))
mn_q2 <- lm(general_rating_nonseed~farmergen , data =avg_q)
senm2<- sqrt(diag(vcov(mn_q2)))
mn_q3 <- lm(location~farmergen , data =avg_q)
senm3<- sqrt(diag(vcov(mn_q3)))
mn_q4 <- lm(price~farmergen , data =avg_q)
senm4<- sqrt(diag(vcov(mn_q4)))
mn_q5 <- lm(quality~farmergen , data =avg_q)
senm5<- sqrt(diag(vcov(mn_q5)))
mn_q6 <- lm(stock~farmergen , data =avg_q)
senm6<- sqrt(diag(vcov(mn_q6)))
mn_q7 <- lm(reputation~farmergen , data =avg_q)
senm7<- sqrt(diag(vcov(mn_q7)))


s_deal7<- rbind(c((format(round(sum(mn_q1$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q2$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q3$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q4$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q5$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q6$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q7$coefficients[1]),digits=3),nsmall=0))),
                c((format(round(senm1[1],digits=3),nsmall=0)),
                  (format(round(senm2[1],digits=3),nsmall=0)),
                  (format(round(senm3[1],digits=3),nsmall=0)),
                  (format(round(senm4[1],digits=3),nsmall=0)),
                  (format(round(senm5[1],digits=3),nsmall=0)),
                  (format(round(senm6[1],digits=3),nsmall=0)),
                  (format(round(senm7[1],digits=3),nsmall=0))),
                c((format(round(summary(mn_q1)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q2)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q3)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q4)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q5)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q6)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q7)$coefficients[1,4],digits=3),nsmall=0))),

                c((format(round(sum(mn_q1$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q2$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q3$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q4$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q5$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q6$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q7$coefficients[2]),digits=3),nsmall=0))),
                c((format(round(senm1[2],digits=3),nsmall=0)),
                  (format(round(senm2[2],digits=3),nsmall=0)),
                  (format(round(senm3[2],digits=3),nsmall=0)),
                  (format(round(senm4[2],digits=3),nsmall=0)),
                  (format(round(senm5[2],digits=3),nsmall=0)),
                  (format(round(senm6[2],digits=3),nsmall=0)),
                  (format(round(senm7[2],digits=3),nsmall=0))),
                c((format(round(summary(mn_q1)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q2)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q3)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q4)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q5)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q6)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q7)$coefficients[2,4],digits=3),nsmall=0))),

                c((format(round(summary(mn_q1)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q2)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q3)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q4)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q5)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q6)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q7)$r.squared,digits=3),nsmall=0))),
                c((format(round(summary(mn_q1)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q2)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q3)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q4)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q5)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q6)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q7)$adj.r.squared,digits=3),nsmall=0))),
                c((format(round(nobs(mn_q1),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q2),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q3),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q4),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q5),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q6),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q7),digits=3),nsmall=0)))
)


#### regressions with controls - non-seed related ratings


mn_q8 <- lm(overall_rating~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =avg_q)
senm8<- sqrt(diag(vcov(mn_q8)))
mn_q9 <- lm(general_rating_nonseed~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data =avg_q)
senm9<- sqrt(diag(vcov(mn_q9)))
mn_q10 <- lm(location~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data =avg_q)
senm10<- sqrt(diag(vcov(mn_q10)))
mn_q11 <- lm(price~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data =avg_q)
senm11<- sqrt(diag(vcov(mn_q11)))
mn_q12 <- lm(quality~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =avg_q)
senm12<- sqrt(diag(vcov(mn_q12)))
mn_q13 <- lm(stock~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data =avg_q)
senm13<- sqrt(diag(vcov(mn_q13)))
mn_q14 <- lm(reputation~farmergen +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data =avg_q)
senm14<- sqrt(diag(vcov(mn_q14)))

s_deal8<- rbind(c((format(round(sum(mn_q8$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q9$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q10$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q11$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q12$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q13$coefficients[1]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q14$coefficients[1]),digits=3),nsmall=0))),
                c((format(round(senm8[1],digits=3),nsmall=0)),
                  (format(round(senm9[1],digits=3),nsmall=0)),
                  (format(round(senm10[1],digits=3),nsmall=0)),
                  (format(round(senm11[1],digits=3),nsmall=0)),
                  (format(round(senm12[1],digits=3),nsmall=0)),
                  (format(round(senm13[1],digits=3),nsmall=0)),
                  (format(round(senm14[1],digits=3),nsmall=0))),
                c((format(round(summary(mn_q8)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$coefficients[1,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$coefficients[1,4],digits=3),nsmall=0))),

                c((format(round(sum(mn_q8$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q9$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q10$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q11$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q12$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q13$coefficients[2]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q14$coefficients[2]),digits=3),nsmall=0))),
                c((format(round(senm8[2],digits=3),nsmall=0)),
                  (format(round(senm9[2],digits=3),nsmall=0)),
                  (format(round(senm10[2],digits=3),nsmall=0)),
                  (format(round(senm11[2],digits=3),nsmall=0)),
                  (format(round(senm12[2],digits=3),nsmall=0)),
                  (format(round(senm13[2],digits=3),nsmall=0)),
                  (format(round(senm14[2],digits=3),nsmall=0))),
                c((format(round(summary(mn_q8)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$coefficients[2,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$coefficients[2,4],digits=3),nsmall=0))),

                c((format(round(sum(mn_q8$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q9$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q10$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q11$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q12$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q13$coefficients[3]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q14$coefficients[3]),digits=3),nsmall=0))),
                c((format(round(senm8[3],digits=3),nsmall=0)),
                  (format(round(senm9[3],digits=3),nsmall=0)),
                  (format(round(senm10[3],digits=3),nsmall=0)),
                  (format(round(senm11[3],digits=3),nsmall=0)),
                  (format(round(senm12[3],digits=3),nsmall=0)),
                  (format(round(senm13[3],digits=3),nsmall=0)),
                  (format(round(senm14[3],digits=3),nsmall=0))),
                c((format(round(summary(mn_q8)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$coefficients[3,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$coefficients[3,4],digits=3),nsmall=0))),

                c((format(round(sum(mn_q8$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q9$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q10$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q11$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q12$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q13$coefficients[4]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q14$coefficients[4]),digits=3),nsmall=0))),
                c((format(round(senm8[4],digits=3),nsmall=0)),
                  (format(round(senm9[4],digits=3),nsmall=0)),
                  (format(round(senm10[4],digits=3),nsmall=0)),
                  (format(round(senm11[4],digits=3),nsmall=0)),
                  (format(round(senm12[4],digits=3),nsmall=0)),
                  (format(round(senm13[4],digits=3),nsmall=0)),
                  (format(round(senm14[4],digits=3),nsmall=0))),
                c((format(round(summary(mn_q8)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$coefficients[4,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$coefficients[4,4],digits=3),nsmall=0))),

                c((format(round(sum(mn_q8$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q9$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q10$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q11$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q12$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q13$coefficients[5]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q14$coefficients[5]),digits=3),nsmall=0))),
                c((format(round(senm8[5],digits=3),nsmall=0)),
                  (format(round(senm9[5],digits=3),nsmall=0)),
                  (format(round(senm10[5],digits=3),nsmall=0)),
                  (format(round(senm11[5],digits=3),nsmall=0)),
                  (format(round(senm12[5],digits=3),nsmall=0)),
                  (format(round(senm13[5],digits=3),nsmall=0)),
                  (format(round(senm14[5],digits=3),nsmall=0))),
                c((format(round(summary(mn_q8)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$coefficients[5,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$coefficients[5,4],digits=3),nsmall=0))),

                c((format(round(sum(mn_q8$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q9$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q10$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q11$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q12$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q13$coefficients[6]),digits=3),nsmall=0)),
                  (format(round(sum(mn_q14$coefficients[6]),digits=3),nsmall=0))),
                c((format(round(senm8[6],digits=3),nsmall=0)),
                  (format(round(senm9[6],digits=3),nsmall=0)),
                  (format(round(senm10[6],digits=3),nsmall=0)),
                  (format(round(senm11[6],digits=3),nsmall=0)),
                  (format(round(senm12[6],digits=3),nsmall=0)),
                  (format(round(senm13[6],digits=3),nsmall=0)),
                  (format(round(senm14[6],digits=3),nsmall=0))),
                c((format(round(summary(mn_q8)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$coefficients[6,4],digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$coefficients[6,4],digits=3),nsmall=0))),


                c((format(round(summary(mn_q8)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$r.squared,digits=3),nsmall=0))),
                c((format(round(summary(mn_q8)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q9)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q10)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q11)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q12)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q13)$adj.r.squared,digits=3),nsmall=0)),
                  (format(round(summary(mn_q14)$adj.r.squared,digits=3),nsmall=0))),
                c((format(round(nobs(mn_q8),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q9),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q10),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q11),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q12),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q13),digits=3),nsmall=0)),
                  (format(round(nobs(mn_q14),digits=3),nsmall=0)))
)




####################################################################
################### FE at the dealer level #########################
####################################################################

fedatad_base <- merge(rating_dyads, baseline_dealer, by="shop_ID")

fedatad <- merge(fedatad_base, farmers_seed, by="farmer_ID")

fedatad[fedatad=="n/a"]<- NA
fedatad[fedatad=="98"]<- NA
fedatad[fedatad=="999"]<- NA

#create farmer's gender dummy
fedatad$farmergen <- ifelse(fedatad$Check2.check.maize.q15 == "Male", 1, 0)

###farmer characteristics for controls
fedatad$educ_f <- 0
fedatad$educ_f[fedatad$Check2.check.maize.q17=="c" | fedatad$Check2.check.maize.q17=="d" | fedatad$Check2.check.maize.q17=="e" |
                 fedatad$Check2.check.maize.q17=="f"  ] <- 1 #educated farmers -- finished primary educ
fedatad$educ_f[ fedatad$Check2.check.maize.q17=="g" ] <- NA
table(fedatad$educ_f)

fedatad$married <- ifelse(fedatad$Check2.check.maize.q16 == 'a', 1, 0)  #married farmers

#age of farmers
fedatad$Check2.check.maize.q14[ fedatad$Check2.check.maize.q14==999 ] <- NA
summary(fedatad$Check2.check.maize.q14 )

#distance from tarmac road
fedatad$Check2.check.maize.q8[ fedatad$Check2.check.maize.q8==999 ] <- NA
summary(fedatad$Check2.check.maize.q8 )


###dealer characteristics for  controls

#education of dealers
table(fedatad$maize.owner.agree.educ) 
fedatad$prim <- 0
#finished secondary educ --- e and f ; a,b,c,d --- did not finish secondary educ
fedatad$prim[fedatad$maize.owner.agree.educ=="e"|fedatad$maize.owner.agree.educ=="f"] <- 1
fedatad$prim[fedatad$maize.owner.agree.educ=="g"]<- NA
table(fedatad$prim)

#age of dealer
summary(fedatad$maize.owner.agree.age)
fedatad$maize.owner.agree.age[fedatad$maize.owner.agree.age==999] <- NA
table(fedatad$maize.owner.agree.age)

#distance of shop to nearest tarmac road
table(fedatad$maize.owner.agree.q3)
fedatad$maize.owner.agree.q3[fedatad$maize.owner.agree.q3==999] <- NA
summary(fedatad$maize.owner.agree.q3)

#distance of shop to nearest murram road
table(fedatad$maize.owner.agree.q4)

#selling only farm inputs
table(fedatad$maize.owner.agree.q5)
fedatad$inputsale<- ifelse(fedatad$maize.owner.agree.q5== 'Yes', 1, 0)

#Q8. When was this agro-input shop established? (year)
fedatad$years_shop <- 2020 - as.numeric(as.character(substr(fedatad$maize.owner.agree.q8, start=1, stop=4)))

#seed stored in dedicated area?
fedatad$maize.owner.agree.q69
fedatad$dedarea<-as.character(fedatad$maize.owner.agree.temp.q69)
fedatad$dedicated_area<- ifelse(fedatad$dedarea== 'Yes', 1, 0)
table(fedatad$dedicated_area)

#problem with rats or pests?
#we try to formulate this as a good quality variable --- So, no problem with pests is good
fedatad$maize.owner.agree.q71
fedatad$pest<-as.character(fedatad$maize.owner.agree.temp.q71)
fedatad$pest_prob<- ifelse(fedatad$pest== 'No', 1, 0)
table(fedatad$pest_prob)

#roof leak proof?
fedatad$maize.owner.agree.q72
fedatad$roof<-as.character(fedatad$maize.owner.agree.temp.q72)
fedatad$leakproof<- ifelse(fedatad$roof== 'Yes', 1, 0)
table(fedatad$leakproof)

#roof insulated?
fedatad$maize.owner.agree.q73
fedatad$roof_insu<-as.character(fedatad$maize.owner.agree.temp.q73)
fedatad$insulated<- ifelse(fedatad$roof_insu== 'Yes', 1, 0)
table(fedatad$insulated)

#walls insulated?
fedatad$maize.owner.agree.q74
fedatad$wall_insu<-as.character(fedatad$maize.owner.agree.temp.q74)
fedatad$wall_heatproof<- ifelse(fedatad$wall_insu== 'Yes', 1, 0)
table(fedatad$wall_heatproof)

#area ventilated?
fedatad$maize.owner.agree.q75
fedatad$vent<-as.character(fedatad$maize.owner.agree.temp.q75)
fedatad$ventilation<- ifelse(fedatad$vent== 'Yes', 1, 0)
table(fedatad$ventilation)

#Q78. Lighting conditions in area where seed is stored?
#we try to formulate this as good quality variable --- 2 is good lighting
fedatad$badlighting <- 0
fedatad$badlighting[fedatad$maize.owner.agree.temp.q78=="2"]<-1
table(fedatad$badlighting)

#Q79. On what surface are seed stored?
#we try to formulate this as good quality variable ---- 3,4,5 are good storage surfaces
fedatad$badstored <- 0
fedatad$badstored[fedatad$maize.owner.agree.temp.q79=="3"|fedatad$maize.owner.agree.temp.q79=="4"|fedatad$maize.owner.agree.temp.q79=="5"]<-1
fedatad$badstored[fedatad$maize.owner.agree.temp.q79==96]<-NA
table(fedatad$badstored)

#Q80. Do you see maize seed that is stored in open bags or containers?
#we try to formulate this as good quality variable --- Not being stored in open containers is good
fedatad$maize.owner.agree.q80
fedatad$open<-as.character(fedatad$maize.owner.agree.temp.q80)
fedatad$open_storage<- ifelse(fedatad$open== 'No', 1, 0)
table(fedatad$open_storage)

#Q81. Do you see any official certificates displayed in the store (eg that the shop was inspected ,that the owner attended trainings
#or that the business is registered with some association)
fedatad$maize.owner.agree.q81
fedatad$cert<-as.character(fedatad$maize.owner.agree.temp.q81)
fedatad$cert_yes<- ifelse(fedatad$cert== 'Yes', 1, 0)
table(fedatad$cert_yes)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
fedatad$shop_rate<-as.numeric(as.character(fedatad$maize.owner.agree.temp.q82))
table(fedatad$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
#we try to formulate this as good quality variable, not getting any complaint is good
table(fedatad$maize.owner.agree.q96)
fedatad$complaint<- ifelse(fedatad$maize.owner.agree.q96== 'No', 1, 0)
table(fedatad$complaint)

#Q70. Enter the temperature in the seed store (where seed is stored)
table(fedatad$maize.owner.agree.q70)
fedatad$maize.owner.agree.q70[fedatad$maize.owner.agree.q70==999] <- NA


###AVERAGE RATINGS
fedatad$quality<- as.numeric(fedatad$quality_rating)
fedatad$general<- as.numeric(fedatad$seed_quality_general_rating)
fedatad$yield <- as.numeric(fedatad$seed_yield_rating)
fedatad$drought_resistent <- as.numeric(fedatad$seed_drought_rating)
fedatad$disease_resistent <- as.numeric(fedatad$seed_disease_rating)
fedatad$early_maturing<- as.numeric(fedatad$seed_maturing_rating)
fedatad$germination<- as.numeric(fedatad$seed_germinate_rating)

fedatad$general_rating_nonseed<- as.numeric(fedatad$general_rating)
fedatad$location<- as.numeric(fedatad$location_rating)
fedatad$price <- as.numeric(fedatad$price_rating)
fedatad$stock <- as.numeric(fedatad$stock_rating)
fedatad$reputation<- as.numeric(fedatad$reputation_rating)


fedatad$score <-  rowMeans(fedatad[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")],na.rm=T)
fedatad$overall_rating <-  rowMeans(fedatad[c("general_rating_nonseed", "location","price","quality","stock","reputation")],na.rm=T)


## SEED RELATED RATINGS ##

#### regressions without controls - seed related ratings

summary(lm(score~farmergen +shop_ID.x, data = fedatad))
#summary(lm(quality~farmergen+shop_ID.x , data = fedatad))
summary(lm(general~farmergen +shop_ID.x, data = fedatad))
summary(lm(yield~farmergen +shop_ID.x, data = fedatad))
summary(lm(drought_resistent~farmergen+shop_ID.x , data = fedatad))
summary(lm(disease_resistent~farmergen+shop_ID.x , data = fedatad))
summary(lm(early_maturing~farmergen+shop_ID.x , data = fedatad))
summary(lm(germination~farmergen+shop_ID.x , data = fedatad))


############# plm method

plmd1<-plm(score~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id1<-mean(fixef(plmd1))
sed1<- sqrt(diag(vcov(plmd1)))

coefd1<-coeftest(plmd1, vcovHC(plmd1, type = "HC0", cluster = "time"))

#plmd2<-plm(quality~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
#id2<-mean(fixef(plmd2))
#sed2<- sqrt(diag(vcov(plmd2)))

plmd3<-plm(general~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id3<-mean(fixef(plmd3))
sed3<- sqrt(diag(vcov(plmd3)))

coefd3<-coeftest(plmd3, vcovHC(plmd3, type = "HC0", cluster = "time"))

plmd4<-plm(yield~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id4<-mean(fixef(plmd4))
sed4<- sqrt(diag(vcov(plmd4)))

coefd4<-coeftest(plmd4, vcovHC(plmd4, type = "HC0", cluster = "time"))

plmd5<-plm(drought_resistent~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id5<-mean(fixef(plmd5))
sed5<- sqrt(diag(vcov(plmd5)))

coefd5<-coeftest(plmd5, vcovHC(plmd5, type = "HC0", cluster = "time"))

plmd6<-plm(disease_resistent~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id6<-mean(fixef(plmd6))
sed6<- sqrt(diag(vcov(plmd6)))

coefd6<-coeftest(plmd6, vcovHC(plmd6, type = "HC0", cluster = "time"))


plmd7<-plm(early_maturing~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id7<-mean(fixef(plmd7))
sed7<- sqrt(diag(vcov(plmd7)))

coefd7<-coeftest(plmd7, vcovHC(plmd7, type = "HC0", cluster = "time"))


plmd8<-plm(germination~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id8<-mean(fixef(plmd8))
sed8<- sqrt(diag(vcov(plmd8)))

coefd8<-coeftest(plmd8, vcovHC(plmd8, type = "HC0", cluster = "time"))



fed1<- rbind( c((format(round(id1[1],digits=3),nsmall=0)),
             #   (format(round(id2[1],digits=3),nsmall=0)),
                (format(round(id3[1],digits=3),nsmall=0)),
                (format(round(id4[1],digits=3),nsmall=0)),
                (format(round(id5[1],digits=3),nsmall=0)),
                (format(round(id6[1],digits=3),nsmall=0)),
                (format(round(id7[1],digits=3),nsmall=0)),
                (format(round(id8[1],digits=3),nsmall=0))),
              c((format(round(sum(plmd1$coefficients[1]),digits=3),nsmall=0)),
              #  (format(round(sum(plmd2$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd3$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd4$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd5$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd6$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd7$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd8$coefficients[1]),digits=3),nsmall=0))),
              c((format(round(coefd1[1,2],digits=3),nsmall=0)),
                (format(round(coefd3[1,2],digits=3),nsmall=0)),
                (format(round(coefd4[1,2],digits=3),nsmall=0)),
                (format(round(coefd5[1,2],digits=3),nsmall=0)),
                (format(round(coefd6[1,2],digits=3),nsmall=0)),
                (format(round(coefd7[1,2],digits=3),nsmall=0)),
                (format(round(coefd8[1,2],digits=3),nsmall=0))),
              c((format(round(coefd1[1,4],digits=3),nsmall=0)),
                (format(round(coefd3[1,4],digits=3),nsmall=0)),
                (format(round(coefd4[1,4],digits=3),nsmall=0)),
                (format(round(coefd5[1,4],digits=3),nsmall=0)),
                (format(round(coefd6[1,4],digits=3),nsmall=0)),
                (format(round(coefd7[1,4],digits=3),nsmall=0)),
                (format(round(coefd8[1,4],digits=3),nsmall=0))),


              c((format(round(summary(plmd1)$r.squared[1],digits=3),nsmall=0)),
               # (format(round(summary(plmd2)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd3)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd4)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd5)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd6)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd7)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd8)$r.squared[1],digits=3),nsmall=0))),
              c((format(round(summary(plmd1)$r.squared[2],digits=3),nsmall=0)),
              #  (format(round(summary(plmd2)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd3)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd4)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd5)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd6)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd7)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd8)$r.squared[2],digits=3),nsmall=0))),
              c((format(round(nobs(plmd1),digits=3),nsmall=0)),
               # (format(round(nobs(plmd2),digits=3),nsmall=0)),
                (format(round(nobs(plmd3),digits=3),nsmall=0)),
                (format(round(nobs(plmd4),digits=3),nsmall=0)),
                (format(round(nobs(plmd5),digits=3),nsmall=0)),
                (format(round(nobs(plmd6),digits=3),nsmall=0)),
                (format(round(nobs(plmd7),digits=3),nsmall=0)),
                (format(round(nobs(plmd8),digits=3),nsmall=0)))
)


#### regressions with controls - seed related ratings

summary(lm(score~farmergen +shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
#summary(lm(quality~farmergen+shop_ID.x +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(general~farmergen +shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(yield~farmergen +shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(drought_resistent~farmergen+shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data = fedatad))
summary(lm(disease_resistent~farmergen+shop_ID.x +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(early_maturing~farmergen+shop_ID.x +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(germination~farmergen+shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data = fedatad))


############# plm method

plmd9<-plm(score~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id9<-mean(fixef(plmd9))
sed9<- sqrt(diag(vcov(plmd9)))

coefd9<-coeftest(plmd9, vcovHC(plmd9, type = "HC0", cluster = "time"))


#plmd10<-plm(quality~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
#id10<-mean(fixef(plmd10))
#sed10<- sqrt(diag(vcov(plmd10)))

plmd11<-plm(general~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id11<-mean(fixef(plmd11))
sed11<- sqrt(diag(vcov(plmd11)))

coefd11<-coeftest(plmd11, vcovHC(plmd11, type = "HC0", cluster = "time"))

plmd12<-plm(yield~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id12<-mean(fixef(plmd12))
sed12<- sqrt(diag(vcov(plmd12)))

coefd12<-coeftest(plmd12, vcovHC(plmd12, type = "HC0", cluster = "time"))


plmd13<-plm(drought_resistent~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id13<-mean(fixef(plmd13))
sed13<- sqrt(diag(vcov(plmd13)))

coefd13<-coeftest(plmd13, vcovHC(plmd13, type = "HC0", cluster = "time"))


plmd14<-plm(disease_resistent~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id14<-mean(fixef(plmd14))
sed14<- sqrt(diag(vcov(plmd14)))

coefd14<-coeftest(plmd14, vcovHC(plmd14, type = "HC0", cluster = "time"))


plmd15<-plm(early_maturing~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id15<-mean(fixef(plmd15))
sed15<- sqrt(diag(vcov(plmd15)))

coefd15<-coeftest(plmd15, vcovHC(plmd15, type = "HC0", cluster = "time"))


plmd16<-plm(germination~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id16<-mean(fixef(plmd16))
sed16<- sqrt(diag(vcov(plmd16)))

coefd16<-coeftest(plmd16, vcovHC(plmd16, type = "HC0", cluster = "time"))


fed2<- rbind( c((format(round(id9[1],digits=3),nsmall=0)),
               # (format(round(id10[1],digits=3),nsmall=0)),
                (format(round(id11[1],digits=3),nsmall=0)),
                (format(round(id12[1],digits=3),nsmall=0)),
                (format(round(id13[1],digits=3),nsmall=0)),
                (format(round(id14[1],digits=3),nsmall=0)),
                (format(round(id15[1],digits=3),nsmall=0)),
                (format(round(id16[1],digits=3),nsmall=0))),
              c((format(round(sum(plmd9$coefficients[1]),digits=3),nsmall=0)),
            #    (format(round(sum(plmd10$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[1]),digits=3),nsmall=0))),
            c((format(round(coefd9[1,2],digits=3),nsmall=0)),
              (format(round(coefd11[1,2],digits=3),nsmall=0)),
              (format(round(coefd12[1,2],digits=3),nsmall=0)),
              (format(round(coefd13[1,2],digits=3),nsmall=0)),
              (format(round(coefd14[1,2],digits=3),nsmall=0)),
              (format(round(coefd15[1,2],digits=3),nsmall=0)),
              (format(round(coefd16[1,2],digits=3),nsmall=0))),
            c((format(round(coefd9[1,4],digits=3),nsmall=0)),
              (format(round(coefd11[1,4],digits=3),nsmall=0)),
              (format(round(coefd12[1,4],digits=3),nsmall=0)),
              (format(round(coefd13[1,4],digits=3),nsmall=0)),
              (format(round(coefd14[1,4],digits=3),nsmall=0)),
              (format(round(coefd15[1,4],digits=3),nsmall=0)),
              (format(round(coefd16[1,4],digits=3),nsmall=0))),

              c((format(round(sum(plmd9$coefficients[2]),digits=3),nsmall=0)),
              #  (format(round(sum(plmd10$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[2]),digits=3),nsmall=0))),
            c((format(round(coefd9[2,2],digits=3),nsmall=0)),
              (format(round(coefd11[2,2],digits=3),nsmall=0)),
              (format(round(coefd12[2,2],digits=3),nsmall=0)),
              (format(round(coefd13[2,2],digits=3),nsmall=0)),
              (format(round(coefd14[2,2],digits=3),nsmall=0)),
              (format(round(coefd15[2,2],digits=3),nsmall=0)),
              (format(round(coefd16[2,2],digits=3),nsmall=0))),
            c((format(round(coefd9[2,4],digits=3),nsmall=0)),
              (format(round(coefd11[2,4],digits=3),nsmall=0)),
              (format(round(coefd12[2,4],digits=3),nsmall=0)),
              (format(round(coefd13[2,4],digits=3),nsmall=0)),
              (format(round(coefd14[2,4],digits=3),nsmall=0)),
              (format(round(coefd15[2,4],digits=3),nsmall=0)),
              (format(round(coefd16[2,4],digits=3),nsmall=0))),

              c((format(round(sum(plmd9$coefficients[3]),digits=3),nsmall=0)),
              #  (format(round(sum(plmd10$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[3]),digits=3),nsmall=0))),
            c((format(round(coefd9[3,2],digits=3),nsmall=0)),
              (format(round(coefd11[3,2],digits=3),nsmall=0)),
              (format(round(coefd12[3,2],digits=3),nsmall=0)),
              (format(round(coefd13[3,2],digits=3),nsmall=0)),
              (format(round(coefd14[3,2],digits=3),nsmall=0)),
              (format(round(coefd15[3,2],digits=3),nsmall=0)),
              (format(round(coefd16[3,2],digits=3),nsmall=0))),
            c((format(round(coefd9[3,4],digits=3),nsmall=0)),
              (format(round(coefd11[3,4],digits=3),nsmall=0)),
              (format(round(coefd12[3,4],digits=3),nsmall=0)),
              (format(round(coefd13[3,4],digits=3),nsmall=0)),
              (format(round(coefd14[3,4],digits=3),nsmall=0)),
              (format(round(coefd15[3,4],digits=3),nsmall=0)),
              (format(round(coefd16[3,4],digits=3),nsmall=0))),

              c((format(round(sum(plmd9$coefficients[4]),digits=3),nsmall=0)),
           #     (format(round(sum(plmd10$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[4]),digits=3),nsmall=0))),
           c((format(round(coefd9[4,2],digits=3),nsmall=0)),
             (format(round(coefd11[4,2],digits=3),nsmall=0)),
             (format(round(coefd12[4,2],digits=3),nsmall=0)),
             (format(round(coefd13[4,2],digits=3),nsmall=0)),
             (format(round(coefd14[4,2],digits=3),nsmall=0)),
             (format(round(coefd15[4,2],digits=3),nsmall=0)),
             (format(round(coefd16[4,2],digits=3),nsmall=0))),
           c((format(round(coefd9[4,4],digits=3),nsmall=0)),
             (format(round(coefd11[4,4],digits=3),nsmall=0)),
             (format(round(coefd12[4,4],digits=3),nsmall=0)),
             (format(round(coefd13[4,4],digits=3),nsmall=0)),
             (format(round(coefd14[4,4],digits=3),nsmall=0)),
             (format(round(coefd15[4,4],digits=3),nsmall=0)),
             (format(round(coefd16[4,4],digits=3),nsmall=0))),

              c((format(round(sum(plmd9$coefficients[5]),digits=3),nsmall=0)),
              #  (format(round(sum(plmd10$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[5]),digits=3),nsmall=0))),
           c((format(round(coefd9[5,2],digits=3),nsmall=0)),
             (format(round(coefd11[5,2],digits=3),nsmall=0)),
             (format(round(coefd12[5,2],digits=3),nsmall=0)),
             (format(round(coefd13[5,2],digits=3),nsmall=0)),
             (format(round(coefd14[5,2],digits=3),nsmall=0)),
             (format(round(coefd15[5,2],digits=3),nsmall=0)),
             (format(round(coefd16[5,2],digits=3),nsmall=0))),
           c((format(round(coefd9[5,4],digits=3),nsmall=0)),
             (format(round(coefd11[5,4],digits=3),nsmall=0)),
             (format(round(coefd12[5,4],digits=3),nsmall=0)),
             (format(round(coefd13[5,4],digits=3),nsmall=0)),
             (format(round(coefd14[5,4],digits=3),nsmall=0)),
             (format(round(coefd15[5,4],digits=3),nsmall=0)),
             (format(round(coefd16[5,4],digits=3),nsmall=0))),
           
              c((format(round(summary(plmd9)$r.squared[1],digits=3),nsmall=0)),
               # (format(round(summary(plmd10)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd11)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd12)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd13)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd14)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd15)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmd16)$r.squared[1],digits=3),nsmall=0))),
              c((format(round(summary(plmd9)$r.squared[2],digits=3),nsmall=0)),
              #  (format(round(summary(plmd10)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd11)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd12)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd13)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd14)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd15)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmd16)$r.squared[2],digits=3),nsmall=0))),
              c((format(round(nobs(plmd9),digits=3),nsmall=0)),
             #   (format(round(nobs(plmd10),digits=3),nsmall=0)),
                (format(round(nobs(plmd11),digits=3),nsmall=0)),
                (format(round(nobs(plmd12),digits=3),nsmall=0)),
                (format(round(nobs(plmd13),digits=3),nsmall=0)),
                (format(round(nobs(plmd14),digits=3),nsmall=0)),
                (format(round(nobs(plmd15),digits=3),nsmall=0)),
                (format(round(nobs(plmd16),digits=3),nsmall=0)))
)




## NON-SEED RELATED RATINGS ##

#### regressions without controls - non-seed related ratings

summary(lm(overall_rating~farmergen +shop_ID.x, data = fedatad))
summary(lm(general_rating_nonseed~farmergen+shop_ID.x , data = fedatad))
summary(lm(location~farmergen +shop_ID.x, data = fedatad))
summary(lm(quality~farmergen +shop_ID.x, data = fedatad))
summary(lm(price~farmergen+shop_ID.x , data = fedatad))
summary(lm(stock~farmergen+shop_ID.x , data = fedatad))
summary(lm(reputation~farmergen+shop_ID.x , data = fedatad))


############# plm method

plmdn1<-plm(overall_rating~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn1<-mean(fixef(plmdn1))
sedn1<- sqrt(diag(vcov(plmdn1)))

coefdn1<-coeftest(plmdn1, vcovHC(plmdn1, type = "HC0", cluster = "time"))

plmdn2<-plm(general_rating_nonseed~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn2<-mean(fixef(plmdn2))
sedn2<- sqrt(diag(vcov(plmdn2)))

coefdn2<-coeftest(plmdn2, vcovHC(plmdn2, type = "HC0", cluster = "time"))

plmdn3<-plm(location~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn3<-mean(fixef(plmdn3))
sedn3<- sqrt(diag(vcov(plmdn3)))

coefdn3<-coeftest(plmdn3, vcovHC(plmdn3, type = "HC0", cluster = "time"))

plmdn4<-plm(quality~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn4<-mean(fixef(plmdn4))
sedn4<- sqrt(diag(vcov(plmdn4)))

coefdn4<-coeftest(plmdn4, vcovHC(plmdn4, type = "HC0", cluster = "time"))

plmdn5<-plm(price~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn5<-mean(fixef(plmdn5))
sedn5<- sqrt(diag(vcov(plmdn5)))

coefdn5<-coeftest(plmdn5, vcovHC(plmdn5, type = "HC0", cluster = "time"))


plmdn6<-plm(stock~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn6<-mean(fixef(plmdn6))
sedn6<- sqrt(diag(vcov(plmdn6)))

coefdn6<-coeftest(plmdn6, vcovHC(plmdn6, type = "HC0", cluster = "time"))


plmdn7<-plm(reputation~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn7<-mean(fixef(plmdn7))
sedn7<- sqrt(diag(vcov(plmdn7)))

coefdn7<-coeftest(plmdn7, vcovHC(plmdn7, type = "HC0", cluster = "time"))


fed3<- rbind( c((format(round(idn1[1],digits=3),nsmall=0)),
                (format(round(idn2[1],digits=3),nsmall=0)),
                (format(round(idn3[1],digits=3),nsmall=0)),
                (format(round(idn4[1],digits=3),nsmall=0)),
                (format(round(idn5[1],digits=3),nsmall=0)),
                (format(round(idn6[1],digits=3),nsmall=0)),
                (format(round(idn7[1],digits=3),nsmall=0))),
              c((format(round(sum(plmdn1$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn2$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn3$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn4$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn5$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn6$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn7$coefficients[1]),digits=3),nsmall=0))),
              c((format(round(coefdn1[1,2],digits=3),nsmall=0)),
                (format(round(coefdn2[1,2],digits=3),nsmall=0)),
                (format(round(coefdn3[1,2],digits=3),nsmall=0)),
                (format(round(coefdn4[1,2],digits=3),nsmall=0)),
                (format(round(coefdn5[1,2],digits=3),nsmall=0)),
                (format(round(coefdn6[1,2],digits=3),nsmall=0)),
                (format(round(coefdn7[1,2],digits=3),nsmall=0))),
              c((format(round(coefdn1[1,4],digits=3),nsmall=0)),
                (format(round(coefdn2[1,4],digits=3),nsmall=0)),
                (format(round(coefdn3[1,4],digits=3),nsmall=0)),
                (format(round(coefdn4[1,4],digits=3),nsmall=0)),
                (format(round(coefdn5[1,4],digits=3),nsmall=0)),
                (format(round(coefdn6[1,4],digits=3),nsmall=0)),
                (format(round(coefdn7[1,4],digits=3),nsmall=0))),


              c((format(round(summary(plmdn1)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn2)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn3)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn4)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn5)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn6)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn7)$r.squared[1],digits=3),nsmall=0))),
              c((format(round(summary(plmdn1)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn2)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn3)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn4)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn5)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn6)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn7)$r.squared[2],digits=3),nsmall=0))),
              c((format(round(nobs(plmdn1),digits=3),nsmall=0)),
                (format(round(nobs(plmdn2),digits=3),nsmall=0)),
                (format(round(nobs(plmdn3),digits=3),nsmall=0)),
                (format(round(nobs(plmdn4),digits=3),nsmall=0)),
                (format(round(nobs(plmdn5),digits=3),nsmall=0)),
                (format(round(nobs(plmdn6),digits=3),nsmall=0)),
                (format(round(nobs(plmdn7),digits=3),nsmall=0)))
)


#### regressions with controls - non-seed related ratings

summary(lm(overall_rating~farmergen +shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(general_rating_nonseed~farmergen+shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data = fedatad))
summary(lm(location~farmergen +shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(quality~farmergen +shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(price~farmergen+shop_ID.x +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad))
summary(lm(stock~farmergen+shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data = fedatad))
summary(lm(reputation~farmergen+shop_ID.x+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data = fedatad))


############# plm method

plmdn9<-plm(overall_rating~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn9<-mean(fixef(plmdn9))
sedn9<- sqrt(diag(vcov(plmdn9)))

coefdn9<-coeftest(plmdn9, vcovHC(plmdn9, type = "HC0", cluster = "time"))


plmdn10<-plm(general_rating_nonseed~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn10<-mean(fixef(plmdn10))
sedn10<- sqrt(diag(vcov(plmdn10)))

coefdn10<-coeftest(plmdn10, vcovHC(plmdn10, type = "HC0", cluster = "time"))


plmdn11<-plm(location~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn11<-mean(fixef(plmdn11))
sedn11<- sqrt(diag(vcov(plmdn11)))

coefdn11<-coeftest(plmdn11, vcovHC(plmdn11, type = "HC0", cluster = "time"))


plmdn12<-plm(quality~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn12<-mean(fixef(plmdn12))
sedn12<- sqrt(diag(vcov(plmdn12)))

coefdn12<-coeftest(plmdn12, vcovHC(plmdn12, type = "HC0", cluster = "time"))


plmdn13<-plm(price~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn13<-mean(fixef(plmdn13))
sedn13<- sqrt(diag(vcov(plmdn13)))

coefdn13<-coeftest(plmdn13, vcovHC(plmdn13, type = "HC0", cluster = "time"))


plmdn14<-plm(stock~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn14<-mean(fixef(plmdn14))
sedn14<- sqrt(diag(vcov(plmdn14)))

coefdn14<-coeftest(plmdn14, vcovHC(plmdn14, type = "HC0", cluster = "time"))


plmdn15<-plm(reputation~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn15<-mean(fixef(plmdn15))
sedn15<- sqrt(diag(vcov(plmdn15)))

coefdn15<-coeftest(plmdn15, vcovHC(plmdn15, type = "HC0", cluster = "time"))



fed4<- rbind( c((format(round(idn9[1],digits=3),nsmall=0)),
                (format(round(idn10[1],digits=3),nsmall=0)),
                (format(round(idn11[1],digits=3),nsmall=0)),
                (format(round(idn12[1],digits=3),nsmall=0)),
                (format(round(idn13[1],digits=3),nsmall=0)),
                (format(round(idn14[1],digits=3),nsmall=0)),
                (format(round(idn15[1],digits=3),nsmall=0))),
              c((format(round(sum(plmdn9$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[1]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[1]),digits=3),nsmall=0))),
              c((format(round(coefdn9[1,2],digits=3),nsmall=0)),
                (format(round(coefdn10[1,2],digits=3),nsmall=0)),
                (format(round(coefdn11[1,2],digits=3),nsmall=0)),
                (format(round(coefdn12[1,2],digits=3),nsmall=0)),
                (format(round(coefdn13[1,2],digits=3),nsmall=0)),
                (format(round(coefdn14[1,2],digits=3),nsmall=0)),
                (format(round(coefdn15[1,2],digits=3),nsmall=0))),
              c((format(round(coefdn9[1,4],digits=3),nsmall=0)),
                (format(round(coefdn10[1,4],digits=3),nsmall=0)),
                (format(round(coefdn11[1,4],digits=3),nsmall=0)),
                (format(round(coefdn12[1,4],digits=3),nsmall=0)),
                (format(round(coefdn13[1,4],digits=3),nsmall=0)),
                (format(round(coefdn14[1,4],digits=3),nsmall=0)),
                (format(round(coefdn15[1,4],digits=3),nsmall=0))),

              c((format(round(sum(plmdn9$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[2]),digits=3),nsmall=0))),
              c((format(round(coefdn9[2,2],digits=3),nsmall=0)),
                (format(round(coefdn10[2,2],digits=3),nsmall=0)),
                (format(round(coefdn11[2,2],digits=3),nsmall=0)),
                (format(round(coefdn12[2,2],digits=3),nsmall=0)),
                (format(round(coefdn13[2,2],digits=3),nsmall=0)),
                (format(round(coefdn14[2,2],digits=3),nsmall=0)),
                (format(round(coefdn15[2,2],digits=3),nsmall=0))),
              c((format(round(coefdn9[2,4],digits=3),nsmall=0)),
                (format(round(coefdn10[2,4],digits=3),nsmall=0)),
                (format(round(coefdn11[2,4],digits=3),nsmall=0)),
                (format(round(coefdn12[2,4],digits=3),nsmall=0)),
                (format(round(coefdn13[2,4],digits=3),nsmall=0)),
                (format(round(coefdn14[2,4],digits=3),nsmall=0)),
                (format(round(coefdn15[2,4],digits=3),nsmall=0))),

              c((format(round(sum(plmdn9$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[3]),digits=3),nsmall=0))),
              c((format(round(coefdn9[3,2],digits=3),nsmall=0)),
                (format(round(coefdn10[3,2],digits=3),nsmall=0)),
                (format(round(coefdn11[3,2],digits=3),nsmall=0)),
                (format(round(coefdn12[3,2],digits=3),nsmall=0)),
                (format(round(coefdn13[3,2],digits=3),nsmall=0)),
                (format(round(coefdn14[3,2],digits=3),nsmall=0)),
                (format(round(coefdn15[3,2],digits=3),nsmall=0))),
              c((format(round(coefdn9[3,4],digits=3),nsmall=0)),
                (format(round(coefdn10[3,4],digits=3),nsmall=0)),
                (format(round(coefdn11[3,4],digits=3),nsmall=0)),
                (format(round(coefdn12[3,4],digits=3),nsmall=0)),
                (format(round(coefdn13[3,4],digits=3),nsmall=0)),
                (format(round(coefdn14[3,4],digits=3),nsmall=0)),
                (format(round(coefdn15[3,4],digits=3),nsmall=0))),

              c((format(round(sum(plmdn9$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[4]),digits=3),nsmall=0))),
              c((format(round(coefdn9[4,2],digits=3),nsmall=0)),
                (format(round(coefdn10[4,2],digits=3),nsmall=0)),
                (format(round(coefdn11[4,2],digits=3),nsmall=0)),
                (format(round(coefdn12[4,2],digits=3),nsmall=0)),
                (format(round(coefdn13[4,2],digits=3),nsmall=0)),
                (format(round(coefdn14[4,2],digits=3),nsmall=0)),
                (format(round(coefdn15[4,2],digits=3),nsmall=0))),
              c((format(round(coefdn9[4,4],digits=3),nsmall=0)),
                (format(round(coefdn10[4,4],digits=3),nsmall=0)),
                (format(round(coefdn11[4,4],digits=3),nsmall=0)),
                (format(round(coefdn12[4,4],digits=3),nsmall=0)),
                (format(round(coefdn13[4,4],digits=3),nsmall=0)),
                (format(round(coefdn14[4,4],digits=3),nsmall=0)),
                (format(round(coefdn15[4,4],digits=3),nsmall=0))),

              c((format(round(sum(plmdn9$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[5]),digits=3),nsmall=0))),
              c((format(round(coefdn9[5,2],digits=3),nsmall=0)),
                (format(round(coefdn10[5,2],digits=3),nsmall=0)),
                (format(round(coefdn11[5,2],digits=3),nsmall=0)),
                (format(round(coefdn12[5,2],digits=3),nsmall=0)),
                (format(round(coefdn13[5,2],digits=3),nsmall=0)),
                (format(round(coefdn14[5,2],digits=3),nsmall=0)),
                (format(round(coefdn15[5,2],digits=3),nsmall=0))),
              c((format(round(coefdn9[5,4],digits=3),nsmall=0)),
                (format(round(coefdn10[5,4],digits=3),nsmall=0)),
                (format(round(coefdn11[5,4],digits=3),nsmall=0)),
                (format(round(coefdn12[5,4],digits=3),nsmall=0)),
                (format(round(coefdn13[5,4],digits=3),nsmall=0)),
                (format(round(coefdn14[5,4],digits=3),nsmall=0)),
                (format(round(coefdn15[5,4],digits=3),nsmall=0))),

              c((format(round(summary(plmdn9)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn10)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn11)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn12)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn13)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn14)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmdn15)$r.squared[1],digits=3),nsmall=0))),
              c((format(round(summary(plmdn9)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn10)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn11)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn12)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn13)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn14)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmdn15)$r.squared[2],digits=3),nsmall=0))),
              c((format(round(nobs(plmdn9),digits=3),nsmall=0)),
                (format(round(nobs(plmdn10),digits=3),nsmall=0)),
                (format(round(nobs(plmdn11),digits=3),nsmall=0)),
                (format(round(nobs(plmdn12),digits=3),nsmall=0)),
                (format(round(nobs(plmdn13),digits=3),nsmall=0)),
                (format(round(nobs(plmdn14),digits=3),nsmall=0)),
                (format(round(nobs(plmdn15),digits=3),nsmall=0)))
)



#################################################################################################################
#################################################################################################################
###################                             RANDOM EFFECTS                    ###############################
#################################################################################################################
#################################################################################################################

########## SEED RATINGS 

#### One way random effect at the dealer level  -- no controls 
plmb1<-plm(score ~ farmergen, data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
           effect = "individual")
coefb1<-coeftest(plmb1, vcovHC(plmb1, type = "HC0", cluster = "time"))
summary(plmb1)
coefb1

plmb2<-plm(general ~ farmergen, data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
           effect = "individual")
coefb2<-coeftest(plmb2, vcovHC(plmb2, type = "HC0", cluster = "time"))
summary(plmb2)
coefb2

plmb3<-plm(yield ~ farmergen, data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
           effect = "individual")
coefb3<-coeftest(plmb3, vcovHC(plmb3, type = "HC0", cluster = "time"))
summary(plmb3)
coefb3

plmb4<-plm(drought_resistent~ farmergen, data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
           effect = "individual")
coefb4<-coeftest(plmb4, vcovHC(plmb4, type = "HC0", cluster = "time"))
summary(plmb4)
coefb4

plmb5<-plm(disease_resistent~ farmergen, data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
           effect = "individual")
coefb5<-coeftest(plmb5, vcovHC(plmb5, type = "HC0", cluster = "time"))
summary(plmb5)
coefb5

plmb6<-plm(early_maturing ~ farmergen, data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
           effect = "individual")
coefb6<-coeftest(plmb6, vcovHC(plmb6, type = "HC0", cluster = "time"))
summary(plmb6)
coefb6

plmb7<-plm(germination ~ farmergen, data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
           effect = "individual")
coefb7<-coeftest(plmb7, vcovHC(plmb7, type = "HC0", cluster = "time"))
summary(plmb7)
coefb7


tab_model(plmb1, plmb2,plmb3, plmb4, plmb5, plmb6, plmb7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)


ranb1<- rbind( c((format(round(sum(coefb1[1,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb2[1,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb3[1,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb4[1,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb5[1,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb6[1,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb7[1,1]),digits=3),nsmall=0))),
              c((format(round(coefb1[1,2],digits=3),nsmall=0)),
                (format(round(coefb2[1,2],digits=3),nsmall=0)),
                (format(round(coefb3[1,2],digits=3),nsmall=0)),
                (format(round(coefb4[1,2],digits=3),nsmall=0)),
                (format(round(coefb5[1,2],digits=3),nsmall=0)),
                (format(round(coefb6[1,2],digits=3),nsmall=0)),
                (format(round(coefb7[1,2],digits=3),nsmall=0))),
              c((format(round(coefb1[1,4],digits=3),nsmall=0)),
                (format(round(coefb2[1,4],digits=3),nsmall=0)),
                (format(round(coefb3[1,4],digits=3),nsmall=0)),
                (format(round(coefb4[1,4],digits=3),nsmall=0)),
                (format(round(coefb5[1,4],digits=3),nsmall=0)),
                (format(round(coefb6[1,4],digits=3),nsmall=0)),
                (format(round(coefb7[1,4],digits=3),nsmall=0))),
              
              c((format(round(sum(coefb1[2,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb2[2,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb3[2,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb4[2,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb5[2,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb6[2,1]),digits=3),nsmall=0)),
                (format(round(sum(coefb7[2,1]),digits=3),nsmall=0))),
              c((format(round(coefb1[2,2],digits=3),nsmall=0)),
                (format(round(coefb2[2,2],digits=3),nsmall=0)),
                (format(round(coefb3[2,2],digits=3),nsmall=0)),
                (format(round(coefb4[2,2],digits=3),nsmall=0)),
                (format(round(coefb5[2,2],digits=3),nsmall=0)),
                (format(round(coefb6[2,2],digits=3),nsmall=0)),
                (format(round(coefb7[2,2],digits=3),nsmall=0))),
              c((format(round(coefb1[2,4],digits=3),nsmall=0)),
                (format(round(coefb2[2,4],digits=3),nsmall=0)),
                (format(round(coefb3[2,4],digits=3),nsmall=0)),
                (format(round(coefb4[2,4],digits=3),nsmall=0)),
                (format(round(coefb5[2,4],digits=3),nsmall=0)),
                (format(round(coefb6[2,4],digits=3),nsmall=0)),
                (format(round(coefb7[2,4],digits=3),nsmall=0))),
              
            
              c((format(round(summary(plmb1)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmb2)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmb3)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmb4)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmb5)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmb6)$r.squared[1],digits=3),nsmall=0)),
                (format(round(summary(plmb7)$r.squared[1],digits=3),nsmall=0))),
              c((format(round(summary(plmb1)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmb2)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmb3)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmb4)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmb5)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmb6)$r.squared[2],digits=3),nsmall=0)),
                (format(round(summary(plmb7)$r.squared[2],digits=3),nsmall=0))),
              c((format(round(nobs(plmb1),digits=3),nsmall=0)),
                (format(round(nobs(plmb2),digits=3),nsmall=0)),
                (format(round(nobs(plmb3),digits=3),nsmall=0)),
                (format(round(nobs(plmb4),digits=3),nsmall=0)),
                (format(round(nobs(plmb5),digits=3),nsmall=0)),
                (format(round(nobs(plmb6),digits=3),nsmall=0)),
                (format(round(nobs(plmb7),digits=3),nsmall=0)))
)


#### One way random effect at the farmer level  -- no controls 
#plmb8<-plm(score ~ farmergen, data =fedatad,  model = "random", index = c("farmer_ID","shop_ID.x"),
        #   effect = "individual")


# #### Two way random effect at the dealer level and farmer level   -- no controls
# 
lmerb8<-lmer(score~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
lmerb9<-lmer(general~farmergen  + (1 | shop_ID.x) + (1 | farmer_ID), data = fedatad)
lmerb10<-lmer(yield ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
lmerb11<-lmer(drought_resistent ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
lmerb12<-lmer(disease_resistent ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
lmerb13<-lmer(early_maturing ~farmergen  + (1 | shop_ID.x) + (1 | farmer_ID), data = fedatad)
lmerb14<-lmer(germination ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
# 
# #tab_model(lmerb8, lmerb9, lmerb10, lmerb11, lmerb12, lmerb13, lmerb14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# 
# lmer_two5<- rbind( c((format(round(sum(coef(summary(lmerb8))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb9))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb10))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb11))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb12))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb13))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb14))[1,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerb8))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb9))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb10))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb11))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb12))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb13))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb14))[1,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerb8))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb9))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb10))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb11))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb12))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb13))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb14))[1,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerb8))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb9))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb10))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb11))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb12))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb13))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerb14))[2,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerb8))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb9))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb10))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb11))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb12))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb13))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb14))[2,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerb8))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb9))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb10))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb11))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb12))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb13))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerb14))[2,3],digits=3),nsmall=0))),
# 
# 
#                    c((format(round(summary(lmerb8)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb9)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb10)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb11)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb12)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb13)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb14)$r.squared[1],digits=3),nsmall=0))),
#                    c((format(round(summary(lmerb8)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb9)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb10)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb11)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb12)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb13)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerb14)$r.squared[2],digits=3),nsmall=0))),
#                    c((format(round(nobs(lmerb8),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerb9),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerb10),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerb11),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerb12),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerb13),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerb14),digits=3),nsmall=0)))
# )
# 
# 
# 
# #### Two way random effect at the dealer level and farmer level --- with controls --- controls are farmer and dealer characteristics 
# 
lmerbc8<-lmer(score~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbc9<-lmer(general~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbc10<-lmer(yield~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbc11<-lmer(drought_resistent ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbc12<-lmer(disease_resistent~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbc13<-lmer(early_maturing~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbc14<-lmer(germination~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)
# 
# #tab_model(lmerbc8, lmerbc9, lmerbc10, lmerbc11, lmerbc12, lmerbc13, lmerbc14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# lmer_two6<- rbind( c((format(round(sum(coef(summary(lmerbc8))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[1,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[1,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[1,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[2,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[2,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[2,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[3,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[3,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[3,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[4,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[4,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[4,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[5,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[5,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[5,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[6,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[6,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[6,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[7,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[7,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[7,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[8,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[8,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[8,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[9,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[9,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[9,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[10,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[10,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[10,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[11,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[11,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[11,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[12,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[12,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[12,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[13,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[13,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[13,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[14,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[14,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[14,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[15,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[15,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[15,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[16,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[16,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[16,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[17,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[17,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[17,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[18,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[18,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[18,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[19,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[19,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[19,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[20,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[20,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[20,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[21,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[21,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[21,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[22,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[22,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[22,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[23,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[23,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[23,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[24,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[24,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[24,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbc8))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc9))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc10))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc11))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc12))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc13))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbc14))[25,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[25,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbc8))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc9))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc10))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc11))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc12))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc13))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbc14))[25,3],digits=3),nsmall=0))),
# 
# 
#                    c((format(round(summary(lmerbc8)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc9)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc10)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc11)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc12)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc13)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc14)$r.squared[1],digits=3),nsmall=0))),
#                    c((format(round(summary(lmerbc8)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc9)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc10)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc11)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc12)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc13)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbc14)$r.squared[2],digits=3),nsmall=0))),
#                    c((format(round(nobs(lmerbc8),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbc9),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbc10),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbc11),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbc12),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbc13),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbc14),digits=3),nsmall=0)))
# )


#### One way random effect at the dealer level  -- with controls -- controls are only farmer characteristics 
plmbc1<-plm(score ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbc1<-coeftest(plmbc1, vcovHC(plmbc1, type = "HC0", cluster = "time"))
summary(plmbc1)
coefbc1

plmbc2<-plm(general ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbc2<-coeftest(plmbc2, vcovHC(plmbc2, type = "HC0", cluster = "time"))
summary(plmbc2)
coefbc2

plmbc3<-plm(yield ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbc3<-coeftest(plmbc3, vcovHC(plmbc3, type = "HC0", cluster = "time"))
summary(plmbc3)
coefbc3

plmbc4<-plm(drought_resistent ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbc4<-coeftest(plmbc4,  vcovHC(plmbc4, type = "HC0", cluster = "time"))
summary(plmbc4)
coefbc4

plmbc5<-plm(disease_resistent ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbc5<-coeftest(plmbc5,  vcovHC(plmbc5, type = "HC0", cluster = "time"))
summary(plmbc5)
coefbc5

plmbc6<-plm(early_maturing ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbc6<-coeftest(plmbc6,  vcovHC(plmbc6, type = "HC0", cluster = "time"))
summary(plmbc6)
coefbc6

plmbc7<-plm(germination ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbc7<-coeftest(plmbc7,  vcovHC(plmbc7, type = "HC0", cluster = "time"))
summary(plmbc7)
coefbc7

tab_model(plmbc1, plmbc2,plmbc3, plmbc4, plmbc5, plmbc6, plmbc7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)

ranbc1<- rbind( c((format(round(sum(coefbc1[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc2[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc3[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc4[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc5[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc6[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc7[1,1]),digits=3),nsmall=0))),
               c((format(round(coefbc1[1,2],digits=3),nsmall=0)),
                 (format(round(coefbc2[1,2],digits=3),nsmall=0)),
                 (format(round(coefbc3[1,2],digits=3),nsmall=0)),
                 (format(round(coefbc4[1,2],digits=3),nsmall=0)),
                 (format(round(coefbc5[1,2],digits=3),nsmall=0)),
                 (format(round(coefbc6[1,2],digits=3),nsmall=0)),
                 (format(round(coefbc7[1,2],digits=3),nsmall=0))),
               c((format(round(coefbc1[1,4],digits=3),nsmall=0)),
                 (format(round(coefbc2[1,4],digits=3),nsmall=0)),
                 (format(round(coefbc3[1,4],digits=3),nsmall=0)),
                 (format(round(coefbc4[1,4],digits=3),nsmall=0)),
                 (format(round(coefbc5[1,4],digits=3),nsmall=0)),
                 (format(round(coefbc6[1,4],digits=3),nsmall=0)),
                 (format(round(coefbc7[1,4],digits=3),nsmall=0))),
              
               c((format(round(sum(coefbc1[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc2[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc3[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc4[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc5[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc6[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc7[2,1]),digits=3),nsmall=0))),
               c((format(round(coefbc1[2,2],digits=3),nsmall=0)),
                 (format(round(coefbc2[2,2],digits=3),nsmall=0)),
                 (format(round(coefbc3[2,2],digits=3),nsmall=0)),
                 (format(round(coefbc4[2,2],digits=3),nsmall=0)),
                 (format(round(coefbc5[2,2],digits=3),nsmall=0)),
                 (format(round(coefbc6[2,2],digits=3),nsmall=0)),
                 (format(round(coefbc7[2,2],digits=3),nsmall=0))),
               c((format(round(coefbc1[2,4],digits=3),nsmall=0)),
                 (format(round(coefbc2[2,4],digits=3),nsmall=0)),
                 (format(round(coefbc3[2,4],digits=3),nsmall=0)),
                 (format(round(coefbc4[2,4],digits=3),nsmall=0)),
                 (format(round(coefbc5[2,4],digits=3),nsmall=0)),
                 (format(round(coefbc6[2,4],digits=3),nsmall=0)),
                 (format(round(coefbc7[2,4],digits=3),nsmall=0))),
               
               c((format(round(sum(coefbc1[3,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc2[3,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc3[3,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc4[3,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc5[3,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc6[3,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc7[3,1]),digits=3),nsmall=0))),
               c((format(round(coefbc1[3,2],digits=3),nsmall=0)),
                 (format(round(coefbc2[3,2],digits=3),nsmall=0)),
                 (format(round(coefbc3[3,2],digits=3),nsmall=0)),
                 (format(round(coefbc4[3,2],digits=3),nsmall=0)),
                 (format(round(coefbc5[3,2],digits=3),nsmall=0)),
                 (format(round(coefbc6[3,2],digits=3),nsmall=0)),
                 (format(round(coefbc7[3,2],digits=3),nsmall=0))),
               c((format(round(coefbc1[3,4],digits=3),nsmall=0)),
                 (format(round(coefbc2[3,4],digits=3),nsmall=0)),
                 (format(round(coefbc3[3,4],digits=3),nsmall=0)),
                 (format(round(coefbc4[3,4],digits=3),nsmall=0)),
                 (format(round(coefbc5[3,4],digits=3),nsmall=0)),
                 (format(round(coefbc6[3,4],digits=3),nsmall=0)),
                 (format(round(coefbc7[3,4],digits=3),nsmall=0))),
               
               c((format(round(sum(coefbc1[4,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc2[4,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc3[4,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc4[4,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc5[4,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc6[4,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc7[4,1]),digits=3),nsmall=0))),
               c((format(round(coefbc1[4,2],digits=3),nsmall=0)),
                 (format(round(coefbc2[4,2],digits=3),nsmall=0)),
                 (format(round(coefbc3[4,2],digits=3),nsmall=0)),
                 (format(round(coefbc4[4,2],digits=3),nsmall=0)),
                 (format(round(coefbc5[4,2],digits=3),nsmall=0)),
                 (format(round(coefbc6[4,2],digits=3),nsmall=0)),
                 (format(round(coefbc7[4,2],digits=3),nsmall=0))),
               c((format(round(coefbc1[4,4],digits=3),nsmall=0)),
                 (format(round(coefbc2[4,4],digits=3),nsmall=0)),
                 (format(round(coefbc3[4,4],digits=3),nsmall=0)),
                 (format(round(coefbc4[4,4],digits=3),nsmall=0)),
                 (format(round(coefbc5[4,4],digits=3),nsmall=0)),
                 (format(round(coefbc6[4,4],digits=3),nsmall=0)),
                 (format(round(coefbc7[4,4],digits=3),nsmall=0))),
               
               c((format(round(sum(coefbc1[5,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc2[5,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc3[5,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc4[5,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc5[5,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc6[5,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc7[5,1]),digits=3),nsmall=0))),
               c((format(round(coefbc1[5,2],digits=3),nsmall=0)),
                 (format(round(coefbc2[5,2],digits=3),nsmall=0)),
                 (format(round(coefbc3[5,2],digits=3),nsmall=0)),
                 (format(round(coefbc4[5,2],digits=3),nsmall=0)),
                 (format(round(coefbc5[5,2],digits=3),nsmall=0)),
                 (format(round(coefbc6[5,2],digits=3),nsmall=0)),
                 (format(round(coefbc7[5,2],digits=3),nsmall=0))),
               c((format(round(coefbc1[5,4],digits=3),nsmall=0)),
                 (format(round(coefbc2[5,4],digits=3),nsmall=0)),
                 (format(round(coefbc3[5,4],digits=3),nsmall=0)),
                 (format(round(coefbc4[5,4],digits=3),nsmall=0)),
                 (format(round(coefbc5[5,4],digits=3),nsmall=0)),
                 (format(round(coefbc6[5,4],digits=3),nsmall=0)),
                 (format(round(coefbc7[5,4],digits=3),nsmall=0))),
               
               c((format(round(sum(coefbc1[6,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc2[6,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc3[6,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc4[6,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc5[6,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc6[6,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbc7[6,1]),digits=3),nsmall=0))),
               c((format(round(coefbc1[6,2],digits=3),nsmall=0)),
                 (format(round(coefbc2[6,2],digits=3),nsmall=0)),
                 (format(round(coefbc3[6,2],digits=3),nsmall=0)),
                 (format(round(coefbc4[6,2],digits=3),nsmall=0)),
                 (format(round(coefbc5[6,2],digits=3),nsmall=0)),
                 (format(round(coefbc6[6,2],digits=3),nsmall=0)),
                 (format(round(coefbc7[6,2],digits=3),nsmall=0))),
               c((format(round(coefbc1[6,4],digits=3),nsmall=0)),
                 (format(round(coefbc2[6,4],digits=3),nsmall=0)),
                 (format(round(coefbc3[6,4],digits=3),nsmall=0)),
                 (format(round(coefbc4[6,4],digits=3),nsmall=0)),
                 (format(round(coefbc5[6,4],digits=3),nsmall=0)),
                 (format(round(coefbc6[6,4],digits=3),nsmall=0)),
                 (format(round(coefbc7[6,4],digits=3),nsmall=0))),
              
              
               c((format(round(summary(plmbc1)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbc2)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbc3)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbc4)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbc5)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbc6)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbc7)$r.squared[1],digits=3),nsmall=0))),
               c((format(round(summary(plmbc1)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbc2)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbc3)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbc4)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbc5)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbc6)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbc7)$r.squared[2],digits=3),nsmall=0))),
               c((format(round(nobs(plmbc1),digits=3),nsmall=0)),
                 (format(round(nobs(plmbc2),digits=3),nsmall=0)),
                 (format(round(nobs(plmbc3),digits=3),nsmall=0)),
                 (format(round(nobs(plmbc4),digits=3),nsmall=0)),
                 (format(round(nobs(plmbc5),digits=3),nsmall=0)),
                 (format(round(nobs(plmbc6),digits=3),nsmall=0)),
                 (format(round(nobs(plmbc7),digits=3),nsmall=0)))
)


######### NON SEED

#### One way random effect at the dealer level  -- no controls 

plmbn1<-plm(overall_rating ~ farmergen , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbn1<-coeftest(plmbn1,  vcovHC(plmbn1, type = "HC0", cluster = "time"))
summary(plmbn1)
coefbn1

plmbn2<-plm(general_rating_nonseed ~ farmergen , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbn2<-coeftest(plmbn2,  vcovHC(plmbn2, type = "HC0", cluster = "time"))
summary(plmbn2)
coefbn2

plmbn3<-plm(location ~ farmergen , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbn3<-coeftest(plmbn3,  vcovHC(plmbn3, type = "HC0", cluster = "time"))
summary(plmbn3)
coefbn3

plmbn4<-plm(price ~ farmergen , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbn4<-coeftest(plmbn4,  vcovHC(plmbn4, type = "HC0", cluster = "time"))
summary(plmbn4)
coefbn4

plmbn5<-plm(quality ~ farmergen , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbn5<-coeftest(plmbn5,  vcovHC(plmbn5, type = "HC0", cluster = "time"))
summary(plmbn5)
coefbn5

plmbn6<-plm(stock ~ farmergen , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbn6<-coeftest(plmbn6,  vcovHC(plmbn6, type = "HC0", cluster = "time"))
summary(plmbn6)
coefbn6

plmbn7<-plm(reputation ~ farmergen , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
            effect = "individual")
coefbn7<-coeftest(plmbn7,  vcovHC(plmbn7, type = "HC0", cluster = "time"))
summary(plmbn7)
coefbn7

tab_model(plmbn1, plmbn2,plmbn3, plmbn4, plmbn5, plmbn6, plmbn7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)


ranbn1<- rbind( c((format(round(sum(coefbn1[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn2[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn3[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn4[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn5[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn6[1,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn7[1,1]),digits=3),nsmall=0))),
               c((format(round(coefbn1[1,2],digits=3),nsmall=0)),
                 (format(round(coefbn2[1,2],digits=3),nsmall=0)),
                 (format(round(coefbn3[1,2],digits=3),nsmall=0)),
                 (format(round(coefbn4[1,2],digits=3),nsmall=0)),
                 (format(round(coefbn5[1,2],digits=3),nsmall=0)),
                 (format(round(coefbn6[1,2],digits=3),nsmall=0)),
                 (format(round(coefbn7[1,2],digits=3),nsmall=0))),
               c((format(round(coefbn1[1,4],digits=3),nsmall=0)),
                 (format(round(coefbn2[1,4],digits=3),nsmall=0)),
                 (format(round(coefbn3[1,4],digits=3),nsmall=0)),
                 (format(round(coefbn4[1,4],digits=3),nsmall=0)),
                 (format(round(coefbn5[1,4],digits=3),nsmall=0)),
                 (format(round(coefbn6[1,4],digits=3),nsmall=0)),
                 (format(round(coefbn7[1,4],digits=3),nsmall=0))),
               
               c((format(round(sum(coefbn1[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn2[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn3[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn4[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn5[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn6[2,1]),digits=3),nsmall=0)),
                 (format(round(sum(coefbn7[2,1]),digits=3),nsmall=0))),
               c((format(round(coefbn1[2,2],digits=3),nsmall=0)),
                 (format(round(coefbn2[2,2],digits=3),nsmall=0)),
                 (format(round(coefbn3[2,2],digits=3),nsmall=0)),
                 (format(round(coefbn4[2,2],digits=3),nsmall=0)),
                 (format(round(coefbn5[2,2],digits=3),nsmall=0)),
                 (format(round(coefbn6[2,2],digits=3),nsmall=0)),
                 (format(round(coefbn7[2,2],digits=3),nsmall=0))),
               c((format(round(coefbn1[2,4],digits=3),nsmall=0)),
                 (format(round(coefbn2[2,4],digits=3),nsmall=0)),
                 (format(round(coefbn3[2,4],digits=3),nsmall=0)),
                 (format(round(coefbn4[2,4],digits=3),nsmall=0)),
                 (format(round(coefbn5[2,4],digits=3),nsmall=0)),
                 (format(round(coefbn6[2,4],digits=3),nsmall=0)),
                 (format(round(coefbn7[2,4],digits=3),nsmall=0))),
               
               
               c((format(round(summary(plmbn1)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbn2)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbn3)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbn4)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbn5)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbn6)$r.squared[1],digits=3),nsmall=0)),
                 (format(round(summary(plmbn7)$r.squared[1],digits=3),nsmall=0))),
               c((format(round(summary(plmbn1)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbn2)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbn3)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbn4)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbn5)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbn6)$r.squared[2],digits=3),nsmall=0)),
                 (format(round(summary(plmbn7)$r.squared[2],digits=3),nsmall=0))),
               c((format(round(nobs(plmbn1),digits=3),nsmall=0)),
                 (format(round(nobs(plmbn2),digits=3),nsmall=0)),
                 (format(round(nobs(plmbn3),digits=3),nsmall=0)),
                 (format(round(nobs(plmbn4),digits=3),nsmall=0)),
                 (format(round(nobs(plmbn5),digits=3),nsmall=0)),
                 (format(round(nobs(plmbn6),digits=3),nsmall=0)),
                 (format(round(nobs(plmbn7),digits=3),nsmall=0)))
)


# #### Two way random effect at the dealer level and farmer level  -- no controls 
# 
lmerbn8<-lmer(overall_rating~farmergen  + (1 | shop_ID.x) + (1 | farmer_ID), data = fedatad)
lmerbn9<-lmer(general_rating_nonseed ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
lmerbn10<-lmer(location ~farmergen  + (1 | shop_ID.x) + (1 | farmer_ID), data = fedatad)
lmerbn11<-lmer(price ~farmergen  + (1 | shop_ID.x) + (1 | farmer_ID), data = fedatad)
lmerbn12<-lmer(quality ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
lmerbn13<-lmer(stock ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
lmerbn14<-lmer(reputation ~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) , data = fedatad)
# 
# #tab_model(lmerbn8, lmerbn9,lmerbn10,lmerbn11,lmerbn12,lmerbn13,lmerbn14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# 
# lmer_two7<- rbind( c((format(round(sum(coef(summary(lmerbn8))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn9))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn10))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn11))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn12))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn13))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn14))[1,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbn8))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn9))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn10))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn11))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn12))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn13))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn14))[1,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbn8))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn9))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn10))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn11))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn12))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn13))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn14))[1,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbn8))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn9))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn10))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn11))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn12))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn13))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbn14))[2,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbn8))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn9))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn10))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn11))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn12))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn13))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn14))[2,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbn8))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn9))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn10))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn11))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn12))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn13))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbn14))[2,3],digits=3),nsmall=0))),
# 
# 
#                    c((format(round(summary(lmerbn8)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn9)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn10)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn11)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn12)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn13)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn14)$r.squared[1],digits=3),nsmall=0))),
#                    c((format(round(summary(lmerbn8)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn9)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn10)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn11)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn12)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn13)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbn14)$r.squared[2],digits=3),nsmall=0))),
#                    c((format(round(nobs(lmerbn8),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbn9),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbn10),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbn11),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbn12),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbn13),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbn14),digits=3),nsmall=0)))
# )
# 
# 
# #### Two way random effect at the dealer level and farmer level --- with controls --- controls are farmer and dealer characteristics 
# 
lmerbnc8<-lmer(overall_rating~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbnc9<-lmer(general_rating_nonseed~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbnc10<-lmer(location~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = fedatad)

lmerbnc11<-lmer(price~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbnc12<-lmer(quality~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)

lmerbnc13<-lmer(stock~farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4, data = fedatad)

lmerbnc14<-lmer(reputation ~ farmergen  + (1 | shop_ID.x)+ (1 | farmer_ID) +educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
+years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedatad)
# 
# #tab_model(lmerbnc8, lmerbnc9,lmerbnc10,lmerbnc11,lmerbnc12,lmerbnc13,lmerbnc14, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)
# 
# lmer_two8<- rbind( c((format(round(sum(coef(summary(lmerbnc8))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[1,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[1,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[1,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[1,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[1,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[1,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[2,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[2,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[2,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[2,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[2,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[2,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[3,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[3,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[3,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[3,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[3,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[3,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[4,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[4,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[4,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[4,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[4,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[4,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[5,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[5,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[5,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[5,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[5,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[5,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[6,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[6,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[6,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[6,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[6,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[6,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[7,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[7,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[7,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[7,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[7,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[7,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[8,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[8,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[8,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[8,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[8,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[8,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[9,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[9,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[9,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[9,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[9,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[9,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc10))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc11))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc13))[10,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc14))[10,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[10,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[10,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc10))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc11))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc13))[10,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc14))[10,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[11,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[11,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[11,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[11,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[11,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[11,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[11,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[11,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[11,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc11))[11,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[11,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[11,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[12,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[12,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[12,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmerbnc14))[12,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[12,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc11))[12,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[12,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[12,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[12,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[12,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[12,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[12,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[13,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[13,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[13,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmerbnc14))[13,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[13,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[13,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[13,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc14))[13,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[13,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[13,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[13,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[13,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[14,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[14,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[14,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[14,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[14,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[14,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[14,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[14,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[14,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc11))[14,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[14,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[14,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[15,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmerbnc11))[15,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[15,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmerbnc14))[15,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[15,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[15,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[15,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc14))[15,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[15,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[15,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[15,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc14))[15,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[16,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmerbnc11))[16,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[16,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmerbnc14))[16,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[16,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[16,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[16,2],digits=3),nsmall=0)),
#                       0,
#                      (format(round(coef(summary(lmerbnc14))[16,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[16,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[16,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[16,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[16,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[17,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[17,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[17,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[17,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[17,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[17,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[17,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[17,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[17,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc11))[17,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[17,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[17,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[18,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[18,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[18,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[18,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[18,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[18,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[18,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[18,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[18,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[18,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[18,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[18,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[19,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[19,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[19,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[19,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[19,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[19,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[19,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[19,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[19,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmerbnc11))[19,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[19,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[19,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[20,1]),digits=3),nsmall=0)),
#                     0),
#                      (format(round(sum(coef(summary(lmerbnc11))[20,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[20,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmerbnc14))[20,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[20,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[20,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[20,2],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmerbnc14))[20,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[20,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[20,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[20,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmerbnc14))[20,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[21,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[21,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[21,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmerbnc14))[21,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[21,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc11))[21,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[21,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[21,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[21,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[21,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[21,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmerbnc14))[21,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[22,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[22,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[22,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[22,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[22,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[22,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[22,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[22,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[22,3],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc11))[22,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[22,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[22,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[23,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc11))[23,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[23,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[23,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[23,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[23,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[23,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[23,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[23,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[23,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[23,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[23,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[24,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmerbnc11))[24,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[24,1]),digits=3),nsmall=0)),
#                     0,
#                      (format(round(sum(coef(summary(lmerbnc14))[24,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[24,2],digits=3),nsmall=0)),
#                      0,
#                      (format(round(coef(summary(lmerbnc11))[24,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[24,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[24,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[24,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[24,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[24,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[24,3],digits=3),nsmall=0))),
# 
#                    c((format(round(sum(coef(summary(lmerbnc8))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc9))[25,1]),digits=3),nsmall=0)),
#                      0,
#                      (format(round(sum(coef(summary(lmerbnc11))[25,1]),digits=3),nsmall=0)),
#                      (format(round(sum(coef(summary(lmerbnc12))[25,1]),digits=3),nsmall=0)),
#                    0,
#                      (format(round(sum(coef(summary(lmerbnc14))[25,1]),digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[25,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[25,2],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[25,2],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc14))[25,2],digits=3),nsmall=0))),
#                    c((format(round(coef(summary(lmerbnc8))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc9))[25,3],digits=3),nsmall=0)),
#                     0,
#                      (format(round(coef(summary(lmerbnc11))[25,3],digits=3),nsmall=0)),
#                      (format(round(coef(summary(lmerbnc12))[25,3],digits=3),nsmall=0)),
#                    0,
#                      (format(round(coef(summary(lmerbnc14))[25,3],digits=3),nsmall=0))),
# 
# 
#                    c((format(round(summary(lmerbnc8)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc9)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc10)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc11)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc12)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc13)$r.squared[1],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc14)$r.squared[1],digits=3),nsmall=0))),
#                    c((format(round(summary(lmerbnc8)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc9)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc10)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc11)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc12)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc13)$r.squared[2],digits=3),nsmall=0)),
#                      (format(round(summary(lmerbnc14)$r.squared[2],digits=3),nsmall=0))),
#                    c((format(round(nobs(lmerbnc8),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbnc9),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbnc10),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbnc11),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbnc12),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbnc13),digits=3),nsmall=0)),
#                      (format(round(nobs(lmerbnc14),digits=3),nsmall=0)))
# )
# 

#### One way random effect at the dealer level  -- with controls -- controls are farmer characteristics 

plmbnc1<-plm(overall_rating ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
             effect = "individual")
coefbnc1<-coeftest(plmbnc1,  vcovHC(plmbnc1, type = "HC0", cluster = "time"))
summary(plmbnc1)
coefbnc1

plmbnc2<-plm(general_rating_nonseed ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
             effect = "individual")
coefbnc2<-coeftest(plmbnc2,  vcovHC(plmbnc2, type = "HC0", cluster = "time"))
summary(plmbnc2)
coefbnc2

plmbnc3<-plm(location ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
             effect = "individual")
coefbnc3<-coeftest(plmbnc3,  vcovHC(plmbnc3, type = "HC0", cluster = "time"))
summary(plmbnc3)
coefbnc3

plmbnc4<-plm(price ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
             effect = "individual")
coefbnc4<-coeftest(plmbnc4,  vcovHC(plmbnc4, type = "HC0", cluster = "time"))
summary(plmbnc4)
coefbnc4

plmbnc5<-plm(quality ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
             effect = "individual")
coefbnc5<-coeftest(plmbnc5,  vcovHC(plmbnc5, type = "HC0", cluster = "time"))
summary(plmbnc5)
coefbnc5

plmbnc6<-plm(stock ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
             effect = "individual")
coefbnc6<-coeftest(plmbnc6,  vcovHC(plmbnc6, type = "HC0", cluster = "time"))
summary(plmbnc6)
coefbnc6

plmbnc7<-plm(reputation ~ farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8 , data =fedatad,  model = "random", index = c("shop_ID.x", "farmer_ID"),
             effect = "individual")
coefbnc7<-coeftest(plmbnc7,  vcovHC(plmbnc7, type = "HC0", cluster = "time"))
summary(plmbnc7)
coefbnc7

tab_model(plmbnc1, plmbnc2,plmbnc3, plmbnc4, plmbnc5, plmbnc6, plmbnc7, p.style = c("stars"), p.threshold = c(0.1, 0.05, 0.01), collapse.ci=TRUE)

ranbnc1<- rbind( c((format(round(sum(coefbnc1[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc2[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc3[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc4[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc5[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc6[1,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc7[1,1]),digits=3),nsmall=0))),
                c((format(round(coefbnc1[1,2],digits=3),nsmall=0)),
                  (format(round(coefbnc2[1,2],digits=3),nsmall=0)),
                  (format(round(coefbnc3[1,2],digits=3),nsmall=0)),
                  (format(round(coefbnc4[1,2],digits=3),nsmall=0)),
                  (format(round(coefbnc5[1,2],digits=3),nsmall=0)),
                  (format(round(coefbnc6[1,2],digits=3),nsmall=0)),
                  (format(round(coefbnc7[1,2],digits=3),nsmall=0))),
                c((format(round(coefbnc1[1,4],digits=3),nsmall=0)),
                  (format(round(coefbnc2[1,4],digits=3),nsmall=0)),
                  (format(round(coefbnc3[1,4],digits=3),nsmall=0)),
                  (format(round(coefbnc4[1,4],digits=3),nsmall=0)),
                  (format(round(coefbnc5[1,4],digits=3),nsmall=0)),
                  (format(round(coefbnc6[1,4],digits=3),nsmall=0)),
                  (format(round(coefbnc7[1,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefbnc1[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc2[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc3[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc4[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc5[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc6[2,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc7[2,1]),digits=3),nsmall=0))),
                c((format(round(coefbnc1[2,2],digits=3),nsmall=0)),
                  (format(round(coefbnc2[2,2],digits=3),nsmall=0)),
                  (format(round(coefbnc3[2,2],digits=3),nsmall=0)),
                  (format(round(coefbnc4[2,2],digits=3),nsmall=0)),
                  (format(round(coefbnc5[2,2],digits=3),nsmall=0)),
                  (format(round(coefbnc6[2,2],digits=3),nsmall=0)),
                  (format(round(coefbnc7[2,2],digits=3),nsmall=0))),
                c((format(round(coefbnc1[2,4],digits=3),nsmall=0)),
                  (format(round(coefbnc2[2,4],digits=3),nsmall=0)),
                  (format(round(coefbnc3[2,4],digits=3),nsmall=0)),
                  (format(round(coefbnc4[2,4],digits=3),nsmall=0)),
                  (format(round(coefbnc5[2,4],digits=3),nsmall=0)),
                  (format(round(coefbnc6[2,4],digits=3),nsmall=0)),
                  (format(round(coefbnc7[2,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefbnc1[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc2[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc3[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc4[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc5[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc6[3,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc7[3,1]),digits=3),nsmall=0))),
                c((format(round(coefbnc1[3,2],digits=3),nsmall=0)),
                  (format(round(coefbnc2[3,2],digits=3),nsmall=0)),
                  (format(round(coefbnc3[3,2],digits=3),nsmall=0)),
                  (format(round(coefbnc4[3,2],digits=3),nsmall=0)),
                  (format(round(coefbnc5[3,2],digits=3),nsmall=0)),
                  (format(round(coefbnc6[3,2],digits=3),nsmall=0)),
                  (format(round(coefbnc7[3,2],digits=3),nsmall=0))),
                c((format(round(coefbnc1[3,4],digits=3),nsmall=0)),
                  (format(round(coefbnc2[3,4],digits=3),nsmall=0)),
                  (format(round(coefbnc3[3,4],digits=3),nsmall=0)),
                  (format(round(coefbnc4[3,4],digits=3),nsmall=0)),
                  (format(round(coefbnc5[3,4],digits=3),nsmall=0)),
                  (format(round(coefbnc6[3,4],digits=3),nsmall=0)),
                  (format(round(coefbnc7[3,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefbnc1[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc2[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc3[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc4[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc5[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc6[4,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc7[4,1]),digits=3),nsmall=0))),
                c((format(round(coefbnc1[4,2],digits=3),nsmall=0)),
                  (format(round(coefbnc2[4,2],digits=3),nsmall=0)),
                  (format(round(coefbnc3[4,2],digits=3),nsmall=0)),
                  (format(round(coefbnc4[4,2],digits=3),nsmall=0)),
                  (format(round(coefbnc5[4,2],digits=3),nsmall=0)),
                  (format(round(coefbnc6[4,2],digits=3),nsmall=0)),
                  (format(round(coefbnc7[4,2],digits=3),nsmall=0))),
                c((format(round(coefbnc1[4,4],digits=3),nsmall=0)),
                  (format(round(coefbnc2[4,4],digits=3),nsmall=0)),
                  (format(round(coefbnc3[4,4],digits=3),nsmall=0)),
                  (format(round(coefbnc4[4,4],digits=3),nsmall=0)),
                  (format(round(coefbnc5[4,4],digits=3),nsmall=0)),
                  (format(round(coefbnc6[4,4],digits=3),nsmall=0)),
                  (format(round(coefbnc7[4,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefbnc1[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc2[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc3[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc4[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc5[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc6[5,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc7[5,1]),digits=3),nsmall=0))),
                c((format(round(coefbnc1[5,2],digits=3),nsmall=0)),
                  (format(round(coefbnc2[5,2],digits=3),nsmall=0)),
                  (format(round(coefbnc3[5,2],digits=3),nsmall=0)),
                  (format(round(coefbnc4[5,2],digits=3),nsmall=0)),
                  (format(round(coefbnc5[5,2],digits=3),nsmall=0)),
                  (format(round(coefbnc6[5,2],digits=3),nsmall=0)),
                  (format(round(coefbnc7[5,2],digits=3),nsmall=0))),
                c((format(round(coefbnc1[5,4],digits=3),nsmall=0)),
                  (format(round(coefbnc2[5,4],digits=3),nsmall=0)),
                  (format(round(coefbnc3[5,4],digits=3),nsmall=0)),
                  (format(round(coefbnc4[5,4],digits=3),nsmall=0)),
                  (format(round(coefbnc5[5,4],digits=3),nsmall=0)),
                  (format(round(coefbnc6[5,4],digits=3),nsmall=0)),
                  (format(round(coefbnc7[5,4],digits=3),nsmall=0))),
                
                c((format(round(sum(coefbnc1[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc2[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc3[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc4[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc5[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc6[6,1]),digits=3),nsmall=0)),
                  (format(round(sum(coefbnc7[6,1]),digits=3),nsmall=0))),
                c((format(round(coefbnc1[6,2],digits=3),nsmall=0)),
                  (format(round(coefbnc2[6,2],digits=3),nsmall=0)),
                  (format(round(coefbnc3[6,2],digits=3),nsmall=0)),
                  (format(round(coefbnc4[6,2],digits=3),nsmall=0)),
                  (format(round(coefbnc5[6,2],digits=3),nsmall=0)),
                  (format(round(coefbnc6[6,2],digits=3),nsmall=0)),
                  (format(round(coefbnc7[6,2],digits=3),nsmall=0))),
                c((format(round(coefbnc1[6,4],digits=3),nsmall=0)),
                  (format(round(coefbnc2[6,4],digits=3),nsmall=0)),
                  (format(round(coefbnc3[6,4],digits=3),nsmall=0)),
                  (format(round(coefbnc4[6,4],digits=3),nsmall=0)),
                  (format(round(coefbnc5[6,4],digits=3),nsmall=0)),
                  (format(round(coefbnc6[6,4],digits=3),nsmall=0)),
                  (format(round(coefbnc7[6,4],digits=3),nsmall=0))),
                
                
                c((format(round(summary(plmbnc1)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc2)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc3)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc4)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc5)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc6)$r.squared[1],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc7)$r.squared[1],digits=3),nsmall=0))),
                c((format(round(summary(plmbnc1)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc2)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc3)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc4)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc5)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc6)$r.squared[2],digits=3),nsmall=0)),
                  (format(round(summary(plmbnc7)$r.squared[2],digits=3),nsmall=0))),
                c((format(round(nobs(plmbnc1),digits=3),nsmall=0)),
                  (format(round(nobs(plmbnc2),digits=3),nsmall=0)),
                  (format(round(nobs(plmbnc3),digits=3),nsmall=0)),
                  (format(round(nobs(plmbnc4),digits=3),nsmall=0)),
                  (format(round(nobs(plmbnc5),digits=3),nsmall=0)),
                  (format(round(nobs(plmbnc6),digits=3),nsmall=0)),
                  (format(round(nobs(plmbnc7),digits=3),nsmall=0)))
)


###########################################################################################################################################
