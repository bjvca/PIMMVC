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
between_farmer$badlighting <- 0
between_farmer$badlighting[between_farmer$maize.owner.agree.temp.q78=="1"|between_farmer$maize.owner.agree.temp.q78=="3"]<-1
table(between_farmer$badlighting)

#Q79. On what surface are seed stored?
between_farmer$badstored <- 0
between_farmer$badstored[between_farmer$maize.owner.agree.temp.q79=="1"|between_farmer$maize.owner.agree.temp.q79=="2"]<-1
between_farmer$badstored[between_farmer$maize.owner.agree.temp.q79==96]<-NA
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
table(between_farmer$maize.owner.agree.q96)
between_farmer$complaint<- ifelse(between_farmer$maize.owner.agree.q96== 'Yes', 1, 0)  
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

m9<-lm(score~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
se9 <- sqrt(diag(vcov(m9)))

#m10<-lm(quality~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14+dealer_age +dealer_educ +tarmac_dealer +
 #        murram_dealer+ farm_inputs+years_shop +dedicatedarea +pestprob +roof_insu+ wall_heatproof +ventilation +
  #       badlighting +badstored+ open_storage+ cert+ shop_rate+ complaint + temp +leakproof, data = bfm)
#se10 <- sqrt(diag(vcov(m10)))

m11<-lm(general~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
se11 <- sqrt(diag(vcov(m11)))

m12<-lm(yield~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
se12 <- sqrt(diag(vcov(m12)))

m13<-lm(drought_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
se13<- sqrt(diag(vcov(m13)))

m14<-lm(disease_resistent ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
se14 <- sqrt(diag(vcov(m14)))

m15<-lm(early_maturing ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
se15<- sqrt(diag(vcov(m15)))

m16<-lm(germination ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
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

n9<-lm(overall_rating~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
sen9 <- sqrt(diag(vcov(n9)))

n10<-lm(general_rating_nonseed~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
sen10 <- sqrt(diag(vcov(n10)))

n11<-lm(location~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
sen11 <- sqrt(diag(vcov(n11)))

n12<-lm(price~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
sen12 <- sqrt(diag(vcov(n12)))

n13<-lm(quality ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
sen13<- sqrt(diag(vcov(n13)))

n14<-lm(stock ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
sen14 <- sqrt(diag(vcov(n14)))

n15<-lm(reputation ~gender_avg + educ_f + married + Check2.check.maize.q8 + Check2.check.maize.q14, data = bfm)
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

#Q78. Lighting conditions in area where seed is stored?
avg$badlighting <- 0
avg$badlighting[avg$maize.owner.agree.temp.q78=="1"|avg$maize.owner.agree.temp.q78=="3"]<-1
table(avg$badlighting)

#Q79. On what surface are seed stored?
avg$badstored <- 0
avg$badstored[avg$maize.owner.agree.temp.q79=="1"|avg$maize.owner.agree.temp.q79=="2"]<-1
avg$badstored[avg$maize.owner.agree.temp.q79==96]<-NA
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
table(avg$maize.owner.agree.q96)
avg$complaint<- ifelse(avg$maize.owner.agree.q96== 'Yes', 1, 0)  
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

fedata <- merge(rating_dyads, baseline_dealer, by="shop_ID")

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

#Q78. Lighting conditions in area where seed is stored?
fedata$badlighting <- 0
fedata$badlighting[fedata$maize.owner.agree.temp.q78=="1"|fedata$maize.owner.agree.temp.q78=="3"]<-1
table(fedata$badlighting)

#Q79. On what surface are seed stored?
fedata$badstored <- 0
fedata$badstored[fedata$maize.owner.agree.temp.q79=="1"|fedata$maize.owner.agree.temp.q79=="2"]<-1
fedata$badstored[fedata$maize.owner.agree.temp.q79==96]<-NA
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
table(fedata$maize.owner.agree.q96)
fedata$complaint<- ifelse(fedata$maize.owner.agree.q96== 'Yes', 1, 0)  
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

#plm2<-plm(quality~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
#i2<-mean(fixef(plm2))
#seplm2<- sqrt(diag(vcov(plm2)))

plm3<-plm(general~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i3<-mean(fixef(plm3))
seplm3<- sqrt(diag(vcov(plm3)))

plm4<-plm(yield~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i4<-mean(fixef(plm4))
seplm4<- sqrt(diag(vcov(plm4)))

plm5<-plm(drought_resistent~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i5<-mean(fixef(plm5))
seplm5<- sqrt(diag(vcov(plm5)))

plm6<-plm(disease_resistent~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i6<-mean(fixef(plm6))
seplm6<- sqrt(diag(vcov(plm6)))

plm7<-plm(early_maturing~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i7<-mean(fixef(plm7))
seplm7<- sqrt(diag(vcov(plm7)))

plm8<-plm(germination~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i8<-mean(fixef(plm8))
seplm8<- sqrt(diag(vcov(plm8)))

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
           c((format(round(seplm1[1],digits=3),nsmall=0)),
            # (format(round(seplm2[1],digits=3),nsmall=0)),
             (format(round(seplm3[1],digits=3),nsmall=0)),
             (format(round(seplm4[1],digits=3),nsmall=0)),
             (format(round(seplm5[1],digits=3),nsmall=0)),
             (format(round(seplm6[1],digits=3),nsmall=0)),
             (format(round(seplm7[1],digits=3),nsmall=0)),
             (format(round(seplm8[1],digits=3),nsmall=0))),
           c((format(round(summary(plm1)$coefficients[1,4],digits=3),nsmall=0)),
          #   (format(round(summary(plm2)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm3)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm4)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm5)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm6)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm7)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm8)$coefficients[1,4],digits=3),nsmall=0))),
           
           
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

plm12<-plm(yield~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i12<-mean(fixef(plm12))
seplm12<- sqrt(diag(vcov(plm12)))

plm13<-plm(drought_resistent~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i13<-mean(fixef(plm13))
seplm13<- sqrt(diag(vcov(plm13)))

plm14<-plm(disease_resistent~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i14<-mean(fixef(plm14))
seplm14<- sqrt(diag(vcov(plm14)))

plm15<-plm(early_maturing~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i15<-mean(fixef(plm15))
seplm15<- sqrt(diag(vcov(plm15)))

plm16<-plm(germination~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
i16<-mean(fixef(plm16))
seplm16<- sqrt(diag(vcov(plm16)))



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
           c((format(round(seplm9[1],digits=3),nsmall=0)),
            # (format(round(seplm10[1],digits=3),nsmall=0)),
             (format(round(seplm11[1],digits=3),nsmall=0)),
             (format(round(seplm12[1],digits=3),nsmall=0)),
             (format(round(seplm13[1],digits=3),nsmall=0)),
             (format(round(seplm14[1],digits=3),nsmall=0)),
             (format(round(seplm15[1],digits=3),nsmall=0)),
             (format(round(seplm16[1],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[1,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[1,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[1,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[2]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[2]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[2]),digits=3),nsmall=0))),
           c((format(round(seplm9[2],digits=3),nsmall=0)),
             #(format(round(seplm10[2],digits=3),nsmall=0)),
             (format(round(seplm11[2],digits=3),nsmall=0)),
             (format(round(seplm12[2],digits=3),nsmall=0)),
             (format(round(seplm13[2],digits=3),nsmall=0)),
             (format(round(seplm14[2],digits=3),nsmall=0)),
             (format(round(seplm15[2],digits=3),nsmall=0)),
             (format(round(seplm16[2],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[2,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[2,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[2,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[3]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[3]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[3]),digits=3),nsmall=0))),
           c((format(round(seplm9[3],digits=3),nsmall=0)),
            # (format(round(seplm10[3],digits=3),nsmall=0)),
             (format(round(seplm11[3],digits=3),nsmall=0)),
             (format(round(seplm12[3],digits=3),nsmall=0)),
             (format(round(seplm13[3],digits=3),nsmall=0)),
             (format(round(seplm14[3],digits=3),nsmall=0)),
             (format(round(seplm15[3],digits=3),nsmall=0)),
             (format(round(seplm16[3],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[3,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[3,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[3,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[4]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[4]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[4]),digits=3),nsmall=0))),
           c((format(round(seplm9[4],digits=3),nsmall=0)),
            # (format(round(seplm10[4],digits=3),nsmall=0)),
             (format(round(seplm11[4],digits=3),nsmall=0)),
             (format(round(seplm12[4],digits=3),nsmall=0)),
             (format(round(seplm13[4],digits=3),nsmall=0)),
             (format(round(seplm14[4],digits=3),nsmall=0)),
             (format(round(seplm15[4],digits=3),nsmall=0)),
             (format(round(seplm16[4],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[4,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[4,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[4,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[5]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[5]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[5]),digits=3),nsmall=0))),
           c((format(round(seplm9[5],digits=3),nsmall=0)),
            # (format(round(seplm10[5],digits=3),nsmall=0)),
             (format(round(seplm11[5],digits=3),nsmall=0)),
             (format(round(seplm12[5],digits=3),nsmall=0)),
             (format(round(seplm13[5],digits=3),nsmall=0)),
             (format(round(seplm14[5],digits=3),nsmall=0)),
             (format(round(seplm15[5],digits=3),nsmall=0)),
             (format(round(seplm16[5],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[5,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[5,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[5,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[6]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[6]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[6]),digits=3),nsmall=0))),
           c((format(round(seplm9[6],digits=3),nsmall=0)),
            # (format(round(seplm10[6],digits=3),nsmall=0)),
             (format(round(seplm11[6],digits=3),nsmall=0)),
             (format(round(seplm12[6],digits=3),nsmall=0)),
             (format(round(seplm13[6],digits=3),nsmall=0)),
             (format(round(seplm14[6],digits=3),nsmall=0)),
             (format(round(seplm15[6],digits=3),nsmall=0)),
             (format(round(seplm16[6],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[6,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[6,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[6,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[7]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[7]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[7]),digits=3),nsmall=0))),
           c((format(round(seplm9[7],digits=3),nsmall=0)),
           #  (format(round(seplm10[7],digits=3),nsmall=0)),
             (format(round(seplm11[7],digits=3),nsmall=0)),
             (format(round(seplm12[7],digits=3),nsmall=0)),
             (format(round(seplm13[7],digits=3),nsmall=0)),
             (format(round(seplm14[7],digits=3),nsmall=0)),
             (format(round(seplm15[7],digits=3),nsmall=0)),
             (format(round(seplm16[7],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[7,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[7,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[7,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[8]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[8]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[8]),digits=3),nsmall=0))),
           c((format(round(seplm9[8],digits=3),nsmall=0)),
            # (format(round(seplm10[8],digits=3),nsmall=0)),
             (format(round(seplm11[8],digits=3),nsmall=0)),
             (format(round(seplm12[8],digits=3),nsmall=0)),
             (format(round(seplm13[8],digits=3),nsmall=0)),
             (format(round(seplm14[8],digits=3),nsmall=0)),
             (format(round(seplm15[8],digits=3),nsmall=0)),
             (format(round(seplm16[8],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[8,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[8,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[8,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[9]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[9]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[9]),digits=3),nsmall=0))),
           c((format(round(seplm9[9],digits=3),nsmall=0)),
            # (format(round(seplm10[9],digits=3),nsmall=0)),
             (format(round(seplm11[9],digits=3),nsmall=0)),
             (format(round(seplm12[9],digits=3),nsmall=0)),
             (format(round(seplm13[9],digits=3),nsmall=0)),
             (format(round(seplm14[9],digits=3),nsmall=0)),
             (format(round(seplm15[9],digits=3),nsmall=0)),
             (format(round(seplm16[9],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[9,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[9,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[9,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[10]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[10]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[10]),digits=3),nsmall=0))),
           c((format(round(seplm9[10],digits=3),nsmall=0)),
            # (format(round(seplm10[10],digits=3),nsmall=0)),
             (format(round(seplm11[10],digits=3),nsmall=0)),
             (format(round(seplm12[10],digits=3),nsmall=0)),
             (format(round(seplm13[10],digits=3),nsmall=0)),
             (format(round(seplm14[10],digits=3),nsmall=0)),
             (format(round(seplm15[10],digits=3),nsmall=0)),
             (format(round(seplm16[10],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[10,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[10,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[10,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[11]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[11]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[11]),digits=3),nsmall=0))),
           c((format(round(seplm9[11],digits=3),nsmall=0)),
           #  (format(round(seplm10[11],digits=3),nsmall=0)),
             (format(round(seplm11[11],digits=3),nsmall=0)),
             (format(round(seplm12[11],digits=3),nsmall=0)),
             (format(round(seplm13[11],digits=3),nsmall=0)),
             (format(round(seplm14[11],digits=3),nsmall=0)),
             (format(round(seplm15[11],digits=3),nsmall=0)),
             (format(round(seplm16[11],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[11,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[11,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[11,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[12]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[12]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[12]),digits=3),nsmall=0))),
           c((format(round(seplm9[12],digits=3),nsmall=0)),
            # (format(round(seplm10[12],digits=3),nsmall=0)),
             (format(round(seplm11[12],digits=3),nsmall=0)),
             (format(round(seplm12[12],digits=3),nsmall=0)),
             (format(round(seplm13[12],digits=3),nsmall=0)),
             (format(round(seplm14[12],digits=3),nsmall=0)),
             (format(round(seplm15[12],digits=3),nsmall=0)),
             (format(round(seplm16[12],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[12,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[12,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[12,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[13]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[13]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[13]),digits=3),nsmall=0))),
           c((format(round(seplm9[13],digits=3),nsmall=0)),
            # (format(round(seplm10[13],digits=3),nsmall=0)),
             (format(round(seplm11[13],digits=3),nsmall=0)),
             (format(round(seplm12[13],digits=3),nsmall=0)),
             (format(round(seplm13[13],digits=3),nsmall=0)),
             (format(round(seplm14[13],digits=3),nsmall=0)),
             (format(round(seplm15[13],digits=3),nsmall=0)),
             (format(round(seplm16[13],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[13,4],digits=3),nsmall=0)),
           #  (format(round(summary(plm10)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[13,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[13,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[14]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[14]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[14]),digits=3),nsmall=0))),
           c((format(round(seplm9[14],digits=3),nsmall=0)),
           #  (format(round(seplm10[14],digits=3),nsmall=0)),
             (format(round(seplm11[14],digits=3),nsmall=0)),
             (format(round(seplm12[14],digits=3),nsmall=0)),
             (format(round(seplm13[14],digits=3),nsmall=0)),
             (format(round(seplm14[14],digits=3),nsmall=0)),
             (format(round(seplm15[14],digits=3),nsmall=0)),
             (format(round(seplm16[14],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[14,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[14,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[14,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[15]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[15]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[15]),digits=3),nsmall=0))),
           c((format(round(seplm9[15],digits=3),nsmall=0)),
         #    (format(round(seplm10[15],digits=3),nsmall=0)),
             (format(round(seplm11[15],digits=3),nsmall=0)),
             (format(round(seplm12[15],digits=3),nsmall=0)),
             (format(round(seplm13[15],digits=3),nsmall=0)),
             (format(round(seplm14[15],digits=3),nsmall=0)),
             (format(round(seplm15[15],digits=3),nsmall=0)),
             (format(round(seplm16[15],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[15,4],digits=3),nsmall=0)),
          #   (format(round(summary(plm10)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[15,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[15,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[16]),digits=3),nsmall=0)),
          #   (format(round(sum(plm10$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[16]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[16]),digits=3),nsmall=0))),
           c((format(round(seplm9[16],digits=3),nsmall=0)),
         #    (format(round(seplm10[16],digits=3),nsmall=0)),
             (format(round(seplm11[16],digits=3),nsmall=0)),
             (format(round(seplm12[16],digits=3),nsmall=0)),
             (format(round(seplm13[16],digits=3),nsmall=0)),
             (format(round(seplm14[16],digits=3),nsmall=0)),
             (format(round(seplm15[16],digits=3),nsmall=0)),
             (format(round(seplm16[16],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[16,4],digits=3),nsmall=0)),
           #  (format(round(summary(plm10)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[16,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[16,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[17]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[17]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[17]),digits=3),nsmall=0))),
           c((format(round(seplm9[17],digits=3),nsmall=0)),
            # (format(round(seplm10[17],digits=3),nsmall=0)),
             (format(round(seplm11[17],digits=3),nsmall=0)),
             (format(round(seplm12[17],digits=3),nsmall=0)),
             (format(round(seplm13[17],digits=3),nsmall=0)),
             (format(round(seplm14[17],digits=3),nsmall=0)),
             (format(round(seplm15[17],digits=3),nsmall=0)),
             (format(round(seplm16[17],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[17,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[17,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[17,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[18]),digits=3),nsmall=0)),
            # (format(round(sum(plm10$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[18]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[18]),digits=3),nsmall=0))),
           c((format(round(seplm9[18],digits=3),nsmall=0)),
           #  (format(round(seplm10[18],digits=3),nsmall=0)),
             (format(round(seplm11[18],digits=3),nsmall=0)),
             (format(round(seplm12[18],digits=3),nsmall=0)),
             (format(round(seplm13[18],digits=3),nsmall=0)),
             (format(round(seplm14[18],digits=3),nsmall=0)),
             (format(round(seplm15[18],digits=3),nsmall=0)),
             (format(round(seplm16[18],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[18,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[18,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[18,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[19]),digits=3),nsmall=0)),
           #  (format(round(sum(plm10$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[19]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[19]),digits=3),nsmall=0))),
           c((format(round(seplm9[19],digits=3),nsmall=0)),
             #(format(round(seplm10[19],digits=3),nsmall=0)),
             (format(round(seplm11[19],digits=3),nsmall=0)),
             (format(round(seplm12[19],digits=3),nsmall=0)),
             (format(round(seplm13[19],digits=3),nsmall=0)),
             (format(round(seplm14[19],digits=3),nsmall=0)),
             (format(round(seplm15[19],digits=3),nsmall=0)),
             (format(round(seplm16[19],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[19,4],digits=3),nsmall=0)),
            # (format(round(summary(plm10)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[19,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[19,4],digits=3),nsmall=0))),
           
           c((format(round(sum(plm9$coefficients[20]),digits=3),nsmall=0)),
         #    (format(round(sum(plm10$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm11$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm12$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm13$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm14$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm15$coefficients[20]),digits=3),nsmall=0)),
             (format(round(sum(plm16$coefficients[20]),digits=3),nsmall=0))),
           c((format(round(seplm9[20],digits=3),nsmall=0)),
           #  (format(round(seplm10[20],digits=3),nsmall=0)),
             (format(round(seplm11[20],digits=3),nsmall=0)),
             (format(round(seplm12[20],digits=3),nsmall=0)),
             (format(round(seplm13[20],digits=3),nsmall=0)),
             (format(round(seplm14[20],digits=3),nsmall=0)),
             (format(round(seplm15[20],digits=3),nsmall=0)),
             (format(round(seplm16[20],digits=3),nsmall=0))),
           c((format(round(summary(plm9)$coefficients[20,4],digits=3),nsmall=0)),
           #  (format(round(summary(plm10)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(plm11)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(plm12)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(plm13)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(plm14)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(plm15)$coefficients[20,4],digits=3),nsmall=0)),
             (format(round(summary(plm16)$coefficients[20,4],digits=3),nsmall=0))),

           
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

plmm2<-plm(general_rating_nonseed~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im2<-mean(fixef(plmm2))
seplmm2<- sqrt(diag(vcov(plmm2)))

plmm3<-plm(location~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im3<-mean(fixef(plmm3))
seplmm3<- sqrt(diag(vcov(plmm3)))

plmm4<-plm(price~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im4<-mean(fixef(plmm4))
seplmm4<- sqrt(diag(vcov(plmm4)))

plmm5<-plm(quality~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im5<-mean(fixef(plmm5))
seplmm5<- sqrt(diag(vcov(plmm5)))

plmm6<-plm(stock~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im6<-mean(fixef(plmm6))
seplmm6<- sqrt(diag(vcov(plmm6)))

plmm7<-plm(reputation~genderdummy, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im7<-mean(fixef(plmm7))
seplmm7<- sqrt(diag(vcov(plmm7)))

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
             c((format(round(seplmm1[1],digits=3),nsmall=0)),
               (format(round(seplmm2[1],digits=3),nsmall=0)),
               (format(round(seplmm3[1],digits=3),nsmall=0)),
               (format(round(seplmm4[1],digits=3),nsmall=0)),
               (format(round(seplmm5[1],digits=3),nsmall=0)),
               (format(round(seplmm6[1],digits=3),nsmall=0)),
               (format(round(seplmm7[1],digits=3),nsmall=0))),
             c((format(round(summary(plmm1)$coefficients[1,4],digits=3),nsmall=0)),
               (format(round(summary(plmm2)$coefficients[1,4],digits=3),nsmall=0)),
               (format(round(summary(plmm3)$coefficients[1,4],digits=3),nsmall=0)),
               (format(round(summary(plmm4)$coefficients[1,4],digits=3),nsmall=0)),
               (format(round(summary(plmm5)$coefficients[1,4],digits=3),nsmall=0)),
               (format(round(summary(plmm6)$coefficients[1,4],digits=3),nsmall=0)),
               (format(round(summary(plmm7)$coefficients[1,4],digits=3),nsmall=0))),
             
             
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

plmm9<-plm(general_rating_nonseed~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im9<-mean(fixef(plmm9))
seplmm9<- sqrt(diag(vcov(plmm9)))

plmm10<-plm(location~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im10<-mean(fixef(plmm10))
seplmm10<- sqrt(diag(vcov(plmm10)))

plmm11<-plm(price~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im11<-mean(fixef(plmm11))
seplmm11<- sqrt(diag(vcov(plmm11)))

plmm12<-plm(quality~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im12<-mean(fixef(plmm12))
seplmm12<- sqrt(diag(vcov(plmm12)))

plmm13<-plm(stock~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im13<-mean(fixef(plmm13))    
#4.332454
seplmm13<- sqrt(diag(vcov(plmm13)))

plmm14<-plm(reputation~genderdummy+ maize.owner.agree.age +prim +maize.owner.agree.q3 +maize.owner.agree.q4+inputsale+
             +years_shop +dedicated_area +pest_prob +insulated+ wall_heatproof +ventilation +
             badlighting +badstored+ open_storage+ cert_yes+ shop_rate+ complaint + maize.owner.agree.q70 +leakproof, data = fedata, index=c("farmer_ID","shop_ID"), model="within")
im14<-mean(fixef(plmm14))
seplmm14<- sqrt(diag(vcov(plmm14)))



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
            c((format(round(seplmm8[1],digits=3),nsmall=0)),
              (format(round(seplmm9[1],digits=3),nsmall=0)),
              (format(round(seplmm10[1],digits=3),nsmall=0)),
              (format(round(seplmm11[1],digits=3),nsmall=0)),
              (format(round(seplmm12[1],digits=3),nsmall=0)),
              (format(round(seplmm13[1],digits=3),nsmall=0)),
              (format(round(seplmm14[1],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[1,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[1,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[1,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[1,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[1,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[1,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[1,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[2]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[2]),digits=3),nsmall=0))),
            c((format(round(seplmm8[2],digits=3),nsmall=0)),
              (format(round(seplmm9[2],digits=3),nsmall=0)),
              (format(round(seplmm10[2],digits=3),nsmall=0)),
              (format(round(seplmm11[2],digits=3),nsmall=0)),
              (format(round(seplmm12[2],digits=3),nsmall=0)),
              (format(round(seplmm13[2],digits=3),nsmall=0)),
              (format(round(seplmm14[2],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[2,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[2,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[2,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[2,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[2,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[2,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[2,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[3]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[3]),digits=3),nsmall=0))),
            c((format(round(seplmm8[3],digits=3),nsmall=0)),
              (format(round(seplmm9[3],digits=3),nsmall=0)),
              (format(round(seplmm10[3],digits=3),nsmall=0)),
              (format(round(seplmm11[3],digits=3),nsmall=0)),
              (format(round(seplmm12[3],digits=3),nsmall=0)),
              (format(round(seplmm13[3],digits=3),nsmall=0)),
              (format(round(seplmm14[3],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[3,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[3,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[3,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[3,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[3,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[3,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[3,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[4]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[4]),digits=3),nsmall=0))),
            c((format(round(seplmm8[4],digits=3),nsmall=0)),
              (format(round(seplmm9[4],digits=3),nsmall=0)),
              (format(round(seplmm10[4],digits=3),nsmall=0)),
              (format(round(seplmm11[4],digits=3),nsmall=0)),
              (format(round(seplmm12[4],digits=3),nsmall=0)),
              (format(round(seplmm13[4],digits=3),nsmall=0)),
              (format(round(seplmm14[4],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[4,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[4,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[4,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[4,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[4,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[4,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[4,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[5]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[5]),digits=3),nsmall=0))),
            c((format(round(seplmm8[5],digits=3),nsmall=0)),
              (format(round(seplmm9[5],digits=3),nsmall=0)),
              (format(round(seplmm10[5],digits=3),nsmall=0)),
              (format(round(seplmm11[5],digits=3),nsmall=0)),
              (format(round(seplmm12[5],digits=3),nsmall=0)),
              (format(round(seplmm13[5],digits=3),nsmall=0)),
              (format(round(seplmm14[5],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[5,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[5,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[5,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[5,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[5,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[5,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[5,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[6]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[6]),digits=3),nsmall=0))),
            c((format(round(seplmm8[6],digits=3),nsmall=0)),
              (format(round(seplmm9[6],digits=3),nsmall=0)),
              (format(round(seplmm10[6],digits=3),nsmall=0)),
              (format(round(seplmm11[6],digits=3),nsmall=0)),
              (format(round(seplmm12[6],digits=3),nsmall=0)),
              (format(round(seplmm13[6],digits=3),nsmall=0)),
              (format(round(seplmm14[6],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[6,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[6,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[6,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[6,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[6,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[6,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[6,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[7]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[7]),digits=3),nsmall=0))),
            c((format(round(seplmm8[7],digits=3),nsmall=0)),
              (format(round(seplmm9[7],digits=3),nsmall=0)),
              (format(round(seplmm10[7],digits=3),nsmall=0)),
              (format(round(seplmm11[7],digits=3),nsmall=0)),
              (format(round(seplmm12[7],digits=3),nsmall=0)),
              (format(round(seplmm13[7],digits=3),nsmall=0)),
              (format(round(seplmm14[7],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[7,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[7,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[7,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[7,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[7,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[7,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[7,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[8]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[8]),digits=3),nsmall=0))),
            c((format(round(seplmm8[8],digits=3),nsmall=0)),
              (format(round(seplmm9[8],digits=3),nsmall=0)),
              (format(round(seplmm10[8],digits=3),nsmall=0)),
              (format(round(seplmm11[8],digits=3),nsmall=0)),
              (format(round(seplmm12[8],digits=3),nsmall=0)),
              (format(round(seplmm13[8],digits=3),nsmall=0)),
              (format(round(seplmm14[8],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[8,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[8,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[8,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[8,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[8,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[8,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[8,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[9]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[9]),digits=3),nsmall=0))),
            c((format(round(seplmm8[9],digits=3),nsmall=0)),
              (format(round(seplmm9[9],digits=3),nsmall=0)),
              (format(round(seplmm10[9],digits=3),nsmall=0)),
              (format(round(seplmm11[9],digits=3),nsmall=0)),
              (format(round(seplmm12[9],digits=3),nsmall=0)),
              (format(round(seplmm13[9],digits=3),nsmall=0)),
              (format(round(seplmm14[9],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[9,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[9,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[9,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[9,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[9,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[9,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[9,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[10]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[10]),digits=3),nsmall=0))),
            c((format(round(seplmm8[10],digits=3),nsmall=0)),
              (format(round(seplmm9[10],digits=3),nsmall=0)),
              (format(round(seplmm10[10],digits=3),nsmall=0)),
              (format(round(seplmm11[10],digits=3),nsmall=0)),
              (format(round(seplmm12[10],digits=3),nsmall=0)),
              (format(round(seplmm13[10],digits=3),nsmall=0)),
              (format(round(seplmm14[10],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[10,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[10,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[10,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[10,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[10,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[10,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[10,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[11]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[11]),digits=3),nsmall=0))),
            c((format(round(seplmm8[11],digits=3),nsmall=0)),
              (format(round(seplmm9[11],digits=3),nsmall=0)),
              (format(round(seplmm10[11],digits=3),nsmall=0)),
              (format(round(seplmm11[11],digits=3),nsmall=0)),
              (format(round(seplmm12[11],digits=3),nsmall=0)),
              (format(round(seplmm13[11],digits=3),nsmall=0)),
              (format(round(seplmm14[11],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[11,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[11,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[11,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[11,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[11,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[11,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[11,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[12]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[12]),digits=3),nsmall=0))),
            c((format(round(seplmm8[12],digits=3),nsmall=0)),
              (format(round(seplmm9[12],digits=3),nsmall=0)),
              (format(round(seplmm10[12],digits=3),nsmall=0)),
              (format(round(seplmm11[12],digits=3),nsmall=0)),
              (format(round(seplmm12[12],digits=3),nsmall=0)),
              (format(round(seplmm13[12],digits=3),nsmall=0)),
              (format(round(seplmm14[12],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[12,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[12,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[12,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[12,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[12,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[12,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[12,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[13]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[13]),digits=3),nsmall=0))),
            c((format(round(seplmm8[13],digits=3),nsmall=0)),
              (format(round(seplmm9[13],digits=3),nsmall=0)),
              (format(round(seplmm10[13],digits=3),nsmall=0)),
              (format(round(seplmm11[13],digits=3),nsmall=0)),
              (format(round(seplmm12[13],digits=3),nsmall=0)),
              (format(round(seplmm13[13],digits=3),nsmall=0)),
              (format(round(seplmm14[13],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[13,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[13,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[13,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[13,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[13,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[13,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[13,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[14]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[14]),digits=3),nsmall=0))),
            c((format(round(seplmm8[14],digits=3),nsmall=0)),
              (format(round(seplmm9[14],digits=3),nsmall=0)),
              (format(round(seplmm10[14],digits=3),nsmall=0)),
              (format(round(seplmm11[14],digits=3),nsmall=0)),
              (format(round(seplmm12[14],digits=3),nsmall=0)),
              (format(round(seplmm13[14],digits=3),nsmall=0)),
              (format(round(seplmm14[14],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[14,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[14,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[14,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[14,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[14,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[14,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[14,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[15]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[15]),digits=3),nsmall=0))),
            c((format(round(seplmm8[15],digits=3),nsmall=0)),
              (format(round(seplmm9[15],digits=3),nsmall=0)),
              (format(round(seplmm10[15],digits=3),nsmall=0)),
              (format(round(seplmm11[15],digits=3),nsmall=0)),
              (format(round(seplmm12[15],digits=3),nsmall=0)),
              (format(round(seplmm13[15],digits=3),nsmall=0)),
              (format(round(seplmm14[15],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[15,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[15,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[15,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[15,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[15,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[15,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[15,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[16]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[16]),digits=3),nsmall=0))),
            c((format(round(seplmm8[16],digits=3),nsmall=0)),
              (format(round(seplmm9[16],digits=3),nsmall=0)),
              (format(round(seplmm10[16],digits=3),nsmall=0)),
              (format(round(seplmm11[16],digits=3),nsmall=0)),
              (format(round(seplmm12[16],digits=3),nsmall=0)),
              (format(round(seplmm13[16],digits=3),nsmall=0)),
              (format(round(seplmm14[16],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[16,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[16,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[16,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[16,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[16,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[16,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[16,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[17]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[17]),digits=3),nsmall=0))),
            c((format(round(seplmm8[17],digits=3),nsmall=0)),
              (format(round(seplmm9[17],digits=3),nsmall=0)),
              (format(round(seplmm10[17],digits=3),nsmall=0)),
              (format(round(seplmm11[17],digits=3),nsmall=0)),
              (format(round(seplmm12[17],digits=3),nsmall=0)),
              (format(round(seplmm13[17],digits=3),nsmall=0)),
              (format(round(seplmm14[17],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[17,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[17,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[17,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[17,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[17,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[17,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[17,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[18]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[18]),digits=3),nsmall=0))),
            c((format(round(seplmm8[18],digits=3),nsmall=0)),
              (format(round(seplmm9[18],digits=3),nsmall=0)),
              (format(round(seplmm10[18],digits=3),nsmall=0)),
              (format(round(seplmm11[18],digits=3),nsmall=0)),
              (format(round(seplmm12[18],digits=3),nsmall=0)),
              (format(round(seplmm13[18],digits=3),nsmall=0)),
              (format(round(seplmm14[18],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[18,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[18,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[18,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[18,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[18,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[18,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[18,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[19]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[19]),digits=3),nsmall=0))),
            c((format(round(seplmm8[19],digits=3),nsmall=0)),
              (format(round(seplmm9[19],digits=3),nsmall=0)),
              (format(round(seplmm10[19],digits=3),nsmall=0)),
              (format(round(seplmm11[19],digits=3),nsmall=0)),
              (format(round(seplmm12[19],digits=3),nsmall=0)),
              (format(round(seplmm13[19],digits=3),nsmall=0)),
              (format(round(seplmm14[19],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[19,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[19,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[19,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[19,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[19,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[19,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[19,4],digits=3),nsmall=0))),
            
            c((format(round(sum(plmm8$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm9$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm10$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm11$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm12$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm13$coefficients[20]),digits=3),nsmall=0)),
              (format(round(sum(plmm14$coefficients[20]),digits=3),nsmall=0))),
            c((format(round(seplmm8[20],digits=3),nsmall=0)),
              (format(round(seplmm9[20],digits=3),nsmall=0)),
              (format(round(seplmm10[20],digits=3),nsmall=0)),
              (format(round(seplmm11[20],digits=3),nsmall=0)),
              (format(round(seplmm12[20],digits=3),nsmall=0)),
              (format(round(seplmm13[20],digits=3),nsmall=0)),
              (format(round(seplmm14[20],digits=3),nsmall=0))),
            c((format(round(summary(plmm8)$coefficients[20,4],digits=3),nsmall=0)),
              (format(round(summary(plmm9)$coefficients[20,4],digits=3),nsmall=0)),
              (format(round(summary(plmm10)$coefficients[20,4],digits=3),nsmall=0)),
              (format(round(summary(plmm11)$coefficients[20,4],digits=3),nsmall=0)),
              (format(round(summary(plmm12)$coefficients[20,4],digits=3),nsmall=0)),
              (format(round(summary(plmm13)$coefficients[20,4],digits=3),nsmall=0)),
              (format(round(summary(plmm14)$coefficients[20,4],digits=3),nsmall=0))),
          
            
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
betwd$pest_prob<- ifelse(betwd$pest== 'Yes', 1, 0)  
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
betwd$badlighting[betwd$maize.owner.agree.temp.q78=="1"|betwd$maize.owner.agree.temp.q78=="3"]<-1
table(betwd$badlighting)

#Q79. On what surface are seed stored?
betwd$badstored <- 0
betwd$badstored[betwd$maize.owner.agree.temp.q79=="1"|betwd$maize.owner.agree.temp.q79=="2"]<-1
betwd$badstored[betwd$maize.owner.agree.temp.q79==96]<-NA
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
betwd$cert<- ifelse(betwd$cert== 'Yes', 1, 0)  
table(betwd$cert)

#Q82. On a scale of 1 to 5, rate this shop in terms of cleanness and professionality 1 poor 5 excellent
betwd$shop_rate<-as.numeric(as.character(betwd$maize.owner.agree.temp.q82))
table(betwd$shop_rate)

#Q96. Since last season, did you receive any complaint from a customer that seed you sold was not good?
table(betwd$maize.owner.agree.q96)
betwd$complaint<- ifelse(betwd$maize.owner.agree.q96== 'Yes', 1, 0)  
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

bd9<-lm(score_bd~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
        +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
        + complaint + temp+leakproof, data = betwd)
sebd9<- sqrt(diag(vcov(bd9)))

#bd10<-lm(quality~gender_avgf +  farmer_educ+farmer_married+farmer_age+farmer_tarmac +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
 #          +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
  #         + complaint + temp+leakproof, data = betwd)
#sebd10<- sqrt(diag(vcov(bd10)))

bd11<- lm(general~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebd11<- sqrt(diag(vcov(bd11)))

bd12<-lm(yield~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebd12<- sqrt(diag(vcov(bd12)))

bd13<-lm(drought_resistent~gender_avgf +  maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebd13<- sqrt(diag(vcov(bd13)))

bd14<-lm(disease_resistent~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebd14<- sqrt(diag(vcov(bd14)))

bd15<-lm(early_maturing~gender_avgf + maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebd15<- sqrt(diag(vcov(bd15)))

bd16<-lm(germination~gender_avgf+maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
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

bdn9<-lm(avg_rating~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebdn9<- sqrt(diag(vcov(bdn9)))

bdn10<-lm(general_nonseed~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebdn10<- sqrt(diag(vcov(bdn10)))

bdn11<-lm(location~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebdn11<- sqrt(diag(vcov(bdn11)))

bdn12<-lm(price~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebdn12<- sqrt(diag(vcov(bdn12)))

bdn13<-lm(quality~gender_avgf +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebdn13<- sqrt(diag(vcov(bdn13)))

bdn14<-lm(stock~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
           +roof_insu+ wall_heatproof +ventilation +badlighting +badstored+ open_storage+ cert+ shop_rate
           + complaint + temp+leakproof, data = betwd)
sebdn14<- sqrt(diag(vcov(bdn14)))

bdn15<-lm(reputation~gender_avgf  +maize.owner.agree.age +prim +maize.owner.agree.q3 + maize.owner.agree.q4+ inputsale+years_shop +dedicated_area +pest_prob 
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
                  (format(round(sum(bdn11$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[11]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[11]),digits=3),nsmall=0))),
                c((format(round(sebdn9[11],digits=3),nsmall=0)),
                  (format(round(sebdn10[11],digits=3),nsmall=0)),
                  (format(round(sebdn11[11],digits=3),nsmall=0)),
                  (format(round(sebdn12[11],digits=3),nsmall=0)),
                  (format(round(sebdn13[11],digits=3),nsmall=0)),
                  (format(round(sebdn14[11],digits=3),nsmall=0)),
                  (format(round(sebdn15[11],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[11,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[11,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[12]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[12]),digits=3),nsmall=0))),
                c((format(round(sebdn9[12],digits=3),nsmall=0)),
                  (format(round(sebdn10[12],digits=3),nsmall=0)),
                  (format(round(sebdn11[12],digits=3),nsmall=0)),
                  (format(round(sebdn12[12],digits=3),nsmall=0)),
                  (format(round(sebdn13[12],digits=3),nsmall=0)),
                  (format(round(sebdn14[12],digits=3),nsmall=0)),
                  (format(round(sebdn15[12],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[12,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[12,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[13]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[13]),digits=3),nsmall=0))),
                c((format(round(sebdn9[13],digits=3),nsmall=0)),
                  (format(round(sebdn10[13],digits=3),nsmall=0)),
                  (format(round(sebdn11[13],digits=3),nsmall=0)),
                  (format(round(sebdn12[13],digits=3),nsmall=0)),
                  (format(round(sebdn13[13],digits=3),nsmall=0)),
                  (format(round(sebdn14[13],digits=3),nsmall=0)),
                  (format(round(sebdn15[13],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[13,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[13,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[14]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[14]),digits=3),nsmall=0))),
                c((format(round(sebdn9[14],digits=3),nsmall=0)),
                  (format(round(sebdn10[14],digits=3),nsmall=0)),
                  (format(round(sebdn11[14],digits=3),nsmall=0)),
                  (format(round(sebdn12[14],digits=3),nsmall=0)),
                  (format(round(sebdn13[14],digits=3),nsmall=0)),
                  (format(round(sebdn14[14],digits=3),nsmall=0)),
                  (format(round(sebdn15[14],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[14,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[14,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[15]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[15]),digits=3),nsmall=0))),
                c((format(round(sebdn9[15],digits=3),nsmall=0)),
                  (format(round(sebdn10[15],digits=3),nsmall=0)),
                  (format(round(sebdn11[15],digits=3),nsmall=0)),
                  (format(round(sebdn12[15],digits=3),nsmall=0)),
                  (format(round(sebdn13[15],digits=3),nsmall=0)),
                  (format(round(sebdn14[15],digits=3),nsmall=0)),
                  (format(round(sebdn15[15],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[15,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[15,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[16]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[16]),digits=3),nsmall=0))),
                c((format(round(sebdn9[16],digits=3),nsmall=0)),
                  (format(round(sebdn10[16],digits=3),nsmall=0)),
                  (format(round(sebdn11[16],digits=3),nsmall=0)),
                  (format(round(sebdn12[16],digits=3),nsmall=0)),
                  (format(round(sebdn13[16],digits=3),nsmall=0)),
                  (format(round(sebdn14[16],digits=3),nsmall=0)),
                  (format(round(sebdn15[16],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[16,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[16,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[17]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[17]),digits=3),nsmall=0))),
                c((format(round(sebdn9[17],digits=3),nsmall=0)),
                  (format(round(sebdn10[17],digits=3),nsmall=0)),
                  (format(round(sebdn11[17],digits=3),nsmall=0)),
                  (format(round(sebdn12[17],digits=3),nsmall=0)),
                  (format(round(sebdn13[17],digits=3),nsmall=0)),
                  (format(round(sebdn14[17],digits=3),nsmall=0)),
                  (format(round(sebdn15[17],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[17,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[17,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[18]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[18]),digits=3),nsmall=0))),
                c((format(round(sebdn9[18],digits=3),nsmall=0)),
                  (format(round(sebdn10[18],digits=3),nsmall=0)),
                  (format(round(sebdn11[18],digits=3),nsmall=0)),
                  (format(round(sebdn12[18],digits=3),nsmall=0)),
                  (format(round(sebdn13[18],digits=3),nsmall=0)),
                  (format(round(sebdn14[18],digits=3),nsmall=0)),
                  (format(round(sebdn15[18],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[18,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[18,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[19]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[19]),digits=3),nsmall=0))),
                c((format(round(sebdn9[19],digits=3),nsmall=0)),
                  (format(round(sebdn10[19],digits=3),nsmall=0)),
                  (format(round(sebdn11[19],digits=3),nsmall=0)),
                  (format(round(sebdn12[19],digits=3),nsmall=0)),
                  (format(round(sebdn13[19],digits=3),nsmall=0)),
                  (format(round(sebdn14[19],digits=3),nsmall=0)),
                  (format(round(sebdn15[19],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[19,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[19,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[20]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[20]),digits=3),nsmall=0))),
                c((format(round(sebdn9[20],digits=3),nsmall=0)),
                  (format(round(sebdn10[20],digits=3),nsmall=0)),
                  (format(round(sebdn11[20],digits=3),nsmall=0)),
                  (format(round(sebdn12[20],digits=3),nsmall=0)),
                  (format(round(sebdn13[20],digits=3),nsmall=0)),
                  (format(round(sebdn14[20],digits=3),nsmall=0)),
                  (format(round(sebdn15[20],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[20,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[20,4],digits=3),nsmall=0))),
                
                c((format(round(sum(bdn9$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn10$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn11$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn12$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn13$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn14$coefficients[21]),digits=3),nsmall=0)),
                  (format(round(sum(bdn15$coefficients[21]),digits=3),nsmall=0))),
                c((format(round(sebdn9[21],digits=3),nsmall=0)),
                  (format(round(sebdn10[21],digits=3),nsmall=0)),
                  (format(round(sebdn11[21],digits=3),nsmall=0)),
                  (format(round(sebdn12[21],digits=3),nsmall=0)),
                  (format(round(sebdn13[21],digits=3),nsmall=0)),
                  (format(round(sebdn14[21],digits=3),nsmall=0)),
                  (format(round(sebdn15[21],digits=3),nsmall=0))),
                c((format(round(summary(bdn9)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn10)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn11)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn12)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn13)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn14)$coefficients[21,4],digits=3),nsmall=0)),
                  (format(round(summary(bdn15)$coefficients[21,4],digits=3),nsmall=0))),
              
                
                
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

fedatad <- merge(rating_dyads, farmers_seed, by="farmer_ID")

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

#plmd2<-plm(quality~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
#id2<-mean(fixef(plmd2))
#sed2<- sqrt(diag(vcov(plmd2)))

plmd3<-plm(general~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id3<-mean(fixef(plmd3))
sed3<- sqrt(diag(vcov(plmd3)))

plmd4<-plm(yield~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id4<-mean(fixef(plmd4))
sed4<- sqrt(diag(vcov(plmd4)))

plmd5<-plm(drought_resistent~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id5<-mean(fixef(plmd5))
sed5<- sqrt(diag(vcov(plmd5)))

plmd6<-plm(disease_resistent~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id6<-mean(fixef(plmd6))
sed6<- sqrt(diag(vcov(plmd6)))

plmd7<-plm(early_maturing~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id7<-mean(fixef(plmd7))
sed7<- sqrt(diag(vcov(plmd7)))

plmd8<-plm(germination~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id8<-mean(fixef(plmd8))
sed8<- sqrt(diag(vcov(plmd8)))

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
              c((format(round(sed1[1],digits=3),nsmall=0)),
              #  (format(round(sed2[1],digits=3),nsmall=0)),
                (format(round(sed3[1],digits=3),nsmall=0)),
                (format(round(sed4[1],digits=3),nsmall=0)),
                (format(round(sed5[1],digits=3),nsmall=0)),
                (format(round(sed6[1],digits=3),nsmall=0)),
                (format(round(sed7[1],digits=3),nsmall=0)),
                (format(round(sed8[1],digits=3),nsmall=0))),
              c((format(round(summary(plmd1)$coefficients[1,4],digits=3),nsmall=0)),
              #  (format(round(summary(plmd2)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd3)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd4)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd5)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd6)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd7)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd8)$coefficients[1,4],digits=3),nsmall=0))),
              
              
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

#plmd10<-plm(quality~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
#id10<-mean(fixef(plmd10))
#sed10<- sqrt(diag(vcov(plmd10)))

plmd11<-plm(general~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id11<-mean(fixef(plmd11))
sed11<- sqrt(diag(vcov(plmd11)))

plmd12<-plm(yield~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id12<-mean(fixef(plmd12))
sed12<- sqrt(diag(vcov(plmd12)))

plmd13<-plm(drought_resistent~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id13<-mean(fixef(plmd13))
sed13<- sqrt(diag(vcov(plmd13)))

plmd14<-plm(disease_resistent~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id14<-mean(fixef(plmd14))
sed14<- sqrt(diag(vcov(plmd14)))

plmd15<-plm(early_maturing~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id15<-mean(fixef(plmd15))
sed15<- sqrt(diag(vcov(plmd15)))

plmd16<-plm(germination~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
id16<-mean(fixef(plmd16))
sed16<- sqrt(diag(vcov(plmd16)))

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
              c((format(round(sed9[1],digits=3),nsmall=0)),
              #  (format(round(sed10[1],digits=3),nsmall=0)),
                (format(round(sed11[1],digits=3),nsmall=0)),
                (format(round(sed12[1],digits=3),nsmall=0)),
                (format(round(sed13[1],digits=3),nsmall=0)),
                (format(round(sed14[1],digits=3),nsmall=0)),
                (format(round(sed15[1],digits=3),nsmall=0)),
                (format(round(sed16[1],digits=3),nsmall=0))),
              c((format(round(summary(plmd9)$coefficients[1,4],digits=3),nsmall=0)),
              #  (format(round(summary(plmd10)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd11)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd12)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd13)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd14)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd15)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmd16)$coefficients[1,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmd9$coefficients[2]),digits=3),nsmall=0)),
              #  (format(round(sum(plmd10$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[2]),digits=3),nsmall=0))),
              c((format(round(sed9[2],digits=3),nsmall=0)),
             #   (format(round(sed10[2],digits=3),nsmall=0)),
                (format(round(sed11[2],digits=3),nsmall=0)),
                (format(round(sed12[2],digits=3),nsmall=0)),
                (format(round(sed13[2],digits=3),nsmall=0)),
                (format(round(sed14[2],digits=3),nsmall=0)),
                (format(round(sed15[2],digits=3),nsmall=0)),
                (format(round(sed16[2],digits=3),nsmall=0))),
              c((format(round(summary(plmd9)$coefficients[2,4],digits=3),nsmall=0)),
             #   (format(round(summary(plmd10)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmd11)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmd12)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmd13)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmd14)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmd15)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmd16)$coefficients[2,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmd9$coefficients[3]),digits=3),nsmall=0)),
              #  (format(round(sum(plmd10$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[3]),digits=3),nsmall=0))),
              c((format(round(sed9[3],digits=3),nsmall=0)),
             #   (format(round(sed10[3],digits=3),nsmall=0)),
                (format(round(sed11[3],digits=3),nsmall=0)),
                (format(round(sed12[3],digits=3),nsmall=0)),
                (format(round(sed13[3],digits=3),nsmall=0)),
                (format(round(sed14[3],digits=3),nsmall=0)),
                (format(round(sed15[3],digits=3),nsmall=0)),
                (format(round(sed16[3],digits=3),nsmall=0))),
              c((format(round(summary(plmd9)$coefficients[3,4],digits=3),nsmall=0)),
              #  (format(round(summary(plmd10)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmd11)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmd12)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmd13)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmd14)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmd15)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmd16)$coefficients[3,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmd9$coefficients[4]),digits=3),nsmall=0)),
           #     (format(round(sum(plmd10$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[4]),digits=3),nsmall=0))),
              c((format(round(sed9[4],digits=3),nsmall=0)),
             #   (format(round(sed10[4],digits=3),nsmall=0)),
                (format(round(sed11[4],digits=3),nsmall=0)),
                (format(round(sed12[4],digits=3),nsmall=0)),
                (format(round(sed13[4],digits=3),nsmall=0)),
                (format(round(sed14[4],digits=3),nsmall=0)),
                (format(round(sed15[4],digits=3),nsmall=0)),
                (format(round(sed16[4],digits=3),nsmall=0))),
              c((format(round(summary(plmd9)$coefficients[4,4],digits=3),nsmall=0)),
               # (format(round(summary(plmd10)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmd11)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmd12)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmd13)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmd14)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmd15)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmd16)$coefficients[4,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmd9$coefficients[5]),digits=3),nsmall=0)),
              #  (format(round(sum(plmd10$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd11$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd12$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd13$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd14$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd15$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmd16$coefficients[5]),digits=3),nsmall=0))),
              c((format(round(sed9[5],digits=3),nsmall=0)),
              #  (format(round(sed10[5],digits=3),nsmall=0)),
                (format(round(sed11[5],digits=3),nsmall=0)),
                (format(round(sed12[5],digits=3),nsmall=0)),
                (format(round(sed13[5],digits=3),nsmall=0)),
                (format(round(sed14[5],digits=3),nsmall=0)),
                (format(round(sed15[5],digits=3),nsmall=0)),
                (format(round(sed16[5],digits=3),nsmall=0))),
              c((format(round(summary(plmd9)$coefficients[5,4],digits=3),nsmall=0)),
               # (format(round(summary(plmd10)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmd11)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmd12)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmd13)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmd14)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmd15)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmd16)$coefficients[5,4],digits=3),nsmall=0))),
              
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

plmdn2<-plm(general_rating_nonseed~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn2<-mean(fixef(plmdn2))
sedn2<- sqrt(diag(vcov(plmdn2)))

plmdn3<-plm(location~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn3<-mean(fixef(plmdn3))
sedn3<- sqrt(diag(vcov(plmdn3)))

plmdn4<-plm(quality~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn4<-mean(fixef(plmdn4))
sedn4<- sqrt(diag(vcov(plmdn4)))

plmdn5<-plm(price~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn5<-mean(fixef(plmdn5))
sedn5<- sqrt(diag(vcov(plmdn5)))

plmdn6<-plm(stock~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn6<-mean(fixef(plmdn6))
sedn6<- sqrt(diag(vcov(plmdn6)))

plmdn7<-plm(reputation~farmergen, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn7<-mean(fixef(plmdn7))
sedn7<- sqrt(diag(vcov(plmdn7)))


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
              c((format(round(sedn1[1],digits=3),nsmall=0)),
                (format(round(sedn2[1],digits=3),nsmall=0)),
                (format(round(sedn3[1],digits=3),nsmall=0)),
                (format(round(sedn4[1],digits=3),nsmall=0)),
                (format(round(sedn5[1],digits=3),nsmall=0)),
                (format(round(sedn6[1],digits=3),nsmall=0)),
                (format(round(sedn7[1],digits=3),nsmall=0))),
              c((format(round(summary(plmdn1)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn2)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn3)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn4)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn5)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn6)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn7)$coefficients[1,4],digits=3),nsmall=0))),
              
              
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

plmdn10<-plm(general_rating_nonseed~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn10<-mean(fixef(plmdn10))
sedn10<- sqrt(diag(vcov(plmdn10)))

plmdn11<-plm(location~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn11<-mean(fixef(plmdn11))
sedn11<- sqrt(diag(vcov(plmdn11)))

plmdn12<-plm(quality~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn12<-mean(fixef(plmdn12))
sedn12<- sqrt(diag(vcov(plmdn12)))

plmdn13<-plm(price~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn13<-mean(fixef(plmdn13))
sedn13<- sqrt(diag(vcov(plmdn13)))

plmdn14<-plm(stock~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn14<-mean(fixef(plmdn14))
sedn14<- sqrt(diag(vcov(plmdn14)))

plmdn15<-plm(reputation~farmergen+educ_f+married+Check2.check.maize.q14+Check2.check.maize.q8, data = fedatad, index=c("shop_ID.x","farmer_ID"), model="within")
idn15<-mean(fixef(plmdn15))
sedn15<- sqrt(diag(vcov(plmdn15)))


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
              c((format(round(sedn9[1],digits=3),nsmall=0)),
                (format(round(sedn10[1],digits=3),nsmall=0)),
                (format(round(sedn11[1],digits=3),nsmall=0)),
                (format(round(sedn12[1],digits=3),nsmall=0)),
                (format(round(sedn13[1],digits=3),nsmall=0)),
                (format(round(sedn14[1],digits=3),nsmall=0)),
                (format(round(sedn15[1],digits=3),nsmall=0))),
              c((format(round(summary(plmdn9)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn10)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn11)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn12)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn13)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn14)$coefficients[1,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn15)$coefficients[1,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmdn9$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[2]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[2]),digits=3),nsmall=0))),
              c((format(round(sedn9[2],digits=3),nsmall=0)),
                (format(round(sedn10[2],digits=3),nsmall=0)),
                (format(round(sedn11[2],digits=3),nsmall=0)),
                (format(round(sedn12[2],digits=3),nsmall=0)),
                (format(round(sedn13[2],digits=3),nsmall=0)),
                (format(round(sedn14[2],digits=3),nsmall=0)),
                (format(round(sedn15[2],digits=3),nsmall=0))),
              c((format(round(summary(plmdn9)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn10)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn11)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn12)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn13)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn14)$coefficients[2,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn15)$coefficients[2,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmdn9$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[3]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[3]),digits=3),nsmall=0))),
              c((format(round(sedn9[3],digits=3),nsmall=0)),
                (format(round(sedn10[3],digits=3),nsmall=0)),
                (format(round(sedn11[3],digits=3),nsmall=0)),
                (format(round(sedn12[3],digits=3),nsmall=0)),
                (format(round(sedn13[3],digits=3),nsmall=0)),
                (format(round(sedn14[3],digits=3),nsmall=0)),
                (format(round(sedn15[3],digits=3),nsmall=0))),
              c((format(round(summary(plmdn9)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn10)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn11)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn12)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn13)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn14)$coefficients[3,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn15)$coefficients[3,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmdn9$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[4]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[4]),digits=3),nsmall=0))),
              c((format(round(sedn9[4],digits=3),nsmall=0)),
                (format(round(sedn10[4],digits=3),nsmall=0)),
                (format(round(sedn11[4],digits=3),nsmall=0)),
                (format(round(sedn12[4],digits=3),nsmall=0)),
                (format(round(sedn13[4],digits=3),nsmall=0)),
                (format(round(sedn14[4],digits=3),nsmall=0)),
                (format(round(sedn15[4],digits=3),nsmall=0))),
              c((format(round(summary(plmdn9)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn10)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn11)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn12)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn13)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn14)$coefficients[4,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn15)$coefficients[4,4],digits=3),nsmall=0))),
              
              c((format(round(sum(plmdn9$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn10$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn11$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn12$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn13$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn14$coefficients[5]),digits=3),nsmall=0)),
                (format(round(sum(plmdn15$coefficients[5]),digits=3),nsmall=0))),
              c((format(round(sedn9[5],digits=3),nsmall=0)),
                (format(round(sedn10[5],digits=3),nsmall=0)),
                (format(round(sedn11[5],digits=3),nsmall=0)),
                (format(round(sedn12[5],digits=3),nsmall=0)),
                (format(round(sedn13[5],digits=3),nsmall=0)),
                (format(round(sedn14[5],digits=3),nsmall=0)),
                (format(round(sedn15[5],digits=3),nsmall=0))),
              c((format(round(summary(plmdn9)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn10)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn11)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn12)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn13)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn14)$coefficients[5,4],digits=3),nsmall=0)),
                (format(round(summary(plmdn15)$coefficients[5,4],digits=3),nsmall=0))),
              
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

