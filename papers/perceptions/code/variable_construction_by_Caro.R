#1. Quantity of maize seed sold in kg
baseline_dealers$mid_maize.owner.agree.q20 <- NA
baseline_dealers$mid_maize.owner.agree.q20[baseline_dealers$check.owner.agree.q20=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q20[baseline_dealers$check.owner.agree.q20=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.q32 <- NA
baseline_dealers$mid_maize.owner.agree.q32[baseline_dealers$check.owner.agree.q32=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q32[baseline_dealers$check.owner.agree.q32=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.q45 <- NA
baseline_dealers$mid_maize.owner.agree.q45[baseline_dealers$check.owner.agree.q45=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q45[baseline_dealers$check.owner.agree.q45=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.q57 <- NA
baseline_dealers$mid_maize.owner.agree.q57[baseline_dealers$check.owner.agree.q57=="Yes"] <- 1
baseline_dealers$mid_maize.owner.agree.q57[baseline_dealers$check.owner.agree.q57=="No"] <- 0

baseline_dealers$mid_maize.owner.agree.long10h.q25 <-baseline_dealers$check.owner.agree.long10h.q25 #x
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.long10h.q25=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q25 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q25)) #x
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.long10h.q25==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.long10h.q25_unadj <- baseline_dealers$mid_maize.owner.agree.long10h.q25
baseline_dealers$mid_maize.owner.agree.long10h.q25[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x

baseline_dealers$mid_maize.owner.agree.longe7h.q37 <- baseline_dealers$check.owner.agree.longe7H.q38 #x #different numbers in bl and ml
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.longe7h.q37=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe7h.q37 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe7h.q37)) #x
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.longe7h.q37==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe7h.q37[baseline_dealers$mid_maize.owner.agree.q32=="0"] <- 0 #x

baseline_dealers$mid_maize.owner.agree.longe5.q50 <- baseline_dealers$check.owner.agree.longe5.q50 #x
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.longe5.q50=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q50 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q50)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.longe5.q50==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q50_unadj <- baseline_dealers$mid_maize.owner.agree.longe5.q50
baseline_dealers$mid_maize.owner.agree.longe5.q50[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x

baseline_dealers$mid_maize.owner.agree.longe4.q62 <- baseline_dealers$check.owner.agree.longe4.q62 #x
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.longe4.q62=="n/a"] <- NA #x
baseline_dealers$mid_maize.owner.agree.longe4.q62 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe4.q62)) #x
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.longe4.q62==999]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe4.q62[baseline_dealers$mid_maize.owner.agree.q57=="0"] <- 0 #x

baseline_dealers$mid_quantitysold <- baseline_dealers$mid_maize.owner.agree.long10h.q25+baseline_dealers$mid_maize.owner.agree.longe7h.q37+baseline_dealers$mid_maize.owner.agree.longe5.q50+baseline_dealers$mid_maize.owner.agree.longe4.q62 #x
baseline_dealers <- trim("mid_quantitysold",baseline_dealers,trim_perc=.02) #x

#2. Sales price of maize seed in UGX/kg
baseline_dealers$mid_maize.owner.agree.long10h.q26 <- baseline_dealers$check.owner.agree.long10h.q26 #x
baseline_dealers$mid_maize.owner.agree.longe7h.q38 <- baseline_dealers$check.owner.agree.longe7H.q39 #x #numer changed from bl to ml
baseline_dealers$mid_maize.owner.agree.longe5.q51 <-  baseline_dealers$check.owner.agree.longe5.q51 #x
baseline_dealers$mid_maize.owner.agree.longe4.q63 <-  baseline_dealers$check.owner.agree.longe4.q63 #x

baseline_dealers$mid_maize.owner.agree.long10h.q26[baseline_dealers$mid_maize.owner.agree.long10h.q26=="n/a"]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe7h.q38[baseline_dealers$mid_maize.owner.agree.longe7h.q38=="n/a"]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe5.q51[baseline_dealers$mid_maize.owner.agree.longe5.q51=="n/a"]<-NA #x
baseline_dealers$mid_maize.owner.agree.longe4.q63[baseline_dealers$mid_maize.owner.agree.longe4.q63=="n/a"]<-NA #x

baseline_dealers$mid_maize.owner.agree.long10h.q26 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.long10h.q26)) #x
baseline_dealers$mid_maize.owner.agree.longe7h.q38 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe7h.q38)) #x
baseline_dealers$mid_maize.owner.agree.longe5.q51 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe5.q51)) #x
baseline_dealers$mid_maize.owner.agree.longe4.q63 <- as.numeric(as.character(baseline_dealers$mid_maize.owner.agree.longe4.q63)) #x

baseline_dealers$mid_av_salesprices <- rowMeans(baseline_dealers[c("mid_maize.owner.agree.long10h.q26"
                                                                   ,"mid_maize.owner.agree.longe7h.q38"
                                                                   ,"mid_maize.owner.agree.longe5.q51"
                                                                   ,"mid_maize.owner.agree.longe4.q63")],na.rm = T) #x
baseline_dealers <- trim("mid_av_salesprices",baseline_dealers,trim_perc=.02) #x

#3. Revenue from maize seed in mln UGX
baseline_dealers$mid_revenue_long10h.q25 <- (baseline_dealers$mid_maize.owner.agree.long10h.q25*baseline_dealers$mid_maize.owner.agree.long10h.q26) #x
baseline_dealers$mid_revenue_long10h.q25[baseline_dealers$mid_maize.owner.agree.q20=="0"] <- 0 #x

baseline_dealers$mid_revenue_longe7h <- (baseline_dealers$mid_maize.owner.agree.longe7h.q37*baseline_dealers$mid_maize.owner.agree.longe7h.q38) #x
baseline_dealers$mid_revenue_longe7h[baseline_dealers$mid_maize.owner.agree.q32=="0"] <- 0 #x

baseline_dealers$mid_revenue_longe5 <- (baseline_dealers$mid_maize.owner.agree.longe5.q50*baseline_dealers$mid_maize.owner.agree.longe5.q51) #x
baseline_dealers$mid_revenue_longe5[baseline_dealers$mid_maize.owner.agree.q45=="0"] <- 0 #x

baseline_dealers$mid_revenue_longe4 <- (baseline_dealers$mid_maize.owner.agree.longe4.q62*baseline_dealers$mid_maize.owner.agree.longe4.q63) #x
baseline_dealers$mid_revenue_longe4[baseline_dealers$mid_maize.owner.agree.q57=="0"] <- 0 #x

baseline_dealers$mid_revenue <- (baseline_dealers$mid_revenue_long10h.q25+baseline_dealers$mid_revenue_longe7h
                                 +baseline_dealers$mid_revenue_longe5+baseline_dealers$mid_revenue_longe4) #x
baseline_dealers$mid_revenue <- baseline_dealers$mid_revenue/1000000 #x

baseline_dealers <- trim("mid_revenue",baseline_dealers,trim_perc=.02) #x
baseline_dealers$mid_revenue <- ihs(baseline_dealers$mid_revenue) #x