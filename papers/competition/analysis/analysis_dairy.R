### run from /PIMMVC/papers/competition/
rm(list=ls())
library(stringr)
path <- getwd()
#path <- strsplit(path, "/papers/competition")[[1]]

farmers <- read.csv(paste(path,"data/dairy_panel/wave_3/farmers.csv", sep="/"), stringsAsFactors = TRUE)

# Q95: How many different traders operate in your neigbourhood ?
farmers$dairy.sales.q95[farmers$dairy.sales.q95 == "n/a"] <- NA
farmers$dairy.sales.q95[farmers$dairy.sales.q95 == "999"] <- NA
farmers$dairy.sales.q95[farmers$dairy.sales.q95 == "9999"] <- NA
farmers$dairy.sales.q95 <- as.numeric(as.character(farmers$dairy.sales.q95))

farmers$dairy.sales.R3[farmers$dairy.sales.R3 == "n/a"] <- NA
farmers$dairy.sales.R3[farmers$dairy.sales.R3 == "999"] <- NA
farmers$dairy.sales.R3 <- as.numeric(as.character(farmers$dairy.sales.R3))
### relational contract index



#we ask 

farmers$MCC_sup_market <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83XX))==1

summary(lm(MCC_sup_market~dairy.sales.R3 , data=farmers))

farmers$MCC_sup_adv <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X12))==1

summary(lm(MCC_sup_adv~dairy.sales.R3 , data=farmers))



farmers$MCC_sup_training <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X17))==1

summary(lm(MCC_sup_training~dairy.sales.R3 , data=farmers))



farmers$MCC_sup_saving <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X19))==1

summary(lm(MCC_sup_saving~dairy.sales.R3 , data=farmers))


farmers$MCC_sup_price <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X22))==1

summary(lm(MCC_sup_price~dairy.sales.R3 , data=farmers))


farmers$MCC_sup_cans <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X8))==1

summary(lm(MCC_sup_cans~dairy.sales.R3 , data=farmers))


farmers$MCC_sup_drugs <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X9))==1

summary(lm(MCC_sup_drugs~dairy.sales.R3 , data=farmers))



farmers$MCC_sup_vet <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X10))==1

summary(lm(MCC_sup_vet~dairy.sales.R3 , data=farmers))



farmers$MCC_sup_feed <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X11))==1

summary(lm(MCC_sup_feed~dairy.sales.R3 , data=farmers))


farmers$MCC_sup_soft_loan <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X13))==1

summary(lm(MCC_sup_soft_loan~dairy.sales.R3 , data=farmers))


farmers$MCC_sup_loan_sacco <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X14))==1

summary(lm(MCC_sup_loan_sacco~dairy.sales.R3 , data=farmers))

farmers$MCC_sup_chopper <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X15))==1
summary(lm(MCC_sup_chopper~dairy.sales.R3 , data=farmers)) ### < 5 percent get

farmers$MCC_sup_tractor <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X16))==1
summary(lm(MCC_sup_tractor~dairy.sales.R3 , data=farmers)) ### < 5 percent get

q83X18
farmers$MCC_sup_trans <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X18))==1

summary(lm(MCC_sup_trans~dairy.sales.R3 , data=farmers))

q83X20
farmers$MCC_sup_salt <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X20))==1

summary(lm(MCC_sup_salt~dairy.sales.R3 , data=farmers))

q83X21
farmers$MCC_sup_seed <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X21))==1

summary(lm(MCC_sup_seed~dairy.sales.R3 , data=farmers))



#### for traders

q83X1

farmers$trader_sup_market <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X1))==1

summary(lm(trader_sup_market~dairy.sales.q95, data=farmers))


farmers$trader_sup_cans <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X2))==1

summary(lm(trader_sup_cans~dairy.sales.q95, data=farmers))



farmers$trader_sup_adv <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X6))==1

summary(lm(trader_sup_adv~dairy.sales.q95 , data=farmers))

q83X7

farmers$trader_sup_loan <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X7))==1

summary(lm(trader_sup_loan~dairy.sales.q95, data=farmers))

q83X5
farmers$trader_sup_feed <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X5))==1

summary(lm(trader_sup_feed~dairy.sales.q95, data=farmers))

q83X4
farmers$trader_sup_trans <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X4))==1

summary(lm(trader_sup_trans~dairy.sales.q95, data=farmers))

q83X3
farmers$trader_sup_drugs <- as.numeric(as.character(farmers$dairy.sales.neighbour_assistance.sub_neighbour_assistance.q83X3))==1

summary(lm(trader_sup_drugs~dairy.sales.q95, data=farmers))






