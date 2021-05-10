rm(list=ls())

traders <- read.csv("/home/bjvca/data/projects/PIMMVC/data/public/traders.csv")
farmers <- read.csv("/home/bjvca/data/projects/PIMMVC/data/public/farmers.csv")
#These are the variables that 
prop.table(table(traders$hh.maize.q30.f)) 

#competition indicators:

prop.table(table(traders$hh.maize.q40))
#40. Do you make agreements on a maximum buyer price with other traders?
# 1= never, 4 is always
#         1          2          3          4 
#0.39296188 0.29912023 0.08211144 0.22580645 

#farmer level
farmers$hh.maize.q105[farmers$hh.maize.q105==999] <- NA

#This is for farmers:
#integer	q105	105. Please estimate how many of these maize traders or middlemen are buying maize in your village or neighborhood.

#how to make these relational contract indicators? example for credit 

##step 1: merge 
#credit
prop.table(table(traders$hh.maize.q30.f))[2] *100
#input
prop.table(table(traders$hh.maize.q30.a))[2] *100



Does this trader provide credit or give advances (eg buys/pays before harvest)?  
When did you first start selling to this trader (year, write 2018 if this was first year)
Does this trader provides you with storage and handling related inputs such as bags, tarpaulins?
Does this trader provide you with training on maize farming or maize storage and handling?
Does this trader provide you with farming inputs such as maize seed, fertilizer and chemicals?
Is this traders is associated with the cooperative or organization you are part of 

#share of farmers that sold to trader and report that trader provides credit
mean((farmers$hh.maize.trader1.q102n %in% c("1","2"))/(farmers$hh.maize.trader1.q102n %in% c("1","2","3","98")), na.rm=T)

#share of farmers that sold to trader and report that trader provides credit
mean((farmers$hh.maize.trader1.q102r %in% c("Yes"))/(farmers$hh.maize.trader1.q102r %in% c("Yes","No","98")), na.rm=T)


#now merge
merged <- merge(farmers, traders, by.x="id.trader1", by.y="id.trader")

merged$hh.maize.q105[merged$hh.maize.q105==999] <- NA
merged$hh.maize.q105[merged$hh.maize.q105==0] <- NA
#test if households report more competition in traders say they collide
summary(lm(hh.maize.q105~(hh.maize.q40.y==4), data=merged))
