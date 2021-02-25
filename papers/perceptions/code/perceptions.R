path <- getwd()
library(ggplot2)
library(ggsignif) 
path_2 <- strsplit(path, "/papers/perceptions")[[1]]
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))



trans <- c("hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("ID","id.agro1","hh.maize.agro1.q108h","hh.maize.agro1.q108i","hh.maize.agro1.q108j","hh.maize.agro1.q108k","hh.maize.agro1.q108l")],"Yes")
names(stack1) <- c("farmerID","id.agro","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")
stack2 <- cbind(farmers[c("ID","id.agro2","hh.maize.agro2.q109h","hh.maize.agro2.q109i","hh.maize.agro2.q109j","hh.maize.agro2.q109k","hh.maize.agro2.q109l","hh.maize.agro2.q110")])
names(stack2) <- c("farmerID","id.agro","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")
stack3 <- cbind(farmers[c("ID","id.agro3","hh.maize.agro3.q111h","hh.maize.agro3.q111i","hh.maize.agro3.q111j","hh.maize.agro3.q111k","hh.maize.agro3.q111l","hh.maize.agro3.q112")])
names(stack3) <- c("farmerID","id.agro","rating_location","rating_price","rating_quality","rating_stock","rating_reputation", "bought")

ratings <-rbind(stack1,stack2,stack3)
ratings[c("id.agro","bought")] <- lapply(ratings[c("id.agro","bought")], function(x) as.factor(as.character(x)) )


ratings <- subset(ratings, !is.na(rating_reputation) )
### simple average
ratings$rating_overall <- rowSums(ratings[c("rating_location","rating_price","rating_quality","rating_stock","rating_reputation")])/5
summary(ratings$rating_overall)
tapply(ratings$rating_overall,ratings$bought, mean )
wilcox.test(ratings$rating_quality~ratings$bought)

dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))

dealers$dealer_rating_overall <- rowSums(dealers[c("hh.maize.q79","hh.maize.q80","hh.maize.q81","hh.maize.q82","hh.maize.q83")])/5
summary(dealers$dealer_rating_overall)

wilcox.test(ratings$rating_overall,dealers$dealer_rating_overall)
### these are the three tests in the graph
wilcox.test(ratings$rating_overall[ratings$bought=="No"],dealers$dealer_rating_overall)
wilcox.test(ratings$rating_overall[ratings$bought=="Yes"],dealers$dealer_rating_overall)
wilcox.test(ratings$rating_overall[ratings$bought=="Yes"],ratings$rating_overall[ratings$bought=="No"])

### create graphs
## first graph is a simple bar chart of means - overall rating of customers, non-customers and dealers

df <- data.frame(c(mean(dealers$dealer_rating_overall),tapply(ratings$rating_overall,ratings$bought, mean )[2:3]))
names(df) <- "score"
rownames(df) <- NULL
df$levels <- c("dealer","non-customer","customer")
df <- df[order(df$score,decreasing = TRUE),]
df$levels <- factor(df$levels,levels= c("dealer","customer","non-customer"))

png(paste(path, "figures/fig_dealer_1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("dealer", "customer")), annotations="***", y_position = 4.3, tip_length = 0.03) +
  geom_signif(comparisons = list(c("dealer", "non-customer")), annotations="***", y_position = 4.5, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.1, tip_length = 0.03)
dev.off()

## now create these kind of likert scales bar charts for the different components of the scores, again for the three categories

plot_non_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$bought=="Yes"),2)[,1],
prop.table(table(ratings$rating_price, ratings$bought=="Yes"),2)[,1],
prop.table(table(ratings$rating_quality, ratings$bought=="Yes"),2)[,1],
prop.table(table(ratings$rating_stock, ratings$bought=="Yes"),2)[,1],
prop.table(table(ratings$rating_reputation, ratings$bought=="Yes"),2)[,1]))
names(plot_non_customer) <- c("location","price","quality","stock","reputation")

plot_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_price, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_quality, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_stock, ratings$bought=="Yes"),2)[,2],
prop.table(table(ratings$rating_reputation, ratings$bought=="Yes"),2)[,2]))
names(plot_customer) <- c("location","price","quality","stock","reputation")

plot_dealer <- data.frame(cbind(c(0,prop.table(table(dealers$hh.maize.q79))),
c(0,prop.table(table(dealers$hh.maize.q80))),
c(0,0,prop.table(table(dealers$hh.maize.q81))),
c(prop.table(table(dealers$hh.maize.q82))),
c(prop.table(table(dealers$hh.maize.q83)))))
names(plot_dealer) <- c("location","price","quality","stock","reputation")
png(paste(path, "figures/fig_dealer_2.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
par(mfrow=c(1,3), xpd=NA, mar = c(10, 5,5, 1)) 
colfunc<-colorRampPalette(c("red", "green"))
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="non-customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_dealer), col=colfunc(5), main="dealer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()

### now do this for traders
 
trans <- c("hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j","hh.maize.trader1.q102k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j","hh.maize.trader2.q103k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j","hh.maize.trader3.q104k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("id.trader1","hh.maize.trader1.q102g","hh.maize.trader1.q102h","hh.maize.trader1.q102i","hh.maize.trader1.q102j","hh.maize.trader1.q102k")],"Yes")
names(stack1) <- c("id.trader","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")
stack2 <- cbind(farmers[c("id.trader2","hh.maize.trader2.q103g","hh.maize.trader2.q103h","hh.maize.trader2.q103i","hh.maize.trader2.q103j","hh.maize.trader2.q103k","hh.maize.trader2.q103l")])
names(stack2) <- c("id.trader","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")
stack3 <- cbind(farmers[c("id.trader3","hh.maize.trader3.q104g","hh.maize.trader3.q104h","hh.maize.trader3.q104i","hh.maize.trader3.q104j","hh.maize.trader3.q104k","hh.maize.trader3.q104l")])
names(stack3) <- c("id.trader","rating_location","rating_price","rating_quality","rating_honesty","rating_reputation", "sold")

ratings <-rbind(stack1,stack2,stack3)
ratings[c("id.trader","sold")] <- lapply(ratings[c("id.trader","sold")], function(x) as.factor(as.character(x)) )

ratings <- subset(ratings,!is.na(rating_reputation))
### simple average
ratings$rating_overall <- rowSums(ratings[c("rating_location","rating_price","rating_quality","rating_honesty","rating_reputation")])/5
summary(ratings$rating_overall)
tapply(ratings$rating_overall,ratings$sold, mean )
wilcox.test(ratings$rating_quality~ratings$sold)

traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

traders$trader_rating_overall <- rowSums(traders[c("hh.maize.q40a","hh.maize.q40b","hh.maize.q40c","hh.maize.q40d","hh.maize.q40e")])/5
summary(traders$trader_rating_overall)

wilcox.test(ratings$rating_overall,traders$trader_rating_overall)
### these are the three tests in the graph
wilcox.test(ratings$rating_overall[ratings$sold=="No"],traders$traders_rating_overall)
wilcox.test(ratings$rating_overall[ratings$sold=="Yes"],traders$trader_rating_overall)
wilcox.test(ratings$rating_overall[ratings$sold=="Yes"],ratings$rating_overall[ratings$sold=="No"])


df <- data.frame(c(mean(traders$trader_rating_overall),tapply(ratings$rating_overall,ratings$sold, mean )[2:3]))
names(df) <- "score"
rownames(df) <- NULL
df$levels <- c("trader","non-customer","customer")
df <- df[order(df$score,decreasing = TRUE),]
df$levels <- factor(df$levels,levels= c("trader","customer","non-customer"))

png(paste(path, "figures/fig_trader_1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("trader", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("trader", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()

# now create these kind of likert scales bar charts for the different components of the scores, again for the three categories

plot_non_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$sold=="Yes"),2)[,1],
prop.table(table(ratings$rating_price, ratings$sold=="Yes"),2)[,1],
prop.table(table(ratings$rating_quality, ratings$sold=="Yes"),2)[,1],
prop.table(table(ratings$rating_honesty, ratings$sold=="Yes"),2)[,1],
prop.table(table(ratings$rating_reputation, ratings$sold=="Yes"),2)[,1]))
names(plot_non_customer) <- c("location","price","quality","honesty","reputation")

plot_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$sold=="Yes"),2)[,2],
prop.table(table(ratings$rating_price, ratings$sold=="Yes"),2)[,2],
prop.table(table(ratings$rating_quality, ratings$sold=="Yes"),2)[,2],
prop.table(table(ratings$rating_honesty, ratings$sold=="Yes"),2)[,2],
prop.table(table(ratings$rating_reputation, ratings$sold=="Yes"),2)[,2]))
names(plot_customer) <- c("location","price","quality","honesty","reputation")

plot_trader <- data.frame(cbind(c(prop.table(table(traders$hh.maize.q40a))),
c(prop.table(table(traders$hh.maize.q40b))),
c(prop.table(table(traders$hh.maize.q40c))),
c(prop.table(table(traders$hh.maize.q40d))),
c(0,prop.table(table(traders$hh.maize.q40e)))))
names(plot_trader) <- c("location","price","quality","honesty","reputation")
png(paste(path, "figures/fig_trader_2.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
par(mfrow=c(1,3), xpd=NA, mar = c(10, 5,5, 1)) 
colfunc<-colorRampPalette(c("red", "green"))
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="non-customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_trader), col=colfunc(5), main="trader", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()

### now do this for millers
 
trans <- c("hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
trans <- c("hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k")
farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )

stack1 <- cbind(farmers[c("id.miller1","hh.maize.miller1.q98g","hh.maize.miller1.q98h","hh.maize.miller1.q98i","hh.maize.miller1.q98j","hh.maize.miller1.q98k")],"Yes")
names(stack1) <- c("id.miller","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")
stack2 <- cbind(farmers[c("id.miller2","hh.maize.miller2.q99g","hh.maize.miller2.q99h","hh.maize.miller2.q99i","hh.maize.miller2.q99j","hh.maize.miller2.q99k","hh.maize.miller2.q99l")])
names(stack2) <- c("id.miller","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")
stack3 <- cbind(farmers[c("id.miller3","hh.maize.miller3.q100g","hh.maize.miller3.q100h","hh.maize.miller3.q100i","hh.maize.miller3.q100j","hh.maize.miller3.q100k","hh.maize.miller3.q100l")])
names(stack3) <- c("id.miller","rating_location","rating_price","rating_quality","rating_service","rating_reputation", "used")

ratings <-rbind(stack1,stack2,stack3)
ratings[c("id.miller","used")] <- lapply(ratings[c("id.miller","used")], function(x) as.factor(as.character(x)) )

ratings <- subset(ratings, !is.na(rating_reputation) )
### simple average
ratings$rating_overall <- rowSums(ratings[c("rating_location","rating_price","rating_quality","rating_service","rating_reputation")])/5
summary(ratings$rating_overall)
tapply(ratings$rating_overall,ratings$used, mean )


millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"))

millers$miller_rating_overall <- rowSums(millers[c("hh.maize.q36","hh.maize.q37","hh.maize.q38","hh.maize.q39","hh.maize.q40")])/5
summary(millers$miller_rating_overall)

wilcox.test(ratings$rating_overall,millers$miller_rating_overall)
### these are the three tests in the graph
wilcox.test(ratings$rating_overall[ratings$used=="No"],millers$miller_rating_overall)
wilcox.test(ratings$rating_overall[ratings$used=="Yes"],millers$miller_rating_overall)
wilcox.test(ratings$rating_overall[ratings$used=="Yes"],ratings$rating_overall[ratings$used=="No"])


df <- data.frame(c(mean(millers$miller_rating_overall),tapply(ratings$rating_overall,ratings$used, mean )[2:3]))
names(df) <- "score"
rownames(df) <- NULL
df$levels <- c("miller","non-customer","customer")
df <- df[order(df$score,decreasing = TRUE),]
df$levels <- factor(df$levels,levels= c("miller","customer","non-customer"))

png(paste(path, "figures/fig_miller_1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
ggplot(df, aes(x=levels, y=score)) +  geom_bar(stat="identity")+theme_minimal() + theme(axis.title = element_blank()) + geom_signif(comparisons = list(c("miller", "customer")), annotations="***", y_position = 4.4, tip_length = 0.03) +
  geom_signif(comparisons = list(c("miller", "non-customer")), annotations="***", y_position = 4.6, tip_length = 0.03) +
  geom_signif(comparisons = list(c("customer", "non-customer")), annotations="***", y_position = 4.2, tip_length = 0.03)
dev.off()


# now create these kind of likert scales bar charts for the different components of the scores, again for the three categories

plot_non_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$used=="Yes"),2)[,1],
prop.table(table(ratings$rating_price, ratings$used=="Yes"),2)[,1],
prop.table(table(ratings$rating_quality, ratings$used=="Yes"),2)[,1],
prop.table(table(ratings$rating_service, ratings$used=="Yes"),2)[,1],
prop.table(table(ratings$rating_reputation, ratings$used=="Yes"),2)[,1]))
names(plot_non_customer) <- c("location","price","quality","service","reputation")

plot_customer <- data.frame(cbind(prop.table(table(ratings$rating_location, ratings$used=="Yes"),2)[,2],
prop.table(table(ratings$rating_price, ratings$used=="Yes"),2)[,2],
prop.table(table(ratings$rating_quality, ratings$used=="Yes"),2)[,2],
prop.table(table(ratings$rating_service, ratings$used=="Yes"),2)[,2],
prop.table(table(ratings$rating_reputation, ratings$used=="Yes"),2)[,2]))
names(plot_customer) <- c("location","price","quality","service","reputation")

plot_miller <- data.frame(cbind(c(prop.table(table(millers$hh.maize.q36))),
c(prop.table(table(millers$hh.maize.q37))),
c(0,prop.table(table(millers$hh.maize.q38))),
c(prop.table(table(millers$hh.maize.q39))),
c(0,prop.table(table(millers$hh.maize.q40)))))
names(plot_miller) <- c("location","price","quality","service","reputation")
png(paste(path, "figures/fig_miller_2.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
par(mfrow=c(1,3), xpd=NA, mar = c(10, 5,5, 1)) 
colfunc<-colorRampPalette(c("red", "green"))
barplot(as.matrix(plot_non_customer), col=colfunc(5), main="non-customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2) 
barplot(as.matrix(plot_customer), col=colfunc(5), main="customer", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
barplot(as.matrix(plot_miller), col=colfunc(5), main="miller", cex.main=1.5,cex.axis=1.5, cex.names=1.5,las=2)
dev.off()

######################################
#inter-rater agreements 
library(agreement)
devtools::install_github("jmgirard/agreement")

newdata <- ratings[ , c("id.agro", "farmerID", "rating_overall")]   

rat <- newdata[!apply(newdata == "", 1, any), ] ##check if it works without this

library(dplyr)
ratnew <- rat %>% rename (Object = id.agro, Rater = farmerID, Score = rating_overall)

##Calculate chance-adjusted indexes of categorical agreement for unordered categories
results1 <- cat_adjusted(ratnew)
summary(results1, ci = TRUE, type = "perc")
tidy(results1, type = "perc")
plot(results1)

##Calculate chance-adjusted indexes of categorical agreement for ordered categories
results2 <- cat_adjusted(ratnew, weighting = "linear")
summary(results2, ci = TRUE, type = "perc")
tidy(results2, type = "perc")
plot(results2)

##Calculate category-specific agreement
results3 <- cat_specific(ratnew)
summary(results3, ci = TRUE, type = "bca")
tidy(results3, type = "bca")
plot(results3)

##Calculate intraclass correlation coefficient for dimensional data with 1 trial
##each rater rates a different group of objects
results4 <- dim_icc(ratnew, model = "1A", type = "agreement", unit = "average",
                    object = Object, rater = Rater, score = Score, warnings = FALSE)
summary(results4)
tidy(results4)
plot(results4, intra = FALSE, inter = TRUE)

res <- dim_icc(highscore, model = "2", type = "agreement", unit = "average",
                             object = Object, rater = Rater, score = Score, warnings = FALSE)

##model 2A:both raters and objects are random, excludes interaction, can be used with single or multiple trials per rater
results_highscore <- dim_icc(highscore, model = "2A", type = "agreement", unit = "average",
                    object = Object, rater = Rater, score = Score, warnings = FALSE)

reshigh <- dim_icc(highscore, model = "2A", type = "consistency", unit = "average",
                             object = Object, rater = Rater, score = Score, warnings = FALSE)

rhigh <- dim_icc(highs, model = "2A", type = "agreement", unit = "average",
                   object = Object, rater = Rater, score = Score, warnings = FALSE)

##each rater rates a different group of objects

rate1b <- dim_icc(ratnew, model = "1B", type = "agreement", unit = "average",
                     object = Object, rater = Rater, score = Score, bootstrap = 20, warnings = TRUE)

rate <- dim_icc(highs, model = "1B", type = "agreement", unit = "single",
                   object = Object, rater = Rater, score = Score, warnings = FALSE)

rate1 <- dim_icc(highs, model = "1B", type = "agreement", unit = "average",
                   object = Object, rater = Rater, score = Score, warnings = FALSE)

highscore <- subset(ratnew, Score>=3)

highs <- subset(ratnew, Score>=4)

##Calculate chance-adjusted indexes of categorical agreement for ordered categories
results_high <- cat_adjusted(highs, weighting = "linear")
summary(results_high, ci = TRUE, type = "perc")
tidy(results_high, type = "perc")
plot(results_high)

r1 <- cat_specific(highs)
summary(r1, ci = TRUE, type = "bca")
tidy(r1, type = "bca")
plot(r1)

###############################################
##checking for missing data
library(mice)
md.pattern(ratings)
#No need for mice. This data set is completely observed.


###############################################
###RATING LOCATION
fe_interceptloc <- lm(rating_location ~ 1, data=ratings)
summary(fe_interceptloc)
#3.47568

tab_model(fe_interceptloc, show.se = TRUE, show.stat = TRUE)

fe_modlocation <- lm(rating_location ~ factor(id.agro) - 1, data = ratings)
summary(fe_modlocation)
##reject null

tab_model(fe_modlocation, show.se = TRUE, show.stat = TRUE)

tab_model(fe_interceptloc, fe_modlocation, show.se = TRUE, show.stat = TRUE)

##testing for normality
library(fBasics)
jarqueberaTest(fe_modlocation$resid)
#Asymptotic p Value: 3.625e-13 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution

##Joint Hypothesis testing using F stat
#Can we reject the hypothesis that the coefficient on id.agro is 0?
#We have to do joint hypothesis test. A joint hypothesis imposes restrictions on multiple regression coefficients.
library(car)
linearHypothesis(fe_modlocation, c("factor(id.agro)=0"))
#we can reject the hypothesis that the coefficients are 0 

##heteroskedasticity robust f-test
linearHypothesis(fe_modlocation, c("factor(id.agro)=0"), white.adjust = "hc1")
##same conclusion


########################################
##RATING PRICE 
fe_interceptprice <- lm(rating_price ~ 1, data=ratings)
summary(fe_interceptprice)
#2.93735

fe_modprice <- lm(rating_price ~ factor(id.agro) - 1, data = ratings)
summary(fe_modprice)
##reject null

tab_model(fe_modprice, show.se = TRUE, show.stat = TRUE)

tab_model(fe_interceptprice, fe_modprice, show.se = TRUE, show.stat = TRUE)

##testing for normality
library(fBasics)
jarqueberaTest(fe_modprice$resid)
#Asymptotic p Value: 0.001193 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution

##Joint Hypothesis testing using F stat
#Can we reject the hypothesis that the coefficient on id.agro is 0?
#We have to do joint hypothesis test. A joint hypothesis imposes restrictions on multiple regression coefficients.
library(car)
linearHypothesis(fe_modprice, c("factor(id.agro)=0"))
#we can reject the hypothesis that the coefficients are 0 

##heteroskedasticity robust f-test
linearHypothesis(fe_modprice, c("factor(id.agro)=0"), white.adjust = "hc1")
##same conclusion


########################################
##RATING QUALITY
fe_interceptqual <- lm(rating_quality ~ 1, data=ratings)
summary(fe_interceptqual)
#3.56224

fe_modqual <- lm(rating_quality ~ factor(id.agro) - 1, data = ratings)
summary(fe_modqual)
##reject null

tab_model(fe_modqual, show.se = TRUE, show.stat = TRUE)

tab_model(fe_interceptqual, fe_modqual, show.se = TRUE, show.stat = TRUE)

##testing for normality
library(fBasics)
jarqueberaTest(fe_modqual$resid)
#Asymptotic p Value: 4.104e-12 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution

##Joint Hypothesis testing using F stat
#Can we reject the hypothesis that the coefficient on id.agro is 0?
#We have to do joint hypothesis test. A joint hypothesis imposes restrictions on multiple regression coefficients.
library(car)
linearHypothesis(fe_modqual, c("factor(id.agro)=0"))
#we can reject the hypothesis that the coefficients are 0 

##heteroskedasticity robust f-test
linearHypothesis(fe_modqual, c("factor(id.agro)=0"), white.adjust = "hc1")
##same conclusion

###############################################################
##RATING STOCK
fe_interceptstock <- lm(rating_stock ~ 1, data=ratings)
summary(fe_interceptstock)
#3.79143

fe_modstock <- lm(rating_stock ~ factor(id.agro) - 1, data = ratings)
summary(fe_modstock)
##reject null

tab_model(fe_modstock, show.se = TRUE, show.stat = TRUE)

tab_model(fe_interceptstock, fe_modstock, show.se = TRUE, show.stat = TRUE)

##testing for normality
library(fBasics)
jarqueberaTest(fe_modstock$resid)
#Asymptotic p Value: < 2.2e-16 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution

##Joint Hypothesis testing using F stat
#Can we reject the hypothesis that the coefficient on id.agro is 0?
#We have to do joint hypothesis test. A joint hypothesis imposes restrictions on multiple regression coefficients.
library(car)
linearHypothesis(fe_modstock, c("factor(id.agro)=0"))
#we can reject the hypothesis that the coefficients are 0 

##heteroskedasticity robust f-test
linearHypothesis(fe_modqual, c("factor(id.agro)=0"), white.adjust = "hc1")
##same conclusion


###############################################################
##RATING REPUTATION
fe_interceptrepu <- lm(rating_reputation ~ 1, data=ratings)
summary(fe_interceptrepu)
#3.73042

fe_modrepu <- lm(rating_reputation ~ factor(id.agro) - 1, data = ratings)
summary(fe_modrepu)
##reject null

tab_model(fe_modrepu, show.se = TRUE, show.stat = TRUE)

tab_model(fe_interceptrepu, fe_modrepu, show.se = TRUE, show.stat = TRUE)

##testing for normality
library(fBasics)
jarqueberaTest(fe_modrepu$resid)
#Asymptotic p Value: < 2.2e-16 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution

##Joint Hypothesis testing using F stat
#Can we reject the hypothesis that the coefficient on id.agro is 0?
#We have to do joint hypothesis test. A joint hypothesis imposes restrictions on multiple regression coefficients.
library(car)
linearHypothesis(fe_modrepu, c("factor(id.agro)=0"))
#we can reject the hypothesis that the coefficients are 0 

##heteroskedasticity robust f-test
linearHypothesis(fe_modrepu, c("factor(id.agro)=0"), white.adjust = "hc1")
##same conclusion

###############################################################
##RATING OVERALL
fe_interceptoverall <- lm(rating_overall ~ 1, data=ratings)
summary(fe_interceptoverall)
#3.49942 

fe_modoverall <- lm(rating_overall ~ factor(id.agro) - 1, data = ratings)
summary(fe_modoverall)
##reject null

tab_model(fe_modoverall, show.se = TRUE, show.stat = TRUE)

tab_model(fe_interceptoverall, fe_modoverall, show.se = TRUE, show.stat = TRUE)

##testing for normality
library(fBasics)
jarqueberaTest(fe_modoverall$resid)
#Asymptotic p Value: < 2.2e-16 
#we reject the null that the skewness and kurtosis of residuals are statistically equal to zero
#therefore, not a normal distribution

##Joint Hypothesis testing using F stat
#Can we reject the hypothesis that the coefficient on id.agro is 0?
#We have to do joint hypothesis test. A joint hypothesis imposes restrictions on multiple regression coefficients.
library(car)
linearHypothesis(fe_modoverall, c("factor(id.agro)=0"))
#we can reject the hypothesis that the coefficients are 0 

##heteroskedasticity robust f-test
linearHypothesis(fe_modoverall, c("factor(id.agro)=0"), white.adjust = "hc1")
##same conclusion




