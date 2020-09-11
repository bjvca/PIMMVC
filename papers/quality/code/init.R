path <- getwd()
library(ggplot2)
path_2 <- strsplit(path, "/papers/quality")[[1]]
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

farmers$tot_plot_size <- rowSums(cbind(as.numeric(as.character(farmers$hh.maize.plot.1..q42)),as.numeric(as.character(farmers$hh.maize.plot.2..q42)) ,
as.numeric(as.character(farmers$hh.maize.plot.3..q42)),  
as.numeric(as.character(farmers$hh.maize.plot.4..q42)), 
as.numeric(as.character(farmers$hh.maize.plot.5..q42)),  
as.numeric(as.character(farmers$hh.maize.plot.6..q42))  ), na.rm=T )
farmers$hh.maize.q95[farmers$hh.maize.q95 > 998] <- NA
farmers$tot_prod <- farmers$hh.maize.q95*farmers$hh.maize.q96
farmers$maize_yield <- farmers$tot_prod/farmers$tot_plot_size

farmers$maize_yield_cap <- farmers$maize_yield/farmers$hh.maize.q28

### remove 999 from 
trans <- paste(paste("hh.maize.transaction", 1:7, sep="."),".q101d", sep=".")

farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
farmers[trans] <- lapply(farmers[trans], function(x) replace(x, x == 999,NA) )


farmers$tot_sold  <- rowSums(farmers[trans], na.rm=T )*farmers$hh.maize.q96

farmers$tot_sold[farmers$tot_sold > farmers$tot_prod] <- NA

farmers$market_part_rate <- farmers$tot_sold / farmers$tot_prod 

### create a graph for transactions
#extract month from date and convert to factor (month) - farmers$hh.maize.transaction.1..q101c
farmers$month_sold_trans1 <-sub("\\/.*", "", farmers$hh.maize.transaction.1..q101c)
farmers$month_sold_trans2 <-sub("\\/.*", "", farmers$hh.maize.transaction.2..q101c)
farmers$month_sold_trans3 <-sub("\\/.*", "", farmers$hh.maize.transaction.3..q101c)
farmers$month_sold_trans4 <-sub("\\/.*", "", farmers$hh.maize.transaction.4..q101c)
farmers$month_sold_trans5 <-sub("\\/.*", "", farmers$hh.maize.transaction.5..q101c)
farmers$month_sold_trans6 <-sub("\\/.*", "", farmers$hh.maize.transaction.6..q101c)
farmers$month_sold_trans7 <-sub("\\/.*", "", farmers$hh.maize.transaction.7..q101c)

farmers$month_sold_trans1[farmers$month_sold_trans1 == "n"] <- NA 
farmers$month_sold_trans2[farmers$month_sold_trans2 == "n"] <- NA
farmers$month_sold_trans3[farmers$month_sold_trans3 == "n"] <- NA
farmers$month_sold_trans4[farmers$month_sold_trans4 == "n"] <- NA
farmers$month_sold_trans5[farmers$month_sold_trans5 == "n"] <- NA
farmers$month_sold_trans6[farmers$month_sold_trans6 == "n"] <- NA
farmers$month_sold_trans7[farmers$month_sold_trans7 == "n"] <- NA
sold_all <- c(farmers$month_sold_trans1, farmers$month_sold_trans2, farmers$month_sold_trans3, farmers$month_sold_trans4, farmers$month_sold_trans5, farmers$month_sold_trans6, farmers$month_sold_trans7)

amount_sold_all <- c(as.numeric(as.character(farmers$hh.maize.transaction.1..q101d))*farmers$hh.maize.q96,
as.numeric(as.character(farmers$hh.maize.transaction.2..q101d))*farmers$hh.maize.q96,
as.numeric(as.character(farmers$hh.maize.transaction.3..q101d))*farmers$hh.maize.q96,
as.numeric(as.character(farmers$hh.maize.transaction.4..q101d))*farmers$hh.maize.q96,
as.numeric(as.character(farmers$hh.maize.transaction.5..q101d))*farmers$hh.maize.q96,
as.numeric(as.character(farmers$hh.maize.transaction.6..q101d))*farmers$hh.maize.q96,
as.numeric(as.character(farmers$hh.maize.transaction.7..q101d))*farmers$hh.maize.q96)

df_fig <- data.frame(sold_all, amount_sold_all)
names(df_fig) <- c("month","amount")
df_fig$month <- as.numeric(as.character(df_fig$month))
df_fig <- subset(df_fig,!is.na(month) & !is.na(amount))
sold_all_full <-  sold_all
sold_all <- sold_all[!is.na(sold_all)]
### add a kernel smoother for quantities sold

png(paste(path, "figures/fig_1.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
plot(prop.table(table(sold_all)))
lines(tapply(df_fig$amount,df_fig$month,sum)/sum(df_fig$amount))
dev.off()

### now look at prices:
trans <- paste(paste("hh.maize.transaction", 1:7, sep="."),".price", sep=".")

farmers[trans] <- lapply(farmers[trans], function(x) as.numeric(as.character(x)) )
farmers[trans] <- lapply(farmers[trans], function(x) replace(x, x < 10,NA) )
farmers[trans] <- lapply(farmers[trans], function(x) replace(x, x > 1800,NA) )

prices <- data.frame(sold_all_full,unlist(lapply(farmers[trans], function(x) c(x))))
names(prices) <- c("month","price")
prices <- data.frame(tapply(prices$price, prices$month, mean, na.rm=T))
names(prices) <- "price"
prices$month <- as.numeric(rownames(prices))

png(paste(path, "figures/fig_2.png",sep = "/"), units="px", height=3200, width= 3200, res=600)
#plot(tapply(prices$price, prices$month, mean, na.rm=T), type="l")
ggplot(prices, aes(x = month, y = price)) + geom_smooth(se=FALSE) + scale_x_discrete(name ="month", limits=1:12)
dev.off()
traders <- read.csv(paste(path_2,"data/public/traders.csv", sep = "/"))

traders$hh.maize.value.q15a[traders$hh.maize.value.q15a + traders$hh.maize.value.q15b + traders$hh.maize.value.q15c + traders$hh.maize.value.q15d + traders$hh.maize.value.q15e >100] <- NA

traders$hh.maize.q22a[traders$hh.maize.q22a == 999] <- 0

traders$hh.maize.q22b[traders$hh.maize.q22b < 100] <- NA
cbind(traders$hh.maize.q22a,traders$hh.maize.q22b)


traders$hh.maize.q23a[traders$hh.maize.q23a == 999] <- 0



summary(traders$hh.maize.q22a)
summary(traders$hh.maize.q23a)

summary(traders$hh.maize.q22b[traders$hh.maize.q22a>0]/traders$hh.maize.q22a[traders$hh.maize.q22a>0])
summary(traders$hh.maize.q23b[traders$hh.maize.q23a>0]/traders$hh.maize.q23a[traders$hh.maize.q23a>0])

mean(traders$hh.maize.q22c)
mean(traders$hh.maize.q23c)

millers <- read.csv(paste(path_2,"data/public/millers.csv", sep = "/"))

millers$hh.maize.q17a[millers$hh.maize.q17a == 1] <-  100
prop.table(table(millers$hh.maize.q17a==100))[2]

#####make reputation variable - 
ratings <- merge(merge(aggregate(as.numeric(as.character(farmers$hh.maize.agro1.q108l)),list(farmers$id.agro1), FUN=mean, na.rm=T), aggregate(as.numeric(as.character(farmers$hh.maize.agro2.q109l)),list(farmers$id.agro2), FUN=mean, na.rm=T), by = "Group.1", all=T)
                 ,
                 aggregate(as.numeric(as.character(farmers$hh.maize.agro3.q111l)),list(farmers$id.agro3), FUN=mean, na.rm=T), by = "Group.1", all=T)
ratings <- subset(ratings,Group.1!="n/a")

ratings <- merge(merge(aggregate(as.numeric(as.character(farmers$hh.maize.agro1.q108j)),list(farmers$id.agro1), FUN=mean, na.rm=T), aggregate(as.numeric(as.character(farmers$hh.maize.agro2.q109j)),list(farmers$id.agro2), FUN=mean, na.rm=T), by = "Group.1", all=T)
                 ,
                 aggregate(as.numeric(as.character(farmers$hh.maize.agro3.q111j)),list(farmers$id.agro3), FUN=mean, na.rm=T), by = "Group.1", all=T)
ratings <- subset(ratings,Group.1!="n/a")

ratings$reputation_av_farmers <- rowMeans(ratings[,2:4], na.rm=T)

dealers <- read.csv(paste(path_2,"data/public/agro_input_dealers.csv", sep = "/"))
dealers <- merge(dealers, ratings[,c(1,5)],by.x="id.agro" ,by.y="Group.1",all.x=T)

#merge dealers to farmers

farmer_merge <-  merge(farmers, dealers[c("id.agro","reputation_av_farmers")],by.x="id.agro1", by.y="id.agro")
summary(lm(maize_yield~reputation_av_farmers, data=farmer_merge))

#####make reputation variable - for millers
ratings <- merge(merge(aggregate(as.numeric(as.character(farmers$hh.maize.miller1.q98i)),list(farmers$id.miller1), FUN=mean, na.rm=T), aggregate(as.numeric(as.character(farmers$hh.maize.miller2.q99i)),list(farmers$id.miller2), FUN=mean, na.rm=T), by = "Group.1", all=T)
                 ,
                 aggregate(as.numeric(as.character(farmers$hh.maize.miller3.q100i)),list(farmers$id.miller3), FUN=mean, na.rm=T), by = "Group.1", all=T)
ratings <- subset(ratings,Group.1!="n/a")



ratings$reputation_av_farmers <- rowMeans(ratings[,2:4], na.rm=T)

millers <- merge(millers, ratings[,c(1,5)],by.x="id.miller" ,by.y="Group.1",all.x=T)

#merge dealers to farmers

farmer_merge <-  merge(farmers, millers[c("id.miller","reputation_av_farmers")],by.x="id.miller1", by.y="id.miller")
summary(lm(maize_yield~reputation_av_farmers, data=farmer_merge))


