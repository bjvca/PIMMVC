### copy pictures
#### find ~/data/projects/PIMMVC/data/raw_non_public/pictures/1831e23ac6df4e44ac29698c0d6b3e2f/ -name '*.jpg' -exec cp -t ~/data/projects/PIMMVC/data/raw_non_public/pictures/ {} +


### for central milk shed
rm(list=ls())
agro_input_dealers <- read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/RawData_Shops_ids.csv")
farmers <- read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/3rd level_Farmers_shops_Traders_Millers_LINKED.csv")
millers <- read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/RawData_Millers_ids.csv")
traders <- read.csv("/home/bjvca/data/projects/PIMMVC/data/raw_non_public/RawData_Traders_ids.csv")

### first for agro_input_dealers
### remove location (GPS)
agro_input_dealers <- agro_input_dealers[,9:234]
agro_input_dealers <- agro_input_dealers[,!(names(agro_input_dealers) %in% c("hh.q4","hh.q5","hh.q5a","hh.phone","hh.consent")) ]  

### mask location IDs
#### NOTE: These IDs need cleaning from sub-county downward

agro_input_dealers$hh.maize.district <- toupper(agro_input_dealers$hh.maize.district)
agro_input_dealers$hh.maize.sub <- toupper(agro_input_dealers$hh.maize.sub)
agro_input_dealers$hh.maize.parish <- toupper(agro_input_dealers$hh.maize.parish)
agro_input_dealers$hh.maize.village <- toupper(agro_input_dealers$hh.maize.village)


i_dist <- 1
agro_input_dealers$distID <- NULL
agro_input_dealers$subID <- NULL
agro_input_dealers$parID <- NULL
agro_input_dealers$vilID <- NULL
for (dist in names(table(agro_input_dealers$hh.maize.district))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(agro_input_dealers$hh.maize.sub[agro_input_dealers$hh.maize.district==dist]))) {
		print(sub)
		i_parish <- 1
		for (parish in names(table(agro_input_dealers$hh.maize.parish[agro_input_dealers$hh.maize.district==dist & agro_input_dealers$hh.maize.sub == sub]))) {
			print(parish)
			i_village <- 1
			for (village in names(table(agro_input_dealers$hh.maize.village[agro_input_dealers$hh.maize.district==dist & agro_input_dealers$hh.maize.sub == sub & agro_input_dealers$hh.maize.parish==parish]))) {
				print(village)
				agro_input_dealers$vilID[agro_input_dealers$hh.maize.district==dist & agro_input_dealers$hh.maize.sub == sub & agro_input_dealers$hh.maize.parish==parish & agro_input_dealers$hh.maize.village== village] <- i_village
				i_village <- i_village + 1
				}
			agro_input_dealers$parID[agro_input_dealers$hh.maize.district==dist & agro_input_dealers$hh.maize.sub == sub & agro_input_dealers$hh.maize.parish==parish] <- i_parish
			i_parish <- i_parish + 1
			}
		agro_input_dealers$subID[agro_input_dealers$hh.maize.district==dist & agro_input_dealers$hh.maize.sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
agro_input_dealers$distID[agro_input_dealers$hh.maize.district==dist ] <- i_dist
i_dist <- i_dist + 1
}

agro_input_dealers$hh.maize.q11a <- NULL
agro_input_dealers$hh.maize.district  <- NULL
agro_input_dealers$hh.maize.sub <- NULL
agro_input_dealers$hh.maize.parish <- NULL
agro_input_dealers$hh.maize.village <- NULL

write.csv(agro_input_dealers, "/home/bjvca/data/projects/PIMMVC/data/public/agro_input_dealers.csv", row.names=F)

### now for farmers
### remove location (GPS)
farmers <- farmers[,9:494]
farmers <- farmers[,!(names(farmers) %in% c("hh.q4","hh.q5","hh.q6","hh.phone","hh.consent")) ]  

### mask location IDs
#### NOTE: These IDs need cleaning from sub-county downward

farmers$hh.maize.district <- toupper(farmers$hh.maize.district)
farmers$hh.maize.sub <- toupper(farmers$hh.maize.sub)
farmers$hh.maize.parish <- toupper(farmers$hh.maize.parish)
farmers$hh.maize.village <- toupper(farmers$hh.maize.village)


i_dist <- 1
farmers$distID <- NULL
farmers$subID <- NULL
farmers$parID <- NULL
farmers$vilID <- NULL
for (dist in names(table(farmers$hh.maize.district))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(farmers$hh.maize.sub[farmers$hh.maize.district==dist]))) {
		print(sub)
		i_parish <- 1
		for (parish in names(table(farmers$hh.maize.parish[farmers$hh.maize.district==dist & farmers$hh.maize.sub == sub]))) {
			print(parish)
			i_village <- 1
			for (village in names(table(farmers$hh.maize.village[farmers$hh.maize.district==dist & farmers$hh.maize.sub == sub & farmers$hh.maize.parish==parish]))) {
				print(village)
				farmers$vilID[farmers$hh.maize.district==dist & farmers$hh.maize.sub == sub & farmers$hh.maize.parish==parish & farmers$hh.maize.village== village] <- i_village
				i_village <- i_village + 1
				}
			farmers$parID[farmers$hh.maize.district==dist & farmers$hh.maize.sub == sub & farmers$hh.maize.parish==parish] <- i_parish
			i_parish <- i_parish + 1
			}
		farmers$subID[farmers$hh.maize.district==dist & farmers$hh.maize.sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
farmers$distID[farmers$hh.maize.district==dist ] <- i_dist
i_dist <- i_dist + 1
}

farmers$hh.maize.plot.1..q41 <- NULL
farmers$hh.maize.plot.2..q41 <- NULL
farmers$hh.maize.plot.3..q41 <- NULL
farmers$hh.maize.plot.4..q41 <- NULL
farmers$hh.maize.plot.5..q41 <- NULL
farmers$hh.maize.plot.6..q41 <- NULL
farmers$hh.maize.plot_select_name <- NULL

farmers$hh.maize.beans.plotb.1..q68 <- NULL
farmers$hh.maize.beans.plotb.2..q68 <- NULL
farmers$hh.maize.beans.plotb.3..q68 <- NULL
farmers$hh.maize.beans.plotb.4..q68 <- NULL
farmers$hh.maize.beans.plot_select_name1 <- NULL

farmers$hh.maize.miller1.q98a <- NULL
farmers$hh.maize.miller1.q98b <- NULL
farmers$hh.maize.miller1.q98c <- NULL
farmers$hh.maize.miller1.q98e <- NULL
farmers$hh.maize.miller1.q98f <- NULL

farmers$hh.maize.miller2.q99a    <- NULL
farmers$hh.maize.miller2.q99b    <- NULL
farmers$hh.maize.miller2.q99c    <- NULL
farmers$hh.maize.miller2.q99e    <- NULL
farmers$hh.maize.miller2.q99f    <- NULL

farmers$hh.maize.miller3.q100a    <- NULL
farmers$hh.maize.miller3.q100b    <- NULL
farmers$hh.maize.miller3.q100c    <- NULL
farmers$hh.maize.miller3.q100e    <- NULL
farmers$hh.maize.miller3.q100f    <- NULL

farmers$hh.maize.trader1.q102a <- NULL
farmers$hh.maize.trader1.q102b <- NULL
farmers$hh.maize.trader1.q102c <- NULL
farmers$hh.maize.trader1.q102e <- NULL
farmers$hh.maize.trader1.q102f <- NULL

farmers$hh.maize.trader2.q103a <- NULL
farmers$hh.maize.trader2.q103b <- NULL
farmers$hh.maize.trader2.q103c <- NULL
farmers$hh.maize.trader2.q103e <- NULL
farmers$hh.maize.trader2.q103f <- NULL

farmers$hh.maize.trader3.q104a <- NULL
farmers$hh.maize.trader3.q104b <- NULL
farmers$hh.maize.trader3.q104c <- NULL
farmers$hh.maize.trader3.q104e <- NULL
farmers$hh.maize.trader3.q104f <- NULL

farmers$hh.maize.agro1.q108a   <- NULL          
farmers$hh.maize.agro1.q108b  <- NULL
farmers$hh.maize.agro1.q108c  <- NULL
farmers$hh.maize.agro1.q108d   <- NULL
farmers$hh.maize.agro1.q108f <- NULL
farmers$hh.maize.agro1.q108g <- NULL

farmers$hh.maize.agro2.q109a   <- NULL          
farmers$hh.maize.agro2.q109b  <- NULL
farmers$hh.maize.agro2.q109c  <- NULL
farmers$hh.maize.agro2.q109d   <- NULL
farmers$hh.maize.agro2.q109f <- NULL
farmers$hh.maize.agro2.q109g <- NULL

farmers$hh.maize.agro3.q111a   <- NULL          
farmers$hh.maize.agro3.q111b  <- NULL
farmers$hh.maize.agro3.q111c  <- NULL
farmers$hh.maize.agro3.q111d   <- NULL
farmers$hh.maize.agro3.q111f <- NULL
farmers$hh.maize.agro3.q111g <- NULL

farmers$ID <- paste("F", rownames(farmers), sep="_")

farmers$hh.maize.district <- NULL
farmers$hh.maize.sub <- NULL
farmers$hh.maize.parish <- NULL
farmers$hh.maize.village <- NULL

write.csv(farmers, "/home/bjvca/data/projects/PIMMVC/data/public/farmers.csv", row.names=F)

### now for millers
### remove location (GPS)
millers <- millers[,9:105]
millers <- millers[,!(names(millers) %in% c( "hh.q4", "hh.q4b",  "hh.q5", "hh.q5a", "hh.phone", "hh.consent")) ]  

### mask location IDs
#### NOTE: These IDs need cleaning from sub-county downward

millers$hh.maize.district <- toupper(millers$hh.maize.district)
millers$hh.maize.sub <- toupper(millers$hh.maize.sub)
millers$hh.maize.parish <- toupper(millers$hh.maize.parish)
millers$hh.maize.village <- toupper(millers$hh.maize.village)


i_dist <- 1
millers$distID <- NULL
millers$subID <- NULL
millers$parID <- NULL
millers$vilID <- NULL
for (dist in names(table(millers$hh.maize.district))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(millers$hh.maize.sub[millers$hh.maize.district==dist]))) {
		print(sub)
		i_parish <- 1
		for (parish in names(table(millers$hh.maize.parish[millers$hh.maize.district==dist & millers$hh.maize.sub == sub]))) {
			print(parish)
			i_village <- 1
			for (village in names(table(millers$hh.maize.village[millers$hh.maize.district==dist & millers$hh.maize.sub == sub & millers$hh.maize.parish==parish]))) {
				print(village)
				millers$vilID[millers$hh.maize.district==dist & millers$hh.maize.sub == sub & millers$hh.maize.parish==parish & millers$hh.maize.village== village] <- i_village
				i_village <- i_village + 1
				}
			millers$parID[millers$hh.maize.district==dist & millers$hh.maize.sub == sub & millers$hh.maize.parish==parish] <- i_parish
			i_parish <- i_parish + 1
			}
		millers$subID[millers$hh.maize.district==dist & millers$hh.maize.sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
millers$distID[millers$hh.maize.district==dist ] <- i_dist
i_dist <- i_dist + 1
}

millers$hh.maize.q11c <- NULL
millers$hh.maize.district <- NULL
millers$hh.maize.sub <- NULL
millers$hh.maize.parish <- NULL
millers$hh.maize.village <- NULL

write.csv(millers, "/home/bjvca/data/projects/PIMMVC/data/public/millers.csv", row.names=F)

## now for traders
### remove location (GPS)
traders <- traders[,9:146]
traders <- traders[,!(names(traders) %in% c( "hh.q2","hh.q3", "hh.phone", "hh.consent")) ]  

### mask location IDs
#### NOTE: These IDs need cleaning from sub-county downward

traders$hh.maize.district <- toupper(traders$hh.maize.district)
traders$hh.maize.sub <- toupper(traders$hh.maize.sub)
traders$hh.maize.parish <- toupper(traders$hh.maize.parish)
traders$hh.maize.village <- toupper(traders$hh.maize.village)


i_dist <- 1
traders$distID <- NULL
traders$subID <- NULL
traders$parID <- NULL
traders$vilID <- NULL
for (dist in names(table(traders$hh.maize.district))) {
	print(dist)
	i_sub <- 1
	for (sub in names(table(traders$hh.maize.sub[traders$hh.maize.district==dist]))) {
		print(sub)
		i_parish <- 1
		for (parish in names(table(traders$hh.maize.parish[traders$hh.maize.district==dist & traders$hh.maize.sub == sub]))) {
			print(parish)
			i_village <- 1
			for (village in names(table(traders$hh.maize.village[traders$hh.maize.district==dist & traders$hh.maize.sub == sub & traders$hh.maize.parish==parish]))) {
				print(village)
				traders$vilID[traders$hh.maize.district==dist & traders$hh.maize.sub == sub & traders$hh.maize.parish==parish & traders$hh.maize.village== village] <- i_village
				i_village <- i_village + 1
				}
			traders$parID[traders$hh.maize.district==dist & traders$hh.maize.sub == sub & traders$hh.maize.parish==parish] <- i_parish
			i_parish <- i_parish + 1
			}
		traders$subID[traders$hh.maize.district==dist & traders$hh.maize.sub == sub] <- i_sub
		i_sub <- i_sub + 1
	}
traders$distID[traders$hh.maize.district==dist ] <- i_dist
i_dist <- i_dist + 1
}


traders$hh.maize.district <- NULL  
traders$hh.maize.sub <- NULL
traders$hh.maize.parish <- NULL
traders$hh.maize.village <- NULL


write.csv(traders, "/home/bjvca/data/projects/PIMMVC/data/public/traders.csv", row.names=F)


