### run from /PIMMVC/papers/competition/
rm(list=ls())
library(stringr)
library(sf)
library(leaflet)
library(leafpop)
library(dplyr)

path <- getwd()

path <- strsplit(path, "/papers/competition")[[1]]


## linking traders to farmers using  - villages were pre-coded for farmers
traders <- read.csv(paste(path,"data/raw_non_public/RawData_Traders_ids.csv", sep="/"), stringsAsFactors = TRUE)
farmers <- read.csv(paste(path,"data/raw_non_public/3rd level_Farmers_shops_Traders_Millers_LINKED.csv", sep="/"), stringsAsFactors = TRUE)
traders$hh.maize.village <- trimws(traders$hh.maize.village, which = "both")
traders$hh.maize.village <- str_replace_all(traders$hh.maize.village, "[\r\n]" , " ")
traders$hh.maize.village <- str_replace_all(traders$hh.maize.village, "\\s+" , " ")
traders$hh.maize.village <- toupper(traders$hh.maize.village)

##Correcting the village names based on map by checking if the village names of traders match the village names of farmers
table(traders$hh.maize.village[traders$hh.maize.village=="NANDEKULA A"])
traders$hh.maize.village[traders$hh.maize.village=="NANDEKULA A"] <- "NANDEKULA_A"

table(traders$hh.maize.village[traders$hh.maize.village=="NANDEKULA B"])
traders$hh.maize.village[traders$hh.maize.village=="NANDEKULA B"] <- "NANDEKULA_B"

table(traders$hh.maize.village[traders$hh.maize.village=="BULUKUMU"])
traders$hh.maize.village[traders$hh.maize.village=="BULUKUMU"] <- "NANDEKULA_B"

table(traders$hh.maize.village[traders$hh.maize.village=="NAKIGO"])
traders$hh.maize.village[traders$hh.maize.village=="NAKIGO"] <- "NANDEKULA_B"

table(traders$hh.maize.village[traders$hh.maize.village=="BUGAMBWE"])
traders$hh.maize.village[traders$hh.maize.village=="BUGAMBWE"] <- "BUGABWE"

table(traders$hh.maize.village[traders$hh.maize.village=="BUDWEGE"])
traders$hh.maize.village[traders$hh.maize.village=="BUDWEGE"] <- "BUDHWEGE"

table(traders$hh.maize.village[traders$hh.maize.village=="BUSU"])
traders$hh.maize.village[traders$hh.maize.village=="BUSU"] <- "BUDHWEGE"

table(traders$hh.maize.village[traders$hh.maize.village=="WALUKUBA"])
traders$hh.maize.village[traders$hh.maize.village=="WALUKUBA"] <- "BUKOYO"

table(traders$hh.maize.village[traders$hh.maize.village=="MADIGANDERE"])
traders$hh.maize.village[traders$hh.maize.village=="MADIGANDERE"] <- "BUKOYO"

table(traders$hh.maize.village[traders$hh.maize.village=="MADIGANDELE"])
traders$hh.maize.village[traders$hh.maize.village=="MADIGANDELE"] <- "BUKOYO"

table(traders$hh.maize.village[traders$hh.maize.village=="NAMASOGA"])
traders$hh.maize.village[traders$hh.maize.village=="NAMASOGA"] <- "NAWANSEGA"

table(traders$hh.maize.village[traders$hh.maize.village=="LUYIRA"])
traders$hh.maize.village[traders$hh.maize.village=="LUYIRA"] <- "BUDHWEGE"

table(traders$hh.maize.village[traders$hh.maize.village=="NKANTU MAIN"])
traders$hh.maize.village[traders$hh.maize.village=="NKANTU MAIN"] <- "NTINDA"

table(traders$hh.maize.village[traders$hh.maize.village=="NKAATU MAIN"])
traders$hh.maize.village[traders$hh.maize.village=="NKAATU MAIN"] <- "NTINDA"

table(traders$hh.maize.village[traders$hh.maize.village=="BULUBANDI"])
traders$hh.maize.village[traders$hh.maize.village=="BULUBANDI"] <- "NANDEKULA_B"

table(traders$hh.maize.village[traders$hh.maize.village=="NKAATU"])
traders$hh.maize.village[traders$hh.maize.village=="NKAATU"] <- "NTINDA"

table(traders$hh.maize.village[traders$hh.maize.village=="KABIRA"])
traders$hh.maize.village[traders$hh.maize.village=="KABIRA"] <- "KAMIRA"

table(traders$hh.maize.village[traders$hh.maize.village=="KABILA"])
traders$hh.maize.village[traders$hh.maize.village=="KABILA"] <- "KAMIRA"

table(traders$hh.maize.village[traders$hh.maize.village=="NAWANSIGE"])
traders$hh.maize.village[traders$hh.maize.village=="NAWANSIGE"] <- "NAWANSEGA"

table(traders$hh.maize.village[traders$hh.maize.village=="BULUBANDA CENTRAL"])
traders$hh.maize.village[traders$hh.maize.village=="BULUBANDA CENTRAL"] <- "NANDEKULA_B"

table(traders$hh.maize.village[traders$hh.maize.village=="LUSAGWA"])
traders$hh.maize.village[traders$hh.maize.village=="LUSAGWA"] <- "LUSAGHWA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUSESA"])
traders$hh.maize.village[traders$hh.maize.village=="BUSESA"] <- "LUSAGHWA"

table(traders$hh.maize.village[traders$hh.maize.village=="NAKISENYI"])
traders$hh.maize.village[traders$hh.maize.village=="NAKISENYI"] <- "NAKISENE"

table(traders$hh.maize.village[traders$hh.maize.village=="IDUDI"])
traders$hh.maize.village[traders$hh.maize.village=="IDUDI"] <- "NAKISENE"

table(traders$hh.maize.village[traders$hh.maize.village=="BUSOOWA"])
traders$hh.maize.village[traders$hh.maize.village=="BUSOOWA"] <- "KAYAIGO"

table(traders$hh.maize.village[traders$hh.maize.village=="BUSOOLA"])
traders$hh.maize.village[traders$hh.maize.village=="BUSOOLA"] <- "NAWANSEGA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUWANGA"])
traders$hh.maize.village[traders$hh.maize.village=="BUWANGA"] <- "KITEIGALWA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUGONDANDALA"])
traders$hh.maize.village[traders$hh.maize.village=="BUGONDANDALA"] <- "BUGODHANDALA"

table(traders$hh.maize.village[traders$hh.maize.village=="NAWANSENGA"])
traders$hh.maize.village[traders$hh.maize.village=="NAWANSENGA"] <- "NAWANSEGA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUBONGE"])
traders$hh.maize.village[traders$hh.maize.village=="BUBONGE"] <- "BUGODHANDALA"

### let us see how much we can merge in this first step?
##first calculate average competition per village
farmers$hh.maize.q105[farmers$hh.maize.q105==999] <- NA
mean_comp <- aggregate(farmers$hh.maize.q105, by= list(farmers$hh.maize.village), FUN=mean, na.rm=T)
names(mean_comp) <- c("village","nr_traders")



traders <- merge(traders, mean_comp, by.x="hh.maize.village", by.y="village", all.x=T)

#create map
traders <-  traders[ is.na(traders$nr_traders),] 

to_plot_f <- farmers[c("hh.maize._gps_longitude", "hh.maize._gps_latitude","hh.maize.village")]
to_plot_t <- traders[c("hh.maize._gps_longitude", "hh.maize._gps_latitude","hh.maize.village")]
to_plot_f$actor <- "farmer" 
to_plot_t$actor <- "trader"
to_plot <- rbind(to_plot_f,to_plot_t)

pal <- colorFactor(c("green", "red"), domain = c("farmer", "trader"))

m <- leaflet() %>% setView(lat = 0.6, lng = 33.5, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=to_plot, lng=~as.numeric(as.character(hh.maize._gps_longitude)), lat=~as.numeric(as.character(hh.maize._gps_latitude)),radius= 3, label=~as.character(hh.maize.village),color=~pal(actor), group="X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))

library(htmlwidgets)
saveWidget(m, file="farmers_traders.html")


