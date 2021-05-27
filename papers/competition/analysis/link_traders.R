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

table(traders$hh.maize.village[traders$hh.maize.village=="NAWANSEGA BUGODHANDHALA"])
traders$hh.maize.village[traders$hh.maize.village=="NAWANSEGA BUGODHANDHALA"] <- "BUGODHANDALA"

table(traders$hh.maize.village[traders$hh.maize.village=="KITEGARWA"])
traders$hh.maize.village[traders$hh.maize.village=="KITEGARWA"] <- "KITEIGALWA"

table(traders$hh.maize.village[traders$hh.maize.village=="KITAIGALWA"])
traders$hh.maize.village[traders$hh.maize.village=="KITAIGALWA"] <- "KITEIGALWA"

table(traders$hh.maize.village[traders$hh.maize.village=="KITEGALWA"])
traders$hh.maize.village[traders$hh.maize.village=="KITEGALWA"] <- "KITEIGALWA"

table(traders$hh.maize.village[traders$hh.maize.village=="LUWOOKO B"])
traders$hh.maize.village[traders$hh.maize.village=="LUWOOKO B"] <- "LUWOOKO_B"

table(traders$hh.maize.village[traders$hh.maize.village=="LUWOOKO CENTRAL"])
traders$hh.maize.village[traders$hh.maize.village=="LUWOOKO CENTRAL"] <- "LUWOOKO_A"

table(traders$hh.maize.village[traders$hh.maize.village=="LUWOOKO A"])
traders$hh.maize.village[traders$hh.maize.village=="LUWOOKO A"] <- "LUWOOKO_A"

table(traders$hh.maize.village[traders$hh.maize.village=="WALUGOMA"])
traders$hh.maize.village[traders$hh.maize.village=="WALUGOMA"] <- "LUWOOKO_A"

table(traders$hh.maize.village[traders$hh.maize.village=="KITOGO"])
traders$hh.maize.village[traders$hh.maize.village=="KITOGO"] <- "LUWOOKO_A"

table(traders$hh.maize.village[traders$hh.maize.village=="BAJAKA"])
traders$hh.maize.village[traders$hh.maize.village=="BAJAKA"] <- "BUJAKA"

table(traders$hh.maize.village[traders$hh.maize.village=="NAKIGUNJJU"])
traders$hh.maize.village[traders$hh.maize.village=="NAKIGUNJJU"] <- "NAKIGUNJU"

table(traders$hh.maize.village[traders$hh.maize.village=="BULESA"])
traders$hh.maize.village[traders$hh.maize.village=="BULESA"] <- "BUKUTA"

table(traders$hh.maize.village[traders$hh.maize.village=="NAKABALE B"])
traders$hh.maize.village[traders$hh.maize.village=="NAKABALE B"] <- "BUKUTA"

table(traders$hh.maize.village[traders$hh.maize.village=="NAWAMBIDI"])
traders$hh.maize.village[traders$hh.maize.village=="NAWAMBIDI"] <- "NAWAMBIDHI"

table(traders$hh.maize.village[traders$hh.maize.village=="NAKONKOLO"])
traders$hh.maize.village[traders$hh.maize.village=="NAKONKOLO"] <- "NAWAMBIDHI"

table(traders$hh.maize.village[traders$hh.maize.village=="BUGUBO"])
traders$hh.maize.village[traders$hh.maize.village=="BUGUBO"] <- "BUDIBYA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUGIRI TOWN"])
traders$hh.maize.village[traders$hh.maize.village=="BUGIRI TOWN"] <- "BUJAKA"

table(traders$hh.maize.village[traders$hh.maize.village=="KAMWOKYA"])
traders$hh.maize.village[traders$hh.maize.village=="KAMWOKYA"] <- "KAYAIGO"

table(traders$hh.maize.village[traders$hh.maize.village=="BWOLE"])
traders$hh.maize.village[traders$hh.maize.village=="BWOLE"] <- "BUJAKA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUSWIRIRI"])
traders$hh.maize.village[traders$hh.maize.village=="BUSWIRIRI"] <- "BUDIBYA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUYEBE"])
traders$hh.maize.village[traders$hh.maize.village=="BUYEBE"] <- "BUYEBE_B"

table(traders$hh.maize.village[traders$hh.maize.village=="MAKHOMA B"])
traders$hh.maize.village[traders$hh.maize.village=="MAKHOMA B"] <- "MAKHOMA A"

table(traders$hh.maize.village[traders$hh.maize.village=="BUWUNI. MAKHOMA A"])
traders$hh.maize.village[traders$hh.maize.village=="BUWUNI. MAKHOMA A"] <- "MAKHOMA A"

table(traders$hh.maize.village[traders$hh.maize.village=="MAHOMA B"])
traders$hh.maize.village[traders$hh.maize.village=="MAHOMA B"] <- "MAKHOMA A"

table(traders$hh.maize.village[traders$hh.maize.village=="BUWUNI"])
traders$hh.maize.village[traders$hh.maize.village=="BUWUNI"] <- "BUYEBE_B"

table(traders$hh.maize.village[traders$hh.maize.village=="BUYEBE B"])
traders$hh.maize.village[traders$hh.maize.village=="BUYEBE B"] <- "BUYEBE_B"

table(traders$hh.maize.village[traders$hh.maize.village=="NONGO BULYAYOBYO"])
traders$hh.maize.village[traders$hh.maize.village=="NONGO BULYAYOBYO"] <- "BULYAYOBYO"

table(traders$hh.maize.village[traders$hh.maize.village=="BULULU TC"])
traders$hh.maize.village[traders$hh.maize.village=="BULULU TC"] <- "BULULU"

table(traders$hh.maize.village[traders$hh.maize.village=="MUTERERE"])
traders$hh.maize.village[traders$hh.maize.village=="MUTERERE"] <- "BULULU"

table(traders$hh.maize.village[traders$hh.maize.village=="MUTERERE TC"])
traders$hh.maize.village[traders$hh.maize.village=="MUTERERE TC"] <- "BULYAYOBYO"

table(traders$hh.maize.village[traders$hh.maize.village=="BUSINI"])
traders$hh.maize.village[traders$hh.maize.village=="BUSINI"] <- "BULYAYOBYO"

table(traders$hh.maize.village[traders$hh.maize.village=="BUWOLYA"])
traders$hh.maize.village[traders$hh.maize.village=="BUWOLYA"] <- "MAKOOVA"

table(traders$hh.maize.village[traders$hh.maize.village=="NAWANSONGA"])
traders$hh.maize.village[traders$hh.maize.village=="NAWANSONGA"] <- "NAWANSEGA"

table(traders$hh.maize.village[traders$hh.maize.village=="MAYUGE"])
traders$hh.maize.village[traders$hh.maize.village=="MAYUGE"] <- "MAKOOVA"

table(traders$hh.maize.village[traders$hh.maize.village=="MAYUGE NILE"])
traders$hh.maize.village[traders$hh.maize.village=="MAYUGE NILE"] <- "MAKOOVA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUWOOLYA"])
traders$hh.maize.village[traders$hh.maize.village=="BUWOOLYA"] <- "BUVUTWA"

table(traders$hh.maize.village[traders$hh.maize.village=="BUDHE"])
traders$hh.maize.village[traders$hh.maize.village=="BUDHE"] <- "BUDDE"

table(traders$hh.maize.village[traders$hh.maize.village=="LUWA KIMASA"])
traders$hh.maize.village[traders$hh.maize.village=="LUWA KIMASA"] <- "BUDDE"

table(traders$hh.maize.village[traders$hh.maize.village=="BULYAIYOBYO"])
traders$hh.maize.village[traders$hh.maize.village=="BULYAIYOBYO"] <- "BULYAYOBYO"

table(traders$hh.maize.village[traders$hh.maize.village=="NSAGO"])
traders$hh.maize.village[traders$hh.maize.village=="NSAGO"] <- "NSANGO"

table(traders$hh.maize.village[traders$hh.maize.village=="BUDHEMBE"])
traders$hh.maize.village[traders$hh.maize.village=="BUDHEMBE"] <- "BUDEMBE"

table(traders$hh.maize.village[traders$hh.maize.village=="MUWAYO"])
traders$hh.maize.village[traders$hh.maize.village=="MUWAYO"] <- "BULUGUYI"

table(traders$hh.maize.village[traders$hh.maize.village=="BULYANSWA"])
traders$hh.maize.village[traders$hh.maize.village=="BULYANSWA"] <- "MAZIRIGA"

table(traders$hh.maize.village[traders$hh.maize.village=="MAKOMA"])
traders$hh.maize.village[traders$hh.maize.village=="MAKOMA"] <- "MAZIRIGA"

#returns 3, however, can only find 2 in the Bugiri region 
table(traders$hh.maize.village[traders$hh.maize.village=="NAMAYEMBA"])

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


