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


### let us see how much we can merge in this first step?
##first calculate average competition per village
farmers$hh.maize.q105[farmers$hh.maize.q105==999] <- NA
mean_comp <- aggregate(farmers$hh.maize.q105, by= list(farmers$hh.maize.village), FUN=mean, na.rm=T)
names(mean_comp) <- c("village","nr_traders")
traders <- merge(traders, mean_comp, by.x="hh.maize.village", by.y="village", all.x=T)

#create map
traders <-  traders[ is.na(traders$nr_traders)] 

to_plot_f <- farmers[c("hh.maize._gps_longitude", "hh.maize._gps_latitude","hh.maize.village")]
to_plot_t <- traders[c("hh.maize._gps_longitude", "hh.maize._gps_latitude","hh.maize.village")]
to_plot_f$actor <- "farmer" 
to_plot_t$actor <- "trader"
to_plot <- rbind(to_plot_f,to_plot_t)

pal <- colorFactor(c("green", "red"), domain = c("farmer", "trader"))

m <- leaflet() %>% setView(lat = 0.6, lng = 33.5, zoom=9)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=to_plot, lng=~as.numeric(as.character(hh.maize._gps_longitude)), lat=~as.numeric(as.character(hh.maize._gps_latitude)),radius= 3, label=~as.character(hh.maize.village),color=~pal(actor), group="X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))

library(htmlwidgets)
saveWidget(m, file="farmers_traders.html")


