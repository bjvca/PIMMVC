#read in raw data as exported from ONA
#execute from /NWO seed system devt Uganda proposal development/baseline/data/agro_input/raw/

rm(list=ls())
set.seed(05112020)  #today's date

library(pracma)
library(sf)
library(leaflet)
library(leafpop)
library(dplyr)
library(clubSandwich)
library(stringr)
library(reshape2)

path <- getwd()


##################### CALCULATING DISTANCE BETWEEN FARMERS AND DEALERS ###################
##########################################################################################


##################   BASELINE   ############################

### reads in raw data (not public)
shops <- rbind(read.csv(paste(path,"perceptions/data_seed_systems/data/raw/baseline/Baseline_DealerXX_2020_09_27_03_06_54_713799.csv", sep="/")),read.csv(paste(path,"perceptions/data_seed_systems/data/raw/baseline/Baseline_DealerXXXX_2020_10_02_14_55_02_970765.csv", sep="/")))
# moisture1 <- read.csv(paste(path,"Moisture_formX2_2020_10_04_07_57_19_019044.csv", sep="/"))[c("id", "reading", "exp",  "date_pack", "origin", "cert", "lot", "verif", "variety", "other_var","company")]
# moisture2 <- read.csv(paste(path,"Moisture_formXX_2020_10_05_08_15_54_391127.csv", sep="/"))[c("id", "reading", "exp", "origin", "cert", "lot", "verif", "variety", "other_var","company")]
# moisture2$date_pack <- "n/a" 
# moisture2 <- moisture2[, c("id", "reading", "exp",  "date_pack", "origin", "cert", "lot", "verif", "variety", "other_var","company")]
# moisture <- data.frame(rbind(moisture1,moisture2))


# moisture$age <- difftime(strptime("01.10.2020", format = "%d.%m.%Y"),strptime(moisture$date_pack,format="%Y-%m-%d"),units="days")
# moisture$age[moisture$date_pack=="n/a"] <- difftime(strptime("01.10.2020", format = "%d.%m.%Y"),strptime(moisture$exp[moisture$date_pack=="n/a"] ,format="%Y-%m-%d"),units="days")+180

#this is for a graph I made for a presentation showing how moisture increases with age
#moisture <- subset(moisture, age > 10)
#moisture <- subset(moisture, age < 110)
#ggplot(data=moisture,aes(age, reading)) +
#  geom_point() +
#  geom_smooth(method = "lm",se = FALSE ) + geom_hline(yintercept=14, color = "red")

### manually correct spelling of IDs to merge seed testing to shop survey data
shops$maize.owner.agree.id <- as.character(shops$maize.owner.agree.id)
# moisture$id <- as.character(moisture$id)
# moisture$id[moisture$id == " 22 Waswa"] <- "23 Waswa"
# moisture$id[moisture$id == "15 Zebulon"] <- "15 Zebuloni"
# moisture$id[moisture$id == "20 Nabatu"] <- "20 Nambafu"
# moisture$id[moisture$id == "21Answer"] <- "21 No answer"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "21 Ntuyo  "] <- "21 Ntuyo"
#moisture$id[moisture$id == "22 Mugoda"] <- "22 Mukodha"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "22 Mutesi  "] <- "22 Mutesi"
# moisture$id[moisture$id == "22 Nakayima"] <- "22 Nakaima"
# moisture$id[moisture$id == "22 Namugabwa"] <- "22 Namugabwe"
# moisture$id[moisture$id == "22 Nyemera"] <- "22 Nyamera"
# moisture$id[moisture$id == "23 Bsdajabaka"] <- "23 Basajabaka"
# moisture$id[moisture$id == "23 Dhizala"] <- "23 Dhizaala"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "23 Kisige  "] <- "23 Kisige"
# moisture$id[moisture$id == "23 Nafula"] <- "23 Nanfula"
# moisture$id[moisture$id == "25 Kauli"] <- "25 Kawuli "
# moisture$id[moisture$id == "25 Nentunze"] <- "25 Netunze"
# moisture$id[moisture$id == "25Nandala"] <- "25 Nandera"
# moisture$id[moisture$id == "26 Babirye"] <- "26 Barbirye"
# moisture$id[moisture$id == "26 Nakawogo"] <- "26 Nakawago"
# moisture$id[moisture$id == "26 Namulondo"] <- "26 Namulonda"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "24 Kageye  Faishali"] <- "24 Kageye"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "26 Nangobi  "] <- "26 Nangobi" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "26 Tefiiro  "] <- "26 Tefiiro" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "26 Tefiro"] <- "26 Tefilo" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "27 Ssetimba"] <- "27 Setimba" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "28 Kaudha"] <- "28 Khauda"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "32 Biribo"] <- "32 Bilibo" 
#moisture$id[moisture$id == "32Pandaya"] <- "32 Pendaya"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Fazali"] <- "36 Fazili"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Kakayi  "] <- "36 Kakayi"    
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Murwanyi"] <- "36 Mulwanyi"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "36 Nakirima  "] <- "36 Nakirima"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "38 Waiswa  "] <- "38 Waiswa"
#moisture$id[moisture$id == "39 Siidamwebyo"] <- "39 Sidamwebyo"
#moisture$id[moisture$id == "40 Atiibwa"] <- "40 Atibwa"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "40 Kisuubi  "] <- "40 Kisubi"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "40 Naigaga"] <- "40 Naigsga" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "42 Osunye"] <-  "42 Osunyo" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "45 Wampende  "] <-  "45 Wampande"    
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "46 Kirya "] <-  "46 Kirya"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "46 Namususwa  "] <-  "46 Namususwa" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "46 Wanama"] <-  "46 Wanawa" 
#moisture$id[moisture$id == "46Nabwere" ] <- "46 Nabwire"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "48 Kanaabi  hardware"] <-  "48 Kanaabi"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "50 Iguude"] <-  "50 Iguube" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "50 Kharende  "] <-  "50 Khalende"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "50 Mpaunka  "] <-  "50 Mpanuka"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "55 Byekwaso  "] <-   "55 Byekwaso" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "21 Mwirugazu  "] <-   "21 Mwelugazu"
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "33 Adikini"] <-  "22 Adikini" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "32 Nakasiko"] <-  "22 Nakasiko" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "22 Wagubi  "] <-  "22 Wagubi"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "32 Kiraire"] <-  "23 Kiraire"   
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "25 Mukose"] <- "23 Mukose" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "24 Nandigobe"] <- "24 Nandigobo"  
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "24 Talima Irene (Birombo)"] <- "24 Talima" 
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "18 Nabirye"] <- "50 Nabirye" 

## these have two records in the shops data - these can not be matched... these are probably the two unknowns below
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "19 Masinde"] <- NA
shops$maize.owner.agree.id[shops$maize.owner.agree.id ==  "22 Muwanguzi"] <- NA
 
#merge in moisture data
#shops <- merge(shops,moisture, by.x="maize.owner.agree.id",by.y="id", all.x=T)

### these are records in the testing data that can not be merged:
#shops$maize.owner.agree.id[is.na(shops[,2])]
# [1] "19 Buyenze"     "21 Kabulandala" "27 Sharifa"     "30 Magada"     
# [5] "33 Awali"       "34 Awali"       "42 Nabayo"      "54 Alex "      
# [9] "Unknown"        "Unknown "
 
#create shop_ID

shops$shop_ID <- paste("AD",rownames(shops), sep="_")
shops$shop_ID <- factor(shops$shop_ID)

## write raw data to create panel with stack survey data
#write.csv(shops,file="wave2_raw.csv", row.names=FALSE)

#Categorizing different input dealers into catchment areas
shops$catchmentID <- NA
counter <- 1

for (shop_1 in names(table(shops$shop_ID))) {
shops$catchmentID[shops$shop_ID == shop_1] <- counter
for (shop_2 in names(table(shops$shop_ID))) {
### key parameter is chosen here: distance to define a catchment area. Here we assume that if shops are less then 5 km apart, they serve the same catchment area
if ( haversine(c(shops$maize.owner.agree._gps_latitude[shops$shop_ID == shop_1] ,shops$maize.owner.agree._gps_longitude[shops$shop_ID == shop_1]),c(shops$maize.owner.agree._gps_latitude[shops$shop_ID == shop_2],shops$maize.owner.agree._gps_longitude[shops$shop_ID == shop_2])) < 2.5) {
if (is.na(shops$catchmentID[shops$shop_ID == shop_2])) {  ## if the shop has not been allocated to a catcchment area yet, create a new one
 shops$catchmentID[shops$shop_ID == shop_2] <- counter
} else {  ## if the shop is already part of a catchment area
## change ID of all shops in catchement area to a new catchment area
 shops$catchmentID[shops$catchmentID == shops$catchmentID[shops$shop_ID == shop_1]]  <- shops$catchmentID[shops$shop_ID == shop_2] 
}

}
}
counter <- counter + 1
}
dim(table(shops$catchmentID))

# reorder catchement ID factor
 	i_catch <- 1
	for (catch in names(table(shops$catchmentID))) {

		shops$catchID[shops$catchmentID == catch] <- i_catch
		i_catch <- i_catch + 1
	}
shops$catchmentID <- NULL
#link to pictures 
shops$maize.owner.agree.q13 <- sub('.*\\/', '',shops$maize.owner.agree.q13 )
shops$maize.owner.agree.q13[shops$maize.owner.agree.q13 == 'a'] <- "no_picture.png"
shops$maize.owner.agree.q13 <-  gsub("jpg","png",shops$maize.owner.agree.q13) ### jpegs were converted to pngs
shops$images <- paste(paste(path,"pictures/converted/resized", sep="/"),shops$maize.owner.agree.q13, sep="/")

### randomization - 4 treatment cells for catchment level interventions
treats <- data.frame(names(table(shops$catchID)),sample(rep(1:4, length=length(table(shops$catchID)))))
names(treats) <- c("catchID", "treat")
shops <- merge(shops,treats, by="catchID")

table(shops$treat)

shops$training <- FALSE
shops$clearing <- FALSE

shops$training[shops$treat %in% c(1,2)] <- TRUE
shops$clearing[shops$treat %in% c(2,4)] <- TRUE

shops$farmer <- NA

shops$farmer[shops$treat == 1] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 1])))
shops$farmer[shops$treat == 2] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 2])))
shops$farmer[shops$treat == 3] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 3])))
shops$farmer[shops$treat == 4] <- sample(rep(c("TRUE","FALSE"), length.out=length(shops$farmer[shops$treat == 4])))

### export treatment assignment for merging in later

#write.csv(shops[c("catchID", "shop_ID","treat","training", "clearing", "farmer")],file="treats_shop_level.csv", row.names=FALSE)

#### prepare sampling list for farmer questionnaire
#farmers_list <- merge(shops[ !(names(shops) %in% c("district","sub"))], read.csv(paste(path,"villages_edited_final.csv", sep="/"))[c("shop_ID","district","sub","sampling_village")], by="shop_ID")
### 10 farmers in each village
#farmers_list <- farmers_list[rep(seq_len(nrow(farmers_list)), each = 10), ]
#generate farmer ID
#first reset the rownames
#rownames(farmers_list) <- NULL

#farmers_list$farmer_ID <- paste("F",as.numeric(rownames(farmers_list)),sep="_")

#farmers_list[c("district","sub","parish","sampling_village", "catchID", "farmer_ID")]

#by catchment area, give me names of all input dealers
store_shops <- array(dim=c(length(table(shops$catchID)),2+18*8))
for (i in 1:length(table(shops$catchID))) {
#print(c(i, names(table(shops$catchID))[i])) #catchment ID
store_shops[i,1] <-i
store_shops[i,2] <- length(shops$shop_ID[shops$catchID==i])  # number of shops in this catchment ID

#print(shops$shop_ID[shops$catchID==i])
#print(shops$maize.owner.agree.q13[shops$catchID==i] )
for (j in 1:length(shops$shop_ID[shops$catchID==i])) {
store_shops[i,j+2] <- as.character(shops$shop_ID[shops$catchID==i])[j] ##ID
store_shops[i,j+2+18] <- as.character(shops$maize.owner.agree.q13[shops$catchID==i])[j] ##image
store_shops[i,j+2+18*2] <-  as.character(shops$maize.owner.agree.biz_name[shops$catchID==i])[j] #name shop
store_shops[i,j+2+18*3] <-  as.character(shops$maize.owner.agree.family_name[shops$catchID==i])[j] #name owner
store_shops[i,j+2+18*4] <-  as.character(shops$maize.owner.agree.dealer_name[shops$catchID==i])[j] #name interviewee
store_shops[i,j+2+18*5] <-  as.character(shops$maize.owner.agree.nickname[shops$catchID==i])[j] #nick name interviewee
store_shops[i,j+2+18*6] <-  as.character(shops$maize.owner.agree.market_name[shops$catchID==i])[j] ##location
store_shops[i,j+2+18*7] <-  as.character(shops$maize.owner.agree.eye[shops$catchID==i])[j] ##description
}
#line <- cbind(i,length(shops$shop_ID[shops$catchID==i]),as.character(shops$shop_ID[shops$catchID==i]),shops$maize.owner.agree.q13[shops$catchID==i])

}
store_shops <- data.frame(store_shops)
names(store_shops) <- c("catchID", "nr_shops_in_catch",paste("ID_shop",seq(1:18), sep="_"), paste("image_shop", seq(1:18),sep="_"), paste("name_shop", seq(1:18),sep="_"), paste("owner_name_shop", seq(1:18),sep="_"), paste("name_person_interviewed", seq(1:18),sep="_"), paste("nickname_person_interviewed", seq(1:18),sep="_"), paste("location_shop", seq(1:18),sep="_"), paste("description_shop", seq(1:18),sep="_"))

## export for charles:
ODK_imp <- shops[c("catchID", "shop_ID","maize.owner.agree.biz_name", "maize.owner.agree.q13","maize.owner.agree.family_name", "maize.owner.agree.dealer_name","maize.owner.agree.nickname","maize.owner.agree.market_name","maize.owner.agree.eye")]
## remove all trailing and leading spaces - this gives issues in ODK
ODK_imp <- data.frame(lapply( ODK_imp, function(x)  trimws(x, which = "both")))
ODK_imp <- data.frame(lapply( ODK_imp,  function(x) str_replace_all(x, "[\r\n]" , " ")))
ODK_imp <- data.frame(lapply( ODK_imp,  function(x) str_replace_all(x, "\\s+" , " ")))
#write.csv(ODK_imp,file="ODK_imp.csv",row.names=FALSE)

# write.csv(test2,file="matcher_file.csv", row.names=FALSE)
# write.csv(shops,file="shops_map.csv", row.names=FALSE)

### make a map with catchment ID coloring and pictures (not public)
# pal <- colorFactor(
#   palette = 'Dark2',
#   domain = shops$catchID
# )




#m <- leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=shops, lng=~maize.owner.agree._gps_longitude, lat=~maize.owner.agree._gps_latitude,radius= 8, color=~pal(catchID), popup = ~as.character(catchID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))  %>%  addPopupImages(  cbind(shops$images,sample(shops$images)), width=137, height =200, group = "X_uuid")

###only those that need to be rated
#m <- leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=shops[shops$clearing==TRUE,], lng=~maize.owner.agree._gps_longitude, lat=~maize.owner.agree._gps_latitude,radius= 8, color=~pal(catchID), popup = ~as.character(shop_ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))  

# library(htmlwidgets)
# saveWidget(m, file="map_input_dealers.html")


#prepare data for public release
##remove GPS coordinates
## remove metadata
#note: use "maize.owner.agree.id" to link records to seed quality test data (moisture, packageing, ...)


to_drop <- c("start","end","deviceid","simserial", "phonenumber", "subscriberid", "start.geopoint","X_start.geopoint_latitude", "X_start.geopoint_longitude","X_start.geopoint_altitude", "X_start.geopoint_precision","maize.owner.agree.eye","maize.owner.agree.id", "meta.instanceID","X_id", "X_uuid","X_submission_time", "X_tags","X_notes","X_version", "X_duration", "X_submitted_by", "X_total_media", "X_media_count","X_media_all_received", "X_xform_id" )   
 
shops <- shops[ , !(names(shops) %in% to_drop)]

##remove names and other data that can be used to id
# to_drop <- c("maize.owner.agree.gps","maize.owner.agree._gps_longitude","maize.owner.agree._gps_latitude") 
# shops <- shops[ , !(names(shops) %in% to_drop)]

#this links to pictures
to_drop <- c("maize.owner.agree.q13","images")
shops <- shops[ , !(names(shops) %in% to_drop)]


#### create list of villages to be included
## step 1: make sure there are no villages that belong to 2 or more catchment areas

# get proper district and subcounty names from edited villages lists (ex-post)

# shops <- merge(shops[ !(names(shops) %in% c("district","sub"))], read.csv(paste(path,"villages_edited_final.csv", sep="/"))[c("shop_ID","district","sub","sampling_village")], by="shop_ID")
# 
# write.csv(shops[c("catchID","district","sub","parish","village","shop_ID","maize.owner.agree.catch_area.Village1", "maize.owner.agree.catch_area.Village2" , "maize.owner.agree.catch_area.Village3")], file="villages.csv")

## write file for wilberfoce to organize trainings:
# wilber <- subset(shops, training == TRUE)
# write.csv(wilber[c("shop_ID","district","sub","parish","village","maize.owner.agree.dealer_name","maize.owner.agree.surname","maize.owner.agree.nickname","maize.owner.agree.phone1","maize.owner.agree.phone2",	"maize.owner.agree.biz_name","maize.owner.agree.family_name",	"maize.owner.agree.market_name","enumerator","other_district","catchID"
# )], file="wilber.csv",row.names=FALSE)

## remove villages where most customers are - this needs to be used for sampling of households
to_drop <- c("maize.owner.agree.catch_area.Village1", "maize.owner.agree.catch_area.Village2" , "maize.owner.agree.catch_area.Village3")  
shops <- shops[ , !(names(shops) %in% to_drop)]

## drop location, names and contact details
to_drop <- c("parish","village" ,"maize.owner.agree.dealer_name", "maize.owner.agree.surname", "maize.owner.agree.nickname", "maize.owner.agree.phone1", "maize.owner.agree.phone2", "maize.owner.agree.biz_name", "maize.owner.agree.family_name", "maize.owner.agree.market_name","enumerator","other_district")
 shops <- shops[ , !(names(shops) %in% to_drop)]

##convert district names and subcounty names to numeric codes 
# i_dist <- 1
# farmers$distID <- NULL
# farmers$subID <- NULL
# 
# for (dist in names(table(shops$district))) {
# 	print(dist)
# 	i_sub <- 1
# 	for (sub in names(table(shops$sub[shops$district==dist]))) {
# 		print(sub)
# 
# 		shops$subID[shops$district==dist & shops$sub == sub] <- i_sub
# 		i_sub <- i_sub + 1
# 	}
# shops$distID[shops$district==dist ] <- i_dist
# i_dist <- i_dist + 1
# }
 
### BUT subcounty names was not pre-populated in the ODK app, so enumerators often spell differently/wrong, so better just delete (or manually correct first)
to_drop <- c("subID","district","sub")
 shops <- shops[ , !(names(shops) %in% to_drop)]

##############################################################################################################
 
### data with the correct gps coordinates of dealers 
 shops_coordinate <- read.csv(paste(path,"perceptions/data_seed_systems/data/raw/baseline/final_richard.csv", sep="/"))
 
#dataset only having gps locations of dealers in baseline 
shops_base<-shops_coordinate[, c("shop_ID", "gps_latitude", "gps_longitude")]
  
#path <- strsplit(path, "/raw")[[1]]
#write.csv(shops,paste(path,"papers/perceptions/data_seed_systems/data/raw/baseline/dealer_location.csv", sep="/"), row.names=FALSE)

#reading in farmers' baseline data to get the locations 
farmers <- read.csv(paste(path,"perceptions/data_seed_systems/data/raw/baseline/baseline_farmer_2021_04_25_15_03_43_633118.csv", sep = "/"), stringsAsFactors = FALSE)


###################### prepping farmers' baseline data #######################

#charles changed this name?
farmers$village[farmers$village == "Buwaje"] <- "Buwagi"
### fix IDs
##one duplicate in village but only 9 farmers interviewed
farmers$farmer_ID[farmers$village == "Musakira" & farmers$farmer_ID == "F_2997" & farmers$X_id == "77127699"] <-  "F_2998"
farmers$farmer_ID[farmers$village == "Kasaka" & farmers$farmer_ID == "F_2516" & farmers$X_id == "77127934"] <-  "F_2512"
farmers$farmer_ID[farmers$village == "Kityelera" & farmers$farmer_ID == "F_1457" & farmers$X_id == "77154707"] <-  "F_1458"
farmers$farmer_ID[farmers$village == "Namusambya" & farmers$farmer_ID == "F_3461" & farmers$X_id == "77158044"] <-  "F_3463"
farmers$farmer_ID[farmers$village == "Bukaleba" & farmers$farmer_ID == "F_374" & farmers$X_id == "77221052"] <-  "F_375"
farmers$farmer_ID[farmers$village == "Nakalanga" & farmers$farmer_ID == "F_231" & farmers$X_id == "77260860"] <-  "F_232"
farmers$farmer_ID[farmers$village == "Naigobya" & farmers$farmer_ID == "F_1131" & farmers$X_id == "77310057"] <-  "F_1139"
farmers$farmer_ID[farmers$village == "Busanda" & farmers$farmer_ID == "F_1842" & farmers$X_id == "77353708"] <-  "F_1848"
farmers$farmer_ID[farmers$village == "Nawantale" & farmers$farmer_ID == "F_1481" & farmers$X_id == "77353775"] <-  "F_1488"
farmers$farmer_ID[farmers$village == "Namatooke" & farmers$farmer_ID == "F_2100" & farmers$X_id == "77386817"] <-  "F_2091"
farmers$farmer_ID[farmers$village == "Nabuguzi" & farmers$farmer_ID == "F_1645" & farmers$X_id == "77431542"] <-  "F_1641"
farmers$farmer_ID[farmers$village == "Matyama" & farmers$farmer_ID == "F_2185" & farmers$X_id == "77431611"] <-  "F_2184"
farmers$farmer_ID[farmers$village == "Namato" & farmers$farmer_ID == "F_1639" & farmers$X_id == "77471346"] <-  "F_1633"
farmers$farmer_ID[farmers$village == "Busenke" & farmers$farmer_ID == "F_3429" & farmers$X_id == "77471516"] <-  "F_3428"
farmers$farmer_ID[farmers$village == "Buyunga (Kaliro)" & farmers$farmer_ID == "F_918" & farmers$X_id == "77471605"] <-  "F_919"
farmers$farmer_ID[farmers$village == "Bumanya" & farmers$farmer_ID == "F_2697" & farmers$X_id == "77520869"] <-  "F_2691"
farmers$farmer_ID[farmers$village == "Nawandio" & farmers$farmer_ID == "F_2867" & farmers$X_id == "77521107"] <-  "F_2866"
farmers$farmer_ID[farmers$village == "Kisozi" & farmers$farmer_ID == "F_2245" & farmers$X_id == "77721638"] <-  "F_2242"

##two duplicates in village but only 8 farmers interviewed
farmers$farmer_ID[farmers$village == "Nasuti" & farmers$farmer_ID == "F_741" & farmers$X_id == "77310028"] <- "F_747"
farmers$farmer_ID[farmers$village == "Nasuti" & farmers$farmer_ID == "F_742" & farmers$X_id == "77310022"] <- "F_750"

farmers$farmer_ID[farmers$village == "Buwaaya" & farmers$farmer_ID == "F_2222" & farmers$X_id == "77127585"] <- "F_2228"
farmers$farmer_ID[farmers$village == "Buwaaya" & farmers$farmer_ID == "F_2225" & farmers$X_id == "77127666"] <- "F_2230"

farmers$farmer_ID[farmers$village == "Kigandaalo" & farmers$farmer_ID == "F_1051" & farmers$X_id == "77221038"] <- "F_1056"
farmers$farmer_ID[farmers$village == "Kigandaalo" & farmers$farmer_ID == "F_1055" & farmers$X_id == "77221059"] <- "F_1060"

##three duplicates in village but only 7 farmers interviewed
farmers$farmer_ID[farmers$village == "Bukowe" & farmers$farmer_ID == "F_146" & farmers$X_id == "77386572"] <- "F_141"
farmers$farmer_ID[farmers$village == "Bukowe" & farmers$farmer_ID == "F_147" & farmers$X_id == "77386603"] <- "F_142"
farmers$farmer_ID[farmers$village == "Bukowe" & farmers$farmer_ID == "F_148" & farmers$X_id == "77386580"] <- "F_144"

##in Mpungwe, 3 IDs are duplicated ( F_2148 F_2149 F_2150 ) but all 10 observations are there
## the duplicates are actually from Bulondo, where only 7 were recorded
farmers$farmer_ID[farmers$village == "Mpungwe" & farmers$farmer_ID == "F_2148" & farmers$X_id == "77127824"] <- "F_3231"
farmers$farmer_ID[farmers$village == "Mpungwe" & farmers$farmer_ID == "F_2149" & farmers$X_id == "77127603"] <- "F_3232"
farmers$farmer_ID[farmers$village == "Mpungwe" & farmers$farmer_ID == "F_2150" & farmers$X_id == "77127627"] <- "F_3234"
farmers$village[farmers$farmer_ID %in% c("F_3231","F_3232","F_3234")] <- "Bulondo"
farmers$sub[farmers$farmer_ID %in% c("F_3231","F_3232","F_3234")] <- "Mpungwe"
farmers$catchID[farmers$farmer_ID %in% c("F_3231","F_3232","F_3234")] <- 111
farmers$farmer_ID[farmers$village == "Kyeeya" & farmers$farmer_ID == "F_265" & farmers$X_id == "77145372"] <- "F_2475"
farmers$village[farmers$farmer_ID  == "F_2475"] <- "Isingo"

farmers$farmer_ID[farmers$village == "Bupala" & farmers$farmer_ID == "F_2316" & farmers$X_id == "77386635"] <- "F_3026"
farmers$village[farmers$farmer_ID  == "F_3026"] <- "Kalalu (Bugweri)"
farmers$sub[farmers$farmer_ID  == "F_3026"] <- "Idudi"
farmers$district[farmers$farmer_ID  == "F_3026"] <- "Bugweri"
farmers$catchID[farmers$farmer_ID  == "F_3026"] <- 9

farmers$farmer_ID[farmers$village == "Buwunga" & farmers$farmer_ID == "F_1892" & farmers$X_id == "77520637"] <- "F_1334"
farmers$village[farmers$farmer_ID  == "F_1334"] <- "Nakabale (Kaliro)"
farmers$sub[farmers$farmer_ID  == "F_1334"] <- "Kaliro Tc"
farmers$district[farmers$farmer_ID  == "F_1334"] <- "Kaliro"
farmers$catchID[farmers$farmer_ID  == "F_1334"] <- 61

farmers$farmer_ID[farmers$village == "Mutelele" & farmers$farmer_ID == "F_1965" & farmers$X_id == "77576784"] <- "F_1955"
farmers$village[farmers$farmer_ID  == "F_1955"] <- "Bululu"
farmers$sub[farmers$farmer_ID  == "F_1955"] <- "Muterere"
farmers$district[farmers$farmer_ID  == "F_1955"] <- "Bugiri"
farmers$catchID[farmers$farmer_ID  == "F_1955"] <- 98

farmers$farmer_ID[farmers$village == "Buyara" & farmers$farmer_ID == "F_1267" & farmers$X_id == "77703794"] <- "F_2587"
farmers$village[farmers$farmer_ID  == "F_2587"] <- "Buwagi"
farmers$sub[farmers$farmer_ID  == "F_2587"] <- "Budondo"
farmers$district[farmers$farmer_ID  == "F_2587"] <- "Jinja"
farmers$catchID[farmers$farmer_ID  == "F_2587"] <- 1


#this village was visited twice - I am removing it but maybe it can still be saved?
farmers <- farmers[!(farmers$village == "Namusambya" & (farmers$X_id %in% c("77386998",
                                                                            "77387000",
                                                                            "77387002",
                                                                            "77387005",
                                                                            "77387006",
                                                                            "77387009",
                                                                            "77387010",
                                                                            "77387012",
                                                                            "77387014",
                                                                            "77387017"))),]

farmers <- farmers[!(farmers$village == "Bumoozi" &  farmers$farmer_ID == "F_736" & farmers$X_id == "77352290"),]
farmers <- farmers[!(farmers$village == "Nawasenga" &  farmers$farmer_ID == "F_639" & farmers$X_id == "77352536"),]
farmers <- farmers[!(farmers$village == "Nsozi" &  farmers$farmer_ID == "F_1109" & farmers$X_id == "77386760"),]
farmers <- farmers[!(farmers$village == "Bukola" &  farmers$farmer_ID == "F_1016" & farmers$X_id == "77409670"),]
farmers <- farmers[!(farmers$village == "Nabinyonyi" &  farmers$farmer_ID == "F_49" & farmers$X_id == "77409793"),]
farmers <- farmers[!(farmers$village == "Izira" &  farmers$farmer_ID == "F_721" & farmers$X_id == "77520701"),]
farmers <- farmers[!(farmers$village == "Kigusa" &  farmers$farmer_ID == "F_2641" & farmers$X_id == "77576761"),]
farmers <- farmers[!(farmers$village == "Mayuge Nilo" &  farmers$farmer_ID == "F_2740" & farmers$X_id == "77576821"),]
farmers <- farmers[!(farmers$village == "Nkoone" &  farmers$farmer_ID == "F_1475" & farmers$X_id == "77576896"),]
farmers <- farmers[!(farmers$village == "Kigusa" &  farmers$farmer_ID == "F_2644" & farmers$X_id == "77577060"),]
farmers <- farmers[!(farmers$village == "Budhaya" &  farmers$farmer_ID == "F_1097" & farmers$X_id == "77577117"),]
farmers <- farmers[!(farmers$village == "Maweito" &  farmers$farmer_ID == "F_3247" & farmers$X_id == "77706258"),]


farmers$farmer_ID[duplicated(farmers$farmer_ID)] 


farmers_base<-farmers[, c("farmer_ID", "Check2.check.maize._gps_latitude", "Check2.check.maize._gps_longitude")] #getting only location info 

ratings_dyads_base <- read.csv(paste(path,"perceptions/data_seed_systems/data/farmer/rating_dyads.csv", sep = "/"), stringsAsFactors = FALSE) #ratings dataset with dealer IDs also 

#merging location datasets of farmers to have a dataset with both farmer and dealer IDS
merged_farmer<-merge(farmers_base, ratings_dyads_base, by="farmer_ID")
farmers_loc_base<-merged_farmer[, c("farmer_ID", "Check2.check.maize._gps_latitude", "Check2.check.maize._gps_longitude", "shop_ID")] #only location info of farmer


#creating dataset with location details of farmers and dealers 
baseline_loc<- merge(shops_base, farmers_loc_base, by="shop_ID")
#baseline_loc<- merge(shops_base, merged_farmer, by="shop_ID")
baseline_loc[baseline_loc=="n/a"]<-NA

## calcualte distance in metres using Haversine formula -- baseline 
baseline_loc$dist_m <- geodist::geodist_vec(
  x1 = baseline_loc$Check2.check.maize._gps_longitude
  , y1 = baseline_loc$Check2.check.maize._gps_latitude
  , x2 = baseline_loc$gps_longitude
  , y2 = baseline_loc$gps_latitude
  , paired = TRUE
  , measure = "haversine"
)

## another method for calculation 
# #gcd.slc <- function(long1, lat1, long2, lat2) {
#   R <- 6371 # Earth mean radius [km]
#   d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
#   return(d) # Distance in km
# }

# baseline_loc$Check2.check.maize._gps_longitude<-as.numeric(as.character(baseline_loc$Check2.check.maize._gps_longitude))
# baseline_loc$Check2.check.maize._gps_latitude <- as.numeric(as.character(baseline_loc$Check2.check.maize._gps_latitude))
# baseline_loc$maize.owner.agree._gps_longitude<- as.numeric (as.character(baseline_loc$maize.owner.agree._gps_longitude))
# baseline_loc$maize.owner.agree._gps_latitude<- as.numeric (as.character(baseline_loc$maize.owner.agree._gps_latitude))
# 
# baseline_loc$distdist<- gcd.slc(baseline_loc$Check2.check.maize._gps_longitude, baseline_loc$Check2.check.maize._gps_latitude, baseline_loc$maize.owner.agree._gps_longitude, 
#                                 baseline_loc$maize.owner.agree._gps_latitude)


## convert to kms
baseline_loc$maize.owner.dist <-baseline_loc$dist_m / 1000
write.csv(baseline_loc,paste(path,"perceptions/data_seed_systems/data/input_dealer/distance_more.csv", sep="/"), row.names=FALSE) #creating the dataset
baseline_loc<-baseline_loc[, c("maize.owner.dist", "shop_ID")] #only distance info

write.csv(baseline_loc,paste(path,"perceptions/data_seed_systems/data/input_dealer/distance.csv", sep="/"), row.names=FALSE) #creating the dataset



##################   MIDLINE   ############################


### reads in raw data (not public)
shops <- read.csv(paste(path,"papers/perceptions/data_seed_systems_midline/data/raw/midline/Dealer_MidlineV4_2022_02_16_03_40_40_437618-1.csv", sep="/"))
shops$shop_ID <- as.character(shops$shop_ID)
### five duplicate... four shops have ID == "n/a" and these n/a's also do not have "clearing" and "trainin" status... add here?
shops$shop_ID[shops$X_uuid =="a3faaf4d-2704-4b80-953a-b968579fe6b9"] <- "AD_131"
shops$clearing[shops$X_uuid =="a3faaf4d-2704-4b80-953a-b968579fe6b9"] <- TRUE
shops$training[shops$X_uuid =="a3faaf4d-2704-4b80-953a-b968579fe6b9"] <- FALSE

shops$shop_ID[shops$X_uuid =="3f435fb7-4832-4fb3-83bd-be1ae701db6e"] <- "AD_306"
shops$clearing[shops$X_uuid =="3f435fb7-4832-4fb3-83bd-be1ae701db6e"] <- FALSE
shops$training[shops$X_uuid =="3f435fb7-4832-4fb3-83bd-be1ae701db6e"] <- TRUE

shops$shop_ID[shops$X_uuid =="2c0f8acf-b804-4816-9545-ff36b332cddb"] <- "AD_65"
shops$clearing[shops$X_uuid =="2c0f8acf-b804-4816-9545-ff36b332cddb"] <- TRUE
shops$training[shops$X_uuid =="2c0f8acf-b804-4816-9545-ff36b332cddb"] <- FALSE

shops$shop_ID[shops$X_uuid =="dc8dc036-8ae8-41e7-a669-48af3e02da74"] <- "AD_270"
shops$clearing[shops$X_uuid =="dc8dc036-8ae8-41e7-a669-48af3e02da74"] <- TRUE
shops$training[shops$X_uuid =="dc8dc036-8ae8-41e7-a669-48af3e02da74"] <- FALSE

shops$shop_ID[shops$X_uuid =="f1efbe32-f4e9-44b1-b2dd-97a38c33897e"] <- "AD_279"
shops$clearing[shops$X_uuid =="f1efbe32-f4e9-44b1-b2dd-97a38c33897e"] <- TRUE
shops$training[shops$X_uuid =="f1efbe32-f4e9-44b1-b2dd-97a38c33897e"] <- FALSE

### this one seems to be a duplicate - wife was interviewed by a different enumerator later in the day...
shops <- subset(shops, X_uuid != "1686391d-9793-46bb-beb4-26922ac42dd7")
#this is a really strange one: duplicate of 
shops <- subset(shops, shop_ID !="AD_323")


### just to try
shops$longe10h_kg <- as.numeric(as.character(shops$owner.agree.long10h.q25))
shops$longe10h_kg[is.na(shops$longe10h_kg)] <- 0

shops$longe7h_kg <-as.numeric(as.character(shops$owner.agree.longe7H.q38))
shops$longe7h_kg[is.na(shops$longe7h_kg)] <- 0

shops$longe5_kg <- as.numeric(as.character(shops$owner.agree.longe5.q50))
shops$longe5_kg[is.na(shops$longe5_kg)] <- 0

shops$longe4_kg <- as.numeric(as.character(shops$owner.agree.longe4.q62))
shops$longe4_kg[is.na(shops$longe4_kg)] <- 0

shops$tot_kg <- shops$longe10h_kg + shops$longe7h_kg+ shops$longe5_kg + shops$longe4_kg

summary(lm(tot_kg~clearing, data=shops))

shops$owner.agree.barcode <- as.numeric(as.character(shops$owner.agree.barcode))

shops$owner.agree.barcode[shops$owner.agree.barcode=="16344495"] <- 298
shops$owner.agree.barcode[shops$owner.agree.barcode=="11217770"] <- 318 

shops$owner.agree.barcode[shops$X_uuid=="bff4c5fe-77b2-405e-8142-4d0b15b87552"] <- NA


#dataset only having gps locations of dealers in midline 
shops_mid<-shops[, c("shop_ID", "owner.agree._gps_latitude", "owner.agree._gps_longitude")]

#reading in farmers' midline data to get the locations 
mid <- read.csv(paste(path,"papers/perceptions/data_seed_systems_midline/data/raw/midline/Farmer_MidlineV1_2022_01_22_09_30_12_083851.csv", sep = "/"), stringsAsFactors = FALSE)

##investigate duplicates here!!!!!! - 25 duplicates -- probably best to investigate using a map

mid$farmer_ID[mid$X_uuid=="78ef5fdd-97ae-4cd8-a6c7-078dc34a1937"] <- "F_2828"
mid$farmer_ID[mid$X_uuid=="48ab1657-dfbb-48ec-b930-b7ea264c25e3"] <- "F_566"
mid$farmer_ID[mid$X_uuid=="496059ab-754c-47da-a0fa-44695016af7c"] <- "F_2968"
mid$farmer_ID[mid$X_uuid=="9ad47133-8867-4e1e-82a2-8a55bd25683d"] <- "F_506"
mid$farmer_ID[mid$X_uuid=="7ab858d0-7194-4979-b791-0f541f7f0231"] <- "F_1738"
mid$farmer_ID[mid$X_uuid=="246c64cf-5de1-48cc-aa55-6b4a9832bc77"] <- "F_1737"
mid$farmer_ID[mid$X_uuid=="671f311f-f554-42d6-bb14-b69bd4044ecd"] <- "F_2036"
mid$farmer_ID[mid$X_uuid=="ed7a604a-8c7d-4556-af9a-2706def572fa"] <-  "F_3380"
mid$farmer_ID[mid$X_uuid=="75f7a8f0-593d-4598-99ac-e62d8aa039db"] <-  "F_108"
mid$farmer_ID[mid$X_uuid=="6d27e24b-2e18-4cd6-9134-674b4f72a922"] <-  "F_2938"
mid$farmer_ID[mid$X_uuid=="fd38d8b7-6d2f-430a-9222-6c4907e11d45"] <-  "F_2245"


# this is a real duplicate - delete 1
mid <- subset(mid,farmer_ID!="F_187")
mid <- subset(mid,farmer_ID!="F_1237")

mid <- subset(mid, !(X_uuid %in% c("aa9b1673-77b0-4901-b5dc-fcafd74bffa0","598e519e-0745-479b-b207-59b1b0a8cb05","32d7dbee-95bc-4967-b8dd-2d93b99d1bca","0912e03f-6da6-46f5-91a5-fd455a9360ce","d35ae30d-4187-4fa4-972f-0914f82b3e80","dd963511-b86f-46a5-8f1f-837b12cd4f43","01f21767-6c56-4740-a039-e3dac4a7dac1","9d92c376-fa17-465a-8d6f-2328f2fd3652","81b4c7b7-06b2-4cd0-946a-679a241d1ff6","7e94052a-913e-49b3-ad76-6f001a22060f","48346776-af31-4687-938a-4887ac60b732","4e339c09-7f8d-48ff-82be-fd6bd709600e","6ddb8e69-9c60-4360-b7fc-c1a7bba8ab95")))

###BELOW: this code was used to investigate duplicate

#vil <- mid[mid$farmer_ID == "F_3028",c("village")][1]

#test <- mid[mid$village == vil,c("check.maize._gps_latitude", "check.maize._gps_longitude","farmer_ID")]
#names(test) <- c("lat","long", "ID")
#m_m <-  leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=test, lng=~as.numeric(as.character(long)), lat=~as.numeric(as.character(lat)),radius= 8, popup = ~as.character(ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

##get data from baseline
#path <- getwd()
#path <- strsplit(path, "midline/data/farmer/raw")[[1]]
#farmers <- read.csv(paste(path,"baseline/data/farmer/raw/baseline_farmers_all_raw.csv",sep="/"))

#test <- farmers[farmers$village == vil,c("Check2.check.maize._gps_latitude", "Check2.check.maize._gps_longitude","farmer_ID")]
#names(test) <- c("lat","long", "ID")
#m_b <-  leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=test, lng=~as.numeric(as.character(long)), lat=~as.numeric(as.character(lat)),radius= 8, popup = ~as.character(ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

###to get uu_IDs 
#test <- mid[mid$village == vil,c("check.maize._gps_latitude", "check.maize._gps_longitude","X_uuid")]
#names(test) <- c("lat","long", "uu_ID")
#m_id <-  leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=test, lng=~as.numeric(as.character(long)), lat=~as.numeric(as.character(lat)),radius= 8, popup = ~as.character(uu_ID))   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

#test <- mid[mid$village == vil,c("check.maize._gps_latitude", "check.maize._gps_longitude","farmer_ID")]
#names(test) <- c("lat","long", "ID")

####ABOVE: this code was used to investigate duplicates


#also remove district, subcounty and village here -- we will use the encoded versions from this from the baseline data
to_drop <- c("start","end","deviceid","simserial","phonenumber","subscriberid","enumerator", "date","district","sub","village","hh_name","enumerator_base","phone1","phone2","farmer_name","q1","chess","new_phone","q3")             
mid <- mid[ , !(names(mid) %in% to_drop)]

to_drop <- c("check.consent","check.sig_consent","check.maize.q5","check.maize.phone1x","check.maize.phone2x","check.maize.q13")

mid <- mid[ , !(names(mid) %in% to_drop)]

#remove names for plot in random plot selection module
to_drop <- c("check.maize.plot.1..plot_num","check.maize.plot_count","check.maize.plot.1..q28","check.maize.plot.2..plot_num","check.maize.plot.2..q28","check.maize.plot.3..plot_num","check.maize.plot.3..q28","check.maize.plot_calc1","check.maize.plot_calc2","check.maize.plot_select_name","check.maize.order")

mid <- mid[ , !(names(mid) %in% to_drop)]

to_drop <- c("check.maize.clear.placeholder","check.maize.clear.shops_count")

mid <- mid[ , !(names(mid) %in% to_drop)]


farmers_mid<-mid[, c("farmer_ID", "check.maize._gps_latitude", "check.maize._gps_longitude")] #getting only location info 

ratings_dyads_mid <- read.csv(paste(path,"papers/perceptions/data_seed_systems_midline/data/farmer/midline_rating_dyads.csv", sep = "/"), stringsAsFactors = FALSE) #ratings dataset with dealer IDs also 

#merging location datasets of farmers to have a dataset with both farmer and dealer IDS
merged_farmer<-merge(farmers_mid, ratings_dyads_mid, by="farmer_ID")
farmers_loc_mid<-merged_farmer[, c("farmer_ID", "check.maize._gps_latitude", "check.maize._gps_longitude", "shop_ID")] #only location info of farmer


#creating dataset with location details of farmers and dealers 
mid_loc<- merge(shops_mid, farmers_loc_mid, by="shop_ID")
mid_loc[mid_loc=="n/a"]<-NA

## calcualte distance in metres using Haversine formula -- midline 
mid_loc$dist_m <- geodist::geodist_vec(
  x1 =mid_loc$check.maize._gps_longitude
  , y1 = mid_loc$check.maize._gps_latitude
  , x2 = mid_loc$owner.agree._gps_longitude
  , y2 = mid_loc$owner.agree._gps_latitude
  , paired = TRUE
  , measure = "haversine"
)

## convert to kms
mid_loc$owner.dist <-mid_loc$dist_m / 1000
mid_loc<-mid_loc[, c("owner.dist", "shop_ID")] #only distance info

write.csv(mid_loc,paste(path,"papers/perceptions/data_seed_systems_midline/data/input_dealer/distance.csv", sep="/"), row.names=FALSE) #creating the dataset

