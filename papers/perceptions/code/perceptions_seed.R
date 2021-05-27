path <- getwd()

options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

################# FARMERS:SEED SYSTEMS ######################

##Farmers' dataset
farmers_seed <- read.csv(paste(path_2,"/papers/perceptions/data_seed_systems/data/farmer/baseline_farmers.csv", sep = "/"))


################# FARMERS : STACK SURVEYS ######################

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

farmers1 <- farmers
farmers1[farmers1=="n/a"] <- NA 

farmers[farmers=="999"] <- NA