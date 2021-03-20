path_2 <- getwd()
library(ggplot2)
library(ggsignif) 
library(miceadds)
library(fBasics)
library(sjPlot)
library(mice)
library(texreg)
library(graphics)
library(png)
options(scipen=999)
path_2 <- strsplit(path, "/papers/perceptions")[[1]]

##Farmers' dataset
farmers <- read.csv(paste(path_2,"data/public/farmers.csv", sep = "/"))

