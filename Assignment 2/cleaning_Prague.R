library(caret)
library(tidyverse)
library(skimr)
library(ggthemes)
library(gridExtra)
library(lattice)
library(glmnet)
library(rpart)
library(rattle)
library(rpart.plot)
library(xtable)
library(Hmisc)
library(modelsummary)


#-----------------------------------------------------------------------------------------
rm(list=ls())

setwd("C:/Users/Пользователь/Desktop/MA1y/Prediction_with_ML/Machine-Learning/Assignment 2")

data2025 <- read.csv("Prague2025q1.csv")
data2024 <- read.csv("Prague2024q4.csv")


drops <- c("host_thumbnail_url","host_picture_url","listing_url","picture_url","host_url","last_scraped","description", "neighborhood_overview", "host_about", "host_response_time", "name", "host_location")

data2024<-data2024[ , !(names(data2024) %in% drops)]
data2025<-data2025[ , !(names(data2025) %in% drops)]

#drop broken lines - where id is not a character of numbers
data2024$junk<-grepl("[[:alpha:]]", data2024$id)
data2024<-subset(data2024,data2024$junk==FALSE)
data2024<-data2024[1:ncol(data2024)-1]

data2025$junk<-grepl("[[:alpha:]]", data2025$id)
data2025<-subset(data2025,data2025$junk==FALSE)
data2025<-data2025[1:ncol(data2025)-1]

#the class and type of each columns
sapply(data2024, class)
sapply(data2024, typeof)

for (perc in c("host_response_rate","host_acceptance_rate")){
  data2024[[perc]]<-gsub("%","",as.character(data2024[[perc]]))
}

data2024$price <- gsub("[\\$,]", "", data2024$price)        
data2024$price <- as.numeric(data2024$price)   

for (perc in c("host_response_rate","host_acceptance_rate")){
  data2025[[perc]]<-gsub("%","",as.character(data2025[[perc]]))
}

#remove dollar signs from price variables
data2025$price <- gsub("[\\$,]", "", data2025$price)        
data2025$price <- as.numeric(data2025$price)

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified",
                 "instant_bookable")){
  data2024[[binary]][data2024[[binary]]=="f"] <- 0
  data2024[[binary]][data2024[[binary]]=="t"] <- 1
}

for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified",
                 "instant_bookable")){
  data2025[[binary]][data2025[[binary]]=="f"] <- 0
  data2025[[binary]][data2025[[binary]]=="t"] <- 1
}

data2024$amenities <- gsub("[\\{\\}\"]", "", data2024$amenities)
data2024$amenities <- tolower(data2024$amenities)
data2024$amenities <- trimws(data2024$amenities)
data2024$amenities <- strsplit(data2024$amenities, ",")
data2024$amenities <- lapply(data2024$amenities, trimws)

amenity_counts <- sort(table(unlist(data2024$amenities)), decreasing = TRUE)

top_n <- 20  
top_amenities <- names(amenity_counts)[1:top_n]

top_dummies <- as.data.frame(do.call(rbind, lapply(data2024$amenities, function(x) {
  as.integer(top_amenities %in% x)
})))

colnames(top_dummies) <- make.names(top_amenities)
data2024 <- cbind(data2024, top_dummies)


data2025$amenities <- gsub("[\\{\\}\"]", "", data2025$amenities)
data2025$amenities <- tolower(data2025$amenities)
data2025$amenities <- trimws(data2025$amenities)
data2025$amenities <- strsplit(data2025$amenities, ",")
data2025$amenities <- lapply(data2025$amenities, trimws)

amenity_counts <- sort(table(unlist(data2025$amenities)), decreasing = TRUE)

top_n <- 20  
top_amenities <- names(amenity_counts)[1:top_n]

top_dummies <- as.data.frame(do.call(rbind, lapply(data2025$amenities, function(x) {
  as.integer(top_amenities %in% x)
})))

colnames(top_dummies) <- make.names(top_amenities)
data2025 <- cbind(data2025, top_dummies)

data2024$amenities <- NULL
data2025$amenities <- NULL

write.csv(data2024, 'Prague2024q4_cleaned.csv')
write.csv(data2025, 'Prague2025q1_cleaned.csv')










































