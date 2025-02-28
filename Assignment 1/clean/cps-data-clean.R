#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.
# version 1.1 2021-05-16
#########################################################################################

# ILLUSTRATION STUDY FOR CHAPTER 9
# DATA US CENSUS 2014

# WHAT THIS CODES DOES:

# Loads the data csv
# Filter the dataset and save a sample used in the analysis

# CLEAR MEMORY
rm(list=ls())

# set working directory for data_repo
setwd("C:/Users/Пользователь/Desktop/MA1y/Prediction with ML/Assignment 1")

#location folders
data_in <- "raw/"
data_out <- "clean/"


#import data
data_all0 <- read.csv(paste0(data_in,"morg2014.csv"),
                     stringsAsFactors = F)

data_all0$occ

#  select variables and rename (to get smaller file)
data_all <- data_all0[, c("hhid", "intmonth", "stfips", "weight", "earnwke", "uhourse", 
                          "grade92", "race", "ethnic", "age", "sex", "marital", "ownchild", "chldpres", "prcitshp",
                         "state","ind02", "occ2012", "class94", "unionmme","unioncov", "lfsr94")]

colnames(data_all)[colnames(data_all) == 'uhourse'] <- 'uhours'
colnames(data_all)[colnames(data_all) == 'class94'] <- 'class' 


#SAMPLE SELECTION
data_all<-subset(data_all,data_all$uhours!=0 & !is.na(data_all$uhours))
data_all<-subset(data_all,data_all$earnwke!=0 & !is.na(data_all$earnwke))
data_all<-subset(data_all,data_all$age>=16 & data_all$age<=64)

library(dplyr)
number = data_all %>%
  group_by(occ2012) %>%
  summarize(number = n())

data <- subset(data_all, occ2012 == 9130)

write.csv(data, paste(data_out,"Driver_or_sales_workers_and_truck_drivers.csv",sep=""))
