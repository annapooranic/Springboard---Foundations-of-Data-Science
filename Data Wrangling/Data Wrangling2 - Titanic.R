library(dplyr)
# Loading Titanic dataset
titanic_data <- read.csv("titanic_original.csv")

#Replacing missing values of embarked with Southampton
titanic_data$embarked[titanic_data$embarked==""]<- "S"

#Finding mean age
mean_age <- mean(titanic_data$age,na.rm = TRUE)

#Replacing NAs of Age column with mean age
titanic_data$age[which(is.na(titanic_data$age))] <- mean_age

#Empty values of Boat column with NA
titanic_data$boat[titanic_data$boat==""]<- NA

#Adding new column has_cabin_number
titanic_data$has_cabin_number <- ifelse(titanic_data$cabin=="",0,1)