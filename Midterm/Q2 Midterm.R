# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Midterm
# Date: 03-31-2020

#Clear the R environment and load the dataset
rm(list=ls())
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\COVID19_v3.csv")

#summarizing each column
summary(data)

#Using complete cases to give us FALSE for missing tables
complete.cases(data)

#COnverting all the cloumns to Factor
clean_data<-(transform(data, ID=as.factor(ID), Age=as.factor(Age), Exposure=as.factor(Exposure), MaritalStatus=as.factor(MaritalStatus), Cases=as.factor(Cases), MonthAtHospital=as.factor(MonthAtHospital), Infected=as.factor(Infected)))
sapply(clean_data, class)

install.packages("plyr")
library(plyr)
frequency_table <- table(data$MaritalStatus, data$Infected) #Frequency table
frequency_table

#Plotting scatter plot one pair at a time
plot(data$Age,data$MaritalStatus)
plot(data$Age,data$MonthAtHospital)
plot(data$MonthAtHospital,data$MaritalStatus)

#Plotting boxplot one pair at a time
boxplot(data$Age, data$MaritalStatus)
boxplot(data$Age, data$MonthAtHospital)
boxplot(data$MonthAtHospital, data$MaritalStatus)

#Replacing N/A with the mean of Cases
for(i in 1:ncol(clean_data)){clean_data[is.na(Cases[,i]), i] <- mean(Cases[,i], na.rm = TRUE)}


  