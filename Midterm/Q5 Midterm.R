# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Midterm
# Date: 03-31-2020


library(e1071)
rm(list= ls())
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\breast-cancer-wisconsin.data.csv")

is.na(data)
data<-na.omit(data)

data$Age<-cut(data$Age , c(0,35,50,60))
data$Age<- factor(data$Age, levels = c("(0,35]","(35,50]","(50,60]") , labels = c("Less 35","35 to 50","More than 50"))

data$MonthAtHospital<-cut(data$MonthAtHospital, c(0,6,32))
data$MonthAtHospital<- factor(data$MonthAtHospital, levels = c("(0,6]","(6,32]") , labels = c("Less than 6 Months","6 months or more"))

index<-sort(sample(nrow(data),round(0.30*nrow(data))))
training<-data[-index,]
testing<-data[index,]

# Train the model with Naive Bayes Algorithm 
nbayes_all<-naiveBayes(Infected~.,data = training)

# Predict using the testing data
category_all<-predict(nbayes_all,testing)

table(NBAYES_ALL = category_all,Class = testing$Infected)

# Calculate Error Rate
NB_wrong<-sum(category_all!=testing$Infected)
NB_error_rate<-NB_wrong/length(category_all)

accuracy<- 1 - NB_error_rate
accuracy
