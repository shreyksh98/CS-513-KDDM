# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Homework 6 Random Forest 
# Date: 04-31-2020

install.packages("randomForest")
library('randomForest')

#Clear all previous variables
rm(list = ls())

#read the csv file
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 2\\KDD\\breast-cancer-data.csv")

# Remove the first column of the dataset
dataset<-data[,-1]
#Replace all NA with zeroes
data[data=="?"]<-NA
dataset[dataset == 'NA']<-0

#Convert all columns to factor datatype
sapply(dataset, class)
new_data<-transform(dataset, F1 = as.factor(F1),F2 = as.factor(F2),F3 = as.factor(F3),F4 = as.factor(F4),F5 = as.factor(F5),F6 = as.factor(F6),F7 = as.factor(F7),F8 = as.factor(F8),F9 = as.factor(F9),Class = as.factor(Class))

# Convert 2,4 in class to Benign and Malignant
new_data$Class<- factor(new_data$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

#Split training and testing(training = 75% , testing = 25%)
set.seed(125)
index<-sort(sample(nrow(new_data),round(0.25*nrow(new_data))))
training<-new_data[-index,]
testing<-new_data[index,]

# Implement Random Forest alogrithm
randomForest_class<-randomForest(Class~.,data = training)
summary(randomForest_class)
plot(randomForest_class)

# Predict whether the new testing value is Benign or Malignant
randomForest_predict<-predict( randomForest_class ,testing , type="class" )
randomForest_predict

#Confusin Matrix
conf_mat<-table(actual=testing[,10],Random_Forest = randomForest_predict )

#Print Accuracy
accuracy<-sum(diag(conf_mat)/nrow(testing)) * 100
accuracy


