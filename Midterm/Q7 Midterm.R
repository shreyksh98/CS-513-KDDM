# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Homework 2 KNN
# Date: 03-08-2020

#Clear the R environment and load the dataset
rm(list=ls())
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\COVID19_v3.csv")

clean_data<-na.omit(data)

train_data = floor(0.7*nrow(clean_data)) #split the dataset into training and testing
train_ind = sample(seq_len(nrow(clean_data)),size = train_data)
train =clean_data[train_ind,]
test=clean_data[-train_ind,]

install.packages("kknn") #install the KNN package
library(kknn)
library(class)

predict_k5<-kknn(formula = Infected~.,train, test, k=5, kernel = "rectangular")
fit<-fitted(predict_k5)
table(Actual=test$Infected, Fitted=fit)
conf_mat1<-table(Actual = test$Infected, Fitted = fit)
conf_mat1 #confusion matrix
accuracy1<-sum(diag(conf_mat1)/nrow(test)) * 100 
accuracy1 #accuracy of the model
