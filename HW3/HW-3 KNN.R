# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Homework 3 KNN
# Date: 03-08-2020

#Clear the R environment and load the dataset
rm(list=ls())
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\breast-cancer-wisconsin.data.csv")

data[data=="?"]<-NA  #replace a value with NA
complete.cases(data)
clean_data<-na.omit(data) #remove the rows with NA

#convert the columns from integer to Factor
clean_data<-(transform(clean_data, F1=as.factor(F1), F2=as.factor(F2), F3=as.factor(F3), F4=as.factor(F4), F5=as.factor(F5), F6=as.factor(F6), F7=as.factor(F7), F8=as.factor(F8), F9=as.factor(F9), Class=as.factor(Class)))
sapply(clean_data, class)

train_data = floor(0.7*nrow(clean_data)) #split the dataset into training and testing
train_ind = sample(seq_len(nrow(clean_data)),size = train_data)
train =clean_data[train_ind,]
test=clean_data[-train_ind,]

install.packages("kknn") #install the KNN package
library(kknn)
library(class)

#Using Knn methodology for K=3
predict_k3<-kknn(formula = Class~.,train, test, k=3, kernel = "rectangular") 
fit<-fitted(predict_k3)
table(Actual=test$Class, Fitted=fit)
conf_mat<-table(Actual = test$Class, Fitted = fit)
conf_mat #confusion matrix
accuracy<-sum(diag(conf_mat)/nrow(test)) * 100 
accuracy #accuracy of the model

#Using Knn methodolog for K=5
predict_k5<-kknn(formula = Class~.,train, test, k=5, kernel = "rectangular")
fit<-fitted(predict_k5)
table(Actual=test$Class, Fitted=fit)
conf_mat1<-table(Actual = test$Class, Fitted = fit)
conf_mat1 #confusion matrix
accuracy1<-sum(diag(conf_mat1)/nrow(test)) * 100 
accuracy1 #accuracy of the model

#Using Knn methodology for K=10
predict_k10<-kknn(formula = Class~.,train, test, k=10, kernel = "rectangular")
fit<-fitted(predict_k10)
table(Actual=test$Class, Fitted=fit)
conf_mat2<-table(Actual = test$Class, Fitted = fit)
conf_mat2 #confusion matrix
accuracy2<-sum(diag(conf_mat2)/nrow(test)) * 100 
accuracy2 #accuracy of the model


