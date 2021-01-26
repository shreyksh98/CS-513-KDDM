# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Homework 5 CART
# Date: 03-22-2020


#Clear the R environment and load the dataset
rm(list=ls())
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\breast-cancer-wisconsin.data.csv")

install.packages("rpart")
install.packages("rpart.plot")     # Enhanced tree plots
install.packages("rattle")         # Fancy tree plot
install.packages("RColorBrewer")   # colors needed for rattle

library(rpart)
library(rpart.plot) 
library(rattle)         
library(RColorBrewer)

#convert the columns from integer to Factor
data<-(transform(data, F1=as.factor(F1), F2=as.factor(F2), F3=as.factor(F3), F4=as.factor(F4), F5=as.factor(F5), F6=as.factor(F6), F7=as.factor(F7), F8=as.factor(F8), F9=as.factor(F9), Class=as.factor(Class)))
sapply(data, class)
data$Class<- factor(data$Class , levels = c("2","4") , labels = c("Benign","Malignant")) #Renaming level 2 and 4 as Benign and Malignant

set.seed(125) #Random Value generator

#SPlit the data into training and testing
train_data = floor(0.7*nrow(data))
train_ind = sample(seq_len(nrow(data)),size = train_data)
train =data[train_ind,]
test=data[-train_ind,]

rpart()
dev.off() #control multiple graphic devices

#Implementing CART methodology
CART_class<-rpart( Class~.,data=train)
rpart.plot(CART_class)
CART_predict2<-predict(CART_class,test, type="class") 
table(Actual=test[,11],CART=CART_predict2)
CART_predict<-predict(CART_class,test) 

CART_predict<-predict(CART_class,test)
str(CART_predict)
CART_predict_cat<-ifelse(CART_predict[,1]<=.5,'Benign','Malignant')

table(Actual=test[,11],CART=CART_predict_cat)
CART_wrong<-sum(test[,11]!=CART_predict_cat)
CART_error_rate<-CART_wrong/length(test[,4]) #Calculating error rate for CART 1
CART_error_rate
CART_predict2<-predict(CART_class,test, type="class")
CART_wrong2<-sum(test[,11]!=CART_predict2)
CART_error_rate2<-CART_wrong2/length(test[,4]) #Calculating error rate for CART 2
CART_error_rate2 

library(rpart.plot)
prp(CART_class) #plotting the tree

fancyRpartPlot(CART_class)
