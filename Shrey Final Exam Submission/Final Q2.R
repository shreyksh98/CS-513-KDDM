# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Final Exam Q2
# Date: 05-13-2020

#install random forest package
install.packages("randomForest")
library('randomForest')

#clear the environment
rm(list = ls())

#load the dataset into R
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 2\\KDD\\Admission_cat.csv") 


set.seed(123)
#splitting the dataset into train and test
train_data = floor(0.7*nrow(data))
train_ind = sample(seq_len(nrow(data)),size = train_data)
train =data[train_ind,]
test=data[-train_ind,]

#applying random forest using the ADMIT column
randomForest_class<-randomForest(ADMIT~.,data = train)
summary(randomForest_class)
#plotting random forest graph
plot(randomForest_class)

randomForest_predict<-predict( randomForest_class ,test , type="class" )
randomForest_predict

random_predict_cart <- ifelse(randomForest_predict<0.5,"0","1")
#creating confusion matrix
conf_mat<-table(actual=test[,2],Random_Forest = random_predict_cart )
conf_mat

#finding out accuracy
accuracy<-sum(diag(conf_mat)/nrow(test)) * 100
accuracy
