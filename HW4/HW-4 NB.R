# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Homework 4 Naive Bayes 
# Date: 03-22-2020

#Clear the R environment and load the dataset
rm(list=ls())
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\breast-cancer-wisconsin.data.csv")

data[data=="?"]<-NA  #replace a value with NA
complete.cases(data)
clean_data<-na.omit(data) #remove the rows with NA

#convert the columns from integer to Factor
clean_data<-(transform(clean_data, F1=as.factor(F1), F2=as.factor(F2), F3=as.factor(F3), F4=as.factor(F4), F5=as.factor(F5), F6=as.factor(F6), F7=as.factor(F7), F8=as.factor(F8), F9=as.factor(F9), Class=as.factor(Class)))
sapply(clean_data, class)
data$Class<- factor(data$Class , levels = c("2","4") , labels = c("Benign","Malignant")) #Renaming level 2 and 4 as Benign and Malignant

install.packages('e1071', dependencies = TRUE)
library(e1071)
library(class) 

set.seed(125)

#SPlit the data into training and testing
train_data = floor(0.7*nrow(data))
train_ind = sample(seq_len(nrow(data)),size = train_data)
train =data[train_ind,]
test=data[-train_ind,]

model_naive<- naiveBayes(Class ~ ., data = train)

predict_naive <- predict(model_naive, test)

conf_matrix <- table(predict_nb=predict_naive,class=test$Class)
print(conf_matrix)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)
