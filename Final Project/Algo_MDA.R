rm(list = ls())
setwd('/Users/aviratbelekar/Desktop/KDD_project')
dataset<-read.csv('attrition_data.csv')
dataset<-dataset[c(2,3,4,22,23,24,26,27,21)]
dataset<-data.frame(lapply(na.omit(dataset),as.numeric))
#Split training and testing(training = 75% , testing = 25%)
set.seed(135)
index<-sort(sample(nrow(dataset),as.integer(0.25*nrow(dataset))))
training<-dataset[-index,]
testing<-dataset[index,]
#install.packages('mda')
#install.packages('lda')
library(MASS)
library(ggplot2)
library(mda)
mda_out<-mda(STATUS~.,data=training)
#plot(mda_predict)
mda_predict<-predict(mda_out,testing)
mda_predict
#mda_predict_cat<-ifelse(mda_predict<1.5,'1','2')
conf_mat<-table(Actual=testing[,9],Predicted=mda_predict)
conf_mat
accuracy<-sum(diag(conf_mat))/ nrow(testing) * 100
accuracy
