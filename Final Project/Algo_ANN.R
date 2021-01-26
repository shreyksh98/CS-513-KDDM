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

library('neuralnet')
ann_class<-neuralnet(STATUS~.,data = training,hidden = 20,threshold = 0.01)
plot(ann_class)
ann_predict<-predict(ann_class,testing)
ann_predict
ann_predict_cat<-ifelse(ann_predict<1.5,'1','2')
conf_mat<-table(Actual=testing[,9],Predicted=ann_predict_cat)
conf_mat

#Evaluating the accuracy
accuracy<-sum(diag(conf_mat))/ nrow(testing) * 100
accuracy
