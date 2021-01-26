rm(list = ls())
setwd('/Users/aviratbelekar/Desktop/KDD_project')
new_data<-read.csv('attrition_data.csv')
new_data<-new_data[c(2,3,4,22,23,24,26,27,21)]
new_datat<-data.frame(lapply(na.omit(new_data),as.numeric))
library(e1071)
index<-sort(sample(nrow(new_data),round(0.30*nrow(new_data))))
training<-new_data[-index,]
testing<-new_data[index,]
# Train the model with Naive Bayes Algorithm 
nbayes_all<-naiveBayes(STATUS~.,data = training)
category_all<-predict(nbayes_all,testing)
table(NBAYES_ALL = category_all,Class = testing$STATUS)

NB_wrong<-sum(category_all!=testing$STATUS)
NB_error_rate<-NB_wrong/length(category_all)

# Compute Accuracy
accuracy<- 1 - NB_error_rate
accuracy
