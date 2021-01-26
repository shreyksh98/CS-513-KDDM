### C50 ###


rm(list=ls())
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
dataset<-read.csv("attrition_data.csv")
View(dataset)
dataset<-dataset[c(2,3,4,22,23,24,26,27,21)]
dataset<-data.frame(lapply(na.omit(dataset),as.numeric))
dataset<-transform(dataset,STATUS=as.factor(STATUS))
View(dataset)

#Installing C50 packages
install.packages("C50")
library(C50)

# Dividing into training and testing data
index<-sort(sample(nrow(dataset),round(.25*nrow(dataset))))
training<-dataset[-index,]
test<-dataset[index,]

# C50 ALgorithm
C50_class<-C5.0(STATUS~.,data=training)
summary(C50_class)
plot(C50_class)
C50_predict<-predict(C50_class,test)
table(actual=test[,9],C50=C50_predict)

# Error rate
Cart_wrong<-sum(test[,9]!=C50_predict)
C50_error_rate<-Cart_wrong/length(test[,9])
C50_error_rate


