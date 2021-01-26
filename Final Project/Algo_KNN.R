### KNN ###


rm(list=ls())
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
dataset<-read.csv("attrition_data.csv")
#View(dataset)
dataset<-dataset[c(2,3,4,22,23,24,26,27,21)]
dataset<-data.frame(lapply(na.omit(dataset),as.numeric))
#View(dataset)



#sorting the dataset and training and testing them
index<-sort(sample(nrow(dataset),round(.25*nrow(dataset))))
training<-dataset[-index,]
test<-dataset[index,]


#Using KNN Algorithm
install.packages("kknn")
library(kknn)
library(class)


#for k = 3
predict_k3 <- kknn(formula = STATUS~. , training, test, k =3, kernel = "rectangular")
fit <- fitted(predict_k3)
predict_k3_class<- ifelse(fit<1.5,"1","2")
predict_k3_class
table(Actual=test$STATUS, predict_k3_class)
#Accuracy
#71.24

#for k = 5
predict_k5 <- kknn(formula = STATUS~. , training, test, k =5, kernel = "rectangular")
fit <- fitted(predict_k5)
predict_k5_class<- ifelse(fit<1.5,"1","2")
predict_k5_class
table(Actual=test$STATUS, predict_k5_class)

#Accuracy of the model with k=5
#71.32


#for k = 7
predict_k7 <- kknn(formula = STATUS~. , training, test, k =7, kernel = "rectangular")
fit <- fitted(predict_k7)
predict_k7_class<- ifelse(fit<1.5,"1","2")
predict_k7_class
table(Actual=test$STATUS, predict_k7_class)

#Accuracy of the model with k=7
#71.53

#for k = 10
predict_k10 <- kknn(formula = STATUS~. , training, test, k =10, kernel = "rectangular")
fit <- fitted(predict_k10)
predict_k10_class<- ifelse(fit<1.5,"1","2")
predict_k10_class
table(Actual=test$STATUS, predict_k10_class)

#Accuracy of the model with k=10
#71.41

summary(dataset)
