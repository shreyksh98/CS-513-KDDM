### Algo_DTree ####

rm(list=ls())
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
dataset<-read.csv("attrition_data.csv")
View(dataset)
dataset<-dataset[c(2,3,4,22,23,24,26,27,21)]
dataset<-data.frame(lapply(na.omit(dataset),as.numeric))
View(dataset)

#### DTree #####

install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#sorting the dataset and training and testing them
set.seed(111)
index<-sort(sample(nrow(dataset),round(.25*nrow(dataset))))
training<-dataset[-index,]
test<-dataset[index,]
dev.off()

# using the CART methodology to get the accuracy
CART_class<-rpart(STATUS~.,data=training)
rpart.plot(CART_class)
CART_predict<-predict(CART_class,test) 
table(Actual=test[,9],CART=CART_predict)
CART_predict2<-predict(CART_class,test) 


CART_predict<-predict(CART_class,test)
str(CART_predict)
CART_predict_cat<-ifelse(CART_predict<=1.5,"1","2")


table(Actual=test[,9],CART=CART_predict_cat)

CART_wrong<-sum(test[,9]!=CART_predict_cat)

# Finding the error rate in the model
CART_error_rate<-CART_wrong/length(test[,9])
CART_error_rate



# Different plots of the DTree Algorithms
library(rpart.plot)
prp(CART_class)
fancyRpartPlot(CART_class)


