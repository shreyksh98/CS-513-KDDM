# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Final Exam Q3
# Date: 05-13-2020

#Clear all previous variables
rm(list = ls())

#load the dataset into R
dsn<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 2\\KDD\\Admission_cat.csv") 

#removing first col, Application ID is not usefull 
str(dsn)
dsn<-dsn[,c(-1)]

#FACTORING THE COLS
dsn$ADMIT <- as.factor(dsn$ADMIT)
dsn$RANK <- as.factor(dsn$RANK)
summary(dsn)
str(dsn)

#SETTING SEEDS
set.seed(123)

#SPLITING THE DATA
index<-sort(sample(nrow(dsn),round(.30*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]

# C50  classification 
library('C50')
C50_class <- C5.0( ADMIT~.,data=training)
summary(C50_class )

#PLOTING THE CLASSIFICATION
plot(C50_class)

#PREDICTING 
C50_predict<-predict( C50_class ,test[,-1] , type="class" )
table(actual=test[,1],C50=C50_predict)

#ACCURACY
wrong<- (test[,1]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,1])
c50_rate
accuracy<-(1-c50_rate)*100
accuracy




