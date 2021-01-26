# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Midterm
# Date: 03-31-2020

rm(list= ls())

# Import all libraries after installing packages
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\breast-cancer-wisconsin.data.csv")

is.na(data)
data<-na.omit(data)

data$Age<-cut(data$Age , c(0,35,50,60))
data$Age<- factor(data$Age, levels = c("(0,35]","(35,50]","(50,60]") , labels = c("Less 35","35 to 50","More than 50"))

data$MonthAtHospital<-cut(data$MonthAtHospital, c(0,6,32))
data$MonthAtHospital<- factor(data$MonthAtHospital, levels = c("(0,6]","(6,32]") , labels = c("Less than 6 Months","6 months or more"))

index<-sort(sample(nrow(data),round(0.30*nrow(data))))
training<-data[-index,]
testing<-data[index,]

#Train the model on the rpart library
cart_class<-rpart(Infected~.,data = training)

# Plot the trained class
rpart.plot(cart_class)

# Predict using the trained model based on class.
cart_predict2<-predict(cart_class,testing,type = 'class')

# Print the confusion matrix
table(Actual =testing[,7],CART = cart_predict2)

# Predict using the trained model based on probabilities.
cart_predict<-predict(cart_class,testing)
#cart_predict
str(cart_predict)
cart_predict_cat<-ifelse(cart_predict[,1]<=0.5,'Yes','No')
table(Actual=testing[,7],CART=cart_predict_cat)

#Calculate the error rate
cart_wrong<-sum(testing[,7]!=cart_predict_cat)
cart_error_rate<-cart_wrong/ length(testing[,7])

cart_predict2<-predict(cart_class,testing, type="class")
cart_wrong2<-sum(testing[,7]!=cart_predict2)
cart_error_rate2<-cart_wrong2/length(testing[,7])

#Plotting the decision tree
library(rpart.plot)
prp(cart_class)

# Fancier plot of Decison Tree
fancyRpartPlot(cart_class)

