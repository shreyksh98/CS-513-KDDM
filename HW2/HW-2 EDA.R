# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Homework 2 KNN
# Date: 03-08-2020

#Q1
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\breast-cancer-wisconsin.data.csv") #load the dataset into R

#Q1.1
summary(data) #Summary function gives us the mean, median and mode for all the columns separeately

#Q1.2
data[data=="?"]<-NA #Realace all the '?' with NA
complete.cases(data)

#Q1.3
data<-(transform(data, F6=as.integer(F6))) #First convert all F6 column into integer
sapply(data,class)

#Q1.4
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE) #Create a for loop to replace NA with the mean
}

#Q1.5
install.packages("plyr")
library(plyr)
counts <- ddply(data, .(data$F6, data$Class), nrow)
names(counts) <- c("F6", "Class", "Freq") #displaying the frequency of Class vs F6
counts

#Q1.6
plot(data[,2:7]) #Scatter plot from F1 to F6

#Q1.7
boxplot(data[8:10]) #Histogram Boxplot from F7 to F9

#Q2
#Q2.1
rm(list=ls()) #Deleting all objects from R environment

#Q2.2
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\SIT Sem 2\\KDD\\breast-cancer-wisconsin.data.csv") #importing and reading the data again

#Q2.3
data[data=="?"]<-NA 
complete.cases(data)
clean_data<-na.omit(data) #deleting the rows that have NA
complete.cases(clean_data)
View(clean_data)
