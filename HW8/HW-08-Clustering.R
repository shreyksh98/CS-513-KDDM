# First Name: Shrey Hitesh 
# Last Name: Kshatriya
# CWID: 10451146
# Course: CS 513-B Spring 2020
# Purpose: Homework 8 Clustering 
# Date: 04-31-2020

#Clear all previous variables
rm(list = ls())

#import the dataset
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 2\\KDD\\wisc_bc_ContinuousVar.csv")

# Displaying the summary of the dataset
summary(data)
table(data$diagnosis)

#To factor the data set
dataset<-na.omit(data)
dataset<-dataset[-1]
dataset_dist<-dist(dataset[,-1])

#Implementing Hclust Algorithm
hclust_results<-hclust(dataset_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
conf_mat<-table(hclust_2,dataset[,1])
accuracy<-sum(diag(conf_mat))/nrow(dataset) * 100
accuracy


# Question 2
# Clearing all previous variables
rm(list = ls())

#import the dataset
data<- read.csv("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 2\\KDD\\wisc_bc_ContinuousVar.csv")


# Displaying the summary of the dataset
summary(data)
table(data$diagnosis)

#To factor the data set
dataset<-na.omit(data)
dataset<-dataset[-1]

# Implement K means algorithm
kmeans_2<- kmeans(dataset[,-1],2,nstart = 10)
kmeans_2$cluster
conf_mat<-table(kmeans_2$cluster,dataset[,1])
accuracy<-sum(diag(conf_mat))/nrow(dataset) * 100
accuracy

