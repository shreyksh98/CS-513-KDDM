#install.packages("earth")
rm(list = ls())
setwd('/Users/aviratbelekar/Desktop/KDD_project')
dataset<-read.csv('attrition_data.csv')
dataset<-data.frame(lapply(dataset,as.numeric))
dataset<-subset(dataset, select = c(1:20,22:27,21))
dataset<-dataset[,-c(14)]
library(earth)
regressor <- earth(STATUS ~ . , data = dataset) # fit lm() model
ev<-evimp(regressor)
plot(ev)
list(ev)
