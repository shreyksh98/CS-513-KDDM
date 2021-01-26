#install.packages("ISLR")
#install.packages("leaps")
rm(list = ls())
setwd('/Users/aviratbelekar/Desktop/KDD_project')
dataset<-read.csv('attrition_data.csv')
dataset<-data.frame(lapply(dataset,as.numeric))
dataset<-subset(dataset, select = c(1:20,22:27,21))
dataset<-dataset[,-c(14)]

library(ISLR)
library(leaps)
regit.full=regsubsets(STATUS~.,dataset,nvmax  = 26)
reg.summary<-summary(regit.full)
names(reg.summary)


plot(reg.summary$rsq,xlab = "Number of Variables",ylab = "Rsqaure",type = "l")
plot(reg.summary$rss,xlab = "Number of Variables",ylab = "Rsqaure",type = "l")
dim(dataset)

#Adjusted R squared
which.max(reg.summary$adjr2)
plot(reg.summary$adjr2,xlab = "Number of Variables",ylab = "Adjusted RSQ",type = "l")
points(12,reg.summary$adjr2[12],col = "blue",cex = 2,pch = 20)

#CP
which.min(reg.summary$cp)
plot(reg.summary$cp,xlab = "Number of Variables",ylab = "CP",type = "l")
points(9,reg.summary$cp[9],col = "blue",cex = 2,pch = 20)

#BIC
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab = "Number of Variables",ylab = "BIC",type = "l")
points(4,reg.summary$cp[4],col = "blue",cex = 2,pch = 20)


plot(regit.full,scale = "r2")
plot(regit.full,scale = "adjr2")
plot(regit.full,scale = "Cp")
plot(regit.full,scale = "bic")



#forward stepwise
regit.fwd<-regsubsets(STATUS~.,data = dataset, nvmax = 26, method = "forward")
summary(regit.fwd)
plot(regit.fwd,scale = "Cp")
round(coef(regit.fwd,which.min(summary(regit.fwd)$cp)),6)

#backward stepwise
regit.bwd<-regsubsets(STATUS~.,data = dataset, nvmax = 26, method = "backward")
summary(regit.bwd)
plot(regit.bwd,scale = "Cp")
coef(regit.bwd,which.min(summary(regit.bwd)$cp))



