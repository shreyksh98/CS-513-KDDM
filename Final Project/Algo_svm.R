#install.packages("caret")
library('e1071')

rm(list = ls())
data<- read.csv("/Users/aviratbelekar/Desktop/KDD_project/attrition_data.csv")
dataset<-data[c(2,3,4,22,23,24,26,27,21)]
dataset<-data.frame(lapply(dataset,as.numeric))

set.seed(125)
train_data = floor(0.7*nrow(dataset))
train_ind = sample(seq_len(nrow(dataset)),size = train_data)
train =dataset[train_ind,]
test=dataset[-train_ind,]


classifier = svm(formula = STATUS ~ ., data = train, type = 'C-classification',kernel = 'linear') 


test_pred <- predict(classifier, newdata = test)
test_pred

conf_mat<-table(test_pred, test$STATUS)
conf_mat
accuracy<-sum(diag(conf_mat)/nrow(test)) * 100
accuracy
