#install.packages("randomForest")
library('randomForest')

rm(list = ls())
data<- read.csv("/Users/aviratbelekar/Desktop/KDD_project/attrition_data.csv")
dataset<-data[c(2,3,4,22,23,24,26,27,21)]
dataset<-data.frame(lapply(dataset,as.numeric))

set.seed(125)
train_data = floor(0.7*nrow(dataset))
train_ind = sample(seq_len(nrow(dataset)),size = train_data)
train =dataset[train_ind,]
test=dataset[-train_ind,]

randomForest_class<-randomForest(STATUS~.,data = train)
summary(randomForest_class)
plot(randomForest_class)

randomForest_predict<-predict( randomForest_class ,test , type="class" )
randomForest_predict

random_predict_cart <- ifelse(randomForest_predict<1.5,"1","2")
conf_mat<-table(actual=test[,9],Random_Forest = random_predict_cart )
conf_mat

accuracy<-sum(diag(conf_mat)/nrow(test)) * 100
accuracy
