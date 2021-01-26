rm(list = ls())
data<- read.csv("/Users/aviratbelekar/Desktop/KDD_project/attrition_data.csv")
dataset<-data[c(2,3,4,22,23,24,26,27,21)]
dataset<-data.frame(lapply(dataset,as.numeric))
dataset$STATUS<- factor(dataset$STATUS , levels = c("1","2") , labels = c("0","1"))
set.seed(125)
train_data = floor(0.7*nrow(dataset))
train_ind = sample(seq_len(nrow(dataset)),size = train_data)
train =dataset[train_ind,]
test=dataset[-train_ind,]
logit<-glm(STATUS~.,data = train,family = 'binomial')
pred_logit<-predict(logit,test)
pred_logit_cat<-ifelse(pred_logit<0.5,'0','1')
conf_mat<-table(Actual = test$STATUS,Predicted = pred_logit_cat)
conf_mat
accuracy<-sum(diag(conf_mat))/ nrow(test) * 100
accuracy
