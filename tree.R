install.packages("rpart")
install.packages("rpart.plot")
install.packages("titanic")
install.packages("ROCR")
library(ROCR)
library(rpart)
library(rpart.plot)
library(titanic)
#Tree
my_titanic<- rpart(Survived~Age+Sex+Pclass+SibSp+Fare, 
                   data = titanic_train, method = "class")
rpart.plot(my_titanic,extra=1, type = 1)

ger_tree<-rpart(good_bad~age+amount+duration+checking,
                data=mydf,method = "class")
rpart.plot(ger_tree,extra=1, type = 1)

ger_logit<-glm(good_bad~age+duration+checking,
               data=mydf, family = "binomial")
summary(ger_logit)
exp(1.644e-02)-1

predict_tree<- predict(ger_tree,mydf,type = "prob")
predict_logit<- predict(ger_logit,mydf,type = "response")
library(ROCR)
pred_val_tree<- prediction(predict_tree[,2],mydf$good_bad)
pred_val_logit<-prediction(predict_logit,mydf$good_bad)
perf_tree<-performance(pred_val_tree,"tpr","fpr")
perf_logit<-performance(pred_val_logit,"tpr","fpr")
plot(perf_tree, col="black")
plot(perf_logit,col="red",add=T)

