my_logit<- glm(good_bad~age, data=mydf, family = "binomial")
summary(my_logit)
exp(0.01844)-1

my_logit_bigger<- glm(good_bad~age+duration+checking+savings, data= mydf, family = "binomial")
summary(my_logit_bigger)

exp(0.209213)-1

my_logit_try<-glm(good_bad~age+property+employed+savings+checking+, data= mydf, family = "binomial")
summary(my_logit_try
        
plot(mydf$age,mydf$amount)
my_linear<-lm(amount~age,data = mydf)
summary(my_linear)

install.packages("quantreg")
library(quantreg)
rq_model<-rq(amount~age, data = mydf, tau=0.2)
summary(rq_model)

rq_model<-rq(amount~age, data = mydf, tau=0.5)
summary(rq_model)

rq_model<-rq(amount~age, data = mydf, tau=0.9)
summary(rq_model)

install.packages("rpart")
install.packages("rpart.plot")
install.packages("titanic")
library(rpart)
library(rpart.plot)
library(titanic)
summary(titanic_train)

logit_tit<-glm(Survived~Pclass+Age+SibSp+Sex,data = titanic_train, family = "binomial")
summary(logit_tit)

exp(-2.623483)-1
exp(-0.376)-1
exp(-0.044385)-1

logit_predict<-predict(logit_tit,titanic_train,
                       type="response")
logit_predict[1:5]
head(titanic_train)

tree_tit<-rpart(Survived~Pclass+Age+SibSp+Sex,data = titanic_train,method = "class",cp=0.07)

#plotcp(tree_tit)
rpart.plot(tree_tit,extra=1, type = 1)
tree_predict<- predict(tree_tit, titanic_train,type="prob")

install.packages("ROCR")
library(ROCR)

pred_val_logic<-prediction(logit_predict,titanic_train$Survived)
pred_val_tree<-prediction(tree_predict[,2],titanic_train$Survived)

perf_logic<- performance(pred_val_logic,"tpr","fpr")
perf_tree<- performance(pred_val_tree,"tpr","fpr")
plot(perf_logic,col="blue")
plot(perf_tree,col="black",add=T)

