mydata2 <- read.csv("german credit card.csv",header=TRUE)
head(mydata2)
summary(mydata2)
my_vect <- as.numeric(replace(mydata2$purpose,"","x"))
head(my_vect)
summary(my_vect)
mydata2$purpose_num <-as.numeric(replace(mydata2$purpose,"","x")) [1:1000]
tail(my_vect)
?gsub
mydata2$purpose_num <-as.numeric(gsub("X","",mydata2$purpose))
mydata2$good_bad <- gsub("good","1",mydata2$good_bad)
mydata2$good_bad <- gsub("bad","0",mydata2$good_bad)
mydata2$good_bad <- as.numeric(mydata2$good_bad)
mydata2$good_bad

my_logit <- glm(good_bad~age,data=mydata2,family="binomial")
summary(my_logit)
exp(0.01844)-1
my_logit_bigger <- glm(good_bad ~ duration+checking+savings, data=mydata2,family="binomial")
summary(my_logit_bigger)
plot(mydata2$age,mydata2$amount)
my_linear <- lm(amount~age,data=mydata2)
summary(my_linear)
library(quantreg)
rq_model <- rq(amount~age,data=mydata2,tau=0.2)
summary(rq_model)

rq_model <- rq(amount~age,data=mydata2,tau=0.5)
summary(rq_model)

rq_model <- rq(amount~age,data=mydata2,tau=0.9)
summary(rq_model)

library(rpart)
library(rpart.plot)
library(titanic)

summary(titanic_train)
fare
sex
ticket
#class
head(titanic_train)
logit_tit <- glm(Survived ~ Pclass+Age+SibSp+Sex,data=titanic_train,family="binomial")
summary(logit_tit)
exp(-2.623)-1
exp(-0.376)-1
exp(-0.0443)-1
exp(-1.317)-1
logit_predict <- predict(logit_tit,titanic_train,type="response")
summary(logit_predict)

logit_predict[1:5]
head(logit_predict,5)

tree_tit <- rpart(Survived ~ Pclass+Age+SibSp+Sex,data=titanic_train,method="class",cp=0.07)
plotcp(tree_tit)
rpart.plot(tree_tit,extra=1,type=1)
tree_predict <- predict(tree_tit,titanic_train, type="prob")
summary(tree_predict)

library(ROCR)
pred_val_logit <- prediction(logit_predict,titanic_train$Survived)
pred_val_tree <- prediction(tree_predict,titanic_train$Survived)
head(tree_predict)

perf_logit <- performance(pred_val_logit,"tpr","fpr")
perf_tree <- performance(pred_val_logit,"tpr","fpr")
plot(perf_logit, col="blue")
plot(perf_tree, col="black", add=T)
