install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(titanic)
library(ROCR)

#From Class 5 ...
#Read excel file
library(readxl)
mydf <- german_credit_card

#Class 9
#Recall class 8  
my_titanic<-rpart(Survived ~ Age + Sex + Pclass + SibSp + Fare,
                  data = titanic_train, method = "class")
rpart.plot(my_titanic, extra = 1, type = 1)
plotcp(my_titanic)

#Create german credit tree
ger_tree <- rpart(good_bad~ age+amount+duration+checking,
                  data = mydf, method = "class")
rpart.plot(ger_tree, extra = 1, type = 1)

#Create a glm
ger_logit <- glm(good_bad~ age+ amount + duration + checking,
                 data = mydf, family = "binomial")

predict_tree <- predict(ger_tree, mydf, type = "prob")
predict_logit <- predict(ger_logit, mydf, type = "response")

pred_val_tree <- prediction(predict_tree[,2], mydf$good_bad)
pred_val_logit <- prediction(predict_logit, mydf$good_bad)

perf_tree <- performance(pred_val_tree, "tpr", "fpr")
perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_tree, col="black")
plot(perf_logit, col="red",add = T)

View(mydf)

#Ploty
install.packages("plotly")
install.packages("ggplot2")
library(ggplot2)
library(plotly)

t <- plot_ly(data = mydf, x =~age, y=~ good_bad)
t
p <- ggplot(mydf, aes(x=age, y=good_bad))+
  geom_jitter(aes(color=checking))
ggplotly(p)

z<- plot_ly(data=mydf, x=~duration, y=~age, z=~amount, color=~ good_bad)
z



