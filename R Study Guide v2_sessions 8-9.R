### Day 8 
#########

my_lin <- lm(amount~age, data=mydf) #running a linear regression using all the data
summary(my_lin)
#install.packages("quantreg")
library(quantreg)
# running quantile regression (tau is your quantile)
rq_mod <- rq(Petal.Length~Sepal.Length, data=iris, tau=.9)
summary(rq_mod)
rq_mod <- rq(Petal.Length~Sepal.Length, data=iris, tau=.5)
summary(rq_mod)
rq_mod <- rq(Petal.Length~Sepal.Length, data=iris, tau=.2)
summary(rq_mod)
#how do the coefficients differ? how do they compare with the initial linear model?
qs <- c(.2,.5,.9)
library(ggplot2)
ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length))+
  geom_point()+
  geom_quantile(quantile=qs)

rq_ger <- rq(amount~age, data=mydf, tau=.9)
summary(rq_ger)


## titanic logistic and titanic decision tree
library(rpart)
library(rpart.plot)
library(titanic)
summary(titanic_train)
my_log_tit <- glm(Survived~Sex+Age+Pclass, data=titanic_train, family="binomial")
summary(my_log_tit)
exp(-2.52)-1 #converting coefficient to business insight
exp(-.036)-1
#lets look at how the model predicts
predict(my_log_tit, titanic_train, type="response")
#building a tree
titanic_tree <- rpart(Survived~Sex+Age+Pclass, 
                      data=titanic_train, method="class", cp=0.013)
#use cp to grow or prune
#plotting the tree
rpart.plot(titanic_tree, type=1, extra=1)
plotcp(titanic_tree)

###################################
#comparing the champion logistic with
#a challenger decision tree - 
#challenge the Status Quo
###################################

predict_logit <- predict(my_log_tit, titanic_train, type="response")
val_1 <- predict(titanic_tree, titanic_train, type="prob")
#predicitng the outcomes for logit and tree using the prediction function
library(ROCR)
pred_val <- prediction(val_1[,2], titanic_train$Survived)
pred_val_logit <- prediction(predict_logit, titanic_train$Survived)
#Calculating performance for the logit and tree
perf <- performance(pred_val, "tpr", "fpr")
perf_logit <- performance(pred_val_logit, "tpr", "fpr")
#let's plot the Lift and Gains chart.
plot(perf, col="black")
plot(perf_logit, col="blue", add=T)
#Which one is better?

##################################
## Class 9
##################################

library(rpart)
library(rpart.plot)

#taking a look at german credit: logistic vs. tree

ger_tree <- rpart(good_bad~age+amount+checking+duration+savings, 
                  data=mydf, method="class")
rpart.plot(ger_tree, extra=1, type=1)
ger_logit <- glm(good_bad~age+amount+checking+duration+savings, 
                 data=mydf, family="binomial")
summary(ger_logit)

predict_logit<- predict(ger_logit, mydf, type="response")
predict_tree <- predict(ger_tree, mydf, type="prob")
library(ROCR)
pred_val_tree <- prediction(predict_tree[,2], mydf$good_bad)
pred_val_logit <- prediction(predict_logit, mydf$good_bad)

perf_tree <- performance(pred_val_tree, "tpr", "fpr")
perf_logit <- performance(pred_val_logit, "tpr", "fpr")
plot(perf_tree, col="black")
plot(perf_logit, col="blue", add=TRUE)

#########################
## We also covered R Markdown in class
# Please practice using the Markdown
#########################

#plotly
#install.packages("plotly")
library(plotly)
t <- plot_ly(data=mydf, x=~age, y=~amount)
t
#advanced scatter plot
t1 <- plot_ly(data=mydf, x=~age, y=~amount, 
              marker=list(size=10, 
                          color='rgba(255,182,193,.9'))%>%
  layout(title="age vs. amount")
t1
#binary plot
p <- plot_ly(data=mydf, z=~age, y=~good_bad)
p
#jitter plot using a ggplot object
library(ggplot2)
z <- ggplot(mydf, aes(x=age, y=good_bad))+
  geom_jitter(aes(color=checking))

ggplotly(z) #converting a ggplot to plotly

