hist(rexp(1000, rate=.4), breaks = seq(from=0, to=30, by=1))
hist(rexp(1000, rate=1), breaks = seq(from=0, to=30, by=1))
hist(rexp(1000, rate=2), breaks = seq(from=0, to=30, by=1))

#Class 5
#Read excel file
library(readxl)
mydf <- read_excel("german credit card.xls")

#Look at column purpose to see what people answer if they are good or bad people
mydf$purpose <- as.numeric(gsub("x","", mydf$purpose))

#Create a new column call "mydf$good_bad"
mydf$good_bad <- gsub("good", "1", mydf$good_bad)
mydf$good_bad <- gsub("bad", "0", mydf$good_bad)
mydf$good_bad <- as.numeric(mydf$good_bad)

#Create a for loop, repeat vector 1,2,3 
for (i in 1:3) {print(i)
}

#Create a nice loop
my_vect <- c()
for(i in 1:10){
  my_vect[i] <- i
}
print(my_vect)

#Suri try
#Create a for loop that takes the value of purpose
#and puts it to a new column 
my_purpose <- c()
for (purpose in mydf) {
  mydf$purpose_num <- my_vect[1:1000]
}
#Class notes warning Unknown or uninitialised column: 'new_col', does not
#remember the last code, but everything is ok
mydf$new_col <- c()
for (i in 1:nrow(mydf)){
  mydf$new_col[i] <- mydf$purpose[i]
}

#Create an if
pk <- c(1,2,3,4,5,6)

if(pk[2] ==2){print("Yupiii!!!")}else(print())

#Sheet cheat case BART
hist(rexp(1000, rate=.1), breaks = seq(from=0, to=200, by=2))
hist(rexp(1000, rate=1.5), breaks = seq(from=0, to=200, by=2))

#Probability of bus arriving > 5 minutes
#P(X>5) = e^ -lambda*x
exp(-1.5^5)
#P(x<5) 1-e^ -lambda*x
1-exp(-1.5^5)

#The form of the scatter plot is the correlation meaning variance or covariance
#The causation is the line of the model that we use to predict

#Class 8
my_logit <- glm(good_bad~age, data = mydf, family = "binomial")
summary(my_logit)
#Checking the slope, makes sense?
exp(.018440)

my_logit_bigger <- glm(good_bad~age + amount + duration + checking+savings, data = mydf, family = "binomial")
summary(my_logit_bigger)

#eliminates amount
my_logit_bigger1 <- glm(good_bad~age + duration + checking+savings, data = mydf, family = "binomial")
summary(my_logit_bigger1)
exp(-.039569)-1
#coef function will grab the coefiencients and use it

#Try to clean the model to get the lowest median in the Deviance Residuals
my_logit_bigger2 <- glm(good_bad~duration + checking+savings, data = mydf, family = "binomial")
summary(my_logit_bigger2)


my_plot <- plot(mydf$age,mydf$amount)

my_linear <- lm(amount ~age, data = mydf)
my_linear
summary(my_linear)


install.packages("SparseM")
library(quantreg)
rq_model1 <- rq(amount~age, data=mydf, tau=.2)
rq_model2 <- rq(amount~age, data=mydf, tau=.5)
rq_model3 <- rq(amount~age, data=mydf, tau=.9)
summary(rq_model1)
summary(rq_model2)
summary(rq_model3)

#Titanic - Survive 
install.packages("rpart")
install.packages("rpart.plot")
install.packages("titanic")
library(rpart)
library(rpart.plot)
library(titanic)

summary(titanic_train)


logit_tit <- glm(Survived~Pclass+Age+SibSp+Sex+Fare, data = titanic_train, family = "binomial")
summary(logit_tit)
exp(-2.613101)-1
exp(-0.392416)-1
exp(-0.043923)-1
exp(-1.254155)-1

logit_tit1 <- glm(Survived~Pclass+Age+SibSp+Sex, data = titanic_train, family = "binomial")
summary(logit_tit1)

#Predict
logit_predict <- predict(logit_tit1, titanic_train, type = "response")
logit_predict[1:5]
head(titanic_train, n=5)

#Decision tree, 
#main split variable the one that have the major power for split
#secundary variable the one behind but weaker to power of split
tree_tit <- rpart(Survived~Pclass+Age+SibSp+Sex, data = titanic_train, method = "class")
rpart.plot(tree_tit, extra=1, type=1)

#most optimal tree
plotcp(tree_tit)

#Create that tree, but it wasnt good to broad i need to increase it
tree_titopt <- rpart(Survived~Pclass+Age+SibSp+Sex, data = titanic_train, method = "class", cp=0.1)
rpart.plot(tree_titopt, extra=1, type=1)

#Increase the fit, but it wasnt good because it is too detailed and doesnt work
tree_titbig <- rpart(Survived~Pclass+Age+SibSp+Sex, data = titanic_train, method = "class", cp=0.001)
rpart.plot(tree_titbig, extra=1, type=1)

tree_tit_mustopt <- rpart(Survived~Pclass+Age+SibSp+Sex, data = titanic_train, method = "class", cp=0.013)
rpart.plot(tree_tit_mustopt, extra=1, type=1)

tree_predict <- predict(tree_tit, titanic_train, type = "prob")


install.packages("ROCR")
library(ROCR)

pred_val_logit<- prediction(logit_predict, titanic_train$Survived)
pred_val_tree <- prediction(tree_predict[,2], titanic_train$Survived)

#TPR True positive rate and False positive rate, means 
#compare with the original data
perf_logit <- performance (pred_val_logit, "tpr", "fpr")
perf_tree <- performance(pred_val_tree, "tpr", "fpr")

plot(perf_logit, col="blue")
plot(perf_tree, col="black", add=T)

#Lets see what happens if I overfitted the model?

?sqldf
