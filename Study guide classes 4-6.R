#################
#Day 4:
#################
library(readxl)
mydf <- read_excel("~/Desktop/HULT R class/HULT R class 2019-20/datasets/german credit card.xls")

#RUB the data using replace function
mydf$purpose <- as.numeric(replace(mydf$purpose, "", "x"))[1:1000]
#RUB the data using gsub
mydf$good_bad <- gsub("good", "1", mydf$good_bad)
mydf$good_bad <- gsub("bad", "0", mydf$good_bad)
mydf$good_bad <- as.numeric(mydf$good_bad)

###############
#Day5:
##############
#The for loop#
##############

for(i in 1:10){
  print(i)
}#closing i loop
###########################################
#creating a new object(vector) with a loop#
###########################################
my_vect <- c()
for(z in 1:20){
  my_vect[z]<- z #why do we use the [z]?
  print(my_vect)
}

####################################
# Imputing using a for loop and if()
#for each NA in purpose, take the previous value
####################################
for(i in 1:nrow(mydf)){ 
  if( is.na( mydf$purpose[i] ) ) {mydf$purpose[i] <- mydf$purpose[i-1]} else {mydf$purpose[i] <- mydf$purpose[i]}
}

########################
#User defined functions#
########################


my_function <- function(x, y, z){ #x,y,z are inputs
  
  #This is the body of the function that has to return an object . The body will use all the variables specified in the function call.
  my_final_obj <- x+y+z
  return(my_final_obj)
}
new <- my_function(x=1, y=2, z=3)#I'm calling "myfunction"

#########################################
# Optimizing a production function 
#This was already shared after Day 5
#########################################

#########################
#Day 5 / descriptive statistics: 
#########################
hist(sample(c(1,2,3,4,5,6), 800, replace=T))#creating a unifor distribution - rolling the dice
hist(sample(c(0,1), 800, replace=T))#creating a binomial distribution - tossing a coin
#plotting the histogram of exponential distributions // BART business case
hist(rexp(50, rate=2),breaks=seq(from=0, to=100, by=2))
hist(rexp(50, rate=0.05),breaks=seq(from=0, to=100, by=2))

exp(-2*5)#probability of train ariving after 5 minutes with lambda=2
exp(-0.5*5) #probability of train ariving after 5 minutes with lambda=-0.5
1-exp(-0.5*5) #probability of train ariving sooner than 5 minutes with lambda =-.5

############################
#Day 6####
############################
#Looking at varaible distributions in german credit:
mydf <- as.data.frame(mydf)
for (i in 1:ncol(mydf)){
  hist(as.numeric(mydf[,i]))
}


#How can we convert from odds to prob
logit2prob <- function(inter, beta1, x){
  logit<- inter +beta1*x
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(c(odds, prob))
}
logit2prob(inter=-11, beta1=0.1912, x=76)
logit2prob(inter=-11, beta1=0.1912, x=77)
logit2prob(inter=-11, beta1=0.1912, x=40)

#running a logisitic regression 
ger_mod<- glm(good_bad~age, data=mydf, family="binomial")
summary(ger_mod)
#interpreting coefficients 
exp(0.01844)-1 #this will give us the percentage decrease in odds of success
#Multi-variate logistic regression
ger_mod<- glm(good_bad~age+duration+checking, data=mydf, family="binomial")
summary(ger_mod)

###################################################################
## Training and testing template was posted under Day 6 in Modules
###################################################################

#lets split our german credit data for training and testing using random sampling
train_index <- sample(1:nrow(mydf), size=600)
test_index <- c(1:nrow(mydf))[-train_index]
gc_train <- mydf[train_index,]
gc_test <- mydf[test_index,]

#Creating one variable logistic regression
my_mod <- glm(good_bad~age, data=gc_train, family="binomial")
summary(my_mod)

#using the model to predict the 1/0 var in the test dataset
predict(my_mod, gc_test, type="response")

#what if the model we got was not very acurate, 
#what if it got just 1s and no 0s?
#then you would do stratified sampling
#install.packages("caTools")
library(caTools) 
train_index <- sample.split(mydf$good_bad, SplitRatio=0.6)
train <- mydf[ train_index,]
test <- mydf[!train_index,]

#Creating one variable logistic regression with stratified sampling
my_mod <- glm(good_bad~age, data=train, family="binomial")
summary(my_mod)
#great! the p-value got better for the estimated age coefficient

#using the model to predict the 1/0 var in the test dataset with stratified sampling
test$response <- predict(my_mod, test, type="response")
View(test)

