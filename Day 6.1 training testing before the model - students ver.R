#lets split our german credit data for training and testing using random sampling
train_index <- sample(1:nrow(mydf), size=600)
test_index <- c(1:nrow(mydf))[-train_index]
gc_train <- mydf[train_index,]
gc_test <- mydf[test_index,]

#Creating one variable logistic regression
my_mod <- glm(good_bad~age,data=gc_train,family="binomial")
summary(my_mod)

#using the model to predict the 1/0 var in the test dataset
predict(my_mod, gc_test, type="binomial")

#what if the model we got was not very acurate, 
#what if it got just 1s and no 0s in the training data?
#then you would do stratified sampling
install.packages("caTools")
library(caTools) 
train_index <- sample.split(xxxxxxxxxxx)
train <- xxxxxxx
test <- gc_test

mydf$new_col <- c(1:1000)
#Creating one variable logistic regression with stratified sampling
my_mod <- glm(binary~age, data=xxxxxxx, family=xxxxxxx)
summary(my_mod)

#using the model to predict the 1/0 var in the test dataset with stratified sampling
test$response <- predict(my_mod, test, type="response")
View(test)

#now that we know that the response from the model is bad quality, 
#what should we do?
#what if age is not the best variable? let's use telephon and property and other

my_mod <- glm(xxxxxxxxxxx, data=train, family="binomial")
summary(my_mod)

#using the model to predict the 1/0 var in the test dataset with stratified sampling
test$response <- predict(xxxxxxx, xxxxxx, type="response")
View(test)

#suddenly our model is much better! Take a look at the AIC score - it's getting lower
#also, look at some of the 0s and how low the probability is!!

#how can I get confidence intervals on the coefficients for this model:
## odds ratios and 95% CI
exp(cbind(OR = coef(xxxxxx), xxxxxxxxx))

