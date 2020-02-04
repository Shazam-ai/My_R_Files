library(MASS)
library(rpart)
library(rpart.plot)
library(plotly)
library(ROCR)
summary(Aids2)
as.data.frame(Aids2)
aids=Aids2
2843*0.7
aids$status<-gsub(aids$status,pattern='A',replacement=1)
aids$status<-gsub(aids$status,pattern='D',replacement=0)

train_index <- sample.split(aids, SplitRatio=0.7)
train <- aids[ train_index,]
test <- aids[!train_index,]
gc_train <- train
gc_test <- test
my_mod <- glm(age~death+T.categ+sex, data=gc_train, family=poisson)
my_mod

predict_logit <-predict(my_mod, gc_train, type='response')
predict_logit

my_tree <- rpart(sex~age+T.categ+death, data=aids,method="class",control=rpart.control(cp=0.033))
rpart.plot(my_tree,type=1,extra=1, box.palette = c("pink","green"))
plotcp(my_tree)

predict_tree <- predict(my_tree, aids, type='prob')
predict_tree


perf_tree<-performance(predict_tree)
perf_logit <- performance(predict_logit)


plot(perf_tree,col="black")
plot(perf_logit,col='blue',add=TRUE) # plot on same chart
# initially both models perform well, low fpr, high tpr. then it gets tricky. perf_logit is better.
# low false positive rate is more important. 