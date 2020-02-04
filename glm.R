library(readxl)
mydf <- read_excel("Downloads/german credit card.xls")

my_vect<- as.numeric(replace(mydf$purpose,"","X"))

mydf$purpose_num<- as.numeric(replace(mydf$purpose,"","X"))[1:1000]

?gsub

mydf$purpose_num<- as.numeric(gsub("X","",mydf$purpose))

mydf$good_bad<-gsub("good","1",mydf$good_bad)
mydf$good_bad<-gsub("bad","0",mydf$good_bad)
mydf$good_bad<-as.numeric(mydf$good_bad)

mydf<- as.data.frame(mydf)                          
for (i in 1:ncol(mydf)){
    hist(as.numeric(mydf[,i]))
}
for (i in 1:ncol(mydf)){
  plot(as.numeric(mydf[,i]))
}                         


size<-c(31,25,62,85,64,81,89,102)
signedup<-c(0,0,1,1,0,1,1,1)
my_sales<-as.data.frame(matrix(nrow=8,ncol=1))
my_sales$size<-size
my_sales$signedup<-signedup
print(my_sales)
my_sales$V1<-NULL

sales_mod<-glm(signedup~size,data=my_sales,family = "binomial")
summary(sales_mod)
exp(0.1912)-1

logit<-inter+betal*x
odds<- exp(logit)
prob<-odds/(1+odds)

odds2prop<-function(inter,betal,x){
  logit<-inter+betal*x
  odds<- exp(logit)
  prob<-odds/(1+odds)
  return(c(odds,prob))
}
odds2prop(inter=-11,betal=0.1912,x=72)
ger_mod<-glm(good_bad~age,data=mydf,family = "binomial")
summary(good_mod)
exp(0.01844)-1
odds2prop<-function(inter,betal,x){
  logit<-inter+betal*x
  odds<- exp(logit)
  prob<-odds/(1+odds)
  return(c(odds,prob))
}
odds2prop(inter=0.200919,betal=0.01844,x=21)

my_ger_better<-glm(good_bad ~ age+savings+amount+checking,
                   data=mydf, family = "binomial")
summary(my_ger_better)
predict(my_ger_better,test,type = "response")
