library(readxl)
mydf <- german_credit_card
mydf$purpose <- as.numeric(gsub("x","",mydf$purpose))
mydf$good_bad <- gsub("good","1",mydf$good_bad)
mydf$good_bad <- gsub("bad","0",mydf$good_bad)
mydf$good_bad <- as.numeric(mydf$good_bad)

for(i in 1:3){
  print(i)
}
my_vect <- c()
for(i in 1:10) {
  my_vect[i] <- i
}
print(my_vect)
mydf$new_col <- c()
for(i in 1:nrow(mydf)){
  mydf$new_col[i] <- mydf$purpose[i]
}
pk <- c(1,2,3,4,5,6)
if (pk[2]==2){print("Yupii!!")}
else
{print("Nay!!")}

mydf$desc <- c()
for(i in 1:nrow(mydf)){
  if(mydf$good_bad[i]==1){mydf$desc[i] <- "positive"} else {mydf$desc[i]<- "negative"}}

for(i in 1:nrow(mydf)){
  if(is.na(mydf$purpose[i])){
    mydf$purpose[i] <- mydf$purpose[i-1]
  }else{
    mydf$purpose[i] <- mydf$purpose[i]
  }
}

my func <- function (x,y,z) {
  my_sum <- x+y+z
  return(my_sum)
}
my_sum(10,12,13)

returns(1,2,6,6,3,5,2,4,6,4,)
mean(returns)

mydf <- as.data.frame(mydf)
for(i in 1:ncol(mydf)){
  hist(as.numeric(mydf[,i]))
}

size <- c(31,25,62,85,64,81,89,102)
signedup  <- c(0,0,1,1,0,1,1,1)
my_sales <- as.data.frame(matrix(nrow=8,ncol=1))
my_sales$size <- size
my_sales$signedup <- signedup
print(my_sales)
my_sales$V1 <- NULL
my_sales
sales_mod <- glm(signedup ~ size, data=my_sales, family="binomial")
summary(sales_mod)
exp(0.1912)
 

odds2prob <- function(inter,betal,x){
  logit <- inter +betal*x
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(c(odds,prob))
}
odds2prob(inter=-11,betal=0.1912, x=40)
odds2prob(inter=-11,betal=0.1912, x=71)
odds2prob(inter=-11,betal=0.1912, x=72)

mydf <- german_credit_card
mydf$purpose <- as.numeric(gsub("x","",mydf$purpose))
mydf$good_bad <- gsub("good","1",mydf$good_bad)
mydf$good_bad <- gsub("bad","0",mydf$good_bad)
mydf$good_bad <- as.numeric(mydf$good_bad)
mydf$good_bad
good_mod <- glm(good_bad ~ age, data=mydf, family="binomial")
summary(good_mod)
exp(0.01884)-1
odds2prob(inter=0.2,betal=0.0184, x=30)
odds2prob(inter=0.2,betal=0.0184, x=31)
odds2prob(inter=0.2,betal=0.0184, x=21)



