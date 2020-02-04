library(readxl)
mydf <- read_excel("Downloads/german credit card.xls")

mydf$purpose<-as.numeric(gsub("x","",mydf$purpose))

mydf$good_bad<-gsub("good","1",mydf$good_bad)
mydf$good_bad<-gsub("bad","0",mydf$good_bad)
mydf$good_bad<-as.numeric(mydf$good_bad)

#For loop
for(i in 1:3){
  print (i)
}

my_vect <- c()
for(i in 1:10){
  my_vect[i] <- i
}



my_vect<- c()
for(i in 1:10){
  my_vect[i]<-i
}

mydf$new_col <- c()
for (i in 1:nrow(mydf)){
  mydf$new_col[i] <- mydf$purpose[i]
}

pk <- c(1,2,3,4,5,6)

if (pk[2]==2){print("Yupii!!!")}


mydf$desc <- c()
for (i in 1:nrow(mydf)){
  
  if(mydf$good_bad[i]=="1"){mydf$desc[i]<-"positive"}else(mydf$desc[i]<-"negative")
}

for (i in 1:nrow(mydf)){
  
  if(is.na(mydf$purpose[i]=="NA")){mydf$purpose[i]<-mydf$purpose[i-1]
  }else{
    mydf$purpose[i]<-mydf$purpose[i]
  }
}

#user defined function
my_func <- function(x,y,z){
  my_sum <- x+y+z
  return(my_sum)
}

my_func(x=1,y=10,z=1980)

install.packages("minpack.lm")
objective<-c(1000,480,1800,1000,990)
library(minpack.lm)
nlsLM(objective-my_func(m1,m2,m3))
mix1<-c(1200,600,1050,860,720)
mix2<-c(1050,310,2100,990,880)
mix3<-c(720,420,1700,1600,1120)

weight <- function(x,y,z){
  my_sum <- x+y+z
  return(my_sum)
}



#dice
returns <- c(2, 4,2,6,5,1,3,4,3,1,6,6,6,5,2,5,2,5,5,4,1,3,2,5,5,6,2,4,3,1,5,5,5,6,6,4,5,4,3,3,3,5,5,5,6,1,6,6,6,1,3,5,3,2,1,1,5,5,3,3,1,1,1,5,3,1,4,6,3,5,1,3,4,4,3,5,5,6)
mean(returns)


#sample
hist(sample(c(1,2,3,4,5,6),5000,replace = T))

hist(sample(c(0,1),5000,replace = T))

hist(rexp(200,rate = 0.1),breaks = seq(from=0, to=200, by=2))

hist(rexp(200,rate = 1.5),breaks = seq(from=0, to=200, by=2))
exp(-1.5*5)
1-exp(-1.5*5)