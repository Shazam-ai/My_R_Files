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

