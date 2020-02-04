get
library(readxl)
mydf <- read_excel("german credit card.xls")
View(mydf)


summary(mydf)

#replace(table or dataframe, empty spot"", value I want to change with "X")
my_vect<- as.numeric(replace(mydf$purpose,"","X" ))

#Create an extra column / variable
mydf$purpose_num <- my_vect[1:1000]

#What happens if I do?
mydf$purpose_num <- my_vect[nrow(),ncol()]

#Check in wich order r added it
tail(my_vect)

#Replace more efficient
?gsub
mydf$purpose_num <- as.numeric(gsub("X","", mydf$purpose))

#Replace of a categorical variable in 3 steps
mydf$good_bad <- gsub("good","1", mydf$good_bad)
mydf$good_bad <- gsub("bad","0", mydf$good_bad)
mydf$good_bad <- as.numeric(mydf$good_bad)

??ggplot2

