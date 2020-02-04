library(readxl)
mydf<- read_excel("german credit card.xls")

#rub data, replace missing values X
mydf$purpose_num<-as.numeric(replace(mydf$purpose, "", "X"))[1:1000]

#rub data, transform good_bad to 1&0 and numeric
mydf$good_bad<-gsub(mydf$good_bad,pattern='good',replacement=1)
mydf$good_bad<-gsub(mydf$good_bad,pattern='bad',replacement=0)

mydf$good_bad<-as.numeric(mydf$good_bad)

#subset good bad
mydf_good <- mydf[which(mydf$good_bad == 1),c('checking','duration')]
mydf_bad <- mydf[which(mydf$good_bad == 0),1:2]
