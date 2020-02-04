#install the packages if necessary
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("fs")) install.packages("fs")
if(!require("readxl")) install.packages("readxl")

#load packages
library(tidyverse)
library(fs)
library(readxl)
library(ggplot2)

#import weekly visit sheet
weekly<-read_excel("Web Analytics Case Student Spreadsheet-2.xls",sheet=2,col_names = TRUE,range = "A5:H71")

summary(weekly)

#import Financial sheet
financial<-read_excel("Web Analytics Case Student Spreadsheet-2.xls",sheet=3,col_names = TRUE,range = "A5:E71")

summary(financial)

#merging the sheets
mydf<-merge(weekly,financial,sort=FALSE)
mydf


summary(mydf)

head(mydf)
tail(mydf)



#importing daily visit sheet

dailyvisits<-read_excel("Web Analytics Case Student Spreadsheet-2.xls",sheet=5,col_names = TRUE,range = "A5:B467")

head(dailyvisits)


#Adding a new column (cost=Revenue-Profit) to mydf

mydf$Revenue
mydf$Profit

for (i in 1:nrow(mydf)){
  mydf$cost[i] <- mydf$Revenue[i]-mydf$Profit[i]
  
}


head(mydf)




#subsetting the Weekly means for the time periods
time_period<-c("May 25 to Aug 30, 2008","Aug 21 to Jan 24, 2009", "Jan 25 to May 23, 2009", "May 24 to Aug 29, 2009")

mydf_visits<-c((mean(mydf$Visits[1:14])),(mean(mydf$Visits[15:35])),(mean(mydf$Visits[36:52])),(mean(mydf$Visits[53:66])))
mydf_uniquevisits<-c((mean(mydf$`Unique Visits`[1:14])),(mean(mydf$`Unique Visits`[15:35])),(mean(mydf$`Unique Visits`[36:52])),(mean(mydf$`Unique Visits`[53:66])))
mydf_pageviews<-c((mean(mydf$Pageviews[1:14])),(mean(mydf$Pageviews[15:35])),(mean(mydf$Pageviews[36:52])),(mean(mydf$Pageviews[53:66])))
mydf_pagespervisit<-c((mean(mydf$`Pages/Visit`[1:14])),(mean(mydf$`Pages/Visit`[15:35])),(mean(mydf$`Pages/Visit`[36:52])),(mean(mydf$`Pages/Visit`[53:66])))
mydf_timeonsite<-c((mean(mydf$`Avg. Time on Site (secs.)`[1:14])),(mean(mydf$`Avg. Time on Site (secs.)`[15:35])),(mean(mydf$`Avg. Time on Site (secs.)`[36:52])),(mean(mydf$`Avg. Time on Site (secs.)`[53:66])))
mydf_bounce<-c((mean(mydf$`Bounce Rate`[1:14])),(mean(mydf$`Bounce Rate`[15:35])),(mean(mydf$`Bounce Rate`[36:52])),(mean(mydf$`Bounce Rate`[53:66])))
mydf_revenue<-c((mean(mydf$Revenue[1:14])),(mean(mydf$Revenue[15:35])),(mean(mydf$Revenue[36:52])),(mean(mydf$Revenue[53:66])))
mydf_profit<-c((mean(mydf$Profit[1:14])),(mean(mydf$Profit[15:35])),(mean(mydf$Profit[36:52])),(mean(mydf$Profit[53:66])))
mydf_lbsold<-c((mean(mydf$`Lbs. Sold`[1:14])),(mean(mydf$`Lbs. Sold`[15:35])),(mean(mydf$`Lbs. Sold`[36:52])),(mean(mydf$`Lbs. Sold`[53:66])))
mydf_inquiries<-c((mean(mydf$Inquiries[1:14])),(mean(mydf$Inquiries[15:35])),(mean(mydf$Inquiries[36:52])),(mean(mydf$Inquiries[53:66])))
mydf_newvisit<-c((mean(mydf$`% New Visits`[1:14])),(mean(mydf$`% New Visits`[15:35])),(mean(mydf$`% New Visits`[36:52])),(mean(mydf$`% New Visits`[53:66])))
mydf_cost<-c((mean(mydf$cost[1:14])),(mean(mydf$cost[15:35])),(mean(mydf$cost[36:52])),(mean(mydf$cost[53:66])))






mydf_visits
mydf_uniquevisits
mydf_pageviews
mydf_pagespervisit
mydf_timeonsite
mydf_bounce
mydf_revenue
mydf_profit
mydf_lbsold
mydf_inquiries
mydf_newvisit
mydf_cost

# Create the matrix of the periodic weekly means values.
Values <- matrix(c(mydf_visits, mydf_uniquevisits,mydf_pageviews,mydf_pagespervisit,mydf_timeonsite,
                   mydf_bounce,mydf_revenue,mydf_profit,mydf_lbsold,mydf_inquiries,mydf_newvisit,mydf_cost ), nrow = 12, ncol = 4, byrow = TRUE)



Values<-t(Values)

Values

# Create the input vectors.
colors = c("green","orange","brown","blue","red","purple","yellow","violet","pink","grey",'black')
months <- c("Initial period","Pre-promotion","Promotion","Post-promotion")
regions <- c("Visits","Unique Visits","Pageviews","Pages/Visit","Avg. Time on Site (secs.)","Bounce Rate","Revenue",
             "Profit","Lbs. Sold","Inquiries","% New Visits",'cost')


# Give the chart file a name
png(file = "barchart_stacked.png")


#creating loop for graphs
for(i in 1:ncol(Values)) {
  barplot(Values[,i], main = regions[i], names.arg = months, xlab = "Time Period", ylab = regions[i], col = colors)
}

# Add the legend to the chart
legend("bottomleft", time_period, cex = 1.3, fill = colors)



#Logistic Regression



