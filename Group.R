### Group Project - Maxime's code

#library 

library(readxl)
library(data.table)
library(ggplot2)
library(plotly)
library(moments)
library(corrplot)
library(PerformanceAnalytics)
library(rsq)
library(psych)

source("http://www.sthda.com/upload/rquery_cormat.r")

#convert into datframe 
alloy <- as.data.frame(Alloy_working_data)

#summary of datframe 
summary(alloy)

#rename columns 
names(alloy) <- c("week", "visits","unique","pageviews","pagevisits","avg_time","bounce","newvisits","revenue","profit","lbs_sold","inquiries")

#regression 
all_f <- glm(unique ~ visits+pageviews+pagevisits+avg_time+bounce+newvisits+revenue+profit+lbs_sold+inquiries, data=alloy)
summary(all_f)

#Scatter matrix
pairs(~revenue+profit+pageviews+inquiries,data=alloy, 
      main="Simple Scatterplot Matrix")

#Scatter plot Q5 using Car package
library(car)

scatterplot(revenue ~ lbs_sold, data=alloy)

#Scatter plot Q5 using GGPLOT2
#revenue vs. lbs_sold

library(ggplot2)
ggplot(alloy, aes(x=lbs_sold, y=revenue)) + 
  geom_point()+
  geom_smooth(method=lm)

#Correl

cor(revenue,lbs_sold)


#Scatter plot Q6 using GGPLOT2
#revenue vs. visits 

ggplot(alloy, aes(x=visits, y=revenue)) + 
  geom_point()+
  geom_smooth(method=lm)

#Correl
attach(alloy)
cor(revenue,visits)

#Import weekly lbs. sold
alloy_2_df <- as.data.frame(alloy_2)

#Summary values Q8
summary(alloy_2_df$lbs_sold2)

#Histogram Q8
ggplot(alloy_2_df, aes(x=alloy_2_df$lbs_sold2)) + geom_histogram(bins = 40)


#Empirical rule check
the_mean <- mean(alloy_2_df$lbs_sold2)
the_sd <- sd(alloy_2_df$lbs_sold2)

#Theoritical observation 
lbs_obs <- length(alloy_2_df$lbs_sold2)

t1 <- round(lbs_obs * 0.68)
t2 <- round(lbs_obs * 0.95)
t3 <- round(lbs_obs * 0.99)

#Actual values calculation
lower_bounds <- the_mean - 1:3*the_sd
upper_bounds <- the_mean + 1:3*the_sd


one_sd <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 > lower_bounds[1] & alloy_2_df$lbs_sold2 < upper_bounds[1])])
two_sd <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 > lower_bounds[2] & alloy_2_df$lbs_sold2 < upper_bounds[2])])
three_sd <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 > lower_bounds[3] & alloy_2_df$lbs_sold2 < upper_bounds[3])])


#Comparison table
library(data.table)
data.table(interval = c("mean ± 1 std. dev","mean ± 2 std. dev","mean ± 3 std. dev"),
           Theoretical_Perc_of_Data = c("68", "95","99"),
           Theoretical_No_Obs = c(t1, t2, t3),
           Actual_No_Obs = c(one_sd, two_sd, three_sd))



#Actual values with ups and lows
one_sd_up <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 < the_mean + the_sd
                                  & alloy_2_df$lbs_sold2 > the_mean)])
one_sd_low <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 < the_mean - the_sd
                                  & alloy_2_df$lbs_sold2 > the_mean)])

two_sd_up <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 < the_mean + 2*the_sd
                                               & alloy_2_df$lbs_sold2 > the_mean + the_sd)])
two_sd_low <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 < the_mean - 2*the_sd
                                                & alloy_2_df$lbs_sold2 > the_mean - the_sd)])

three_sd_up <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 < the_mean + 3*the_sd
                                               & alloy_2_df$lbs_sold2 > the_mean + 2*the_sd)])
three_sd_low <- length(alloy_2_df$lbs_sold2[which(alloy_2_df$lbs_sold2 < the_mean - 3*the_sd
                                                & alloy_2_df$lbs_sold2 > the_mean - 2*the_sd)])

#Comparison table 

data.table(interval = c("mean + 1 std. dev","mean - 1 std. dev",
                        "mean + 2 std. dev","mean - 2 std. dev",
                        "mean + 3 std. dev","mean - 3 std. dev"),
           Theoretical_Perc_of_Data = c("34","34", "13.5","13.5","2","2"),
           Theoretical_No_Obs = c(t1/2, t1/2,
                                  (t2-t1)/2, (t2-t1)/2,
                                  (t3-t2)/2, (t3-t2)/2),
           Actual_No_Obs = c(one_sd_up,one_sd_low,
                             two_sd_up,two_sd_low,
                             three_sd_up,three_sd_low))
library(moments)
skewness(alloy_2_df$lbs_sold2) 
kurtosis(alloy_2_df$lbs_sold2)

### Extra analysis ###

#Heatmaps 
round(cor(alloy[,c(2,3,4,5,6,7,8,9,10,11)]),2)

#Visual heatmaps 
rquery.cormat(alloy[,c(2,3,4,5,6,7,8,9,10,11)], type = "full")

chart.Correlation(alloy[,c(2,3,4,5,6,7,8,9,10,11)], histogram=TRUE, pch=19)

pairs.panels(alloy[,c(2,3,4,5,6,7,8,9,10,11)])

heatmap(x = cor(alloy[,c(2,3,4,5,6,7,8,9,10,11)]), symm = TRUE)

###Other Graphs + Regression

#Profit per week
ggplot(alloy, aes( week, profit)) + 
  geom_bar(stat="identity", width = 0.5, fill="green") +
  labs(title="Profit Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))

#Visits per week
ggplot(alloy, aes(week, visits)) +
  geom_bar(stat="identity", width = 0.5, fill="blue") +
  labs(title="Unique Visit Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))

#Revenue per week
ggplot(alloy, aes(week, revenue))+ 
  geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  labs(title="Revenue Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))

#Bounce rate per week
ggplot(alloy, aes(week, bounce)) +
  geom_bar(stat="identity", width = 0.5, fill="blue") +
  labs(title="Unique Visit Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))

#Regression of Inquiries vs Revenue
ggplot(alloy, aes(x=inquiries, y=revenue))+
  geom_point()+stat_smooth(method=lm)

alloy <- as.data.frame(Alloy_working_data)
summary(alloy)
names(alloy) <- c("week", "visits","unique","pageviews","pagevisits","avg_time","bounce","newvisits","revenue","profit","lbs_sold","inquiries")
all_f <- glm(Visits ~ Revenue+Profit+Pageviews+Inquiries, data=alloy)
summary(all_f)
library(ggplot2)
attach(alloy)
ggplot(alloy, aes(week, unique,group=4)) +
  geom_line(color = 'cyan') +
  geom_area(fill = 'cyan', alpha = .3) +
  labs(x = 'Week'
       , y = 'Unique'
       , title = "Unique Visit per week") +
  scale_x_discrete(breaks = levels(alloy$week)[5])+
  theme(axis.text.x = element_text(angle=90, vjust=0.8))+
  theme(text = element_text(family = 'bold.italic', color = "#444444")
        ,panel.background = element_rect(fill = '#444B5A')
        ,panel.grid.minor = element_line(color = '#4d5566')
        ,panel.grid.major = element_line(color = '#586174')
        ,plot.title = element_text(size = 28)
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 0)
        ,axis.title.x = element_text(hjust = 0)
  ) 


ggplot(alloy, aes(week, revenue,group=4)) +
  geom_line(color = 'green') +
  geom_area(fill = 'green', alpha = .3) +
  labs(x = 'Week'
       , y = 'Revenue'
       , title = "Revenue per week") +
  scale_x_discrete(breaks = levels(alloy$week)[5])+
  theme(axis.text.x = element_text(angle=90, vjust=0.8))+
  theme(text = element_text(family = 'bold.italic', color = "#444444")
        ,panel.background = element_rect(fill = '#444B5A')
        ,panel.grid.minor = element_line(color = '#4d5566')
        ,panel.grid.major = element_line(color = '#586174')
        ,plot.title = element_text(size = 28)
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 0)
        ,axis.title.x = element_text(hjust = 0)
  )

library(ggplot2)
#imoport different tab in same excel sheet

weeklyvisits<-read_excel("Alloy - working data.xls",sheet=1,col_names = TRUE,range = "A5:L67")
head(weeklyvisits)

lbssold <- read_excel("Alloy - working data.xls",sheet=2,col_names = TRUE,range ="A5:B295")
head(lbssold)

dailyvisits<-read_excel("Alloy - working data.xls",sheet=3,col_names = TRUE,range ="A5:B2467")
head(dailyvisits)

referingsites<-read_excel("Alloy - working data.xls",sheet=4,col_names = TRUE,range ="B7:C11")
head(referingsites)

leadssites<-read_excel("Alloy - working data.xls",sheet=4,col_names = TRUE,range ="B14:C24")
head(leadssites)

searchengine<-read_excel("Alloy - working data.xls",sheet=4,col_names = TRUE,range ="B27:C37")
head(searchengine)

geo<-read_excel("Alloy - working data.xls",sheet=4,col_names = TRUE,range ="B40:C50")
head(geo)

browsers<-read_excel("Alloy - working data.xls",sheet=4,col_names = TRUE,range ="B54:C64")
head(browsers)

operatingsystem<-read_excel("Alloy - working data.xls",sheet=4,col_names = TRUE,range ="B68:C78")
head(operatingsystem)
#Q5
# Change the point size, and shape
ggplot(alloy, aes(x=lbs_sold, y=revenue)) +
  geom_point(size=1.2, color='black', shape=23)+geom_smooth(method=lm, se=FALSE)
#Q6
ggplot(alloy, aes(x=visits, y=revenue)) +
  geom_point(size=1.2, color='brown', shape=23)+geom_smooth(method=lm, se=FALSE)
#Q7
attach(alloy)
cor_1 <- cor(revenue,visits, method = "pearson")
summary(cor_1)
#Q8 
#a)
alloy_2 <- as.data.frame(alloy_2)
summary(alloy_2$lbs_sold2)
#b)
library(ggplot2)
h<-ggplot(alloy_2, aes(x=lbs_sold2)) + 
  geom_histogram(color="black", fill="white")
h
#c)
the.mean = mean(alloy_2$lbs_sold2)
the.sd = sd(alloy_2$lbs_sold2)
lower.bounds = the.mean - 1:3*the.sd
upper.bounds = the.mean + 1:3*the.sd
one.sd = (alloy_2$lbs_sold2 > lower.bounds[1] & alloy_2$lbs_sold2 < upper.bounds[1])
two.sd = mean(alloy_2$lbs_sold2 > lower.bounds[2] & alloy_2$lbs_sold2 < upper.bounds[2])
three.sd = mean(alloy_2$lbs_sold2 > lower.bounds[3] & alloy_2$lbs_sold2 < upper.bounds[3])

#Lead_sites graph
g<- ggplot(leadssites,aes(x=leadssites$...1, y=Lead_sites$Visits, fill=leadssites$...1)) +
  geom_bar(stat="identity")+theme_minimal() + guides(fill = guide_legend(reverse=TRUE))+
  xlab("") + ylab("") +
  ggtitle("Visit Per Leads") +labs(fill = "Leads") + scale_x_discrete(breaks = levels(leadssites$...1)[5])+
  theme(plot.title = element_text(color="grey", size=14, face="bold.italic"),
        axis.text.x = element_text(angle=70, vjust = 0.8))
g
#Geographical_frequency
h<-ggplot(geo,aes(x=Geographical_frequency$`Geographical Frequency`, y=Geographical_frequency$Visits, fill=Geographical_frequency$`Geographical Frequency`)) +
  geom_bar(stat="identity")+theme_minimal()+ guides(fill = guide_legend(reverse=TRUE)) +
  xlab("") + ylab("") +ggtitle("Visit per Geo") + labs(fill = "Geo")+
theme(plot.title = element_text(color="grey", size=14, face="bold.italic"),
      axis.text.x = element_text(angle=90, vjust=0.8))
h
#Search Engine _Frequency
i<-ggplot(searchengine,aes(x=Website_Frequency$Website, y=Website_Frequency$Visits, fill=Website_Frequency$Website)) +
  geom_bar(stat="identity")+theme_minimal()+ guides(fill = guide_legend(reverse=TRUE)) +
  xlab("") + ylab("") +ggtitle("Visit per Top 10 Web") + labs(fill = "Website")+
theme(plot.title = element_text(color="grey", size=14, face="bold.italic"),
      axis.text.x = element_text(angle=90, vjust=0.8))
i
# Browser_Distribution
j<-ggplot(browsers,aes(x=Browser_Distribution$`Browser Distribution`, y=Browser_Distribution$Visits, fill=Browser_Distribution$`Browser Distribution`)) +
  geom_bar(stat="identity")+theme_minimal()+ guides(fill = guide_legend(reverse=TRUE)) +
  xlab("") + ylab("") +ggtitle("Visit per Browser") + labs(fill = "Browser")+
theme(plot.title = element_text(color="grey", size=14, face="bold.italic"),
      axis.text.x = element_text(angle=-90, vjust=0.5,hjust=0))
j
#OS_Distrubution
k<-ggplot(operatingsystem,aes(x=OS_Distribution$`OS Distribution`, y=Visits, fill=OS_Distribution$`OS Distribution`)) +
  geom_bar(stat="identity")+theme_minimal() +
  xlab("") + ylab("") +ggtitle("Visit Per Device") + guides(fill = guide_legend(reverse=TRUE)) + labs(fill = "OS")+
theme(plot.title = element_text(color="grey", size=14, face="bold.italic"),
      axis.text.x = element_text(angle=90, vjust=0.8))
k

# Visits Source
l<-ggplot(referingsites,aes(x=referingsites$...1, y=Visits, fill=referingsites$...1)) +
  geom_bar(stat="identity")+theme_minimal() + guides(fill = guide_legend(reverse=TRUE)) +
  xlab("") + ylab("") + ggtitle("Source of Visits")  + labs(fill = "Visits") + scale_x_discrete(breaks = levels(referingsites$...1)[5])
  theme(plot.title = element_text(color="grey", size=14, face="bold.italic"),
        axis.text.x = element_text(angle=90, vjust=0.8))
l




#Dapo file

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


