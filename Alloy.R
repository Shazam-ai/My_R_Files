alloy <- as.data.frame(Alloy_working_data)
summary(alloy)
names(alloy) <- c("week", "visits","unique","pageviews","pagevisits","avg_time","bounce","newvisits","revenue","profit","lbs_sold","inquiries")
all_f <- glm(Visits ~ Revenue+Profit+Pageviews+Inquiries, data=alloy)
summary(all_f)
library(ggplot2)
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
one.sd = (lloy_2$lbs_sold2 > lower.bounds[1] & lloy_2$lbs_sold2 < upper.bounds[1])
two.sd = mean(lloy_2$lbs_sold2 > lower.bounds[2] & lloy_2$lbs_sold2 < upper.bounds[2])
three.sd = mean(lloy_2$lbs_sold2 > lower.bounds[3] & lloy_2$lbs_sold2 < upper.bounds[3])

#Lead_sites graph
g<- ggplot(data= Lead_sites,aes(x=Leads, y=Visits))
g + geom_bar(stat = "identity", position = 'dodge', colour="pink") + 
    ggtitle("Number of visits per website") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
g
#Geographical_frequency
h<-ggplot(Geographical_frequency,aes(x=Geographical_frequency$`Geographical Frequency`, y=Geographical_frequency$Visits, fill=Geographical_frequency$`Geographical Frequency`)) +
  geom_bar(stat="identity")+theme_minimal()+
  xlab("") + ylab("") +ggtitle("Visit per Geo") + labs(fill = "Geo")
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
h
#Website_Frequency
i<-ggplot(Website_Frequency,aes(x=Website_Frequency$Website, y=Website_Frequency$Visits, fill=Website_Frequency$Website)) +
  geom_bar(stat="identity")+theme_minimal()+
  xlab("") + ylab("") +ggtitle("Visit per Top 10 Web") + labs(fill = "Website")
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
i
# Browser_Distribution
j<-ggplot(Browser_Distribution,aes(x=Browser_Distribution$`Browser Distribution`, y=Browser_Distribution$Visits, fill=Browser_Distribution$`Browser Distribution`)) +
  geom_bar(stat="identity")+theme_minimal()+
  xlab("") + ylab("") +ggtitle("Visit per Browser") + labs(fill = "Browser")
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
j
#OS_Distrubution
k<-ggplot(OS_Distribution,aes(x=OS_Distribution$`OS Distribution`, y=Visits, fill=OS_Distribution$`OS Distribution`)) +
  geom_bar(stat="identity")+theme_minimal()+
  xlab("") + ylab("") +ggtitle("Visit per Device") + labs(fill = "OS")
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
k

