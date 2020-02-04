#Class_3_R_scripts

#Testing the presence of Missing Values
y<-c(1,2,3,NA)
is.na(y)

#Reads the data using a mm/dd/yyyy format
strDates<-c("01/09/1965", "08/16/1975")
dates<-as.Date(strDates, "%m/%d/%Y")
dates
 
#ggplot2 example
install.packages("ggplot2")
library(ggplot2)

facebookdata<-read.delim("FacebookNarcissism.dat", header=TRUE)
head(facebookdata)

#graphing the relationship between narcissism and the profile rating
graph<-ggplot(facebookdata, aes(NPQC_R_Total, Rating))
graph + geom_point(shape=17)
graph + geom_point(aes(colour=Rating_Type))

#graphing the relationship between Anxiety and Exam using scatter plot
examdata<-read.delim("Exam Anxiety.dat", header = TRUE)
head(examdata)
scatter<-ggplot(examdata, aes(Anxiety, Exam))
scatter +geom_point() +geom_smooth(method="lm", colour = "Red") +labs(x="Exam Anxiety", y="Exam Performance %")

#Boxplot of hygiene scores on day 1 of the Download Festival split by gender
festivalData<-read.delim("DownloadFestival.dat", header =TRUE)
head(festivalData)
festivalBoxplot<- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() +labs(x="Gender", y="Hygiene (Day 1 of Festival)")

#Bar chart for one independent variable
chickflick<-read.delim("ChickFlick.dat", header=TRUE)
head(chickflick)
bar<-ggplot(chickflick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Film", y = "Mean Arousal")

#Bar chart for two independent variables
bar <- ggplot(chickflick, aes(film, arousal, fill = gender))

bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")

