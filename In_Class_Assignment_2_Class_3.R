womenscatter<-ggplot(women, aes(weight, height))

womenscatter +geom_point() +geom_smooth(method="lm", colour = "Red") +labs(x="weight", y="height")


mtcarsBoxplot<- ggplot(mtcars, aes(cyl,mpg,group=cyl))
mtcarsBoxplot + geom_boxplot() +labs(x="cyl", y="mpg")


SpiderLong<-read.delim("SpiderLong.dat", header =TRUE)
head(SpiderLong)
bar <- ggplot(SpiderLong, aes(Group, Anxiety))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Group", y = "Anxiety")
