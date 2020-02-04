df4<- read.table("teach.txt", header=TRUE, sep="\t")
df4

df5 <- read.csv("generic.csv",header=T)
head(df5)

job<-c("Lecturer", "Assistant Professor", "Professor")
job<-factor(job, order=TRUE, levels=c("Lecturer", "Assistant Professor", "Professor"))
job

