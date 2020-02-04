# Sentiment in tidytext()
library("tidytext")
library("textdata")
library("dplyr")
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
print(sentiments)
table(nrc$sentiment)

sentiments <- bind_rows(
  (mutate(afinn,lexicon="afinn")),
  (mutate(nrc,lexicon="nrc")),
  (mutate(bing,lexicon="bing"))
)
sentiments
unique(sentiments$sentiment) #this is the qualitative
unique(sentiments$lexicon)#this is the lexicon source
summary(sentiments$score) # this is 3rd score that we can use / quantitative

#########################################################
##### Lets take a look at the lexicons one by one #######
#########################################################
#how can we subset the data to get distinct lexicons?
nrc_data <- subset(sentiments, lexicon == "nrc")
unique(nrc_data$sentiment) # these are the nrc options of sentiment labels

