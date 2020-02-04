
library("twitteR")
library("tm")
library(twitteR)
library(tm)
library(dplyr)
library(tidyverse)
library(tidytext)


#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'l9VyCH7efap9RMNrh6Iv8dIDx'
consumer_secret <- 'sEugw9yoloQGsBv2Q2KGzIXsxIoLKDHeUT1cu3S29kfZuTFUd7'
access_token <- '808792291-GnCiqDWb4br1YI8tukxRq2cPBn51toipx32oyEK6'
access_secret <- 'VWorn1PtJAfbzaQ7MePqLvfPlgDzR5Czrz1YeHtWWLdxK'


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#nationalgridpartners', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)

EU <- twitteR::searchTwitter('#sequoia', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)

ASIA <- twitteR::searchTwitter('#A16Z', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(ASIA)
print(stop_words)
cust_stop <- data_frame(
  word=c("http", "https","rt", "t.io"),
  lexicon=rep("custom", each=4)
)# #closing data_frame

#this is where you tokenize all 3 twitter datasets
tidy_usa <- d %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) 
  
  
print(tidy_usa)
tidy_eu <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) 

tidy_asia <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop)

#################################################
### Combining all 3 tidy data frames ############
#################################################

library(tidyr)
dplyr::bind_rows()
frequency <- bind_rows(mutate(tidy_usa, author="USA"),
                       mutate(tidy_eu, author= "EU"),
                       mutate(tidy_asia, author="ASIA")
                        )%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `EU`, `ASIA`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`USA`, 
                      color = abs(`USA`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "USA", x=NULL)


cor.test(data=frequency[frequency$author == "EU",],
         ~proportion + `USA`)

cor.test(data=frequency[frequency$author == "ASIA",],
         ~proportion + `USA`)

