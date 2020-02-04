###############################################################
######Querying Twitter for shares, like Trump tweets###########
###############################################################
#install the necessary packages
#install.packages("twitteR")
#install.packages("tm")

library("twitteR")
library("tm")

#necessary file for Windows
setwd
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'l9VyCH7efap9RMNrh6Iv8dIDx'
consumer_secret <- 'sEugw9yoloQGsBv2Q2KGzIXsxIoLKDHeUT1cu3S29kfZuTFUd7'
access_token <- '808792291-GnCiqDWb4br1YI8tukxRq2cPBn51toipx32oyEK6'
access_secret <- 'VWorn1PtJAfbzaQ7MePqLvfPlgDzR5Czrz1YeHtWWLdxK'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)
View(d)

EU <- twitteR::searchTwitter('#EU + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)

