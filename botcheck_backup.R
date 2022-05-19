library(academictwitteR)
library(tidyverse)
library(tidytext)
library(lubridate)
library(httr)
library(xml2) 
library(RJSONIO)


# install botcheck from github
install.packages("devtools")
library(devtools)
install_github("marsha5813/botcheck")
library(botcheck)

# Setup

Mashape_key = "xxxxxxxxxxxxxxxxxx"

consumer_key = "U3YzeZq1KmX57BFwZOZb9Bio8"
consumer_secret = "D610Kl2kDGWz57XLVBcrvMqPanxcefdg6fexS81QPmHW1NXQ9J"
access_token = "1893238058-VPQa1uzzhBnqNYXbU4pypdH15zAZvMne5EqvqW9"
access_secret = "uwqgcWPrcDlKQLcLMPUmHDfrlF0jebBSMFCviW0T4wNzh"

# store 
myapp = oauth_app("twitter", key=consumer_key, secret=consumer_secret)
sig = sign_oauth1.0(myapp, token=access_token, token_secret=access_secret)



<<<<<<< HEAD
### create coding variable for bot/human 


all_tweets_botchecked <- all_tweets_botchecked %>% 
  mutate(botcheck = cap.universal > 0.7)

all_tweets_botchecked <- all_tweets_botchecked %>%
  mutate(botcheck = factor(botcheck, levels = c("TRUE", "FALSE"), labels = c("bot", "human")))

names(all_tweets_botchecked)
head(all_tweets_botchecked)

#all_tweets_botchecked %>%
select(botcheck)

#count(all_tweets_botchecked$botcheck)

saveRDS(all_tweets_botchecked, file = "all_tweets_botchecked")
=======
  botcheck("barackobama")
>>>>>>> parent of b3b2370 (new 11/5)
