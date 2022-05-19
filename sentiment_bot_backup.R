library(readr)
library(stringr)
library(tidyverse)
library(tidytext)
library("rjson")
library(plyr)
library(sentimentr)
library(lubridate)
library(rcompanion)
library("miceadds")
library(patchwork)
library(readxl)
library(wordcloud)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

###
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load sentiment files
negativ <- read.table("SentiWS_v2.0_Negative.txt", fill = TRUE,encoding = "UTF-8")
positiv <- read.table("SentiWS_v2.0_Positive.txt", fill = TRUE,encoding = "UTF-8")


#delete the NN stuff
negativ = separate(data = negativ, col = V1, sep =  "[|]", into = "V1")
negativ = negativ %>% filter(!V3=="")
positiv = separate(data = positiv, col = V1, sep =  "[|]", into = "V1")

#split into single words
einzelworte_negativ <- strsplit(as.character(negativ$V3), split =",")
einzelworteframe_negativ <- as.data.frame(unlist(einzelworte_negativ))

einzelworte_positiv <- strsplit(as.character(positiv$V3), split =",")
einzelworteframe_positiv <- as.data.frame(unlist(einzelworte_positiv))

#takes the number of words and creates a data frame only with the sentiment scores as many times as the word inflection occurs.
number_words <- summary(einzelworte_negativ)

sentiment_score <- NULL
for (i in 1:1759) {
  j <- 0
  while (j < as.numeric(number_words[i])) {
    sentiment_score <- rbind(sentiment_score, negativ[i,2])
    j <- j+1
  }
}

##
new_negativ <- cbind(as.character(einzelworteframe_negativ[,1]), sentiment_score)
new_negativ <- rbind(negativ[,1:2], new_negativ)

# same for positiv
number_words <- summary(einzelworte_positiv)

sentiment_score <- NULL
for (i in 1:1644) {
  j <- 0
  while (j < as.numeric(number_words[i])) {
    sentiment_score <- rbind(sentiment_score, positiv[i,2])
    j <- j+1
  }
}

#bind the sentiment score with the words
new_positiv <- cbind(as.character(einzelworteframe_positiv[,1]), sentiment_score)
new_positiv <- rbind(positiv[,1:2], new_positiv)


lexicon = bind_rows(new_positiv, new_negativ)
#lexicon = lexicon %>% select(V1, V2) %>% rename(polarity = V2, word = V1) %>% mutate_all(.funs=tolower)
lexicon$V2 = as.numeric(lexicon$V2)
lexicon$V1 = str_to_lower(lexicon$V1)
lexikey = as_key(lexicon)

####### tokenize tweets #############################################################################

tweets <- all_tweets_botchecked %>% select(created_at, id, text, entities, in_reply_to_user_id, author_id, cap.universal, lang, botcheck)

main_text <- str_to_lower(tweets$text)

text_tbl <- tibble(main_text)
text_tbl$id <- c(1:1907349)
text_tbl$time <- tweets$created_at
text_tbl$botcheck <- tweets$botcheck

token.tbl <- text_tbl %>% 
  unnest_tokens(word, main_text)

token.tbl <- tibble(token.tbl)


# lexikey 
lexikey$word <- lexikey$x
lexikey$value <- lexikey$y
lexikey <- lexikey %>% select(word,value)

tbl <- token.tbl %>% 
  inner_join(lexikey)

average <- groupwiseMean(value ~ id ,
                         na.rm =TRUE,
                         traditional =FALSE,
                         data=tbl)

average <- average %>% select(id, Mean)
tweets <- merge(tbl,average,by="id")
tweets$date <- as.Date(tweets$time)
tweets$month <- str_sub(tweets$date, 1,7)
tweets$year <- str_sub(tweets$date, 1,4)
tweets$week <- strftime(tweets$date, format = "%V")

summary<- groupwiseMean(value ~  date,
                        na.rm=TRUE,
                        traditional = FALSE,
                        data = tweets)


ggplot(summary, aes(x=date, y=Mean, color= botcheck)) + 
  theme_minimal() + geom_line() + ylim(-.2, .2) +
  theme(legend.text = element_text(size=10),
        axis.text.x = element_text(size=10, angle=90) ) +
  scale_x_continuous(breaks = round(seq(min(summary$date), max(summary$date), by = 1),1)) 


#### negative vs. positiv 

tweets$value <- as.numeric(tweets$value)

tweets$factor  <- cut(tweets$value, breaks=c(-1 , -0.3, 0.3, 1), labels=c("negative","neutral", "positive"))

tweets2 <- tweets %>% filter(!factor=="NA")

tweets3 <- tweets2 %>% filter(!factor=="neutral")

mean <- groupwiseMean(value ~ date + factor,
                      traditional = FALSE,
                      data =tweets3 )
colors_2 <- c("red", "chartreuse4")

ggplot(mean, aes(x=date, y=n, color=factor, group=factor)) +
  geom_line() + labs(color=NULL, x=NULL, y=NULL) + 
  scale_color_manual(values = colors_2) + theme_bw() +
  theme(legend.text = element_text(size=10),
        axis.text.x = element_text(size=10, angle=90 ),
        axis.text.y = element_text(size=10)
  ) +
  scale_x_continuous(breaks = round(seq(min(mean$date), max(mean$date), by = 1),1)) 


### wordcloud 

wordcloud(token.tbl$word)
#oder
stopwords <- stopwords("german")
#token_frequencies <- table(token.tbl$word)
#token_frequencies <- token_frequencies %>% sort(decreasing = TRUE)
#stops <- names(token_frequencies) %in% stopwords 

docs <- token.tbl %>% filter(!word %in% stopwords)

wordcloud(words = docs$word,  min.freq = 15,
          max.words=500, rot.per=0.35,# ordered.colors=TRUE,
          scale = c(3.5,1.0), random.order= FALSE, 
          colors=c("#67a9cf", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d"))