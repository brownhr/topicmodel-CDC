library(readr)
library(dplyr)
library(tm)
library(textmineR)


tweets <- read_csv(
  "tweets_filter.csv",
  col_types = cols(
    tweet_id = col_character(),
    conversation_id = col_character(),
  )
)

filtered_tweets <- tweets %>% 
  select(
    tweet_id, text.filter, conversation_id, created_at, lang
  )

filtered_tweets <- filtered_tweets %>% 
  filter(lang == "en")


