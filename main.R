
# academictwitteR::set_bearer()

# query <- c("to:CDCgov")

# academictwitteR::get_all_tweets(query = query,
#                                 data_path = "data/academic/pre2020/",
#                                 bind_tweets = F,
#                                 end_tweets = "2019-10-01T00:00:00Z",
#                                 start_tweets = "2019-01-01T00:00:00Z",
#                                 n = 50000)

# academictwitteR::resume_collection(data_path = "data/academic/full/",
#                                    n = 5000000
#                                    
# )



# tweets <- bind_tweets(data_path = "data/academic/full/", output_format = "tidy")

# tweets.content$filter <- str_remove_all(tweets.content$filter, pattern)
# 
# tweets.content <- tweets.content[!(is.na(tweets.content$filter) | tweets.content$filter==""),]
# 
# fx <- list(stripWhitespace,
#            stopwords,
#            removePunctuation,
#            content_transformer(tolower))

# 
tweets_bind <- bind_tweets(data_path = "data/academic/full/",
                      output_format = "tidy")


tweets_bind %>% write_csv(file = "tweets_full.csv")

twformat <- c("%Y-%m-%dT%H:%M:%S.000Z")

tweets <- read_csv("tweets_full.csv",col_types = cols(.default = "c")) %>% 
  mutate(
    timestamp = strptime(x = created_at,
                         format = twformat,
                         tz = "gmt")
  ) %>%
  mutate(
    text.filter = str_remove_all(text, pattern),
    text.filter = str_squish(text.filter),
    parent.text.filter = str_remove_all(sourcetweet_text, pattern),
    parent.text.filter = str_squish(parent.text.filter)
  ) %>% dplyr::filter(text.filter != "")

tweets %>% write_csv(file = 'tweets_filter.csv')

# Grouping by author is potentially unhelpful - ~163,000 unique author IDs, so each
# author is writing two tweets on average

tweets.author <- tweets %>%
  group_by(author_id) %>%
  summarise(text.filter = paste(text.filter, collapse = "; "))

tweets.author %>% write_csv("tweets_author.csv")



emoji <- read_csv(file = "Emoji_Sentiment_Data_v1.0.csv")

emoji <- emoji %>% 
  mutate(
    ppositve = Positive / Occurrences,
    pnegative = Negative / Occurrences,
    pneutral = Neutral / Occurrences
  ) %>% 
  mutate(
    sent = ppositve - pnegative
  )
  
emoji <- emoji %>% 
  mutate(
    sent.sign = sign(sent) %>% as.character(),
    sentiment = recode(
      sent.sign,
      '-1' = 'negative',
      '0' = 'neutral',
      '1' = 'positive'
    )
  )
  

emoji.sent <- emoji[,c("Emoji","sentiment")] %>% 
  rename(
    "word"= "Emoji"
  )
  

afinn <- get_sentiments("afinn")
bing_emoji <- bind_rows(get_sentiments("bing"), emoji.sent) %>% gt()

t.stopwords <- c("amp", "$", "RT")
t.corpus <- tweets.author$text.filter %>% 
  corpus()
t.tokens <- t.corpus %>% 
  tokens(remove_punct = T,
         remove_url = T) %>% 
  tokens_select(pattern = stopwords('en'),
                selection = "remove") %>% 
  tokens_select(pattern = t.stopwords,
                selection = "remove")

t.dfm <- t.tokens %>% dfm(remove_padding = T)
t.lda <- textmodel_lda(t.dfm, k = 5)
t.terms <- seededlda::terms(t.lda, 50) %>% data.frame()


# NRC Citation:
# Mohammad, S. M., and P. D. Turney. 2013. Crowdsourcing a Word-Emotion Association Lexicon. Computational Intelligence 29 (3):436–465.



