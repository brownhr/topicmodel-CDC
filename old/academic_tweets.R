# setup academictwitteR auth:

library(academictwitteR)

# set_bearer()

# Collect all tweets by CDCgov during time period
# date.start <- "2019-01-01T00:00:00Z"
# date.end <- "2021-09-21T00:00:00Z"
# 
# get_all_tweets(
#   users = c("CDCgov"),
#   start_tweets = date.start,
#   end_tweets = date.end,
#   n = Inf,
#   data_path = "CDCtweets/",
#   bind_tweets = F
# )

cdc.filter <- function() {
  # Use academictwitteR to collect tweets into a df
  tweets.CDC <- bind_tweets(data_path = "CDCtweets/",
                            output_format = "tidy")
  tweets.CDC.filter <- tweets.CDC %>%
    # Twitter uses a weird timestamp; convert it into POSIX-CT
    mutate(timestamp = strptime(x = created_at,
                                format = twformat,
                                tz = "gmt"))
  # dplyr::select(tweet_id, conversation_id, text, timestamp) %>%
  
  
  tweets.CDC.filter %>% saveRDS(file = "tweets.CDC.filter")
}
cdc.filter()
tweets.CDC.filter <- readRDS("tweets.CDC.filter")



tweets.CDC.noRT <- tweets.CDC.filter %>%
  mutate(sourcetweet_type = replace_na(sourcetweet_type, "standard")) %>%
  dplyr::select(
    tweet_id,
    conversation_id,
    retweet_count,
    like_count,
    quote_count,
    sourcetweet_type,
    timestamp,
    text
  ) %>%
  dplyr::filter(sourcetweet_type != "retweeted")



noRT.gg <- tweets.CDC.noRT %>%
  ggplot(aes(x = quote_count %>% log10())) +
  geom_histogram()

tweets.CDC.noRT %>%
  dplyr::arrange(retweet_count %>% desc()) %>%
  head(20)

noRT.gg
# mutate(
#   text.filter = str_remove_all(text, pattern),
#   text.filter = tm::removePunctuation(text.filter),
#   text.filter = str_remove_all(text.filter, t.stopwords),
#   text.filter = str_squish(text.filter)
# ) %>% dplyr::filter(text.filter != "") %>%
#   dplyr::select(-text)



# Load topic model 
{
  # Create Document-Term Matrix from CDC Tweets
  
  CDC.dtm <- CreateDtm(
    tweets.CDC.filter$text.filter,
    doc_names = tweets.CDC.filter$tweet_id,
    ngram_window = c(1, 2),
    stem_lemma_function = function(x)
      SnowballC::wordStem(x, "porter")
  )
  reply.theta <- readRDS("LDA.lda")
  
  # Predict topics in CDC data based on LDA model
  
  CDC.theta <- predict(
    object = reply.theta,
    newdata = CDC.dtm,
    method = "gibbs",
    iterations = 200,
    burnin = 150
  )
  # Convert rowname (tweet_id) to discrete column
  CDC.topics <- CDC.theta %>%
    data.frame() %>%
    tibble::rownames_to_column("tweet_id")
  saveRDS(CDC.topics, "CDC.topics.rds")
  
}
CDC.topics <- readRDS("CDC.topics.rds")


# Select maximum topic for each document
# LDA.theta$max.topic <- colnames(LDA.theta[,2:12])[max.col(LDA.theta[,2:12])]
# LDA.theta$max.theta <- apply(LDA.theta[,2:12], 1, max)
CDC.topic <- function() {
  # Find maximum Theta across columns
  CDC.topics$topic <-
    colnames(CDC.topics[, 2:12])[max.col(CDC.topics[, 2:12])]
  CDC.topics <- CDC.topics %>%
    dplyr::select(tweet_id, topic)
  # Join topic data to CDC tweets
  
  tweets.CDC <- tweets.CDC.filter %>%
    left_join(CDC.topics)
  
  saveRDS(tweets.CDC, "tweets.CDC.rds")
}
CDC.topic()
tweets.CDC <- readRDS("tweets.CDC.rds")

tweets.CDC.weekly <- tweets.CDC %>% 
  mutate(
    week = week(timestamp)
  ) %>% 
  group_by(week, topic) %>% 
  tally() %>% 
  pivot_wider(id_cols = week,
              names_from = topic,
              values_from = n) %>% 
  mutate_all(~replace(., is.na(.),0)) %>% 
  pivot_longer(!week, names_to = "topic", values_to = "n") %>% 
  mutate(
    week.data = week %>% as.Date()
  )


tweets.theta.poly <- tweets.theta.poly %>% 
  left_join((LDA.summary[,1:2]), by = c("max.topic" = "topic"))

theta.poly <- tweets.theta.poly %>% 
  dplyr::select(
    tweet_id, c(t_1:t_11)
  ) %>% 
  pivot_longer(
    cols = !tweet_id,
    names_to = "factor",
    values_to = "theta"
  ) %>% 
  left_join((LDA.summary[,1:2]), by = c("factor" = "topic"))



theta.poly %>%
  ggplot(aes(x = theta %>% log10, color = label_1)) +
  geom_density(show.legend = F) +
  facet_wrap( ~ label_1) +
  labs(title = "Per-topic Density Distribution",
       x = "Log10(theta)",
       y = "Density") +
  theme(
    text = element_text(family = "serif"),
    legend.position = 'none',
    plot.title = element_text(hjust = .5)
  )


