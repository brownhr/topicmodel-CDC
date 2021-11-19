tweets.CDC <- readRDS("tweets.CDC.rds")
LDA <- readRDS("LDA.lda")

tweets_full <- read_csv(
  "tweets_full.csv",
  col_types = cols(tweet_id = col_character(),
                   conversation_id = col_character())
)

tweets_filter <- tweets_full %>% 
  filter(lang == "en") %>% 
  select(
    tweet_id, conversation_id, text
  )

theta <- as.data.frame(LDA$theta) %>% rownames_to_column("tweet_id")

tweets.LDA <- inner_join(tweets_filter, theta)

LDA.summary <- SummarizeTopics(LDA)

topic.names <- tibble(
  labels = c("american_people", "big_pharma", "covid_ー", "covid_vaccine",
             "natural_immunity", "public_health", "stay_home", "tested_positive",
             "united_states", "wash_hands", "wear_mask"),
  names = c("Government-Skepticism", "Big-Pharma", "COVID-19-Outbreaks", "Vaccine-Side-Effects",
            "Vaccine-Skepticism", "Public-Health", "Distancing", "COVID-19-Testing",
            "U.S.-Cases-Deaths", "Sanitation", "Masks")
)
topic.names.c <-
  setNames(as.character(topic.names$names),
           topic.names$labels)


LDA.summary <- LDA.summary %>% 
  dplyr::mutate(
    Topic_Name = recode(label_1,
                       !!!topic.names.c) %>% 
      factor(ordered = F)
  ) %>% arrange(Topic_Name)

topic.description <- LDA.summary %>% 
  select(
    topic, Topic_Name
  )
tweets.LDA$max.topic <- colnames(tweets.LDA[,4:14])[max.col(tweets.LDA[,4:14])]
tweets.LDA$max.theta <- apply(tweets.LDA[,4:14], 1, max)


tweets.CDC.rds <- tweets.CDC.rds %>% 
  select(tweet_id, text.filter)


CDC.dtm <- CreateDtm(
  doc_vec = tweets.CDC.rds$text.filter,
  doc_names = tweets.CDC.rds$tweet_id,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords::stopwords("en"),
    stopwords::stopwords(source = "smart"),
    t.stopwords
  ),
  verbose = T
)

CDC.lda <- FitLdaModel(
  CDC.dtm,
  k = 6,
  iterations = 250
)
CDC.summary <- SummarizeTopics(CDC.lda)
LDA.summary <- SummarizeTopics(LDA)


LDA_Summary <- LDA.summary %>% 
  select(
    Topic_Name, prevalence, top_terms_phi, top_terms_gamma
  )


write_csv(LDA_Summary, "LDA_Summary.csv")
