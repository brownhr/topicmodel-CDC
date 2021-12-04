library(dplyr)
library(textmineR)
library(readr)

reply_tweets <- read_csv(
  "tweets_filter.csv",
  col_types = cols(tweet_id = col_character(),
                   conversation_id = col_character())) %>% 
  filter(lang == "en") %>% 
  select(tweet_id, conversation_id, text)


twitter.stopwords <- c("amp", "rt", "co", "t", "cdcgov")

reply_dtm <- CreateDtm(
  doc_vec = reply_tweets$text,
  doc_names = reply_tweets$tweet_id,
  ngram_window = c(1,3),
  stopword_vec = c(stopwords::stopwords(language = "en"),
                   stopwords::stopwords(source = "smart"),
                   twitter.stopwords),
  stem_lemma_function = function(x) SnowballC::wordStem(x, language = "porter")
)

reply_tweets.sample <- reply_tweets %>% 
  slice_sample(n = 20000)


reply_dtm.sample <- CreateDtm(
  doc_vec = reply_tweets.sample$text,
  doc_names = reply_tweets.sample$tweet_id,
  ngram_window = c(1,3),
  stopword_vec = c(stopwords::stopwords(language = "en"),
                   stopwords::stopwords(source = "smart"),
                   twitter.stopwords),
  stem_lemma_function = function(x) SnowballC::wordStem(x, language = "porter")
)


k.list <- seq(3,100,1)

LDA <- FitLdaModel(
  dtm = reply_dtm.sample,
  k = 3,
  iterations = 250,
  alpha = .05,
  optimize_alpha = T,
  beta = .1,
  calc_coherence = T
)

reply_lda.sample.list <- TmParallelApply(
  X = k.list,
  FUN = function(x) {
    LDA <- FitLdaModel(
      dtm = reply_dtm.sample,
      k = x,
      iterations = 500,
      alpha = .05,
      beta = .1,
      calc_coherence = T
    )
    coherence <- mean(LDA$coherence)
    readr::write_rds(coherence, paste0("model_dir/", k, "_coherence.RDS"))
  },
  export = c("reply_dtm.sample", "k.list"),
  cpus
)

