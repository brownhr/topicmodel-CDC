tweets <- read_csv("tweets_filter.csv", col_types = cols(.default = 'c'))
# tweets.author <- read_csv("tweets_author.csv", col_types = cols(.default = 'c'))
# topic.terms <- read_csv(file = "topic_terms.csv")
# emoji <- read_csv(file = "Emoji_Sentiment_Data_v1.0.csv")

twformat <- c("%Y-%m-%dT%H:%M:%S.000Z")

tweets.filter <- tweets %>% 
  mutate(
    timestamp = strptime(x = created_at,
                         format = twformat,
                         tz = "gmt")
  )


rm(tweets)
tweets.filter <- tweets.filter[,c("tweet_id","timestamp","text.filter","conversation_id")] %>% 
  rename(
    text = text.filter
  )

tweets.filter <- tweets.filter %>% 
  mutate(
    week = floor_date(timestamp, unit = "weeks") %>% as.character()
  )

tweets.text <- tweets.filter %>% 
  select(tweet_id, text)

set.seed(2021)

tweets.sample <- tweets.text %>% 
  slice_sample(n = 2500)

# sample.dtm <- textmineR::CreateDtm(
#   doc_vec = tweets.sample$text,
#   doc_names = tweets.sample$tweet_id,
#   ngram_window = c(1,2),
#   stem_lemma_function = function(x) SnowballC::wordStem(x, "porter")
# )






# k_list <- seq(1,20,by = 1)
# model_dir <- paste0("models/models_", digest::digest(head(tweets.sample$text), algo = 'sha1'))
# if (!dir.exists(model_dir)) dir.create(model_dir)
# 
# 
# model_list <- TmParallelApply(X = k_list, FUN = function(k){
#   filename = file.path(model_dir, paste0(k, '_topics.rda'))
#   
#   if (!file.exists(filename)){
#     m <- FitLdaModel(dtm = sample.dtm,
#                      beta = .001,
#                      k = k,
#                      iterations = 200,
#                      burnin = 150)
#     m$k <- k
#     m$coherence <- CalcProbCoherence(phi = m$phi, dtm = sample.dtm, M = 15)
#     save(m, file = filename)
#   } else {
#     load(filename)
#   }
#   
#   m
# }, export = c("sample.dtm", "model_dir"))
# 
# coherence_mat <- data.frame(k = sapply(model_list, function(x)
#   nrow(x$phi)),
#   coherence = sapply(model_list,
#                      function(x) mean(x$coherence)),
#   stringsAsFactors = F)
# 
# coherence_mat %>% write_csv(file = 'coherence_mat.csv')
# 
# coherence_mat %>%
#   ggplot(aes(x = k, y = coherence)) +
#   geom_point() +
#   geom_line(group = 1)
