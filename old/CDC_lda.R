

CDC.dtm <- CreateDtm(
  doc_vec = tweets.CDC$text.filter,
  doc_names = tweets.CDC$tweet_id,
  ngram_window = c(1,2)
)

# 
# iterate_LDA_k(CDC.dtm, n_k = 20)
# 

CDC.LDA <-
  FitLdaModel(CDC.dtm,
              k = 8,
              iterations = 200,
              burnin = 175)



# model_list <- readRDS("models/model_list.rds")

# cmat <- find_top_coherence(modelList = model_list)
# cmat.gg <- cmat %>% 
#   ggplot(
#     aes(x = k, y = coherence)
#   ) + 
#   geom_point() +
#   geom_line(group = 1)


CDC.summary <- SummarizeTopics(CDC.LDA)
