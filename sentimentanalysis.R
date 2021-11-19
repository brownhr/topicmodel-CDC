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






t.stopwords <- c("amp", "$", "RT")

t.tokens <- t.corpus %>%
  tokens(remove_punct = T,
         remove_url = T) %>%
  tokens_select(pattern = stopwords('en'),
                selection = "remove") %>%
  tokens_select(pattern = t.stopwords,
                selection = "remove")





topic.test <- topic.join %>% 
  mutate(
    sentiment = word %>% t.getsent(),
    diff = (value - sentiment) %>% abs()
  )


tweets.filter <- tweets.filter %>% 
  mutate(
    sentiment = text %>% t.getsent()
  )

library(textstem)


tweets.test <- tweets.test %>% 
  mutate(
    sentiment = text %>% t.getsent()
  )

t.getsent <- function(char_v, regex = regex.emoji, lexicon = afinn.emoji){
  word_l = strsplit(char_v, regex, perl = T) %>% lapply(stem_words())
  result = unlist(lapply(word_l, t.sentvals,  
                          lexicon))
  # return(result)
  }
t.sentvals <- function(char_v, regex, lexicon = afinn.emoji){
  data = dplyr::filter(lexicon, .data$word %in% char_v) 
  result = sum(data$value)
  # return(result)
  }


tweets.test %>% 
  # mutate(
  #   month = floor_date(timestamp, unit = "months") %>% as.character()
  # ) %>% 
  dplyr::filter(week %>% as.Date() >= "2020-01-01" %>% as.Date()) %>% 
  group_by(week) %>%
  summarize(
    sent = sentiment %>% mean()
  ) %>%
  ggplot(
    aes(x = week %>% as.Date(), y = sent)
  ) +
  scale_x_date(date_labels = "%b-%d-%Y") +
  geom_point() +
  geom_smooth(method = lm)

tweets.filter %>% 
  ggplot(aes(x = sentiment)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(aes(xintercept = mean(sentiment)), color = 'blue')+ 
  geom_vline(aes(xintercept = median(sentiment)), color = 'red')

library(ggpubr)

tweets.filter %>% 
  select(text, sentiment) %>% 
  write_csv(file = 'filter.csv')


test.dtm <- CreateDtm(tweets.filter$text,
          doc_names = tweets.filter$tweet_id,
          ngram_window = c(1,2))


# test.LDA$theta represents significance of each topic per document

test.LDA <- FitLdaModel(dtm = test.dtm,
                        k = 11,
                        iterations = 200,
                        burnin = 150)



# test.tdf <- TermDocFreq(dtm = test.dtm)
# test.tdf.o <- test.tdf %>% select(term, term_freq, doc_freq)
# 
# sample.dfm <- 
# 

k_list <- seq(1,12,by = 1)
model_dir <- paste0("models_", digest::digest(head(sample.tweets$text), algo = 'sha1'))
if (!dir.exists(model_dir)) dir.create(model_dir)


model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, '_topics.rda'))
  
  if (!file.exists(filename)){
    m <- FitLdaModel(dtm = test.dtm,
                     k = k,
                     iterations = 250)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = test.dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export = c("test.dtm", "model_dir"))

coherence_mat <- data.frame(k = sapply(model_list, function(x)
  nrow(x$phi)),
    coherence = sapply(model_list,
                       function(x) mean(x$coherence)),
    stringsAsFactors = F)

coherence_mat %>% write_csv(file = 'coherence_mat.csv')

coherence_mat %>% 
  ggplot(
    aes(x = k, y = coherence)
  ) + 
  geom_point() +
  geom_line(group = 1)

# With k = 1:30, the highest coherence is at k = 24


model.6 <- model_list[which(coherence_mat$k == 6)][[1]]
model.6$top_terms <- GetTopTerms(phi = model$phi, M = 50)
model.6.top50_wide <- as.data.frame(model.6$top_terms)

model.6.nrc <- model.6.top50_wide %>% 
  gather()


top50.gather <- top30_wide %>% 
  gather()

top50.gather <- top50.gather %>% 
  dplyr::rename(
    word = value
  ) %>% 
  mutate(
    sentiment = word %>% get_sentiment(method = "afinn")
  )
top50.nrc <- top50.gather %>% 
  mutate(
    nrc = get_nrc_sentiment(word)
  )

# Remove more 'twitter' stopwords: rt, amp, etc.


model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[,1])
model$hclust %>% 
  ggdendrogram() + 
  coord_flip()



topic.melt %>%
  distinct(word, .keep_all = T) %>%
  ggplot(aes(x = value, fill = key)) +
  geom_histogram(binwidth = 1,
                 center = 0,
                 color = "black") +
  geom_text(data = topic.means,
            aes(
              x = 0,
              label = paste0('mean = ', mean),
              y = 80
            ),
            nudge_x = 2.5) +
  geom_text(data = topic.means,
            aes(
              x = 0,
              label = paste0('var = ', var),
              y = 70
            ),
            nudge_x = 2.5) +
  # geom_vline(aes(xintercept = mean(value)))+
  facet_wrap( ~ key)
