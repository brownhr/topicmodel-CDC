library(tm)
library(topicmodels)
library(knitr) 
library(kableExtra) 
library(DT)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(flextable)
library(stopwords)
library(parallel)

tweets <- read_csv("tweets_filter.csv", col_types = cols(.default = 'c'))

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
  select(tweet_id, text) %>% 
  rename(
    doc_id = tweet_id
  )


tweets.sample <- tweets.text %>% 
  slice_sample(n = 25000)

minFrequency <- 5

sample.corp <- Corpus(DataframeSource(
  data.frame(tweets.sample, stringsAsFactors = F)))
  
processedCorpus <- tm_map(sample.corp, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, stopwords())
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)


  
sample.DTM <- DocumentTermMatrix(
  x = processedCorpus,
  control = list(bounds = list(global = c(minFrequency, Inf)))
)

sel_idx <- slam::row_sums(sample.DTM) > 0

sample.DTM <- sample.DTM[sel_idx,]

set.seed(2021)

sample.LDA <- LDA(
  sample.DTM,
  k = 12,
  method = "Gibbs",
  control = list(
    verbose = 100,
    best = TRUE,
    nstart = 5,
    iter = 2000,
    burnin = 1000,
    thin = 500,
    seed = list(254672,109,122887,145629037,2)
  )
)




klist <- seq(3, 24, 1)
library(doSNOW)
max.coherence <- function(k) {
  LDA(
    x = sample.DTM,
    k = k,
    method = "Gibbs",
    control = list(
      verbose = 100,
      best = TRUE,
      nstart = 5,
      iter = 2000,
      burnin = 1000,
      thin = 500,
      seed = list(254672, 109, 122887, 145629037, 2)
    )
  )
}

# cl <- makeCluster(getOption("cl.cores", 12))
# clusterExport(cl, list("max.coherence", "LDA", "sample.DTM"))
# 
# clusterEvalQ(cl, library(Iso))
# 
# 
# lda.results <- parLapply(cl, klist, function(x)max.coherence(k = x))
# stopCluster(cl)


lda.results <- readRDS("LDA_Results.RDS")

theta.list <- lapply(lda.results, posterior)


library(doSNOW)

cl <- makeCluster(getOption("cl.cores", 12))

max.topic <- function(x){
  x$topics %>% 
    data.frame() %>% 
    rownames_to_column("tweet_id") %>% 
    rowwise() %>% 
    summarize(max = max(c_across(cols = -tweet_id))) %>%
    summarize(mean = mean(max))
}

clusterExport(cl, list("theta.list", "max.topic"))

max.theta <- parLapply(cl, theta.list, max.topic)
stopCluster(cl)

test.data <- slice_sample(tweets.text, n = 500)

test.corp <- Corpus(DataframeSource(
  data.frame(test.data, stringsAsFactors = F)))



test.corp.clean <-
  tm_map(test.corp, content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords()) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>%
  tm_map(removeNumbers) %>%
  tm_map(stemDocument, language = "en") %>%
  tm_map(stripWhitespace)



test.DTM <- DocumentTermMatrix(
  x = test.corp.clean,
  control = list(bounds = list(global = c(minFrequency, Inf)))
)

sel_idx <- slam::row_sums(test.DTM) > 0

test.DTM <- test.DTM[sel_idx,]


test.results <- posterior(test.lda, test.DTM)
