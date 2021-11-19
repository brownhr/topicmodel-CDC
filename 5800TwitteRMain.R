libraryInit()


# Twitter Credential Keys
appName <- "Brownhr_SentimentAnalysis"
apiKey <- "Fk3RIaNF3aVCVMtSPb9Ezk3Xn"
apiSecret <- "sLkQxCVzMKCIXiBDvYA5FOzm3WZwT7eboF2su6njgiHJrgHHpY"
bearerToken <- "AAAAAAAAAAAAAAAAAAAAAKC0TAEAAAAAHFMVALsI%2F6v6Rgf8Rm2yz0Mql0g%3DeXrgPAxjvRe2Z8rYGyFSKr6agWmw5G6yztyCic64cncwVwLmFe"
accessToken <- "882435996142403585-rJegtxDN4gI07DTYnitOuODWZLviLeN"
accessTokenSecret <- "jOk51v6YKMS0cbujFzXW4RGvXGWWA5ptF5TFCowKeA2aX"

# Twitter Authentication Setup
credentialInit <- function(){
  setup_twitter_oauth(consumer_key = apiKey,
                      consumer_secret = apiSecret,
                      access_token = accessToken,
                      access_secret = accessTokenSecret)
  token <- list(bearer = bearerToken)

  saveRDS(token, "credTokenRDS.tcn_token")

  readRDS("credTokenRDS.tcn_token")
}
credentialInit()

tcntoken <- tcn_token(bearer = bearerToken
                      # ,
                      # consumer_key = apiKey,
                      # consumer_secret = apiSecret
                      )

# URL for CDC status
CDCstatus <- "https://twitter.com/CDCgov/status/"

# CDCSearchResults <- searchTwitter(
#   searchString = "from:CDCgov",
#   resultType = "recent",
#   since = "2018-01-01",
#   retryOnRateLimit = 120,
#   n = 10000
# )


CDCthreads <- list()

for (i in 1:length(CDCsearch)){
  CDCthreads[[i]] <- c(CDCsearch[[i]]$id)
}

writeCDCresults <- function(thread, tid){
  write.csv(x = thread, file = paste0("data/replies/",tid,".csv"))
  
}

getReplies <- function(n){
  for (i in 1:n){
    # threadURL <- paste0(CDCstatus,CDCthreads[[i]])
    
    thread <- tcn_threads(tweet_ids = CDCmasterlist$data__conversation_id[[i]],
                          token = token,
                          skip_list = CDCskip)
    writeCDCresults(thread = ,
                    tid = CDCmasterlist$data__conversation_id[[i]])
    
  }
  
  
}

# thread_all <- tcn_threads(tweet_ids = CDCthreads,
#             token = token,
#             skip_list = CDCskip)

# write.csv(thread_all$tweets, file = "data/all.csv")

CDCmasterlist <- list()
for (i in 1:length(files)){
  results <- fromJSON(file = files[i])$data
  CDCmasterlist <- append(CDCmasterlist, results)
  
  
}



twitterstrptime <- function(x){
  twformat <- c("%Y-%m-%dT%H:%M:%S.000Z")
  tcndates <- (x %>%
                 strptime(
                   format = twformat,
                   tz = "GMT"
                 ) %>%
                 as.POSIXct(
                   tz = "GMT"
                 ))
   tcndates
}


tweets.sample.sent <- tweets.content %>%
  sample_n(size = 1000)

tweets.sample.sent %>% 
  mutate(
    sentiment = get_sentiment(.$filter)
  )

ggplot(tweets.sample.sent, aes(x = sentiment))+
  geom_histogram()

tweets.All <- read.csv("data/all.csv") %>% data.frame()
tweets.content <- tweets.All[,c("created_at","conversation_id","author_id","text","public_metrics.retweet_count","public_metrics.reply_count","public_metrics.like_count","public_metrics.quote_count")]
tweets.text <- tweets.content[,"text"] %>% data.frame()

pattern <- r"(((\@\w*\s)|(\<.*\>)|(https:\/\/.*\w)))"
tweets.content$filter <- str_remove_all(tweets.content$text, pattern)

tweets.content <- tweets.content[!(is.na(tweets.content$filter) | tweets.content$filter==""),]

# academictwitteR::get_all_tweets(query = "to:CDCgov",
#                                 data_path = "data/academic/",
#                                 bind_tweets = F,
#                                 start_tweets = "2020-06-01T00:00:00Z",
#                                 end_tweets = "2020-12-31T00:00:00Z",
#                                 n = 1000)

# update_collection(data_path = "data/academic/full/",
#                   end_tweets = "2021-09-22T00:00:00Z",
#                   n = 100000)

