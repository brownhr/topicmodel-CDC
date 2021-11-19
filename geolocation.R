tweets.user <- read_csv("tweets_full.csv", col_types = cols(.default = "c")) %>% 
  dplyr::select(
    user_username, author_id, user_location
  )
library(twitteR)
keys <- read_csv("keys.csv", col_names = F)

setup_twitter_oauth(consumer_key = keys[1,2],
                    consumer_secret = keys[2,2],
                    access_token = keys[3,2],
                    access_secret = keys[4,2])



tweets.user <- tweets.user %>% 
  dplyr::distinct() %>% 
  dplyr::filter(!is.na(user_location))


getLocation <- function(x){
  user <- getUser(x)
  location <- user$location
  return(location)
}

tweets.loc <- tweets.user %>% 
  head() %>% 
  mutate(
    location = getLocation(user_username)
  )

  