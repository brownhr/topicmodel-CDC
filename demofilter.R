library(readr)
library(dplyr)

CDC.filter <- readRDS("tweets.CDC.filter") %>% 
  select(
    tweet_id, text, conversation_id
  )

Reply_filter <- read_csv(
  "tweets_filter.csv",
  col_types = cols(tweet_id = col_character(),
                   conversation_id = col_character())
) %>% 
  select(
    tweet_id, text, conversation_id
  )

conv_join <- left_join(CDC.filter, Reply_filter, 
                  by = "conversation_id", 
                  suffix = c("_CDC", "_Reply")) %>%
  na.omit()

conv.reply <- conv_join %>% 
  group_by(conversation_id) %>% 
  count()


conv.reply.10 <- conv.reply %>% 
  filter(n == 10)


example <- conv_join %>%
  filter(conversation_id == "1420104200957038594") %>% 
  select(conversation_id, text_CDC, text_Reply)


example[1, "text_CDC"] %>% substr(1, 200) %>% paste0("...") %>% clipr::write_clip()
example %>%  slice_sample(n = 10) %>% pull(text_Reply) %>% substr(1, 85) %>% paste0("...") %>% clipr::write_clip()


ggplot(conv.reply, aes(x = n)) +
  geom_histogram() +
  scale_x_log10()
