tweets <- read_csv("tweets_filter.csv", col_types = cols(.default = 'c'))
# emoji <- read_csv(file = "Emoji_Sentiment_Data_v1.0.csv")
# topic.LDA <- read_csv("lda_theta.csv", col_types = cols(.default = 'd', tweet_id = "c"))
# rm(topic.LDA)
# topic.maxTheta <- read_csv(file = 'topic_max_theta.csv', col_types = cols(.default = 'd', tweet_id= 'c',
#                                                                           max.topic = 'c'))
test.LDA <- readRDS("LDA.lda")
# test.dtm <- readRDS("tweets.dtm")


LDA.theta <- read_csv(file = "LDA_max_theta.csv", col_types = cols(.default = 'd',
                                                                   tweet_id = 'c',
                                                                   max.topic = 'c'))



keys.c <- setNames(as.character(keys$X2), keys$X1)


tweets.filter <- tweets %>% 
  mutate(
    timestamp = strptime(x = created_at,
                         format = twformat,
                         tz = "gmt")
  )
rm(tweets)
tweets.filter <- tweets.filter %>% 
  dplyr::select(
    tweet_id, conversation_id, timestamp, text.filter
  ) %>% 
  dplyr::rename(
    text = text.filter
  )

tweets.filter <- tweets.filter %>% 
  mutate(
    week = floor_date(timestamp, unit = "weeks") %>% as.character()
  )


LDA.summary <- test.LDA %>% textmineR::SummarizeTopics()

LDA.theta <- LDA$theta
LDA.theta <- LDA.theta %>% data.frame() %>% rownames_to_column("tweet_id")
LDA.theta$max.topic <- colnames(LDA.theta[,2:12])[max.col(LDA.theta[,2:12])]
LDA.theta$max.theta <- apply(LDA.theta[,2:12], 1, max)
LDA.theta %>%
  # Unused; more insightful to show distribution of topics, rather than just max?
  # select(
  #   tweet_id, max.topic, max.theta
  # ) %>%
  write_csv(file = 'LDA_max_theta.csv')


tweets.theta.poly <- left_join(tweets.filter, LDA.theta)








# Add column with label_1 field from LDA.summary


rm(tweets.filter)
rm(LDA.theta)

theta.count <- tweets.theta %>% 
  select(week, max.topic) %>% 
  group_by(week, max.topic) %>% 
  tally() 


theta.count <- left_join(theta.count, LDA.summary[,1:2], by = c('max.topic' = 'topic'))







density <- theta.count %>% 
  ggplot(
    aes(x = log10(n + 1), fill = label_1)
  ) +
  geom_histogram(show.legend = F) + 
  facet_wrap(~ label_1) + 
  theme(plot.title = element_text(hjust = .5),
        text = element_text(face = "bold"),
        legend.position = "none") + 
  scale_fill_manual(values = grad.2) +
  labs(
    x = "Weekly Occurances (Log10)",
    y = "Count",
    title = "Density Distribution of Topics per Week",
    fill = "Topic"
  ) 

density %>% 
  ggplotly()

proportion.gg <- theta.count %>% 
  dplyr::filter(
    week.data >= "2020-01-01" %>% as.Date()
  ) %>% 
  ggplot(aes(fill = label_1, y = n, x = week %>% as.Date()))+
  geom_bar(position = "fill", stat = "identity") + 
  labs(
    x = "Week",
    y = "Proportion",
    title = "Proportion Distribution of Topics") + 
  scale_fill_manual(values = grad.2)
proportion.gg %>% ggplotly(dynamicTicks = T)

devtools::install_github("hrbrmstr/streamgraph")

twitter.gradient <- c("#f0f07a", "#9CF58A", "#00EAB8", "#00D7E5", "#00BFFE", "#1DA1FA", "#648CE5", "#8178CA", "#9064AC", "#94538D", "#90446E")

grad.2 <- c("#9cf58a", "#3feeb4", "#00e3dc", "#00d3f7", "#00bffe", "#34adf8", "#549bed", "#6d87de", "#8574c4", "#9162a8", "#94528a", "#90446e")

grad.rainbow <- c("#ff6f91", "#ff8273", "#ff9d5a", "#f2ba4e", "#d4d558", "#a1dd73", "#69e099", "#00e0c1", "#00c9e1", "#00adf4", "#0489eb", "#845ec2")

theta.count <- theta.count %>% 
  pivot_wider(id_cols = week,
              names_from = label_1,
              values_from = n) %>% 
  mutate_all(~replace(., is.na(.),0)) %>% 
  pivot_longer(!week, names_to = "label_1", values_to = "n") %>% 
  mutate(
    week.data = week %>% as.Date()
  )

stream.theta <- theta.count %>% 
  dplyr::filter()
  streamgraph::streamgraph(key = 'label_1',
                           value = 'n',
                           date = 'week',
                           interpolate = "step",width = 1200) %>% 
  sg_axis_x(1, 'month', "%b-%y") %>% 
  sg_legend(show = T, label = "Topic: ")

stream.theta

theta.topterms <- GetTopTerms(test.LDA$phi, M = 20)

LDA.summary <- SummarizeTopics(test.LDA)

theta.topterms <- theta.topterms %>% magrittr::set_colnames(LDA.summary$label_1)
  

