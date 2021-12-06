# Group Reply tweets by CDC conversation ID




theta.conversation_id <- tweets.theta.poly %>% 
  dplyr::select(
    conversation_id, 
    # t_1:t_11, 
    max.topic, timestamp
  ) %>% 
  group_by(conversation_id) %>% 
  count(max.topic) %>% 
  pivot_wider(
    names_from = max.topic,
    values_from = n
  ) %>% 
  mutate_all(~replace(., is.na(.),0))



CDC.conversation_id <- tweets.CDC %>% 
  dplyr::select(
    conversation_id, timestamp, topic
  ) %>% 
  dplyr::distinct(conversation_id, .keep_all = T)

CDC.conversation_id <- CDC.conversation_id %>% 
  left_join(theta.conversation_id, by = c("conversation_id" = "conversation_id"))

CDC.cnv.wide <- CDC.conversation_id %>% 
  na.omit() %>% 
  dplyr::rename(
    topic_CDC = topic
  )


CDC.cnv.long <- CDC.cnv.wide %>% 
  pivot_longer(
    cols = !c(conversation_id, timestamp, topic_CDC)
  ) %>% 
  left_join((LDA.summary[,1:2]), by = c("topic_CDC" = "topic")) %>% 
  left_join((LDA.summary[,1:2]), by = c("name" = "topic")) %>% 
  dplyr::rename(
    topic_Reply = name,
    label_CDC = label_1.x,
    label_Reply = label_1.y
  )



CDC.cnv.wide.2 <- CDC.cnv.long %>% 
  dplyr::select(
    conversation_id, label_CDC, label_Reply, value
  ) %>% 
  pivot_wider(
    names_from = label_Reply,
    values_from = value
  ) %>% na.omit()

CDC.cnv.topics <- CDC.cnv.wide %>% 
  dplyr::select(
    -conversation_id, -timestamp
  ) %>% 
  group_by(topic_CDC) %>% 
  summarize(across(everything(),
                   mean))


topic.assoc <- CDC.cnv.wide.2 %>%
  dplyr::select(-conversation_id) %>%
  group_by(label_CDC) %>%
  summarize(across(everything(),
                   sum)) %>%
  pivot_longer(cols = !label_CDC,
               names_to = "label_Reply",
               values_to = "n")


# Assigning more intuitive names to topics

topic.names <- tibble(
  labels = c("american_people", "big_pharma", "covid_ー", "covid_vaccine",
            "natural_immunity", "public_health", "stay_home", "tested_positive",
            "united_states", "wash_hands", "wear_mask"),
  names = c("Government Skepticism", "Big Pharma", "COVID-19 Outbreaks", "Vaccine Side-Effects",
            "Vaccine Skepticism", "Public Health", "Quarantine, Self-Isolation", "COVID-19 Testing, Symptoms",
            "U.S. Cases & Deaths", "Handwashing, Sanitation", "Masks & Mask Efficacy")
)

topic.names.c <- setNames(as.character(topic.names$names), topic.names$labels)


topic.assoc.named <- topic.assoc %>% 
  dplyr::mutate(
    label_CDC = label_CDC %>% as.character(),
    CDC_Topic = recode(label_CDC,
                  !!!topic.names.c) %>% 
      factor(ordered = F),
    Reply_Topic = recode(label_Reply,
                         !!!topic.names.c) %>% 
      factor(ordered = F)
  ) %>% arrange(CDC_Topic, Reply_Topic)

CDC.assoc.table <- topic.assoc.named %>%
  dplyr::select(CDC_Topic, Reply_Topic, n) %>%
  pivot_wider(names_from = Reply_Topic, values_from = n) %>% 
  mutate(
    CDC_Topic = paste0("CDC_",CDC_Topic)
  ) %>% 
  column_to_rownames(var = "CDC_Topic") %>% 
  prop.table(margin = 2)



CDC.cnv.long.named <- CDC.cnv.long %>%
  mutate(
    label_CDC = recode(label_CDC, !!!topic.names.c),
    label_Reply = recode(label_Reply, !!!topic.names.c)
  )

CDC.variance <- conversation_long %>%
  dplyr::select(label_CDC, label_Reply,
                value) %>% 
  group_by(label_CDC, label_Reply) %>% 
  summarize(.groups = "keep",
    n = n(),
    mean = mean(value),
    SEM = (sd(value)/sqrt(length(value))),
    CI.95 = ((1.96 * sd(value))/sqrt(length(value)))
  
  )

aov()
  

var.gg <- ggplot(
  data = CDC.variance %>% dplyr::filter(n >= 30),
  aes(
    x = mean,
    y = label_Reply,
    group = label_CDC,
    color = label_Reply,
    xmin = (mean - CI.95),
    xmax = (mean + CI.95)
  )
) +
  geom_errorbarh(height = .375, size = .75) +
  geom_point(size = 2) +
  geom_vline(
    xintercept = 0,
    color = "black",
    linetype = "dashed",
    alpha = .5
  ) +
  facet_wrap( ~ label_CDC) +
  scale_y_discrete(breaks = CDC.variance$label_Reply,
                   labels = CDC.variance$label_Reply) +
  scale_color_manual(values = grad.rainbow) +
  labs(
    title = "Distribution of Reply Topics per CDC Topic",
    color = "Topic",
    caption = "CDC Topics with n < 30 were omitted.",
    x = "Mean Count (95% Confidence Interval)",
    y = "Reply Topic"
  ) +
  theme_minimal(base_size = 12)

var.gg

# library(Cairo)
# CairoWin()

ggsave(
  var.gg,
  filename = "ForestPlotCairoCI.png",
  dpi = 300,
  type = "cairo",
  bg = "white",
  width = 14,
  height = 8.5,
  units = "in"
)







CDC.Reply.gg <- topic.assoc.named %>%
  ggplot(aes(
    x = Reply_Topic,
    y = n,
    fill = Reply_Topic,
    group = CDC_Topic,
    text = paste(
      "Reply Topic: ",
      Reply_Topic,
      "\n",
      "n: ",
      n,
      "\n",
      "CDC Topic:",
      CDC_Topic
    )
  )) +
  geom_bar(position = "stack",
           stat = "identity") +
  labs(
    x = "Groups represent CDC Topics",
    y = "Count",
    fill = "Topic",
    title = "Association between CDC-topics and Reply-topics",
    subtitle = "Faceted by CDC Topic"
  ) +
  scale_color_manual(values = grad.rainbow[-3], aesthetics = "fill") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5)
  ) +
  facet_wrap(~ CDC_Topic, as.table = T)





CDC.Reply.plotly <- ggplotly(CDC.Reply.gg,
         tooltip = "text")
CDC.Reply.plotly

htmlwidgets::saveWidget(CDC.Reply.plotly, "C:\\Users\\Harrison Brown\\Documents\\GitHub\\brownhr.github.io\\index.html")
