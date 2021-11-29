library(ggplot2)
library(dplyr)
odds_longer <- oddsratio %>% 
  rename("Intercept" = "(Intercept)") %>% 
  tibble::rownames_to_column(var = "Reply Topic") %>% 
  tidyr::pivot_longer(cols = -c("Reply Topic"), names_to = "CDC Topic", values_to = "Odds")

odds_baseline <- odds_longer %>% 
  filter(
    `Reply Topic` == `CDC Topic`
  ) %>% 
  select(`CDC Topic`, Odds) %>% 
  rename(
    baseline_topic = `CDC Topic`,
    baseline_odds = Odds
  )

odds_calc <- odds_longer %>% 
  group_by(`CDC Topic`) %>%
  left_join(odds_baseline, by = c("CDC Topic" = "baseline_topic")) %>% 
  filter(
    (Odds > baseline_odds)
  )

odds_controversial <- odds_calc %>% 
  select(
    `CDC Topic`, `Reply Topic`, baseline_odds, Odds
  ) %>% 
  mutate(
    baseline_scale = baseline_odds / baseline_odds,
    odds_scale = Odds / baseline_odds
  )



ggplot(odds_longer, aes(y = `CDC Topic`, fill = `Reply Topic`, x = Odds)) +
  geom_col(show.legend = F) +
  facet_wrap( ~ `Reply Topic`)
