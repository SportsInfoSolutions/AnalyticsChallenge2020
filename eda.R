library(tidyverse)
library(funModeling)
library(ggridges)
data <- read_csv(url('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv'))




# Designed Run Gap Analysis --------------------------------------------------------


data %>% filter(EventType == "rush") %>%
  ggplot(aes(x = EPA, color = UsedDesignedGap)) +
  geom_density()


gap_lm <- lm(data = data %>% filter(EventType == "rush"), formula = EPA ~ UsedDesignedGap)
summary(gap_lm)


# General Run Gap Analysis ------------------------------------------------

data %>% filter(EventType == "rush") %>%
  ggplot(aes(x = EPA, y = RunDirection, fill = UsedDesignedGap)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = c(0.5), alpha = 0.5)


# Number of Rushers Listed vs EPA density ----------------------------------

data %>%
  filter(EventType == "rush" | EventType == "pass") %>%
  group_by(GameID, EventID, EventType) %>%
  summarize(qb_rushers = as.factor(sum(IsRushing)),
            epa = head(EPA, 1)) %>%
  ggplot(aes(x = epa, y = qb_rushers, fill = EventType)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = c(0.5), alpha = 0.5)


# Broken Plays ----------------------------------------------------------------

data %>%
  filter(str_detect(PlayDesc, "broken play"))

data %>%
  filter(RunDirection == "Middle") %>%
  select(PlayDesc) %>%
  filter(str_detect(PlayDesc, "left") | str_detect(PlayDesc, "right")) %>%
  unique()

# Plays with at least one player in X technique

data %>%
  select(GameID, EventID, TechniqueName, EventType) %>%
  pivot_wider(names_from = TechniqueName, values_from = TechniqueName, names_prefix = "Tech_",
              values_fn = list(TechniqueName = length),
              values_fill = list(TechniqueName = 0)) %>%
  pivot_longer(starts_with("Tech_"), names_to = "TechniqueName", values_to = "Used") %>%
  mutate(Used = ifelse(Used > 0, 1, 0)) %>%
  ggplot(aes(x = TechniqueName, y = Used, fill = EventType)) +
  geom_bar(stat = "identity",
           #position = "fill"
           ) +
  theme_bw()



