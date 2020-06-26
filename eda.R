library(tidyverse)
library(funModeling)
library(ggridges)
data <- read_csv("data/AnalyticsChallenge2020Data.csv")




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






