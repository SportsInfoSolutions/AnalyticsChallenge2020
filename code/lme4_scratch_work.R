# Try playing around with lme4 package
install.packages("lme4")
library(lme4)

sis_df <- read.csv('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)

# See how many players get pressure on one play
sis_df %>%
  group_by(GameID, EventID) %>%
  summarize(total_pressure = sum(Pressure)) %>%
  arrange(desc(total_pressure)) -> pressures

sis_df <- sis_df %>%
  inner_join(pressures)

# Add Number of listed defenders
sis_df <- sis_df %>%
  group_by(GameID, EventID) %>%
  mutate(ListedDefenders = n())
