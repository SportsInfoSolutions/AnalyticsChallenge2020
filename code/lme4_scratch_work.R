# Try playing around with lme4 package
install.packages("lme4")
library(lme4)

sis_df <- read.csv('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)

# Added a couple of the features that Zach had mentioned

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

# EPA is a character variable for some reason so let's change that
sis_df$EPA <- as.numeric(sis_df$EPA)

# Just kinda messing around with some simple models
sis_model <- lmer(formula = EPA ~ 1 + total_pressure + ListedDefenders + OnFieldPosition +
                    (1|OnFieldPosition),
                  data = sis_df)

summary(sis_model)

# GAM to see how much each player contributes to EPA
library(mgcv)



sis_df <- sis_df %>%
  group_by(GameID, EventID) %>%
  mutate(ForcedFumbleOnPlay = max(ForcedFumble),
         RecoveredFumbleOnPlay = max(RecoveredFumble),
         Sackers = sum(AssistedSack) + sum(SoloSack),
         Sack = ifelse(Sackers == 0, 0, SoloSack + (AssistedSack / Sackers)))

sis_passes <- sis_df %>%
  filter(Attempt != "NULL")

epa_impact_gam <- gam(EPA ~ Pressure + Sack + PassBreakup + Interception + ForcedFumble +
                      RecoveredFumble,
                      data = sis_passes)

summary(epa_impact_gam)


sis_passes <- sis_passes %>%
  modelr::add_predictions(epa_impact_gam, var = "IndividualEPA")

# Do some playing around with individual EPA on passing plays
sis_passes_model <- lmer(formula = EPA ~ 1 + total_pressure + ListedDefenders + OnFieldPosition +
                    (1|OnFieldPosition),
                  data = sis_passes)

summary(sis_passes_model)

