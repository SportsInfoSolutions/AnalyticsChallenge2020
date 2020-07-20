# Try playing around with lme4 package
install.packages("lme4")
library(lme4)
library(dplyr)

sis_df <- read.csv('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)

sis_df <- sis_df %>%
  filter()

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

# Add Run Gap and Side
sis_df <- sis_df %>%
  mutate(run_side = case_when(str_detect(RunDirection, "Left") ~ "L",
                              str_detect(RunDirection, "Right") ~ "R",
                              str_detect(RunDirection, "Middle") ~ "Middle"),
         run_gap = case_when(str_detect(RunDirection, "D Gap") ~ "D",
                             str_detect(RunDirection, "C Gap") ~ "C",
                             str_detect(RunDirection, "B Gap") ~ "B",
                             str_detect(RunDirection, "A Gap") ~ "A",
                             str_detect(RunDirection, "Middle") ~ "Middle"),
         defender_run_gap = case_when(TechniqueName == "0" ~ "Middle / A",
                                      TechniqueName == "1" | TechniqueName == "2i" ~ "A",
                                      TechniqueName == "2" ~ "A / B",
                                      TechniqueName == "3" | TechniqueName == "4i" ~ "B",
                                      TechniqueName == "4" ~ "B / C",
                                      TechniqueName == "5" | TechniqueName == "7" ~ "C",
                                      TechniqueName == "6" ~ "C / D",
                                      TechniqueName == "9" | TechniqueName == "Outside" ~ "D"),
         fill_run_gap = ifelse(str_detect(defender_run_gap, run_gap) & run_side == SideOfBall, 1, 0),
         pass_down = case_when(Down == 2 & ToGo >= 5 ~ 1,
                               (Down == 3 | Down == 4) & ToGo >= 2 ~ 1,
                               TRUE ~ 0))

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


# Anthony's code from create positions.R

basic_tech_ord <- c(1,'2i',2,3,'4i',4,5,7,6,9,'Outside')
tech_ord_LR <- c(paste0('L ',rev(basic_tech_ord)),'NULL 0',paste0('R ',basic_tech_ord))
names(tech_ord_LR) <- 1:length(tech_ord_LR)


tech_df <- sis_df %>% 
  filter(TechniqueName != 'Off Ball') %>% 
  mutate(TechSide = factor(paste0(SideOfBall, ' ', TechniqueName), tech_ord_LR)) %>% 
  select(GameID, EventID, PlayerId, Name, SideOfBall, TechniqueName, TechSide) %>% 
  group_by(GameID, EventID) %>%
  mutate(
    players_listed_LR = gsub('R ','',gsub('L ', '', gsub('NULL ', '', paste0(sort(TechSide), collapse = ' | ')))),
    players_listed_RL = gsub('R ','',gsub('L ', '', gsub('NULL ', '', paste0(rev(sort(TechSide)), collapse = ' | ')))),
    players_listed = ifelse(players_listed_LR > players_listed_RL, players_listed_LR, players_listed_RL),
    players_listed_LR = NULL,
    players_listed_RL = NULL,
    player_order_LR = order(TechSide),
    player_order_RL = order(rev(TechSide)),
    next_tech_to_R = sort(TechSide)[order(TechSide) + 1],
    next_tech_to_L = rev(sort(TechSide))[order(rev(TechSide)) + 1],
    on_ball_cnt = n()
  )

sis_df <- sis_df %>%
  left_join(tech_df)

# Add our defined positions which are as follows:
# EDGE is a stand-up player at the end of the line of scrimmage (Outside tech only)
# NOSE is a 0 or 1 with no one closer to him than a 3 on either side. This is typically 
# the middle player in 3-4 defense
# DT is a player at or between the 4 tech that does not meet the conditions 
# for an EDGE or NOSE
# IDL basically anyone who is left. 5 tech or further out, but not the player at the 
# end of the line either

sis_df <- sis_df %>%
  mutate(UpdatedPosition = case_when(TechniqueName == "Outside" ~ "EDGE",
                                     (TechniqueName == "1" | TechniqueName == "0") &
                                       as.character(next_tech_to_L) >= as.character("L 3") &
                                       as.character(next_tech_to_R) >= as.character("R 3") ~ "NOSE",
                                     TechniqueName == "4i" | TechniqueName <= "4" ~ "DT",
                                     TRUE ~ "IDL"))

# Check counts
sis_df %>%
  group_by(UpdatedPosition) %>%
  summarize(n = n())


# Visualize what positions players with 400+ snaps mostly play
sis_df %>%
  group_by(Name, DefensiveTeam) %>%
  summarize(EDGE = sum(UpdatedPosition == "EDGE"),
            NOSE = sum(UpdatedPosition == "NOSE"),
            DT = sum(UpdatedPosition == "DT"),
            IDL = sum(UpdatedPosition == "IDL"),
            snaps = n()) %>%
  filter(snaps >= 400) %>%
  select(-snaps) %>%
  pivot_longer(cols = EDGE:IDL,
               names_to = "Position",
               values_to = "Snaps") %>%
  ggplot(aes(x = Snaps, y = Name, fill = Position)) +
  geom_col(position = "fill") +
  theme_bw() +
  labs(title = "Most Common Positions by Player",
       subtitle = "Min. 400 Snaps",
       x = "Proportion of Snaps",
       y = "Player")

# Make our model
sis_passes <- sis_df %>%
  filter(Attempt != "NULL")

epa_impact_passes_gam <- gam(EPA ~ Pressure + Sack + PassBreakup + Interception + ForcedFumble +
                               RecoveredFumble,
                             data = sis_passes)

summary(epa_impact_passes_gam)


sis_passes <- sis_passes %>%
  modelr::add_predictions(epa_impact_passes_gam, var = "IndividualEPA")

# Peek at some players' individual EPA

sis_passes %>%
  group_by(Name, DefensiveTeam) %>%
  summarize(sumEPA = sum(IndividualEPA),
            snaps = n()) %>%
  arrange(sumEPA) -> defenders



# Curious so I'll look at the individual EPA by updated position on passing plays
sis_passes %>%
  group_by(UpdatedPosition) %>%
  summarize(mean_EPA = mean(IndividualEPA))


# Try to come up with a model for rushing plays to determine IndividualEPA

sis_rushes <- sis_df %>%
  filter(Attempt == "NULL")

epa_impact_rushes_gam <- gam(EPA ~ FumbleByRusher + ForcedFumble + RecoveredFumble + UsedDesignedGap +
                               fill_run_gap + SoloTackle + AssistedTackle,
                             data = sis_rushes)
summary(epa_impact_rushes_gam)

sis_rushes <- sis_rushes %>%
  modelr::add_predictions(epa_impact_rushes_gam, var = "IndidualEPA")

# How much does each position contribute in terms of Individual EPA on rushing plays?
sis_rushes %>%
  group_by(UpdatedPosition) %>%
  summarize(mean_EPA = mean(EPA),
            count = n())

# Add the EPA models to the main sis_df dataframe
sis_df <- sis_df %>%
  modelr::add_predictions(epa_impact_passes_gam, var = "PassIndividualEPA")

sis_df <- sis_df %>%
  modelr::add_predictions(epa_impact_rushes_gam, var = "RushIndividualEPA")

sis_df <- sis_df %>%
  mutate(IndividualEPA = ifelse(Attempt != "NULL", PassIndividualEPA, RushIndividualEPA)) %>%
  select(-c(PassIndividualEPA, RushIndividualEPA))


# Might not really need this but who knows
sis_df <- sis_df %>%
  mutate(YardsFromGoal = ifelse(SideOfField == "Own", 100 - StartYard, StartYard))


# Do some modeling

#Overall model
sis_model <- lmer(IndividualEPA ~ (1|UpdatedPosition),
                  data = sis_df)

summary(sis_model)

ranef(sis_model)

# Passing downs

sis_pass_down <- sis_df %>%
  filter(pass_down == 1)

sis_pass_down_model <- lmer(IndividualEPA ~ (1|UpdatedPosition),
                  data = sis_pass_down)

summary(sis_pass_down_model)

ranef(sis_pass_down_model)

# Rushing downs

sis_non_pass_down <- sis_df %>%
  filter(pass_down == 0)

sis_non_pass_down_model <- lmer(IndividualEPA ~ (1|UpdatedPosition),
                            data = sis_non_pass_down)

summary(sis_non_pass_down_model)

ranef(sis_non_pass_down_model)


