library(tidyverse)
library(gamm4)
library(lme4)
library(broom.mixed)

sis_df <- read_csv('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv')
madden_df <- read_rds(url('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/madden_ratings.rds'))
player_df <- read_csv('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/madden_lookup.csv')

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


tech_df <- tech_df %>% 
  mutate(
    pos_grp = case_when(
      is.na(next_tech_to_R) | is.na(next_tech_to_L) & (as.numeric(TechSide) >= 23 | as.numeric(TechSide) <= 1) | TechniqueName=='Outside' ~ 'EDGE',
      as.numeric(TechSide) >= 11 & as.numeric(TechSide) <= 13 & as.numeric(next_tech_to_R) >= 16 &  as.numeric(next_tech_to_L) <= 8 ~ 'NOSE',
      as.numeric(TechSide) >= 18 | as.numeric(TechSide) <= 6 ~ 'DT',
      as.numeric(TechSide) >= 7 & as.numeric(TechSide) <= 17 ~ 'IDL',
      TRUE ~ 'None'
    )
  )

sis_tech <- left_join(sis_df, tech_df %>% select(GameID, EventID, PlayerId, pos_grp, on_ball_cnt, player_order_LR, TechSide), by = c("GameID", "EventID", "PlayerId"))


sis_tech_feats <- sis_tech %>% group_by(GameID, EventID) %>% mutate(total_pressure = sum(Pressure), total_defenders = n()) %>% ungroup()

model_df <- sis_tech_feats %>% filter(SoloTackle != 0 | AssistedTackle != 0 | Pressure  != 0 | SoloSack  != 0 | AssistedSack  != 0 | PassBreakup  != 0 | Interception  != 0 | ForcedFumble  != 0 | RecoveredFumble  != 0)





pass_model <- model_df %>% filter(EventType == "pass" | EventType == "challenge pass") %>% filter(Spike == "0")
rush_model <- model_df %>% filter(EventType == "rush" | EventType == "challenge rush")

lmer_pass <- pass_model %>% lmer(data = ., formula = EPA ~ total_pressure*SackOnPlay + on_ball_cnt*total_defenders + (Pressure + SoloSack|pos_grp))

tidy_pass <- broom.mixed::tidy(lmer_pass)
tidy_pass_re <- broom.mixed::tidy(lmer_pass, effects = "ran_vals")








