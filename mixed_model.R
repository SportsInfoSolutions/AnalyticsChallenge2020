library(tidyverse)
library(lme4)
library(ggeffects)
library(sjPlot)
library(arm)
library(lattice)
library(influence.ME)
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


sis_tech_feats <- sis_tech %>% group_by(GameID, EventID) %>% mutate(total_pressures = sum(Pressure), total_defenders = n(), total_rushers = sum(IsRushing)) %>% ungroup()
sis_tech_feats <- sis_tech_feats %>% mutate(Sack = SoloSack + AssistedSack)
sis_tech_feats <- sis_tech_feats %>% mutate(EventType = case_when(
  EventType == "challenge pass" ~ "pass",
  EventType == "challenge rush" ~ "rush",
  TRUE ~ EventType
))

sis_tech_feats$EventType %>% unique()

sis_tech_feats %>% filter(ToGo <= 15) %>%
  ggplot(aes(fill = EventType, x = ToGo)) +
  facet_wrap(~Down) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = 0.5, color = "black") +
  labs(title = "Pass and Run Splits By Yards To Go", subtitle = "Faceted by Down (yards to go <= 15 yards", xlab = "Yards to Go")

sis_tech_feats <- sis_tech_feats %>% mutate(pass_down = case_when(
  Down == 2 & ToGo >= 5 ~ 1,
  Down %in% c(3,4) & ToGo >= 3 ~ 1,
  TRUE ~ 0
))

# Load in madden ratings and lookup table
madden_lookup <- read_csv("Data/madden_lookup.csv") %>% distinct(MaddenId, PlayerId, .keep_all = TRUE)
madden_ratings <- read_rds("Data/madden_ratings.rds") %>% mutate(primary_key = as.numeric(primary_key))

join_lookup <- left_join(sis_tech_feats, madden_lookup %>% dplyr::select(PlayerId, MaddenId), by = c("PlayerId"))
sis_madden <- left_join(join_lookup, madden_ratings %>% dplyr::select(primary_key, ovr_rating), by = c("MaddenId" = "primary_key"))
sis_madden <- sis_madden %>% mutate(ovr_rating = scale(ovr_rating))
# model_df <- sis_tech_feats %>% filter(SoloTackle != 0 | AssistedTackle != 0 | Pressure  != 0 | SoloSack  != 0 | AssistedSack  != 0 | PassBreakup  != 0 | Interception  != 0 | ForcedFumble  != 0 | RecoveredFumble  != 0)

pass_model <- sis_madden %>% 
  filter(EventType == "pass" | EventType == "challenge pass") %>% 
  filter(Spike == "0") %>% 
  mutate(SoloSack = factor(SoloSack), 
         Pressure = factor(Pressure) , 
         PassBreakupOnPlay = factor(PassBreakupOnPlay), 
         AssistedSack = factor(AssistedSack)) %>% drop_na(pos_grp) %>% drop_na(ovr_rating)
rush_model <- sis_madden %>% filter(EventType == "rush" | EventType == "challenge rush")

# Predict Sack - ended up not useful
glmer_sack <- pass_model %>% glmer(data = ., formula = Sack ~ pass_down + Pressure + ovr_rating + (1 + Pressure|pos_grp), family = "binomial", control = glmerControl(optimizer = "bobyqa"))
tidy(glmer_sack, effects = "ran_vals")
summary(glmer_sack)


# Predict Pressure
glmer_pressure <- pass_model %>% glmer(data = ., formula = Pressure ~ IsRushing + pass_down + ovr_rating + (1|pos_grp), family = "binomial")
summary(glmer_pressure)
tidy_pressure <- tidy(glmer_pressure, effects = "ran_vals")


tidy_pressure %>% ggplot(aes(x = estimate, y = level)) +
  geom_point() +
  geom_linerange(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error)) +
  labs(title = "Generating Pressure Random Effect of Position", xlab = "Change in Log Odds", ylab = "Position")



# Predict EPA
lm_tot_pressure <- pass_model %>% lm(data = ., formula = EPA ~ total_pressures + SackOnPlay + total_rushers + on_ball_cnt + total_defenders + pos_grp)
summary(lm_tot_pressure)


lm_pressure <- pass_model %>% lm(data = ., formula = EPA ~ PressureOnPlay + SackOnPlay + total_rushers + on_ball_cnt + total_defenders + pos_grp)
summary(lm_pressure)

lm_tot_nosack <- pass_model %>% lm(data = ., formula = EPA ~ total_pressures + total_rushers + on_ball_cnt + total_defenders + pos_grp)
summary(lm_tot_nosack)

lm_pres_nosack <- pass_model %>% lm(data = ., formula = EPA ~ PressureOnPlay + total_rushers + on_ball_cnt + total_defenders + PassBreakupOnPlay + InterceptionOnPlay + pos_grp)
summary(lm_pres_nosack)





lmer_pass <- pass_model %>% lmer(data = ., formula = EPA ~ total_pressure*SackOnPlay + on_ball_cnt*total_defenders + (Pressure|pos_grp))

tidy_pass <- broom.mixed::tidy(lmer_pass)
tidy_pass_re <- broom.mixed::tidy(lmer_pass, effects = "ran_vals")

summary(lmer_pass)







