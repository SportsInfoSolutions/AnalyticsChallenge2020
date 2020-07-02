sis_df <- read.csv('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)
madden_df <- readRDS(url('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/madden_ratings.rds'))
pbp_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

### do CPOE with pass attempts
### more likley to have success with multiple pass rushers
### all pass rushers are not always included

## join with nflfastR to determine significance of hit/ hurry/ kd/ sack
sis_df %>% 
  filter(grepl('pass', EventType)) %>% 
  group_by(Name) %>% 
  summarize(
    'snaps' = n(),
    'RushFreq' = mean(IsRushing),
    'TeamPressFreq' = mean(as.numeric(ifelse(IsRushing == 1, PressureOnPlay, NA)), na.rm = T),
    'PressFreq' = mean(ifelse(IsRushing == 1, Pressure, NA), na.rm = T)
  ) %>% 
  filter(snaps >= 50) %>% 
  arrange(-PressFreq)
  


sis_df %>% 
  #filter(OnFieldPosition == 'LB') %>% 
  filter(TechniqueName == 'Outside') %>% 
  group_by(GameID, EventID, DefensiveTeam) %>% 
  summarize(players = n()) %>% 
  group_by(DefensiveTeam, players) %>% 
  summarize(plays = n()) %>% 
  mutate(play_freq = plays/sum(plays)) %>% 
  ggplot(aes(x = players, y = play_freq)) + 
  facet_wrap(. ~ DefensiveTeam, scales = 'free_x', ncol = 4) +
  geom_bar(stat = 'identity', size = 1, width = 1, color = 'darkblue') +
  scale_y_continuous(label = percent, expand = expansion(mult = c(0,0.05))) +
  scale_x_continuous(breaks = 0:11) +
  labs(title = 'Down Linemen By Team',
       x = NULL,
       y = NULL) +
  theme_SB + 
  theme(panel.grid.major.x = element_blank())




sis_df %>% 
  filter(TechniqueName == 'Outside') %>%
  group_by(RosterPosition) %>% 
  summarize(
    snaps = n(),
    rush = sum(IsRushing),
    pb = sum(PassBreakup),
    tack = sum(SoloTackle),
    ast_tack = sum(AssistedTackle)
  )

sis_df %>% 
  filter(OffensiveTeam=='Patriots' & DefensiveTeam=='Dolphins' & Quarter==2 & TimeLeft == (7*60+14))



sis_df %>% 
  filter(DefensiveTeam=='Patriots' & OffensiveTeam=='Dolphins' & Quarter==1 & TimeLeft == (1*60+45))






sis_df %>% 
  filter(grepl('pass', EventType)) %>% 
  group_by(TechniqueName) %>% 
  summarize(
    'snaps' = n(),
    'RushFreq' = mean(IsRushing),
    'TeamPressFreq' = mean(as.numeric(ifelse(IsRushing == 1, PressureOnPlay, NA)), na.rm = T),
    'PressFreq' = mean(ifelse(IsRushing == 1, Pressure, NA), na.rm = T)
  ) %>% 
  arrange(-PressFreq)



rusher_cnt <- sis_df %>% 
  filter(grepl('pass', EventType)) %>% 
  group_by(GameID, EventID) %>% 
  summarize(LineRushers = sum(IsRushing))

sis_df %>% 
  left_join(rusher_cnt) %>% 
  filter(grepl('pass', EventType)) %>% 
  group_by(LineRushers, TechniqueName) %>% 
  summarize(
    'snaps' = n(),
    'RushFreq' = mean(IsRushing),
    'TeamPressFreq' = mean(as.numeric(ifelse(IsRushing == 1, PressureOnPlay, NA)), na.rm = T),
    'PressFreq' = mean(ifelse(IsRushing == 1, Pressure, NA), na.rm = T)
  ) %>% 
  filter(snaps >= 50 & LineRushers==3) %>% 
  arrange(-TeamPressFreq)



sis_df %>% 
  filter(Name == 'Aaron Donald') %>% 
  group_by(TechniqueName) %>% 
  summarize(snap = n())



DL_cnt <- sis_df %>% 
  filter(TechniqueName != 'Off Ball') %>%  
  group_by(GameID, EventID) %>% 
  summarize(players = n())





basic_tech_ord <- c(1,'2i',2,3,'4i',4,5,7,6,9,'Outside')
tech_ord_LR <- c(paste0('L ',rev(basic_tech_ord)),'NULL 0',paste0('R ',basic_tech_ord))
#left_most = tech_ord_LR[min(as.numeric(TechSide), na.rm = T)]

reversed_line_combo <- sis_df %>% 
  filter(TechniqueName != 'Off Ball') %>% 
  mutate(TechSide = factor(paste0(SideOfBall, ' ', TechniqueName), tech_ord_LR)) %>% 
  #select(ï..GameID, EventID, TechSide) %>%
  group_by(ï..GameID, EventID) %>% 
  mutate(
    players_listed_LR = gsub('R ','',gsub('L ', '', gsub('NULL ', '', paste0(sort(TechSide), collapse = ' | ')))),
    players_listed_RL = gsub('R ','',gsub('L ', '', gsub('NULL ', '', paste0(rev(sort(TechSide)), collapse = ' | ')))),
    players_listed = ifelse(players_listed_LR > players_listed_RL, players_listed_LR, players_listed_RL),
    on_ball_cnt = n()
  ) %>% 
  select(ï..GameID, EventID, players_listed, on_ball_cnt) %>% 
  distinct %>% 
  group_by(players_listed, on_ball_cnt) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  mutate(freq = count / sum(count)) %>% 
  arrange(-freq) %>% 
  slice(1:30) %>% 
  mutate(players_listed = factor(players_listed, rev(players_listed)))



p <- ggplot(reversed_line_combo, aes(x = players_listed, y = freq, fill = factor(on_ball_cnt), label = players_listed)) + 
  geom_bar(color = 'darkblue', stat = 'identity', show.legend = T, size = 0.6, width = 0.8) +
  geom_text(color = 'white', size = 2, nudge_y = -0.0002, hjust = 1, family = font_SB) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0,0.05)), label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = color_SB[c(1:2,5)], name = 'On Ball Players') +
  labs(title = 'Most Common Defensive Line Configurations',
       subtitle = 'Excludes Off Ball Players',
       y = '% of All Plays') +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'bottom'
  )


brand_plot(p, asp = 9/16, save_name = 'defensive config.png', data_home = 'Data: SIS', fade_borders = 'tr', axis_rot = T)



basic_tech_ord <- c(1,'2i',2,3,'4i',4,5,7,6,9,'Outside')
tech_ord_LR <- c(paste0('L ',rev(basic_tech_ord)),'NULL 0',paste0('R ',basic_tech_ord))
names(tech_ord_LR) <- 1:length(tech_ord_LR)

sis_df %>% 
  select(PlayerId, Name, RosterPosition, DefensiveTeam) %>% 
  distinct %>% 
  write.csv('players.csv', row.names = F)


tech_df <- sis_df %>% 
  filter(TechniqueName != 'Off Ball') %>% 
  mutate(TechSide = factor(paste0(SideOfBall, ' ', TechniqueName), tech_ord_LR)) %>% 
  select(ï..GameID, EventID, PlayerId, Name, TechniqueName, TechSide) %>% 
  group_by(ï..GameID, EventID) %>% 
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


#pivot_wider(names_from = pos_grp, values_from = freq, values_fill = list(freq = 0))
p <- tech_df %>% 
  mutate(
    pos_grp = case_when(
      is.na(next_tech_to_R) | is.na(next_tech_to_L) & (as.numeric(TechSide) >= 23 | as.numeric(TechSide) <= 1) | TechniqueName=='Outside' ~ 'D',
      as.numeric(TechSide) >= 11 & as.numeric(TechSide) <= 13 & as.numeric(next_tech_to_R) >= 16 &  as.numeric(next_tech_to_L) <= 8 ~ 'A',
      (as.numeric(TechSide) >= 18 | as.numeric(TechSide) <= 6) ~ 'C',
      as.numeric(TechSide) >= 7 & as.numeric(TechSide) <= 17 ~ 'B',
      TRUE ~ 'None'
    )
  ) %>% 
  group_by(PlayerId, Name, pos_grp) %>% 
  summarize(n = n()) %>% 
  group_by(PlayerId, Name) %>% 
  mutate(
    snaps = sum(n),
    freq = n / sum(n),
    n = NULL
  ) %>% 
  filter(snaps >= 400) %>% 
  ggplot(aes(x = Name, y = freq, fill = factor(pos_grp), label = Name)) + 
  geom_bar(color = 'darkblue', stat = 'identity', show.legend = T, size = 0.6, width = 0.4, position = 'stack') +
  geom_text(aes(y = .05), color = 'darkblue', size = 2, nudge_x = 0.3, vjust = 0, hjust = 0, family = font_SB) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0,0)), label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = color_SB, name = 'Position') +
  labs(title = 'Positional Breakdown',
       subtitle = 'Min 400 Snaps at Line of Scrimmage',
       y = '% of Snaps') +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'bottom'
  )
brand_plot(p, asp = 9/16, save_name = 'pos snap breakdown.png', data_home = 'Data: SIS', fade_borders = '', axis_rot = T)


tech_df %>% 
  mutate(
    pos_grp = case_when(
      is.na(next_tech_to_R) | is.na(next_tech_to_L) & (as.numeric(TechSide) >= 23 | as.numeric(TechSide) <= 1) | TechniqueName=='Outside' ~ 'D',
      as.numeric(TechSide) >= 11 & as.numeric(TechSide) <= 13 & as.numeric(next_tech_to_R) >= 16 &  as.numeric(next_tech_to_L) <= 8 ~ 'A',
      (as.numeric(TechSide) >= 18 | as.numeric(TechSide) <= 6) ~ 'C',
      as.numeric(TechSide) >= 7 & as.numeric(TechSide) <= 17 ~ 'B',
      TRUE ~ 'None'
    )
  ) %>% 
  group_by(PlayerId, Name, pos_grp) %>% 
  summarize(n = n()) %>% 
  group_by(PlayerId, Name) %>% 
  mutate(
    snaps = sum(n),
    freq = n / sum(n),
    n = NULL
  ) %>% 
  filter(pos_grp=='C') %>% 
  arrange(-freq)





table(pos_asgn_df$pos_grp)

pos_asgn_df %>% 
  filter(pos_grp=='EDGE')





madden_df$portrait_id
names(madden_df)


