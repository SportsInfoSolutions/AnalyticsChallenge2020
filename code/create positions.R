setwd('C:/Users/rei1740/Desktop/Anthony/nfl')

sis_df <- read.csv('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)
madden_df <- readRDS(url('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/madden_ratings.rds'))
player_df <- read.csv('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/madden_lookup.csv', stringsAsFactors = F)

basic_tech_ord <- c(1,'2i',2,3,'4i',4,5,7,6,9,'Outside')
tech_ord_LR <- c(paste0('L ',rev(basic_tech_ord)),'NULL 0',paste0('R ',basic_tech_ord))
names(tech_ord_LR) <- 1:length(tech_ord_LR)


tech_df <- sis_df %>% 
  filter(TechniqueName != 'Off Ball') %>% 
  mutate(TechSide = factor(paste0(SideOfBall, ' ', TechniqueName), tech_ord_LR)) %>% 
  select(ï..GameID, EventID, PlayerId, Name, SideOfBall, TechniqueName, TechSide) %>% 
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


pos_df <- tech_df %>% 
  mutate(
    pos_grp = case_when(
      is.na(next_tech_to_R) | is.na(next_tech_to_L) & (as.numeric(TechSide) >= 23 | as.numeric(TechSide) <= 1) | TechniqueName=='Outside' ~ 'EDGE',
      as.numeric(TechSide) >= 11 & as.numeric(TechSide) <= 13 & as.numeric(next_tech_to_R) >= 16 &  as.numeric(next_tech_to_L) <= 8 ~ 'NOSE',
      as.numeric(TechSide) >= 18 | as.numeric(TechSide) <= 6 ~ 'DT',
      as.numeric(TechSide) >= 7 & as.numeric(TechSide) <= 17 ~ 'IDL',
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
  arrange(-freq) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == 1 & snaps >= 50) %>% 
  left_join(player_df, by = 'PlayerId') %>%
  mutate(MaddenId = as.character(MaddenId)) %>% 
  left_join(madden_df, by = c('MaddenId' = 'primary_key'))


