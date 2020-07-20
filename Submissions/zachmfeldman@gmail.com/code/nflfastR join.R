library(tidyverse)
setwd('C:/Users/rei1740/Desktop/Anthony/nfl')

sis_df <- read.csv('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)
pbp_df <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
team_df <- read.csv('https://raw.githubusercontent.com/ajreinhard/NFL/master/nfl_logo.csv', stringsAsFactors = F)
team_df$team_code[which(team_df$team_code=='OAK')] <- 'LV'

play_df <- sis_df %>% 
  select(GameID, Season, Week, OffensiveTeam, DefensiveTeam, Quarter, TimeLeft, Down, ToGo, FirstDown, Touchdown, EventType, UsedDesignedGap, StartYard, RunDirection, PlayDesc) %>% 
  distinct() %>% 
  left_join(team_df %>% select(team_code, mascot), by = c('OffensiveTeam' = 'mascot')) %>% 
  rename(posteam = team_code) %>% 
  left_join(team_df %>% select(team_code, mascot), by = c('DefensiveTeam' = 'mascot')) %>% 
  rename(defteam = team_code) %>% 
  left_join(pbp_df %>%
              mutate(yardline_50 = as.numeric(substr(yrdln,nchar(yrdln)-1,nchar(yrdln)))) %>% 
              filter(penalty==0)
            , by = c('Week' = 'week', 'posteam', 'defteam', 'Quarter' = 'qtr', 'TimeLeft' = 'quarter_seconds_remaining', 'Down' = 'down', 'ToGo' = 'ydstogo', 'StartYard'='yardline_50')) %>% 
  select(fastR_game_id = game_id, fastR_play_id = play_id, GameID, Season, Week, posteam, defteam, Quarter, TimeLeft, Down, ToGo, FirstDown, Touchdown, EventType, UsedDesignedGap, StartYard, RunDirection, PlayDesc)


missed_plays <- play_df %>% 
  filter(is.na(fastR_play_id)) %>% 
  left_join(pbp_df %>%
              mutate(yardline_50 = as.numeric(substr(yrdln,nchar(yrdln)-1,nchar(yrdln)))) %>% 
              filter(penalty==0)
            , by = c('Week' = 'week', 'posteam', 'defteam', 'Quarter' = 'qtr', 'Down' = 'down', 'ToGo' = 'ydstogo', 'StartYard'='yardline_50')) %>% 
  filter(abs(quarter_seconds_remaining - TimeLeft) < 60) %>% 
  select(fastR_game_id = game_id, fastR_play_id = play_id, GameID, Season, Week, posteam, defteam, Quarter, TimeLeft, Down, ToGo, FirstDown, Touchdown, EventType, UsedDesignedGap, StartYard, RunDirection, PlayDesc)

found_plays <- play_df %>% 
  filter(!is.na(fastR_play_id)) %>% 
  rbind(missed_plays)

still_missing <- anti_join(play_df, found_plays, by = c('Week', 'posteam', 'defteam', 'Quarter', 'TimeLeft', 'PlayDesc'))
still_missing$fastR_game_id <- c(rep('2019_12_PIT_CIN',3), '2019_14_MIA_NYJ')
still_missing$fastR_play_id <- c(1506,1528,1556,3696)

all_plays <- rbind(found_plays, still_missing)

saveRDS(all_plays, 'plays.rds')
