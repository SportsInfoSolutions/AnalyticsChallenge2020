# Project Goals -----------------------------------------------------------

# 1. Which is the most valuable defensive line position? You can define the positions however you like.
# 2. What is the nature of the distribution of talent between the defensive line positions, as you define them
# 3. Not all situations are created equal. In which in-game or roster construction scenarios would the answer to Question 1 change?

# Links + Notes + Ideas -------------------------------------------------------------------

# Links:
# Github w/ Rules: https://github.com/SportsInfoSolutions/AnalyticsChallenge2020
# Article: https://sportsinfosolutionsblog.com/2020/06/24/a-quick-introduction-to-our-analytics-challenge-data-set/


# Ideas:
# Define position as... what action (role) they are taking? location?
#    -role is given as isrusher for only pass plays
# Talent distribution -> actual vs. expectation... or talent ~ predictable?
# Situations... obvious rush, obvious pass... distance based metric, score based metric... offensive layout (e.g. spread, shotgun)


# Libraries ---------------------------------------------------------------
library(RCurl)           # For reading data from github
library(tidyverse)       # ggplot2, dplyr, purr, etc 
library(stringr)         # for searching strings

# Load data ---------------------------------------------------------------

# Initial Load:
# data_url <- getURL("https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv")
# sis_dataset <- read.csv(text = data_url)
# write.csv(sis_dataset, file="data//sis_dataset.csv",row.names=FALSE)

# Reload (has been touched up slightly to correct 1st column name)
sis_dataset_load <- read.csv(file="data//sis_dataset.csv")

# Clean & Set-up -----------------------------------------------------------

# Some stuff (EPA) isn't numeric... make it so
sis_dataset <- sis_dataset_load %>% 
     filter(!is.na(as.numeric((EPA)))) %>%                                           # removes 1x play GameID: 2757, EventID: 915
     filter(EventType %in% c("pass","rush")) %>%                                        # remove challenge plays...
     mutate(EPA = as.numeric(EPA)) %>% 
     mutate(Attempt = as.numeric(Attempt)) %>% 
     mutate(Completion = as.numeric(Completion)) %>% 
     mutate(Spike = as.numeric(Spike)) %>% 
     mutate(ThrowDepth = as.numeric(ThrowDepth)) %>% 
     mutate(PressureOnPlay = as.numeric(PressureOnPlay)) %>% 
     mutate(SackOnPlay = as.numeric(SackOnPlay)) %>% 
     mutate(PassBreakupOnPlay = as.numeric(PassBreakupOnPlay)) %>% 
     mutate(InterceptionOnPlay = as.numeric(InterceptionOnPlay)) %>% 
     mutate(FumbleByPasser = as.numeric(FumbleByPasser)) %>% 
     mutate(FumbleByRusher = as.numeric(FumbleByRusher)) %>% 
     mutate(FumbleByReceiver = as.numeric(FumbleByReceiver)) %>% 
     mutate(Success = ifelse(EPA > 0, 1,0)) %>%
     mutate(UsedDesignedGap = as.numeric(UsedDesignedGap)) %>% 
     # group to calculate 
     group_by(GameID,EventID) %>% 
     mutate(n_def_upfront = n()) %>%
     mutate(n_DL = sum(OnFieldPosition =="DL")) %>% 
     mutate(n_LB = sum(OnFieldPosition == "LB")) %>% 
     mutate(n_rushers = sum(IsRushing)) %>% 
     ungroup() %>% 
     mutate(Blitz = ifelse(n_rushers > 4,1,0)) %>% 
     mutate(EventType = ifelse(str_detect(str_to_lower(PlayDesc), "broken play"),"broken Play",EventType)) %>% 
     mutate(EventType = ifelse(str_detect(str_to_lower(PlayDesc), "scramble"),"scramble",EventType)) %>%
     mutate(EventType = ifelse(str_detect(str_to_lower(PlayDesc), "kneel"),"kneel",EventType)) %>%
     mutate(Shotgun = ifelse(str_detect(str_to_lower(PlayDesc), "shotgun"),1,0)) %>% 
     mutate(Hurryup = ifelse(str_detect(str_to_lower(PlayDesc), "hurry"),1,0)) %>% 
     # reassign rush/scrambles
     mutate(EventType = ifelse(EventType == "rush" & n_rushers != 0,"scramble",EventType)) %>%                # update rushes that had "rushers" ---> scramble
     mutate(EventType = ifelse(EventType == "scramble" & n_rushers == 0,"rush",EventType))                    # update scrambles that had 0 rushers ---> rush
     


# Play x N (number of defensive lineman)
# InterceptionOnPlay ~ was there an interception
# Interception ~ who intercepted it
# RosterPosition ~ as per roster
# OnFieldPosition ~ DL = hand on ground OR, LB 
# TechniqueName ~ "SideOfBall" (from def. perspective) + location vs. C,G,T,TE 
# IsRushing ~ if player rushed, for running plays, always ZERO
# EventType ~ play result (rush/pass) ... NOT designed (e.g. scramble), also includes some "challenge" 
     # UPDATED:
     # pass, rush, broken play, scramble, kneel

# Rush Plays:
# RunDirection ~ Left/Right + Gap A-B-C-D correlate to the C-G-T-TE gap and D = outside [10x unique including NULL]
# UsedDesignedGap ~ 1 = yes, 0 = no

# Pass plays:
# ThrowDepth ~ air yards
# Pressure ~ hits, hurries, knockdowns, sacks
# PassBreakup ~ defensed, batted, deflected, or intercepted passes

# NEW:
# Success rate ~ 1 or 0
# Shotgun ~ 1 or 0
# n_def_upfront ~ # of defenders listed
# n_rushers ~ # of rushers
# Blitz ~ 1 or 0 (>4 rushers)
# Hurryup ~ 1 or 0


# Play Description Review -------------------------------------------------



# which plays were rushes, but had defensive rushers labeled? ---> "scramble" (DONE)
sis_dataset %>% 
     filter(EventType == "rush") %>%
     # filter(str_detect(str_to_lower(PlayDesc), "scramble")) %>%
     # filter(str_detect(str_to_lower(PlayDesc), "a.mccarron")) %>% 
     select(GameID, EventID, PlayDesc, n_rushers, RunDirection, UsedDesignedGap) %>% 
     filter(n_rushers != 0) %>% 
     arrange(n_rushers) %>% 
     unique() %>% 
     knitr::kable()

# which plays were scrambles, didn't have rushers? ---> "rush" (DONE)
sis_dataset %>% 
     filter(EventType == "scramble") %>%
     # filter(str_detect(str_to_lower(PlayDesc), "scramble")) %>%
     # filter(str_detect(str_to_lower(PlayDesc), "a.mccarron")) %>% 
     select(GameID, EventID, PlayDesc, n_rushers, RunDirection, UsedDesignedGap) %>% 
     filter(n_rushers == 0) %>% 
     arrange(UsedDesignedGap) %>% 
     unique() %>% 
     knitr::kable()

# look at broken plays
sis_dataset %>% 
     filter(EventType == "broken Play") %>%
     # filter(str_detect(str_to_lower(PlayDesc), "scramble")) %>%
     # filter(str_detect(str_to_lower(PlayDesc), "a.mccarron")) %>% 
     select(GameID, EventID, PlayDesc, n_rushers, RunDirection, UsedDesignedGap) %>% 
     filter(n_rushers != 0) %>% 
     arrange(UsedDesignedGap) %>% 
     unique() %>% 
     knitr::kable()


# Discovery ---------------------------------------------------------------

# what do we have?
str(sis_dataset)

# what ranges are we working with?
summary(sis_dataset)

# what is important?
# number of rushers?


# create pbp without individual DL to search for importance, stickiness, etc
sis_dataset_pbp <- sis_dataset %>% 
     select(-PlayerId,-Name,-RosterPosition,-OnFieldPosition,-SideOfBall,-TechniqueName,-IsRushing,-SoloTackle,-AssistedTackle,-Pressure,-SoloSack,-AssistedSack,-PassBreakup,-Interception,-ForcedFumble,-RecoveredFumble) %>% 
     unique()


# ***BY TEAM -----------------------------------------------------------------
sis_dataset_team <- sis_dataset_pbp %>% 
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles ~ NO pressures! 
     group_by(DefensiveTeam) %>% 
     summarize(
          n = n(),
          n_rush = sum(EventType == "rush"), 
          n_pass = sum(EventType == "pass"), 
          n_scramble = sum(EventType == "scramble"), 
          avg_def = mean(n_def_upfront),
          blitz_rate = mean(Blitz[EventType != "rush"], na.rm = TRUE),
          avg_rushers = mean(n_rushers, na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType != "rush"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType != "rush"], na.rm = TRUE)
     )
sis_dataset_team %>% knitr::kable()


# Pressure is king
# but it didn't correlate to EPA for this down
# EPA driven by... variance? e.g. 1st downs, scores, and sack/turnovers
sis_dataset %>% 
     select(GameID,EventID,EventType,DefensiveTeam,Down,ToGo,n_def_upfront,n_DL,n_LB,n_rushers,PressureOnPlay,EPA,Success) %>% 
     unique() %>%
     filter(EventType == "pass") %>% 
     filter(Down == 1 & ToGo == 10) %>%
     group_by(DefensiveTeam) %>% 
     summarize(
          n = n(),
          avg_def = mean(n_def_upfront),
          avg_rushers = mean(n_rushers, na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_epa = mean(EPA, na.rm = TRUE),
          avg_success = mean(Success, na.rm = TRUE)
     ) %>%
     arrange(avg_success) %>% 
     # knitr::kable()
     ggplot(aes(x = avg_pressure, y = avg_success, label = DefensiveTeam)) +
     #geom_point(color ="black",alpha=0.9) +
     geom_label() +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw()


# what correlates to pressure
# avg def = no, avg_rushers = no
sis_dataset %>% 
     select(GameID,EventID,EventType,DefensiveTeam,Down,ToGo,n_def_upfront,n_rushers,PressureOnPlay,EPA,Success) %>% 
     unique() %>%
     filter(EventType == "pass") %>% 
     filter(Down == 1 & ToGo == 10) %>%
     group_by(DefensiveTeam) %>% 
     summarize(
          n = n(),
          avg_def = mean(n_def_upfront),
          avg_rushers = mean(n_rushers, na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_epa = mean(EPA, na.rm = TRUE),
          avg_success = mean(Success, na.rm = TRUE)
     ) %>%
     arrange(avg_success) %>% 
     # knitr::kable()
     ggplot(aes(x = avg_epa, y = avg_pressure, label = DefensiveTeam)) +
     #geom_point(color ="black",alpha=0.9) +
     geom_label() +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw()

# avg def vs. avg rushers
sis_dataset %>% 
     select(GameID,EventID,EventType,DefensiveTeam,Down,ToGo,n_def_upfront,n_rushers,PressureOnPlay,EPA,Success) %>% 
     unique() %>%
     filter(EventType == "pass") %>% 
     filter(Down == 1 & ToGo == 10) %>%
     group_by(DefensiveTeam) %>% 
     summarize(
          n = n(),
          avg_def = mean(n_def_upfront),
          avg_rushers = mean(n_rushers, na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_epa = mean(EPA, na.rm = TRUE),
          avg_success = mean(Success, na.rm = TRUE)
     ) %>%
     arrange(avg_success) %>% 
     # knitr::kable()
     ggplot(aes(x = avg_def, y = avg_rushers, label = DefensiveTeam)) +
     #geom_point(color ="black",alpha=0.9) +
     geom_label() +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw()


# rushing...
sis_dataset %>% 
     select(GameID,EventID,EventType,DefensiveTeam,Down,ToGo,n_def_upfront,n_rushers,PressureOnPlay,EPA,Success) %>% 
     unique() %>%
     filter(EventType == "rush") %>% 
     filter(Down == 1 & ToGo == 10) %>%
     group_by(DefensiveTeam) %>% 
     summarize(
          n = n(),
          avg_def = mean(n_def_upfront),
          avg_rushers = mean(n_rushers, na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_epa = mean(EPA, na.rm = TRUE),
          avg_success = mean(Success, na.rm = TRUE)
     ) %>%
     arrange(avg_success) %>% 
     # knitr::kable()
     ggplot(aes(x = avg_def, y = avg_success, label = DefensiveTeam)) +
     #geom_point(color ="black",alpha=0.9) +
     geom_label() +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw()


# pass success rate vs epa ~ 0.5 r-squared (1st and 10) ~ 0.59 r-squared for all downs/distance
sis_dataset %>% 
     select(GameID,EventID,EventType,DefensiveTeam,Down,ToGo,n_def_upfront,n_rushers,PressureOnPlay,EPA,Success) %>% 
     unique() %>%
     # filter(Down == 1 & ToGo == 10) %>%
     group_by(DefensiveTeam) %>% 
     summarize(
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType == "pass"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType == "pass"], na.rm = TRUE)
     ) %>%
     # knitr::kable()
     ggplot(aes(x = avg_epa_pass, y = avg_success_pass, label = DefensiveTeam)) +
     #geom_point(color ="black",alpha=0.9) +
     geom_label() +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw()

# rush success rate vs epa ~ 0.63 r-squared ~ 0.63 r-squared for all downs/distance
sis_dataset %>% 
     select(GameID,EventID,EventType,DefensiveTeam,Down,ToGo,n_def_upfront,n_rushers,PressureOnPlay,EPA,Success) %>% 
     unique() %>%
     filter(Down == 1 & ToGo == 10) %>%
     group_by(DefensiveTeam) %>% 
     summarize(
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType == "pass"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType == "pass"], na.rm = TRUE)
     ) %>%
     # knitr::kable()
     ggplot(aes(x = avg_epa_rush, y = avg_success_rush, label = DefensiveTeam)) +
     #geom_point(color ="black",alpha=0.9) +
     geom_label() +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw()



# What's success is predictable? ------------------------------------------

# BY DEFENSIVE TEAM
sis_dataset_def_by_game <- sis_dataset_pbp %>% 
     group_by(GameID,DefensiveTeam) %>% 
     summarize(
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType == "pass"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType == "pass"], na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_pass_brk_up = mean(PassBreakupOnPlay, na.rm = TRUE)
     ) %>% 
     group_by(DefensiveTeam) %>% 
     mutate(next_avg_success_rush = lead(avg_success_rush, n = 1L, order_by = DefensiveTeam)) %>% 
     mutate(next_avg_epa_rush = lead(avg_epa_rush, n = 1L, order_by = DefensiveTeam)) %>% 
     mutate(next_avg_success_pass = lead(avg_success_pass, n = 1L, order_by = DefensiveTeam)) %>% 
     mutate(next_avg_epa_pass = lead(avg_epa_pass, n = 1L, order_by = DefensiveTeam)) %>% 
     mutate(next_avg_pressure = lead(avg_pressure, n = 1L, order_by = DefensiveTeam)) %>% 
     mutate(next_avg_pass_brk_up = lead(avg_pass_brk_up, n = 1L, order_by = DefensiveTeam)) 
     #knitr::kable()

# defensive team by game plot (can use next_ type data as well as normal)
sis_dataset_def_by_game %>% 
     ggplot(aes( y = next_avg_pressure, x = avg_pressure )) +
     geom_point(color = "black", alpha=0.9) +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw() + 
     labs(
          title = "By Defensive team"
     )

# BY OFFENSIVE TEAM
sis_dataset_off_by_game <- sis_dataset_pbp %>% 
     group_by(GameID,OffensiveTeam) %>% 
     summarize(
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType == "pass"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType == "pass"], na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_pass_brk_up = mean(PassBreakupOnPlay, na.rm = TRUE)
     ) %>% 
     group_by(OffensiveTeam) %>% 
     mutate(next_avg_success_rush = lead(avg_success_rush, n = 1L, order_by = OffensiveTeam)) %>% 
     mutate(next_avg_epa_rush = lead(avg_epa_rush, n = 1L, order_by = OffensiveTeam)) %>% 
     mutate(next_avg_success_pass = lead(avg_success_pass, n = 1L, order_by = OffensiveTeam)) %>% 
     mutate(next_avg_epa_pass = lead(avg_epa_pass, n = 1L, order_by = OffensiveTeam)) %>% 
     mutate(next_avg_pressure = lead(avg_pressure, n = 1L, order_by = OffensiveTeam)) %>% 
     mutate(next_avg_pass_brk_up = lead(avg_pass_brk_up, n = 1L, order_by = OffensiveTeam)) 
#knitr::kable()

#offensive team by game plot (can use next_ type data as well as normal)
sis_dataset_off_by_game %>% 
     ggplot(aes( y = next_avg_pressure, x = avg_pressure )) +
     geom_point(color = "black", alpha=0.9) +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw() +
     labs(
          title = "By offensive team"
     )

# OFFENSVE VS. DEFENSE (game v game +1)
sis_dataset_off_by_game %>% with(lm(next_avg_pass_brk_up ~ avg_pass_brk_up)) %>% summary()
sis_dataset_def_by_game %>% with(lm(next_avg_pass_brk_up ~ avg_pass_brk_up)) %>% summary()

# in game effectiveness (will be the same for off/def)
sis_dataset_def_by_game %>% with(lm(avg_success_rush ~ avg_pressure)) %>% summary()


# pass success rate ~ sig for offensive by game, but still meh.
# pass epa ~ .2 for off, ~.09 for def
# pressure ~ better for def
# rush success > rush epa, but still P = .3 ~ .2 for both off and def
# pass breakup ~ poor for def, .2 for off... 



#...by team:
# n_rushers ~ more pressure
# idea that it's better to get more pressure with LESS players... how to measure? expected pressure? don't know off alignment


# ***BY PLAY -----------------------------------------------------------------

#  No.of DL/LBs ----------------------------------------------

sis_dataset_pbp %>% 
     # filter(EventType == "rush") %>% 
     group_by(n_def_upfront) %>% 
     summarize(
          n = n(),
          avg_intended_gap = mean(as.numeric(UsedDesignedGap[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType == "pass"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType == "pass"], na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_pass_brk_up = mean(PassBreakupOnPlay, na.rm = TRUE)
     ) %>%
     knitr::kable()

# ANOVA
sis_dataset_pbp %>% 
     filter(EventType == "rush") %>% 
     mutate(n_def_upfront = as.factor(n_def_upfront)) %>% 
     with(aov(Success ~ n_def_upfront)) %>% 
     summary()

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "rush") %>% 
     mutate(n_def_upfront = as.factor(n_def_upfront)) %>% 
     with(TukeyHSD(aov(Success ~ n_def_upfront))) # number of defenders up front doesn't impact success rate

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "rush") %>% 
     mutate(n_def_upfront = as.factor(n_def_upfront)) %>% 
     with(TukeyHSD(aov(UsedDesignedGap ~ n_def_upfront)))  # number of defenders up front doesn't impact rush intended gap


# rush gap importance? ----------------------------------------------------

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "rush") %>% 
     mutate(UsedDesignedGap = as.factor(UsedDesignedGap)) %>% 
     with(TukeyHSD(aov(Success ~ UsedDesignedGap)))  # intended rush gap does matter... diff of success ~ 4.3%, 95% confidence ~ 1.7-6.9%

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "rush") %>% 
     mutate(UsedDesignedGap = as.factor(UsedDesignedGap)) %>% 
     with(TukeyHSD(aov(EPA ~ UsedDesignedGap)))  # EPA not quite significant... P ~ 0.11 (delta ~ 0.0346 epa)


# pressure importance? ----------------------------------------------------

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(PressureOnPlay = as.factor(PressureOnPlay)) %>% 
     with(TukeyHSD(aov(Success ~ PressureOnPlay)))  # pressure matters... diff of success ~ -23.3%!

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(PressureOnPlay = as.factor(PressureOnPlay)) %>% 
     with(TukeyHSD(aov(EPA ~ PressureOnPlay)))  # pressure matters... diff of EPA ~ -0.609

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(PressureOnPlay = as.factor(PressureOnPlay)) %>% 
     with(TukeyHSD(aov(PassBreakupOnPlay ~ PressureOnPlay)))  # pressure doesn't correlate to pass breakups

# TukeyHSD
sis_dataset_pbp %>% 
     filter(!is.na(ThrowDepth)) %>% 
     mutate(PressureOnPlay = as.factor(PressureOnPlay)) %>% 
     with(TukeyHSD(aov(ThrowDepth ~ PressureOnPlay)))  # pressure correlates to throw depth ~ -0.7 yards, P ~ 0.0005

#... can look at scrambles and other plays?!
#... how does it impact success... aDOT?


# n rushers and blitz importance -------------------------------------------------------

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Blitz = as.factor(Blitz)) %>% 
     with(TukeyHSD(aov(Success ~ Blitz)))  # Blitz to success P ~ 0.5

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Blitz = as.factor(Blitz)) %>% 
     with(TukeyHSD(aov(EPA ~ Blitz)))  # Blitz to EPA, P ~ 0.13

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Blitz = as.factor(Blitz)) %>% 
     with(TukeyHSD(aov(PressureOnPlay ~ Blitz)))  # Blitz does lead to more pressure ~ 11.39% more P ~ v small

# n rushers

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     filter(between(n_rushers,2,6)) %>% 
     mutate(n_rushers = as.factor(n_rushers)) %>% 
     with(TukeyHSD(aov(Success ~ n_rushers)))  # number or rushers really isn't important to success rate... at all

# TukeyHSD
sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     filter(between(n_rushers,2,6)) %>%
     mutate(n_rushers = as.factor(n_rushers)) %>% 
     with(TukeyHSD(aov(PressureOnPlay ~ n_rushers)))  # 5-3,6-3,5-4,6-4 are were significant ~ more pressure

sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     group_by(n_rushers) %>% 
     summarize(
          n = n(),
          avg_intended_gap = mean(as.numeric(UsedDesignedGap[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType == "pass"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType == "pass"], na.rm = TRUE),
          avg_pressure = mean(PressureOnPlay, na.rm = TRUE),
          avg_pass_brk_up = mean(PassBreakupOnPlay, na.rm = TRUE)
     ) %>%
     knitr::kable()



# play type ---------------------------------------------------------------
# "rush"        "pass"        "scramble"    "kneel"       "broken Play"
# + "spike" in dataset
# + Shotgun or Hurry-up

#look at averages....
sis_dataset_pbp %>% filter(EventType == "pass") %>% summary() # avg pressure ~ 35%... blitz ~ 11%
sis_dataset_pbp %>% filter(EventType == "pass" & Shotgun == "1") %>% summary() 
sis_dataset_pbp %>% filter(EventType == "pass" & Shotgun == "0") %>% summary() 
sis_dataset_pbp %>% filter(EventType == "scramble") %>% summary() #... all scrambles do not show pressure!

# event type
sis_dataset_pbp %>% 
     mutate(EventType = as.factor(EventType)) %>% 
     with(TukeyHSD(aov(Success ~ EventType)))  # pass /= rush /= scramble

# linear model
sis_dataset_pbp %>% 
     mutate(EventType = as.factor(EventType)) %>% 
     with(lm(Success ~ EventType)) %>% summary()  # scramble > Pass > Rush



#... do certain things create more scrambles?

# shotgun

sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Shotgun = as.factor(Shotgun)) %>% 
     with(TukeyHSD(aov(PressureOnPlay ~ Shotgun)))  # shotgun significant... -4.2% less pressure

sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Shotgun = as.factor(Shotgun)) %>% 
     with(TukeyHSD(aov(Success ~ Shotgun)))  # shotgun ~ success... not quite significant... -2.1%

sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Shotgun = as.factor(Shotgun)) %>% 
     with(TukeyHSD(aov(PassBreakupOnPlay ~ Shotgun)))  # shotgun ~ pass breakup is significant.... +3.68% rate

sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Shotgun = as.factor(Shotgun)) %>% 
     with(TukeyHSD(aov(n_rushers ~ Shotgun)))  # shotgun ~ rusher is significant... -0.28 rushers

sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Shotgun = as.factor(Shotgun)) %>% 
     with(TukeyHSD(aov(n_def_upfront ~ Shotgun)))  # shotgun ~ no. of line defenders is significant... -0.51 defenders up front

sis_dataset_pbp %>% 
     filter(EventType == "pass") %>% 
     mutate(Shotgun = as.factor(Shotgun)) %>% 
     with(TukeyHSD(aov(Blitz ~ Shotgun)))  # shotgun ~ blitz is significant... -0.14 blitz rate












# ***BY POSITION -----------------------------------------------------------

# position by....
# OnFieldPosition ~ DL = hand on ground OR, LB 
# TechniqueName ~ "SideOfBall" (from def. perspective) + location vs. C,G,T,TE 
# IsRushing ~ if player rushed, for running plays, always ZERO


# possible confounding factors:
# ... shotgun
# ... blitz
# ... what types of runs are best?

# how to measure which is most valuable... (did work on team level, which "rusher" is most important... not important for rushing though)
# ... could try to prove that the "positions" aren't different to pressure rate OR success rate [THIS]
# ... > group by (a) on field position. (b) group by roster position
# ... > group by (c) on field DL/LB
# ... > already did (d) group by number of defenders, but could split it out by where they are located on those plays
# ... then look at situations (shotgun or not, blitz, rush location and then see who impacts what?)


# by LB/DL ----------------------------------------------------------------

# by DL or LB
sis_dataset_position1 <- sis_dataset %>%
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
     group_by(OnFieldPosition) %>% 
     summarize(
          n = n(),
          n_rush = sum(EventType == "rush"), 
          n_pass = sum(EventType == "pass"), 
          n_scramble = sum(EventType == "scramble"), 
          n_IsRushing = sum(IsRushing),
          down_split = mean(OnFieldPosition == "DL"),
          rusher_rate = mean(IsRushing[EventType != "rush"], na.rm = TRUE),
          pressure_rate = mean(Pressure[EventType != "rush"], na.rm = TRUE),
          true_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE),
          blitz_participation_rate = mean(Blitz[EventType != "rush"]),
          pass_brk_up_rate = mean(PassBreakup[EventType != "rush"], na.rm = TRUE),
          avg_intended_gap = mean(UsedDesignedGap, na.rm = TRUE),
          avg_tackle_Rate = mean((SoloTackle[EventType == "rush"] | AssistedTackle[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType != "rush"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType != "rush"], na.rm = TRUE)
     ) %>% 
     unique()

sis_dataset_position1 %>% glimpse() # review, looking alright.
sis_dataset %>% glimpse()

sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(OnFieldPosition = as.factor(OnFieldPosition)) %>% 
     with(TukeyHSD(aov(Success ~ OnFieldPosition))) # LB-DL no significance if looking at rushing

sis_dataset %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(OnFieldPosition = as.factor(OnFieldPosition)) %>% 
     with(TukeyHSD(aov(Success ~ OnFieldPosition))) # LB-DL ~ 1.3% success rate, P ~ 0.011

sis_dataset %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(OnFieldPosition = as.factor(OnFieldPosition)) %>% 
     with(TukeyHSD(aov(Pressure ~ OnFieldPosition))) # LB-DL ~ 1% pressure rate rate, P ~ 0.0005251



# by roster position ------------------------------------------------------

# by roster position
sis_dataset_position2 <- sis_dataset %>%
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
     group_by(RosterPosition) %>% 
     summarize(
          n = n(),
          n_rush = sum(EventType == "rush"), 
          n_pass = sum(EventType == "pass"), 
          n_scramble = sum(EventType == "scramble"), 
          n_IsRushing = sum(IsRushing),
          down_split = mean(OnFieldPosition == "DL"),
          rusher_rate = mean(IsRushing[EventType != "rush"], na.rm = TRUE),
          pressure_rate = mean(Pressure[EventType != "rush"], na.rm = TRUE),
          true_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE),
          blitz_participation_rate = mean(Blitz[EventType != "rush"]),
          pass_brk_up_rate = mean(PassBreakup[EventType != "rush"], na.rm = TRUE),
          avg_intended_gap = mean(UsedDesignedGap, na.rm = TRUE),
          avg_tackle_Rate = mean((SoloTackle[EventType == "rush"] | AssistedTackle[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType != "rush"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType != "rush"], na.rm = TRUE)
     ) %>% 
     unique()

sis_dataset %>% glimpse()
sis_dataset_position2 %>% glimpse() # review, looking alright.

# Use in presentation
sis_dataset_position2 %>% arrange(-rusher_rate) %>% 
     select(RosterPosition,n,down_split,rusher_rate,true_pressure_rate,blitz_participation_rate) %>%  knitr::kable()
     #    |RosterPosition |     n| down_split| rusher_rate| true_pressure_rate| blitz_participation_rate|
     #    |:--------------|-----:|----------:|-----------:|------------------:|------------------------:|
     #    |FB             |    28|  1.0000000|   1.0000000|          0.0416667|                0.1666667|
     #    |DT             | 26591|  0.9892821|   0.9902416|          0.0624120|                0.1404673|
     #    |DE             | 24972|  0.8407817|   0.9661135|          0.0997181|                0.1054598|
     #    |LB             | 18981|  0.1996207|   0.7979694|          0.1145137|                0.1546205|
     #    |S              |  1422|  0.0028129|   0.4285714|          0.2457912|                0.4025974|
     #    |CB             |   435|  0.0000000|   0.4120172|          0.2083333|                0.3261803|
     #    |T              |     1|  1.0000000|         NaN|                NaN|                      NaN|
     #    |WR             |     1|  0.0000000|         NaN|                NaN|                      NaN|

sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(Success ~ RosterPosition))) # no significance if looking at rushing success

sis_dataset %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(Success ~ RosterPosition))) # no significance for passing success

sis_dataset %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(Pressure ~ RosterPosition))) # the following were significant
# diff          lwr          upr     p adj
# DT-DE -0.034690094 -0.043816240 -0.025563948 0.0000000
# LB-DT  0.033531366  0.023648849  0.043413884 0.0000000
# S-DT   0.056087019  0.024865785  0.087308253 0.0000046


sis_dataset %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(IsRushing ~ RosterPosition)))
# diff         lwr         upr     p adj
# DE-CB  0.546522691  0.49745343  0.59559195 0.0000000
# DT-CB  0.571049690  0.52196519  0.62013419 0.0000000
# FB-CB  0.580645161  0.42326235  0.73802798 0.0000000
# LB-CB  0.379893685  0.33070211  0.42908526 0.0000000
# S-CB   0.004245766 -0.05190583  0.06039737 0.9999366
# DT-DE  0.024526999  0.01618306  0.03287094 0.0000000
# FB-DE  0.034122471 -0.11564296  0.18388790 0.9871731
# LB-DE -0.166629006 -0.17558135 -0.15767666 0.0000000
# S-DE  -0.542276924 -0.57079596 -0.51375788 0.0000000
# FB-DT  0.009595471 -0.14017495  0.15936589 0.9999721
# LB-DT -0.191156005 -0.20019149 -0.18212052 0.0000000
# S-DT  -0.566803923 -0.59534917 -0.53825868 0.0000000
# LB-FB -0.200751476 -0.35055703 -0.05094593 0.0018639
# S-FB  -0.576399395 -0.72863242 -0.42416637 0.0000000
# S-LB  -0.375647919 -0.40437690 -0.34691894 0.0000000 

sis_dataset %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(Blitz ~ RosterPosition))) # the differences in position blitz rates are significant



# by technique (gap) --------------------------------------------------
# TechniqueName ~ "SideOfBall" (from def. perspective) + location vs. C,G,T,TE 

# by Technique
sis_dataset_position3 <- sis_dataset %>%
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
     group_by(SideOfBall,TechniqueName) %>% 
     summarize(
          n = n(),
          n_rush = sum(EventType == "rush"), 
          n_pass = sum(EventType == "pass"), 
          n_scramble = sum(EventType == "scramble"), 
          n_IsRushing = sum(IsRushing),
          down_split = mean(OnFieldPosition == "DL"),
          rusher_rate = mean(IsRushing[EventType != "rush"], na.rm = TRUE),
          pressure_rate = mean(Pressure[EventType != "rush"], na.rm = TRUE),
          true_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE),
          blitz_participation_rate = mean(Blitz[EventType != "rush"]),
          pass_brk_up_rate = mean(PassBreakup[EventType != "rush"], na.rm = TRUE),
          avg_intended_gap = mean(UsedDesignedGap, na.rm = TRUE),
          avg_tackle_Rate = mean((SoloTackle[EventType == "rush"] | AssistedTackle[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType != "rush"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType != "rush"], na.rm = TRUE)
     ) %>% 
     unique()

# use to create "adjusted pressure rate"
# ... can see who was helped by their location (e.g. true pressure vs. expect)
sis_dataset_position3 %>% arrange(-true_pressure_rate) %>% 
     select(SideOfBall,TechniqueName,n,down_split,rusher_rate,true_pressure_rate,blitz_participation_rate,avg_tackle_Rate,avg_intended_gap,pass_brk_up_rate,avg_success_rush,avg_success_pass) %>%  knitr::kable()
     # |SideOfBall |TechniqueName |    n| down_split| rusher_rate| true_pressure_rate| blitz_participation_rate|
     # |:----------|:-------------|----:|----------:|-----------:|------------------:|------------------------:|
     # |NULL       |Off Ball      | 1796|          0|   0.6729894|          0.1364149|                0.1176024|
     # |L          |Outside       | 9811|          0|   0.7481183|          0.1282171|                0.1809907|
     # |L          |9             | 3984|          1|   0.9885095|          0.1225506|                0.0443204|
     # |R          |9             | 3943|          1|   0.9889890|          0.1224696|                0.0493827|
     # |L          |7             | 1536|          1|   0.9916067|          0.1185006|                0.1127098|
     # |R          |Outside       | 9700|          0|   0.7627680|          0.1030622|                0.1831968|
     # |L          |5             | 1761|          1|   0.9829060|          0.0934783|                0.1538462|
     # |L          |4             | 1206|          1|   0.9973369|          0.0907877|                0.1890812|
     # |R          |4             | 1092|          1|   0.9969419|          0.0904908|                0.2155963|
     # |L          |4i            | 1913|          1|   0.9891068|          0.0895742|                0.1670298|
     # |R          |7             | 1588|          1|   0.9820628|          0.0878995|                0.1031390|
     # |R          |5             | 1755|          1|   0.9901961|          0.0869087|                0.1241830|
     # |L          |6             | 1555|          1|   0.9810298|          0.0856354|                0.1273713|
     # |R          |4i            | 1825|          1|   0.9903382|          0.0772358|                0.1521739|
     # |R          |6             | 1172|          1|   0.9800000|          0.0742115|                0.1236364|
     # |R          |3             | 6119|          1|   0.9911602|          0.0730212|                0.1383978|
     # |L          |3             | 6951|          1|   0.9917653|          0.0672029|                0.1204323|
     # |L          |2             |  939|          1|   0.9855856|          0.0658135|                0.0648649|
     # |NULL       |0             | 2416|          1|   0.9810209|          0.0647098|                0.2316754|
     # |R          |2             |  787|          1|   0.9824945|          0.0601336|                0.0809628|
     # |R          |1             | 3404|          1|   0.9942693|          0.0553314|                0.1381089|
     # |L          |1             | 2965|          1|   0.9934683|          0.0545694|                0.1672110|
     # |L          |2i            | 1790|          1|   0.9845709|          0.0470127|                0.0993250|
     # |R          |2i            | 2423|          1|   0.9889868|          0.0460282|                0.0807636|

sis_dataset %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(Pressure ~ TechniqueName))) # the differences in position blitz rates are significant

sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(Success ~ TechniqueName))) # nothing significant to rush success as a whole

sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(UsedDesignedGap ~ TechniqueName))) # nothing significant to intended rush gap as a whole


# by run location ---------------------------------------------------------

sis_dataset_position4 <- sis_dataset %>%
     filter(EventType %in% c("rush")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
     group_by(RunDirection) %>% 
     summarize(
          n = n(),
          n_rush = sum(EventType == "rush"), 
          down_split = mean(OnFieldPosition == "DL"),
          avg_intended_gap = mean(UsedDesignedGap, na.rm = TRUE),
          avg_tackle_Rate = mean((SoloTackle[EventType == "rush"] | AssistedTackle[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE)
     ) %>% 
     unique()

sis_dataset_position4 %>% arrange(-avg_success_rush) %>%  knitr::kable()

sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(RunDirection = as.factor(RunDirection)) %>% 
     with(TukeyHSD(aov(UsedDesignedGap ~ RunDirection))) # significant differences exist

sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     filter(RunDirection != "Middle") %>% 
     mutate(RunDirection = as.factor(RunDirection)) %>% 
     with(TukeyHSD(aov(Success ~ RunDirection))) # significant differences exist for success and epa (e.g outside runs are better)


# look at runs independent of run direction ( A Gap ) ( Off-Tackle B Gap )  ( Off-Tackle C Gap) ( D Gap )

# success
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "a gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(Success ~ TechniqueName)))

# use intended gap
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "a gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(UsedDesignedGap ~ TechniqueName)))

# tackled made
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "a gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov( (AssistedTackle|SoloTackle) ~ TechniqueName)))

# success
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "b gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(Success ~ TechniqueName)))

# use intended gap
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "b gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(UsedDesignedGap ~ TechniqueName)))

# tackled made
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "b gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov( (AssistedTackle|SoloTackle) ~ TechniqueName)))

# success
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "c gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(Success ~ TechniqueName)))

# use intended gap
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "c gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(UsedDesignedGap ~ TechniqueName)))

# tackled made
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "c gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov( (AssistedTackle|SoloTackle) ~ TechniqueName)))

# success
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "d gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(Success ~ TechniqueName)))

# use intended gap
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "d gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(UsedDesignedGap ~ TechniqueName)))

# tackled made
sis_dataset %>% 
     filter(EventType %in% c("rush")) %>%
     mutate(Run_Gap = ifelse(str_detect(str_to_lower(RunDirection), "d gap"),1,0)) %>% 
     filter(Run_Gap == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov( (AssistedTackle|SoloTackle) ~ TechniqueName)))

# ***BY PLAYER ---------------------------------------------------------------

# what is the "cost" of pressure?
# ... the idea here being that you want pressure with as few as players as possible.
# ... could look at team pressure and X% of player most responsible for it (e.g. the 3rd best rate is most important)

# true pressure rate = rate of pressures to IsRushing status
# expected pressure... rate of pressure based on no. of rushers? divided by no. or rushers to get expected pressure rate?


# what's the distribution of rush rate by position?
# look at pressure rate by position (roster)
# lots of pressure rates dominated by non-regular rushers (e.g. S, CB, LB with low rush rates)


# how can we tell who is good against the run? tackles... rush gap! (but does rush gap changing actually matter?)
# tackle rate... 100%/11 players ~ 9% expected, who tackles more often? than expected?
# rush gap rate ~ but would be good to look at WHERE the player was to see if they affected it.... could create "zones" center, off-center, outside?


# set-up data by player
sis_dataset_player <- sis_dataset %>% 
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles ~ NO pressures! 
     group_by(Name) %>% 
     summarize(
          RosterPosition = RosterPosition,
          team = DefensiveTeam,
          n = n(),
          n_rush = sum(EventType == "rush"), 
          n_pass = sum(EventType == "pass"), 
          n_scramble = sum(EventType == "scramble"), 
          n_IsRushing = sum(IsRushing),
          down_split = mean(OnFieldPosition == "DL"),
          rusher_rate = mean(IsRushing[EventType != "rush"], na.rm = TRUE),
          pressure_rate = mean(Pressure[EventType != "rush"], na.rm = TRUE),
          true_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE),
          blitz_participation_rate = mean(Blitz[EventType != "rush"]),
          pass_brk_up_rate = mean(PassBreakup[EventType != "rush"], na.rm = TRUE),
          avg_intended_gap = mean(UsedDesignedGap, na.rm = TRUE),
          avg_tackle_Rate = mean((SoloTackle[EventType == "rush"] | AssistedTackle[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType != "rush"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType != "rush"], na.rm = TRUE)
     ) %>% 
     unique()

# REVIEW + FIXING: is tackle rate correct? think so
glimpse(sis_dataset)
sis_dataset_player %>% filter(n > 100) %>% arrange(-true_pressure_rate) %>% 
     select(rusher_rate:avg_tackle_Rate) %>% head(20) %>% knitr::kable() 


sis_dataset_player %>% 
     filter(n_IsRushing > 10) %>% 
     with(lm(avg_success_pass ~ true_pressure_rate)) %>% summary #success rate is not correlated to true pressure rate by player

sis_dataset_player %>% 
     filter(n_IsRushing > 10) %>% 
     with(lm(true_pressure_rate ~ blitz_participation_rate)) %>% summary() #true pressure rate is not correlated to blitz participation rate





# Plot
sis_dataset_player %>% 
     filter(n_IsRushing > 20) %>% 
     ggplot(aes( y = true_pressure_rate, x = blitz_participation_rate )) +
     geom_point(color = "black", alpha=0.9) +
     geom_smooth(method=lm,formula = y~x, color="red", fill="grey", se=TRUE) +
     ggpmisc::stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
     theme_bw() +
     labs(title = "By Player")


# team level --------------------------------------------------------------
# what is the "cost" of pressure?
# ... the idea here being that you want pressure with as few as players as possible.
# ... could look at team pressure and X% of player most responsible for it (e.g. the 3rd best rate is most important)

#look at players with over 100 rush attempts? ~ 6?
sis_dataset_player %>% group_by(team) %>% summarize(n = sum(n_rush >50)) %>% arrange(-n) %>% knitr::kable()

# create by team, with best "true" pass rushers
sis_dataset_team_player <- 
     sis_dataset_player %>%
     filter(n_rush >50) %>% 
     group_by(team) %>% 
     summarize(
          R1 = max(true_pressure_rate, na.rm = TRUE),
          R2 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[2]],
          R3 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[3]],
          R4 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[4]],
          R5 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[5]],
          avg = mean(true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[1:5]])
          ) %>% 
     arrange(team) %>% 
     left_join(sis_dataset_team, by = c("team"="DefensiveTeam"))

sis_dataset_team_player %>% glimpse()
sis_dataset_team_player %>% select(team,R1,R2,R3,R4,R5,avg_rushers,blitz_rate,avg_pressure) %>% 
     arrange(avg_pressure) %>% slice(1:10) %>% knitr::kable()

sis_dataset_team %>% filter(DefensiveTeam == "Steelers") %>% knitr::kable()

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ R1+R2+R3+R4+R5)) %>% summary()
     #plot()
     # For only the BEST rusher: 0.08 mult. r-squared
     # 2nd best: 0.16
     # 3rd: 0.25
     # 4th: 0.16
     # 5th: 0.01
     # ... for the formulas, R2 and R3 are almost always best.

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ R2+R3)) %>% summary() # to pressure adj r-sq ~ 0.2459

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ R1+R2+R3+R4+R5)) %>% summary() # but no correlation to success...

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ blitz_rate)) %>% summary() #blitz to pressure adj r-sq ~0.13

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ R2+R3+blitz_rate)) %>% summary() # adj r-sq ~ 0.36!

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ R2+R3+blitz_rate)) %>% summary() # terrible adj r-sq, but... add avg def and...

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ R2+R3+blitz_rate+avg_def)) %>% summary() # on to something here!!!! adj R-squared ~ 0.18!!!

# BEST FOR PRESSURE
sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ R2+R3+blitz_rate+avg_def)) %>% summary() # to pressure it's adj r-sq ~ 0.64!!!!

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ R2+R3+blitz_rate+avg_rushers)) %>% summary() # avg_rushers isn't helpful, much worse

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ avg_def)) %>% summary() # avg def shows no correlation by itself

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ avg_rushers)) %>% summary() # avg rushers shows no correlation by itself

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ avg_def)) %>% summary() # avg def shows no correlation by itself

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ avg_rushers)) %>% summary() # avg rushers shows no correlation by itself

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ R2+R3+avg_def)) %>% summary() # removing blitz rate sucks

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ blitz_rate+avg_def)) %>% summary() # removing R (rate) is okay, adj r-sq ~ 0.14

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ blitz_rate+avg_def)) %>% summary() # ...to pressure it's decent, adj. r-sq ~ 0.24

# BEST FOR PASS SUCCESS
sis_dataset_team_player %>% 
     with(lm(formula = avg_success_pass ~ R2+blitz_rate+avg_def+avg_rushers)) %>% summary() # however, adding avg rushers: adj r-sq ~ 0.22

sis_dataset_team_player %>% 
     with(lm(formula = avg_pressure ~ R2+R3+blitz_rate+avg_def+avg_rushers)) %>% summary() # to pressure: adj r-sq ~ 0.6302
     #... avg_rushers is better for success rate, but not helpful for pressure.

# summary for passing (success / pressure)
# ... best for avg pressure ~ R2+R3+blitz_rate+avg_def (can add avg_rushers, but not helpful)
# ... best for avg passing success ~ R2+blitz_rate+avg_def+avg_rushers (can drop rushers, and it still does pretty good)
# ... interesting that dropping the R3 for passing success is important

# RUSHING (Simple, not location based)

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_rush ~ R2+R3)) %>% summary() # good rushers don't correlate to good rush success

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_rush ~ avg_def)) %>% summary() # avg def doesn't correlate to rush success

sis_dataset_team_player %>% 
     with(lm(formula = avg_success_rush ~ R2+R3+blitz_rate+avg_def+avg_rushers)) %>% summary() # this was the best for pass... no correlation for rushing


#... probably should look at the most common players as well... instead of the only best?


# RUSHING... same idea, but set-up for rushing stats
# ... have tackle rate
# ... have avg intended gap rate
sis_dataset_player %>% glimpse()

#look at players with over 75 plays recorded
sis_dataset_player %>% group_by(team) %>% summarize(n = sum(n>75)) %>% arrange(-n) %>% knitr::kable()

# create by team, with best "true" pass rushers
sis_dataset_team_player2 <- 
     sis_dataset_player %>%
     group_by(team) %>% 
     summarize(
          T1 = avg_tackle_Rate[order(n,decreasing = TRUE)[1]],
          T2 = avg_tackle_Rate[order(n,decreasing = TRUE)[2]],
          T3 = avg_tackle_Rate[order(n,decreasing = TRUE)[3]],
          T4 = avg_tackle_Rate[order(n,decreasing = TRUE)[4]],
          T5 = avg_tackle_Rate[order(n,decreasing = TRUE)[5]],
          T_avg = mean(avg_tackle_Rate[order(n,decreasing = TRUE)[1:5]]),
          G1 = avg_intended_gap[order(n,decreasing = TRUE)[1]],
          G2 = avg_intended_gap[order(n,decreasing = TRUE)[2]],
          G3 = avg_intended_gap[order(n,decreasing = TRUE)[3]],
          G4 = avg_intended_gap[order(n,decreasing = TRUE)[4]],
          G5 = avg_intended_gap[order(n,decreasing = TRUE)[5]],
          G_avg = mean(avg_intended_gap[order(n,decreasing = TRUE)[1:5]]),
     ) %>% 
     arrange(team) %>% 
     left_join(sis_dataset_team, by = c("team"="DefensiveTeam"))

# review (it's working)
sis_dataset_team_player2 %>% glimpse()
# review (it's working)


sis_dataset_team_player2 %>% 
     with(lm(formula = avg_success_rush ~ T1+T2+T3+T4+T5)) %>% summary() # tackle rate not correlated to success (individuals ~ same, none)

sis_dataset_team_player2 %>% 
     with(lm(formula = avg_success_rush ~ G1+G2+G3+G4+G5)) %>% summary() # avg gap rate not correlated to success (individuals ~ same, none)

# create by team, with "best" gap stoppers
sis_dataset_team_player3 <- 
     sis_dataset_player %>%
     filter(n > 75) %>% 
     group_by(team) %>% 
     summarize(
          T1 = avg_tackle_Rate[order(avg_intended_gap,decreasing = TRUE)[1]],
          T2 = avg_tackle_Rate[order(avg_intended_gap,decreasing = TRUE)[2]],
          T3 = avg_tackle_Rate[order(avg_intended_gap,decreasing = TRUE)[3]],
          T4 = avg_tackle_Rate[order(avg_intended_gap,decreasing = TRUE)[4]],
          T5 = avg_tackle_Rate[order(avg_intended_gap,decreasing = TRUE)[5]],
          T_avg = mean(avg_tackle_Rate[order(avg_intended_gap,decreasing = TRUE)[1:5]]),
          G1 = avg_intended_gap[order(avg_intended_gap,decreasing = TRUE)[1]],
          G2 = avg_intended_gap[order(avg_intended_gap,decreasing = TRUE)[2]],
          G3 = avg_intended_gap[order(avg_intended_gap,decreasing = TRUE)[3]],
          G4 = avg_intended_gap[order(avg_intended_gap,decreasing = TRUE)[4]],
          G5 = avg_intended_gap[order(avg_intended_gap,decreasing = TRUE)[5]],
          G_avg = mean(avg_intended_gap[order(avg_intended_gap,decreasing = TRUE)[1:5]]),
     ) %>% 
     arrange(team) %>% 
     left_join(sis_dataset_team, by = c("team"="DefensiveTeam"))

sis_dataset_team_player3 %>% 
     with(lm(formula = avg_success_rush ~ T1+T2+T3+T4+T5)) %>% summary() # tackle rate not correlated to success (individuals ~ same, none)

sis_dataset_team_player3 %>% 
     with(lm(formula = avg_success_rush ~ G1+G2+G3+G4+G5)) %>% summary() # avg gap rate not correlated to success (individals ~ same, none)


# BEST!!!
# re-run from earlier with most common players up front
sis_dataset_team_player4 <- 
     sis_dataset_player %>%
     #filter(n > 75) %>% 
     group_by(team) %>% 
     arrange(-n) %>%
     slice(1:5) %>% 
     summarize(
          R1 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[1]],
          R2 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[2]],
          R3 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[3]],
          R4 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[4]],
          R5 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[5]],
          R_avg = mean(true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[1:5]])
     ) %>% 
     arrange(team) %>% 
     left_join(sis_dataset_team, by = c("team"="DefensiveTeam"))

sis_dataset_team_player4 %>% select(team:R_avg) %>% head(10) %>% knitr::kable()

sis_dataset_team_player4 %>% 
     with(lm(formula = avg_pressure ~ R1+R2+R3+R4+R5)) %>% summary() # adj r-sq ~ 0.3972 

sis_dataset_team_player4 %>% with(lm(formula = avg_pressure ~ R1)) %>% summary() # adj r-sq ~ 0.2855 
sis_dataset_team_player4 %>% with(lm(formula = avg_pressure ~ R2)) %>% summary() # adj r-sq ~  0.2539 
sis_dataset_team_player4 %>% with(lm(formula = avg_pressure ~ R3)) %>% summary() # adj r-sq ~ 0.2139 
sis_dataset_team_player4 %>% with(lm(formula = avg_pressure ~ R4)) %>% summary() # adj r-sq ~ 0.2045 
sis_dataset_team_player4 %>% with(lm(formula = avg_pressure ~ R5)) %>% summary() # adj r-sq ~ -0.01068 
sis_dataset_team_player4 %>% with(lm(formula = avg_pressure ~ R_avg)) %>% summary() # adj r-sq ~ 0.4598 

sis_dataset_team_player4 %>% with(lm(formula = avg_success_pass ~ R1)) %>% summary() # adj r-sq ~ 0.2386 
sis_dataset_team_player4 %>% with(lm(formula = avg_success_pass ~ R2)) %>% summary() # adj r-sq ~ 0.01461  
sis_dataset_team_player4 %>% with(lm(formula = avg_success_pass ~ R3)) %>% summary() # adj r-sq ~ 0.02163 
sis_dataset_team_player4 %>% with(lm(formula = avg_success_pass ~ R4)) %>% summary() # adj r-sq ~  0.003234 
sis_dataset_team_player4 %>% with(lm(formula = avg_success_pass ~ R5)) %>% summary() # adj r-sq ~-0.003986 
sis_dataset_team_player4 %>% with(lm(formula = avg_success_pass ~ R_avg)) %>% summary() # adj r-sq ~ 0.1602 

# Use for presentation
sis_dataset_team_player4 %>% 
     with(lm(formula = avg_pressure ~ R1+R2+R3+R4+blitz_rate+avg_def)) %>% summary() # however, adding avg rushers: adj r-sq ~ 0.618

# Use for presentation
sis_dataset_team_player4 %>% 
     with(lm(formula = avg_success_pass ~ R1+blitz_rate+avg_def)) %>% summary() # however, adding avg rushers: adj r-sq ~ 0.3599 ... can drop avg rusher

# Use for presentation
sis_dataset_team_player4 %>% 
     with(lm(formula = avg_def ~ R2+R3)) %>% summary() # teams don't adjust their avg defenders for DL/EDGE talent?

# Use for presentation
sis_dataset_team_player4 %>% 
     with(lm(formula = blitz_rate ~ R2+R3)) %>% summary() # teams don't adjust their blitz rate for DL/EDGE talent?


#Trying to fix!!!!!!!!! THIS WORKS.
# BEST!!!
# re-run from earlier with most common players up front
sis_dataset_team_player6 <- 
     sis_dataset_player %>%
     group_by(team) %>% 
     arrange(-n) %>%
     slice(1:5) %>% 
     summarize(
          R1 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[1]],
          R2 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[2]],
          R3 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[3]],
          R4 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[4]],
          R5 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[5]],
          R_avg = mean(true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[1:5]])
     ) %>% 
     arrange(team) %>% 
     left_join(sis_dataset_team, by = c("team"="DefensiveTeam"))

sis_dataset_team_player6 %>% select(team:R_avg) %>% head(10) %>% knitr::kable()




# re-run from earlier with most common pass rushers
sis_dataset_team_player5 <- 
     sis_dataset_player %>%
     group_by(team) %>% 
     arrange(-n_rush) %>%
     slice(1:5) %>% 
     summarize(
          R1 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[1]],
          R2 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[2]],
          R3 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[3]],
          R4 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[4]],
          R5 = true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[5]],
          R_avg = mean(true_pressure_rate[order(true_pressure_rate,decreasing = TRUE)[1:5]])
     ) %>% 
     arrange(team) %>% 
     left_join(sis_dataset_team, by = c("team"="DefensiveTeam"))

sis_dataset_team_player5 %>% 
     with(lm(formula = avg_pressure ~ R1+R2+R3+R4+R5)) %>% summary() # adj r-sq ~ 0.3919 

sis_dataset_team_player5 %>% with(lm(formula = avg_pressure ~ R1)) %>% summary() # adj r-sq ~ 0.2651 
sis_dataset_team_player5 %>% with(lm(formula = avg_pressure ~ R2)) %>% summary() # adj r-sq ~ 0.2827 
sis_dataset_team_player5 %>% with(lm(formula = avg_pressure ~ R3)) %>% summary() # adj r-sq ~ 0.1135
sis_dataset_team_player5 %>% with(lm(formula = avg_pressure ~ R4)) %>% summary() # adj r-sq ~ 0.1698  
sis_dataset_team_player5 %>% with(lm(formula = avg_pressure ~ R5)) %>% summary() # adj r-sq ~ 0.1019 
sis_dataset_team_player5 %>% with(lm(formula = avg_pressure ~ R_avg)) %>% summary() # adj r-sq ~  0.4169 

sis_dataset_team_player5 %>% with(lm(formula = avg_success_pass ~ R1)) %>% summary() # adj r-sq ~ -0.007238 
sis_dataset_team_player5 %>% with(lm(formula = avg_success_pass ~ R2)) %>% summary() # adj r-sq ~ 0.007419 
sis_dataset_team_player5 %>% with(lm(formula = avg_success_pass ~ R3)) %>% summary() # adj r-sq ~ -0.02216  
sis_dataset_team_player5 %>% with(lm(formula = avg_success_pass ~ R4)) %>% summary() # adj r-sq ~   -0.02725 
sis_dataset_team_player5 %>% with(lm(formula = avg_success_pass ~ R5)) %>% summary() # adj r-sq ~  -0.005157  
sis_dataset_team_player5 %>% with(lm(formula = avg_success_pass ~ R_avg)) %>% summary() # adj r-sq ~ 0.00806 

sis_dataset_team_player5 %>% 
     with(lm(formula = avg_pressure ~ R1+R2+blitz_rate+avg_def)) %>% summary() # however, adding avg rushers: adj r-sq ~ 0.5582

sis_dataset_team_player5 %>% 
     with(lm(formula = avg_success_pass ~ R1+R2+blitz_rate+avg_def+avg_rushers)) %>% summary() # however, adding avg rushers: adj r-sq ~ 0.1531 


# ADJ. PRESSURE -----------------------------------------------

# Expected pressure model should include:
# ... team position
# ... on field position
# ... blitz, shotgun, etc
# ... *however* at the end of the day, there is not a ton of data here, lean toward data!

# how to model pressure rate (e.g. model by play... then scale out to player average)
# ... start with technique
# ... adjust for player position
# ... adjust for blitz
# ... adjust for shotgun(?)

# by roster position
sis_dataset_position2 %>% arrange(-rusher_rate) %>% 
     select(RosterPosition,n,down_split,rusher_rate,true_pressure_rate,blitz_participation_rate) %>%  knitr::kable()

# by technique
sis_dataset_position3 %>% arrange(-true_pressure_rate) %>% 
     select(SideOfBall,TechniqueName,n,down_split,rusher_rate,true_pressure_rate,
            blitz_participation_rate,avg_tackle_Rate,avg_intended_gap,pass_brk_up_rate,avg_success_rush,avg_success_pass) %>%
     knitr::kable()

sis_dataset %>% filter(EventType == "pass" & Blitz == 1) %>% summary() # average pass play ~ 0.08305 pressure, 0.08906   for blitz

# by technique + roster position
sis_dataset_position5 <- sis_dataset %>%
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
     group_by(TechniqueName,RosterPosition) %>% 
     summarize(
          n = n(),
          n_rush = sum(EventType == "rush"), 
          n_pass = sum(EventType == "pass"), 
          n_scramble = sum(EventType == "scramble"), 
          n_IsRushing = sum(IsRushing),
          down_split = mean(OnFieldPosition == "DL"),
          rusher_rate = mean(IsRushing[EventType != "rush"], na.rm = TRUE),
          pressure_rate = mean(Pressure[EventType != "rush"], na.rm = TRUE),
          true_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE),
          blitz_participation_rate = mean(Blitz[EventType != "rush"]),
          pass_brk_up_rate = mean(PassBreakup[EventType != "rush"], na.rm = TRUE),
          avg_intended_gap = mean(UsedDesignedGap, na.rm = TRUE),
          avg_tackle_Rate = mean((SoloTackle[EventType == "rush"] | AssistedTackle[EventType == "rush"]), na.rm = TRUE),
          avg_success_rush = mean(Success[EventType == "rush"], na.rm = TRUE),
          avg_epa_rush = mean(EPA[EventType == "rush"], na.rm = TRUE),
          avg_success_pass = mean(Success[EventType != "rush"], na.rm = TRUE),
          avg_epa_pass = mean(EPA[EventType != "rush"], na.rm = TRUE)
     ) %>% 
     unique()

sis_dataset_position5 %>% filter(n>20) %>% select(TechniqueName,RosterPosition,n,n_rush,true_pressure_rate,blitz_participation_rate) %>% 
     arrange(-true_pressure_rate) %>% knitr::kable()
# CBs only rush from outside
# DEs from all over
# DT can be all over as well
# S from outside mainly + 9(very few times)
# ***DROP FB, T, WR


# either assume... position is important, or does DE/LB/DT make a difference?
sis_dataset %>% 
     filter(EventType == "pass") %>% 
     filter(RosterPosition %in% c("LB","DE","DT","CB","S")) %>% 
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(IsRushing ~ RosterPosition))) # For IsRushing: LB / DE / DT are all different (but S = CB)


sis_dataset %>% 
     filter(EventType == "pass") %>% 
     filter(RosterPosition %in% c("LB","DE")) %>% 
     filter(TechniqueName == "Outside") %>% 
     filter(IsRushing == 1) %>% 
     mutate(SideOfBall = as.factor(SideOfBall)) %>% 
     with(TukeyHSD(aov(Pressure ~ SideOfBall))) # Left vs. Right Matters across positions as a whole... P ~ 0.0299992
     # but, not for individual positions (0 + off ball don't have location)
     # 1 ~ 0.8266043
     # 2i ~ 0.8614342, 2 ~ 0.6511644
     # 3 ~ 0.4459323 (no 3i)
     # 4i ~ 0.383408, 4 ~ 0.8880022
     # 5 ~ 0.546297
     # 6 ~ 0.4405113
     # 7 ~ 0.0601357
     # 9 ~ 0.9325717
     # outside ~ 0.0007655, R-L -0.02340941... but for CB,S not significant... is driven by LB/DE ~ 0.0006143 (R-L ~ -2.3%)! ... but is this driven by talent or by position?!
          # ... probably need to assume this is driven by TALENT (e.g. def talent = off talent... don't have a good reason to split R v L, but can try)

sis_dataset %>% 
     filter(EventType == "pass") %>% 
     filter(RosterPosition %in% c("LB","DE","DT","CB","S")) %>% 
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(Pressure ~ RosterPosition))) # For pressure... essentially everyone is the same, except for DT... they are odd balls

sis_dataset %>% 
     filter(EventType == "pass") %>% 
     filter(RosterPosition %in% c("LB","DE","DT","CB","S")) %>% 
     filter(IsRushing == 1) %>% 
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(Pressure ~ RosterPosition))) # CB=S, everyone else is different from one another

# looking at technique (true pressure)
sis_dataset %>% 
     filter(EventType == "pass") %>% 
     filter(RosterPosition %in% c("LB","DE","DT","CB","S")) %>% 
     filter(IsRushing == 1) %>% 
     mutate(TechniqueName = as.factor(TechniqueName)) %>% 
     with(TukeyHSD(aov(Pressure ~ TechniqueName))) # 

     # 0
     # 2=1
     # 3=1
     # 4=2 /= 1
     # outside


# building adjusted pressure index
# ... by line position
# ... except for S & CB


# build -------------------------------------------------------------------
exp_pressure_index <- sis_dataset %>%
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
     filter(RosterPosition %in% c("LB","DE","DT")) %>% 
     group_by(TechniqueName) %>% 
     summarize(
          Position = "LB",
          exp_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE)
     ) %>% 
     rbind(
          sis_dataset %>%
               filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
               filter(RosterPosition %in% c("LB","DE","DT")) %>% 
               group_by(TechniqueName) %>% 
               summarize(
                    Position = "DT",
                    exp_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE)
               )
     ) %>% 
     rbind(
          sis_dataset %>%
               filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
               filter(RosterPosition %in% c("LB","DE","DT")) %>% 
               group_by(TechniqueName) %>% 
               summarize(
                    Position = "DE",
                    exp_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE)
               )
     ) %>% 
     rbind(
     sis_dataset %>%
          filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
          filter(RosterPosition %in% c("CB","S")) %>% 
          group_by(TechniqueName) %>% 
          summarize(
               Position = "CB",
               exp_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE)
          )
     ) %>% 
     rbind(
          sis_dataset %>%
               filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles -> NO pressures! 
               filter(RosterPosition %in% c("CB","S")) %>% 
               group_by(TechniqueName) %>% 
               summarize(
                    Position = "S",
                    exp_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE)
               )
     ) %>% 
     mutate(exp_pressure_rate = ifelse(exp_pressure_rate == 1, 0.2327366,exp_pressure_rate))

exp_pressure_index %>% tail(4) %>% gt::gt()
knitr::kable()

# add sis exp_pressure into data...
# add in  exp pressure to sis dataset
sis_dataset_exp_pressure <- sis_dataset %>%
     left_join(exp_pressure_index, by = c("TechniqueName"="TechniqueName","RosterPosition"="Position")) %>% 
     mutate(exp_true_pressure_rate = ifelse(IsRushing == 1,exp_pressure_rate,0)) %>%  
     mutate(adj_true_pressure_rate = ifelse(IsRushing == 1, Pressure - exp_pressure_rate,0))


# set-up data by player w/ expected pressure rate
sis_dataset_player_exp_true_pressure <- sis_dataset_exp_pressure %>% 
     filter(EventType %in% c("rush","pass","scramble")) %>% # filter out kneels and broken plays.... note scrambles ~ NO pressures! 
     group_by(Name) %>% 
     summarize(
          RosterPosition = RosterPosition,
          team = DefensiveTeam,
          n = n(),
          n_rush = sum(EventType == "rush"), 
          n_pass = sum(EventType == "pass"), 
          n_scramble = sum(EventType == "scramble"), 
          n_IsRushing = sum(IsRushing),
          down_split = mean(OnFieldPosition == "DL"),
          rusher_rate = mean(IsRushing[EventType != "rush"], na.rm = TRUE),
          pressure_rate = mean(Pressure[EventType != "rush"], na.rm = TRUE),
          true_pressure_rate = mean(Pressure[IsRushing == 1], na.rm = TRUE),
          exp_true_pressure_rate = mean(exp_true_pressure_rate[IsRushing == 1], na.rm = TRUE),
          adj_true_pressure_rate = mean(adj_true_pressure_rate[IsRushing == 1], na.rm = TRUE),
          blitz_participation_rate = mean(Blitz[EventType != "rush"]),
     ) %>% 
     unique()

# adjusted pressure rate is working v. well.
sis_dataset_player_exp_true_pressure %>% filter(n_rush > 30) %>% 
     select(Name,RosterPosition,team,n_rush,true_pressure_rate,adj_true_pressure_rate) %>%
     arrange(-adj_true_pressure_rate) %>% 
     head(20) %>%
     knitr::kable()

# now check to see if it correlates to stuff, like before (by player, team level)


# corr ExTrPr --------------------------------------------------------------------
# re-run from earlier with most common players up front
sis_dataset_team_player_ETP <- 
     sis_dataset_player_exp_true_pressure %>%
     group_by(team) %>% 
     arrange(-n) %>% 
     slice(1:5) %>% 
     summarize(
          R1 = adj_true_pressure_rate[order(adj_true_pressure_rate,decreasing = TRUE)[1]],
          R2 = adj_true_pressure_rate[order(adj_true_pressure_rate,decreasing = TRUE)[2]],
          R3 = adj_true_pressure_rate[order(adj_true_pressure_rate,decreasing = TRUE)[3]],
          R4 = adj_true_pressure_rate[order(adj_true_pressure_rate,decreasing = TRUE)[4]],
          R5 = adj_true_pressure_rate[order(adj_true_pressure_rate,decreasing = TRUE)[5]],
          R_avg = mean(adj_true_pressure_rate[order(adj_true_pressure_rate,decreasing = TRUE)[1:5]])
     ) %>% 
     arrange(team) %>% 
     left_join(sis_dataset_team, by = c("team"="DefensiveTeam"))

sis_dataset_team_player_ETP %>% 
     with(lm(formula = avg_pressure ~ R1+R2+R3+R4+R5)) %>% summary() # adj r-sq ~ 0.3353   ... not quite as good, but still decent.

sis_dataset_team_player_ETP %>% with(lm(formula = avg_pressure ~ R1)) %>% summary() # adj r-sq ~ 0.1464    
sis_dataset_team_player_ETP %>% with(lm(formula = avg_pressure ~ R2)) %>% summary() # adj r-sq ~ 0.2798 
sis_dataset_team_player_ETP %>% with(lm(formula = avg_pressure ~ R3)) %>% summary() # adj r-sq ~ 0.3347 
sis_dataset_team_player_ETP %>% with(lm(formula = avg_pressure ~ R4)) %>% summary() # adj r-sq ~ 0.3293 
sis_dataset_team_player_ETP %>% with(lm(formula = avg_pressure ~ R5)) %>% summary() # adj r-sq ~ 0.09162   
sis_dataset_team_player_ETP %>% with(lm(formula = avg_pressure ~ R_avg)) %>% summary() # adj r-sq ~ 0.3898  

sis_dataset_team_player_ETP %>% with(lm(formula = avg_success_pass ~ R1)) %>% summary() # adj r-sq ~ 0.1717 
sis_dataset_team_player_ETP %>% with(lm(formula = avg_success_pass ~ R2)) %>% summary() # adj r-sq ~ 0.02803   
sis_dataset_team_player_ETP %>% with(lm(formula = avg_success_pass ~ R3)) %>% summary() # adj r-sq ~  0.01475 
sis_dataset_team_player_ETP %>% with(lm(formula = avg_success_pass ~ R4)) %>% summary() # adj r-sq ~  0.0485
sis_dataset_team_player_ETP %>% with(lm(formula = avg_success_pass ~ R5)) %>% summary() # adj r-sq ~  -0.02659 
sis_dataset_team_player_ETP %>% with(lm(formula = avg_success_pass ~ R_avg)) %>% summary() # adj r-sq ~   0.08685  

# Use for presentation
sis_dataset_team_player_ETP %>% 
     with(lm(formula = avg_pressure ~ R3+blitz_rate+avg_def)) %>% summary() # however, adding avg rushers: adj r-sq ~ 0.6201

# Use for presentation
sis_dataset_team_player_ETP %>% 
     with(lm(formula = avg_success_pass ~ R1+blitz_rate+avg_def+avg_rushers)) %>% summary() # adj r-sq ~ 0.1338 (use above from *BEST* ~ true pressure rate)

# Use for presentation
sis_dataset_team_player_ETP %>% 
     with(lm(formula = avg_def ~ R1+R2)) %>% summary() # teams don't adjust their avg defenders for DL/EDGE talent?

# Use for presentation
sis_dataset_team_player_ETP %>% 
     with(lm(formula = blitz_rate ~ R1+R2)) %>% summary() # teams don't adjust their blitz rate for DL/EDGE talent?




# corr - ExTrPr v other ------------------------------------------------------
sis_dataset_exp_pressure %>% glimpse()


# this should be done on the player level (roster position)
sis_dataset_exp_pressure %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(exp_pressure_rate ~ RosterPosition)))
     # S/CB first, then LB DE DT
     # DT-DE -3.01%
     # LB-DE  1.3%
     # LB-DT  4.4%

# field position (up down)
sis_dataset_exp_pressure %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(OnFieldPosition = as.factor(OnFieldPosition)) %>% 
     with(TukeyHSD(aov(exp_pressure_rate ~ OnFieldPosition)))
     # LB-DL 3.9% more expected true pressure P ~ 0

# 
sis_dataset_exp_pressure %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(RosterPosition = as.factor(RosterPosition)) %>% 
     with(TukeyHSD(aov(adj_true_pressure_rate ~ RosterPosition)))
     # None significant... (e.g. it's working) P ~ 0.13 for LB and DE to DT... so there *could* be something there.

sis_dataset_exp_pressure %>% 
     filter(EventType %in% c("pass")) %>%
     mutate(OnFieldPosition = as.factor(OnFieldPosition)) %>% 
     with(TukeyHSD(aov(adj_true_pressure_rate ~ OnFieldPosition)))
     # Working! (it's v. not significant)


# Talent distribution -----------------------------------------------------
# split by players with rate of inside-to-out

#looking at rush rates
sis_dataset_exp_pressure %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("0","1","2i","2"), "DL_interior","Other")) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("3","4","4i","5"), "DL_edge",DL_position)) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("6","7","9","Outside"), "DL_edge_plus",DL_position)) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("Off Ball"), "DL_offline",DL_position)) %>%
     filter(EventType %in% c("pass")) %>%
     filter(Shotgun == 1) %>% 
     mutate(DL_position = as.factor(DL_position)) %>% 
     with(TukeyHSD(aov(exp_pressure_rate ~ DL_position)))
     # true expected pressure rate is significant across these positions (expected)
     # however rush rate is NOT significant between two 
     # adjusted pressure rate is not significant (meaning it's working)... still working for blitz

#looking at rush rates
sis_dataset_exp_pressure %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("0","1","2i","2"), "DL_interior","Other")) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("3","4","4i","5"), "DL_edge",DL_position)) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("6","7","9","Outside"), "DL_edge_plus",DL_position)) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("Off Ball"), "DL_offline",DL_position)) %>%
     filter(RosterPosition %in% c("DT","DE","LB","S","CB")) %>% 
     group_by(RosterPosition) %>% 
     summarise(
          n = n(),
          Interior_rate = sum(DL_position == "DL_interior") / n(),
          Edge_rate = sum(DL_position == "DL_edge") / n(),
          Edge_plus_rate = sum(DL_position == "DL_edge_plus") / n(),
          Offline_rate = sum(DL_position == "DL_offline") / n()
     ) %>% 
     knitr::kable() # this would make a nice plot


library(ggridges)             # for ridge plot
#looking at rush rates
varplot <- sis_dataset_exp_pressure %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("0","1","2i","2"), "DL_interior","Other")) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("3","4","4i","5"), "DL_edge",DL_position)) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("6","7","9","Outside"), "DL_edge_plus",DL_position)) %>% 
     mutate(DL_position = ifelse(TechniqueName %in% c("Off Ball"), "DL_offline",DL_position)) %>%
     filter(RosterPosition %in% c("DT","DE","LB")) %>% 
     group_by(Name,DL_position) %>% 
     summarize(
          n = n(),
          adj_true_pressure_rate = mean(adj_true_pressure_rate[IsRushing == 1],na.rm = 0),
          ) %>% 
     filter(n > 20) 
     
varplot %>% 
     ggplot(aes(x = adj_true_pressure_rate, y = DL_position, fill = DL_position)) +
     geom_density_ridges() +
     theme_ridges() + 
     labs(fill = "",
          title = "Talent Distribution of Defensive Linemen", 
          subtitle = "EDGE, and EDGE plus players show more variation in talent than interior players",                 # << Input description
          x = "Adjusted True Pressure Rate",
          y = "Position", 
          caption ="Created by @rbkeeney.")

varplot %>% 
     group_by(DL_position) %>% 
     summarize(
          mean = mean(adj_true_pressure_rate,na.rm = TRUE),
          sd = sd(adj_true_pressure_rate, na.rm = T),
          var = sd/mean
     )
