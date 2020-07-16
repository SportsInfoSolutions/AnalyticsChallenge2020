library(tidyverse)
library(readr)
library(RCurl)


x <- getURL("https://raw.githubusercontent.com/keegan-abdoo/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv")
pbp <- read.csv(text = x, na = c("NULL", "NA"))


# #Bind DL Technique and side of field
# pbp <- pbp %>% mutate(TechniqueName = 
#                         replace(TechniqueName, SideOfBall %in% c("L","R"), 
#                                 paste0(SideOfBall[SideOfBall %in% c("L","R")], TechniqueName[SideOfBall %in% c("L","R")])))

# Look at pressure rates from edge players vs interior players
pbp <- pbp %>% mutate(EdgeRusher = if_else(TechniqueName %in% 
                                             c("7", "Outside", "9", "6", "5") & IsRushing == 1, 1, 0))


DEDT <- pbp %>% filter (EventType == "pass" & IsRushing == 1) %>% group_by(PlayerId) %>% 
  summarise(Player = paste(unique(Name), collapse = '-'),
            Position = paste(unique(RosterPosition), collapse = '-'),
            RushSnaps = n(), Pressure = sum(Pressure), 
            PressureRate = Pressure/RushSnaps, meanEPA = mean(EPA),
            EdgeRate = sum(EdgeRusher)/RushSnaps,
            EPA = sum(EPA)) %>% ungroup() %>% filter(RushSnaps >= 100)


ggplot(data = DEDT, aes(x = PressureRate)) + 
  geom_density(data = filter(DEDT, EdgeRate >= .50), aes(fill = "Edge", alpha = 0.05)) + 
  geom_density(data = filter(DEDT, EdgeRate <= .50), aes(fill = "Interior", alpha = 0.05)) + 
  scale_fill_manual(values = c("red","blue"))

#How pressure from the edge vs interior affects the play
Pressures <- pbp %>% filter (EventType == "pass") %>%
  mutate(EdgePressure = if_else(TechniqueName %in% c("7", "Outside", "9", "6", "5") & Pressure == 1, 1, 0),
         InteriorPressure = if_else(TechniqueName %in% c("1", "0", "3", "4", "2", "4i", "2i") & 
                                      Pressure == 1, 1, 0)) %>% group_by(GameID, EventID) %>%
  mutate(EdgePressure = max(EdgePressure), InteriorPressure = max(InteriorPressure)) %>%
  summarise(EdgePressure = mean(EdgePressure),
            InteriorPressure = mean(InteriorPressure),
            Complete = paste(unique(Completion), collapse = "-"),
            OffensiveYardage = mean(OffensiveYardage),
            EPA = mean(EPA), success = mean(success))

ggplot(data = Pressures, aes(x = EPA)) +
  geom_density(data = filter(Pressures, InteriorPressure == 1 & EdgePressure == 0),
               aes(fill = "Interior Pressure", alpha = 0.05)) +
  geom_density(data = filter(Pressures, InteriorPressure == 0 & EdgePressure == 1),
               aes(fill = "Edge Pressure", alpha = 0.05)) +
  geom_density(data = filter(Pressures, InteriorPressure == 0 & EdgePressure == 0),
               aes(fill = "No Pressure", alpha = 0.05)) +
  # geom_density(data = filter(Pressures, InteriorPressure == 1 & EdgePressure == 1),
  #              aes(fill = "Edge + Interior Pressure", alpha = 0.05)) +
  scale_fill_manual(values = c("red","blue", "green", "yellow"))



#Look at pressure rates by technique - league wide

techs <- pbp %>% filter (EventType == "pass" & IsRushing == 1) %>% group_by(tech_side) %>% 
  summarise(RushSnaps = n(), Pressure = sum(Pressure), 
            PressureRate = Pressure/RushSnaps, meanEPA = mean(EPA),EPA = sum(EPA)) %>% 
  ungroup()


ggplot(data = techs , aes(x = tech_side, y = PressureRate)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  scale_x_discrete(limits = c("LOLB", "L9","L6", "L7", "L5", "L4", "L4i", "L3", "L2", "L2i", "L1", "0",
                              "R1", "R2i", "R2", "R3", "R4i", "R4", "R5", "R7", "R6", "R9", "ROLB"))



#Look at whether the presence of 0/1 tech defenders affect the outcome of inside runs

#Create column with DL alignments for each play vs run 

dfrun <- pbp %>% filter (EventType == "rush") %>% group_by(GameID, EventID) %>%
  mutate(in_designed_gap = max(in_designed_gap)) %>% 
  summarise(Quarter = mean(Quarter), TimeLeft = mean(TimeLeft),
            Down = mean(Down), ToGo = mean(ToGo), StartYard = mean(yardline_100),
            OffensiveYardage = mean(OffensiveYardage), EPA = mean(EPA), success = mean(success),
            RunDirection= paste(unique(RunDirection), collapse = '-'),
            UsedDesignedGap = paste(unique(UsedDesignedGap), collapse = '-'),
            in_designed_gap = paste(unique(in_designed_gap), collapse = '-'),
            PlayDesc = paste(unique(PlayDesc), collapse = '-'),
            DL = paste(unique(tech_side), collapse = '-'))

#Remove kneeldowns

dfrun <- dfrun %>% filter(!str_detect(PlayDesc, "kneels"))


#Compare distribution of EPA and Offensive Yards Gained and success rate
#of plays with a defender in the run gap and plays without

ggplot(data = dfrun, aes(x = EPA)) + 
  geom_density(data = filter(dfrun, in_designed_gap == 1 & UsedDesignedGap == 1), 
               aes(fill = "Defender Present", alpha = 0.05)) + 
  geom_density(data = filter(dfrun, in_designed_gap == 0 & UsedDesignedGap == 1), 
               aes(fill = "Defender not-present", alpha = 0.05)) + 
  scale_fill_manual(values = c("red","blue"))

ggplot(data = dfrun, aes(x = OffensiveYardage)) + 
  geom_density(data = filter(dfrun, in_designed_gap == 1 & UsedDesignedGap == 1), 
               aes(fill = "Defender Present", alpha = 0.05)) + 
  geom_density(data = filter(dfrun, in_designed_gap == 0 & UsedDesignedGap == 1), 
               aes(fill = "Defender not-present", alpha = 0.05)) + 
  scale_fill_manual(values = c("red","blue"))


ggplot(data = filter(dfrun, in_designed_gap == 1) , aes(x = success)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue") + theme_minimal() +
  scale_x_discrete(limits = c(0,1)) + scale_y_continuous(breaks = seq(0.05, 0.60, 0.05))

