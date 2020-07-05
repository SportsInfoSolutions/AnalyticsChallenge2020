library(tidyverse)
library(readr)
library(RCurl)


x <- getURL("https://raw.githubusercontent.com/keegan-abdoo/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv")
pbp <- read.csv(text = x)
#Import play-by-play data

#Convert starting yard line to 0-100 format
pbp <- pbp %>% filter(!is.na(StartYard)) %>% 
  mutate(StartYard100 = replace(StartYard, SideOfField == "Oppo", (100 - StartYard[SideOfField == "Oppo"])))


#Bind DL Technique and side of field
pbp <- pbp %>% mutate(TechniqueName = 
                        replace(TechniqueName, SideOfBall %in% c("L","R"), 
                                paste0(SideOfBall[SideOfBall %in% c("L","R")], TechniqueName[SideOfBall %in% c("L","R")])))


#Create column with DL alignments for each play
df <- pbp %>% group_by(GameID, EventID) %>%
  summarise(Quarter = mean(Quarter), TimeLeft = mean(TimeLeft),
            Down = mean(Down), ToGo = mean(ToGo), StartYard = mean(StartYard100),
            Yards = mean(OffensiveYardage), EPA = mean(EPA), 
            DL = paste(unique(TechniqueName), collapse = '-'))

players <- pbp %>% group_by(PlayerId) %>% 
  summarise(Snap = n(), meanEPA = mean(EPA), 
            EPA = sum(EPA), Player = paste(unique(Name), collapse = '-')) %>% ungroup()

techs <- pbp %>% group_by(TechniqueName, EventType) %>% 
  summarise(Snap = n(), meanEPA = mean(EPA), 
            EPA = sum(EPA)) %>% ungroup()


