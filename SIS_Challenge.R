library(tidyverse)
library(readr)
library(RCurl)


x <- getURL("https://raw.githubusercontent.com/keegan-abdoo/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv")
pbp <- read.csv(text = x)


#Convert starting yard line to 0-100 format
pbp <- pbp %>% filter(!is.na(StartYard)) %>% 
  mutate(StartYard100 = replace(StartYard, SideOfField == "Oppo", (100 - StartYard[SideOfField == "Oppo"])))


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
            OffensiveYardage = mean(OffensiveYardage),
            EPA = mean(EPA))

ggplot(data = Pressures, aes(x = EPA)) +
  geom_density(data = filter(Pressures, InteriorPressure == 1 & EdgePressure == 0),
               aes(fill = "Interior Pressure", alpha = 0.05)) +
  geom_density(data = filter(Pressures, InteriorPressure == 0 & EdgePressure == 1),
               aes(fill = "Edge Pressure", alpha = 0.05)) +
  geom_density(data = filter(Pressures, InteriorPressure == 0 & EdgePressure == 0),
               aes(fill = "No Pressure", alpha = 0.05)) +
  geom_density(data = filter(Pressures, InteriorPressure == 1 & EdgePressure == 1),
               aes(fill = "Edge + Interior Pressure", alpha = 0.05)) +
  scale_fill_manual(values = c("red","blue", "green", "yellow"))


#Look at whether the presence of 0/1 tech defenders affect the outcome of inside runs

#Create column with DL alignments for each play vs run 

dfrun <- pbp %>% filter (EventType == "rush") %>% group_by(GameID, EventID) %>%
  summarise(Quarter = mean(Quarter), TimeLeft = mean(TimeLeft),
            Down = mean(Down), ToGo = mean(ToGo), StartYard = mean(StartYard100),
            OffensiveYardage = mean(OffensiveYardage), EPA = mean(EPA),
            RunDirection= paste(unique(RunDirection), collapse = '-'),
            UsedDesignedGap = paste(unique(UsedDesignedGap), collapse = '-'),
            PlayDesc = paste(unique(PlayDesc), collapse = '-'),
            DL = paste(unique(TechniqueName), collapse = '-'))

#Create two datasets, one with all A Gap runs with at least one 1 or 0 tech is present and the other
#without either
tech1 <- dfrun %>% filter(str_detect(DL, c("(R1)|(L1)|(0)")) & str_detect(RunDirection, c("(A Gap)")) & 
                            UsedDesignedGap == 1)
tech1out <- dfrun %>% filter(!str_detect(DL, c("(R1)|(L1)|(0)")) & str_detect(RunDirection, c("(A Gap)")) & 
                               UsedDesignedGap == 1)


#Compare distribution of EPA and Offensive Yards Gained of the two sub groups
ggplot(data = tech1, aes(x = EPA)) + 
  geom_density(data = tech1, aes(fill = "0/1 Tech present", alpha = 0.05)) + 
  geom_density(data = tech1out, aes(fill = "0/1 Tech not-present", alpha = 0.05)) + 
  scale_fill_manual(values = c("red","blue"))

ggplot(data = tech1, aes(x = OffensiveYardage)) + 
  geom_density(data = tech1, aes(fill = "0/1 Tech present", alpha = 0.005)) + 
  geom_density(data = tech1out, aes(fill = "0/1 Tech not-present", alpha = 0.005)) + 
  scale_fill_manual(values = c("red","blue"))

