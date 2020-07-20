library(tidyverse)
library(ggplot2)
library(ggrepel)
library(gridExtra)


#import data
data <- read_csv("AnalyticsChallenge2020Data.csv") %>% 
  janitor::clean_names()

#CHange "NULL" to NA
is.na(data) <- data == "NULL"


#EDA - check for missing values
skimr::skim(data)

data$game_id <- factor(data$game_id)
data$season <- factor(data$season)
data$week <- factor(data$week)
data$down <- factor(data$down)
data$quarter <- factor(data$quarter)


data$used_designed_gap <- as.numeric(as.character(data$used_designed_gap))
data$pressure_on_play <- as.numeric(as.character(data$pressure_on_play))


#create a formation column using the technique name
formation_data <- data %>% 
  group_by(game_id, event_id) %>% 
  summarize(formation_technique = paste(sort(technique_name), collapse=", "))

#create a formation column using the roster position
formation_data2 <- data %>% 
  group_by(game_id, event_id) %>% 
  summarize(formation_position = paste(sort(roster_position), collapse=", "))

#create left/right technique pairing
l_r_technique <- data %>% 
  group_by(game_id, event_id, side_of_ball) %>% 
  summarize(technique_side = paste(sort(technique_name), collapse=", "),
            formation_side = paste(sort(roster_position), collapse=", ")) %>% 
  pivot_wider(names_from = side_of_ball, values_from = c(technique_side, formation_side))

#Create number of rushers and blitzers
num_of_rushers <- data %>% 
  group_by(game_id, event_id, event_type) %>% 
  summarize(num_of_rushers = sum(is_rushing, na.rm = TRUE),
            num_of_players = n()) %>% 
  mutate(blitz = factor(ifelse(num_of_rushers > 4 | (num_of_players > 4 & event_type == 'rush'), 'Blitz','No Blitz')),
         isBlitz = factor(ifelse(num_of_rushers > 4 | (num_of_players > 4 & event_type == 'rush'), 1,0))) %>% 
  select(-event_type)


#joing the formation value to the original dataset
data2 <- data %>% 
  left_join(formation_data, by = c("game_id","event_id")) %>%
  left_join(formation_data2, by = c("game_id","event_id")) %>%  
  left_join(l_r_technique, by = c("game_id","event_id")) %>%
  left_join(num_of_rushers, by = c("game_id","event_id"))

run_pass_epa <- data2 %>% 
  group_by(event_type, blitz) %>% 
  summarize(event_epa = mean(epa, na.rm = TRUE) )

data2 <- data2 %>% 
  left_join(run_pass_epa, by = c("event_type","blitz")) %>%
  mutate(success = ifelse(epa > 0, 1, 0),
         adj_epa = epa - event_epa,
         adj_success = ifelse(adj_epa > 0, 1, 0),
         pos_tech = paste(roster_position,technique_name, sep = "_"),
         DE = ifelse(roster_position == "DE", 1,0),
         DT = ifelse(roster_position == "DT", 1,0),
         LB = ifelse(roster_position == "LB", 1,0),
         four_i = ifelse(technique_name == "4i", 1,0))


#Position Rates
rate_data <- data2 %>% 
  group_by(roster_position, technique_name) %>% 
  summarise(mean_epa = mean(epa, na.rm = TRUE),
            mean_adj_epa = mean(adj_epa, na.rm = TRUE),
            n = n(),
            success_rate = mean(success, na.rm = TRUE),
            adj_success_rate = mean(adj_success, na.rm = TRUE),
            disrupt_rate = mean(used_designed_gap, na.rm = TRUE),
            pressure_rate = mean(pressure, na.rm = TRUE)) %>% 
  filter(roster_position != "S" & roster_position != "CB") %>% 
  unite(pos_tech,roster_position:technique_name, sep = "_", remove = FALSE) %>% 
  filter(n >= 300) %>% 
  arrange(mean_epa)



##################################################################
#POSITION GRAPHS

##############
##Distribution of talent across dline


#Positions by Down
down_dist <- data2 %>% 
  group_by(down, roster_position) %>% 
  summarise(n = n()) %>% 
  filter(roster_position != "FB" & roster_position != "WR" & roster_position != "T")

ggplot(down_dist, aes(x=down, y=n)) +
  geom_bar(aes(fill = roster_position), position = "dodge",stat="identity", width=0.5) +
  labs(title = "How Often Each Position is Lined Up on the Defensive Line",
       fill = "Roster Position") +
  xlab("Down") + 
  theme_minimal() +
  ylab("Total Number of Appearances") +
theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"))





#Regular EPA by roster position bar chart
a1 <- ggplot(event_dist, aes(x=reorder(roster_position, mean_epa), y=mean_epa, fill=roster_position)) +
  geom_bar(stat="identity", width=0.5)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "EPA by Roster Position") +
  xlab("Roster Position") + 
  ylab("Mean EPA") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"))


# epa and success rate
a2 <- ggplot(rate_data, aes(x=mean_epa, y=success_rate,  col = roster_position)) +
  geom_point(aes(size=n),alpha = .5) + 
  theme_minimal() +
  #  geom_text_repel(aes(label = as.character(pos_tech))) +
  geom_text_repel(aes(label = ifelse(success_rate < .43,as.character(pos_tech),""))) +
  labs(title = "Position & Techique Pairing",
       col = "Roster Position") + 
  xlab("Mean EPA") + 
  ylab("Success Rate") +
  guides(size=FALSE) +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"))

grid.arrange(a1, a2, ncol=2)





#Adjusted EPA by Roster Position
event_dist <- data2 %>% 
  group_by(roster_position) %>% 
  summarise(n = n(),
            mean_epa = mean(epa, na.rm = TRUE),
            mean_adj_epa = mean(adj_epa, na.rm = TRUE)
  ) %>% 
  # filter(roster_position != "FB" & roster_position != "WR" & roster_position != "T")
  filter(!roster_position %in% c("FB", "WR", "T", "CB", "S"))


#adj epa and success rate
ggplot(rate_data, aes(x=mean_adj_epa, y=adj_success_rate,  col = roster_position)) +
  geom_point(aes(size=n),alpha = .5) + 
  theme_minimal() +
  #  geom_text_repel(aes(label = as.character(pos_tech))) +
  geom_text_repel(aes(label = ifelse(mean_adj_epa < -0.025 | adj_success_rate < .445,as.character(pos_tech),""))) +
  labs(title = "Position & Techique Pairing",
       col = "Roster Position") + 
  xlab("Adjusted EPA") + 
  ylab("Adjusted Success Rate") +
  guides(size=FALSE) +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"))

#Adjusted EPA by roster position bar chart
ggplot(event_dist, aes(x=reorder(roster_position, mean_adj_epa), y=mean_adj_epa, fill=roster_position)) +
  #  geom_bar(aes(fill = event_type), position = "dodge",stat="identity", width=0.5)
  geom_bar(stat="identity", width=0.8)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Adj. EPA by Roster Position") +
  xlab("Roster Position") + 
  ylab("Mean Adjusted EPA")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"))


#remove CB and S
rate_data2 <- rate_data %>% 
  filter(roster_position != "CB" & roster_position != "S")

#Showing ADJ EPA for each technique broken down by position
ggplot(rate_data2, aes(x=reorder(technique_name, technique_name), y=mean_adj_epa, fill=roster_position)) +
    geom_bar(aes(fill = roster_position), position = "dodge",stat="identity", width=0.5) +
#  geom_bar(stat="identity", width=0.8)+
  theme_minimal() +
#  theme(legend.position = "none") +
  labs(title = "Roster Position by Technique",
       fill = "Roster Position") +
  xlab("Technique") + 
  ylab("Mean Adjusted EPA")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"))




##Create +/- metrics where posioin is on the field vs off

plus_minus <- data2 %>% 
  group_by(game_id, event_id) %>% 
  summarise(DE_on = ifelse(sum(DE) > 0 , "On-field", "Off-field"),
            DT_on = ifelse(sum(DT) > 0 , "On-field", "Off-field"),
            LB_on = ifelse(sum(LB) > 0 , "On-field", "Off-field")) %>% 
  rename(DE = DE_on,
         DT = DT_on,
         LB = LB_on) %>% 
  pivot_longer(DE:LB, names_to = "roster_position", values_to = "split") %>% 
  ungroup()

plus_minus <- plus_minus %>% 
  left_join(select(data2, c(adj_epa, game_id,event_id)), by = c("game_id","event_id")) %>% 
  distinct() %>% 
  group_by(roster_position, split) %>% 
  summarise(mean_adj_epa = mean(adj_epa, na.rm = TRUE),
            n = n()) %>% 
  mutate(percent_plays = n / sum(n),
         percent_plays = scales::percent(percent_plays))



ggplot(plus_minus, aes(x=roster_position, y=n, fill = split)) +
  geom_bar(aes(fill = split), position = "dodge",stat="identity", width=0.5) +
  labs(title = "+/- Roster Distribution") +
  xlab("Roster Position") + 
  theme_minimal() +
  ylab("Total # of Plays") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"))





#Showing +/- ADJ EPA by position
ggplot(plus_minus, aes(x=reorder(roster_position, roster_position), y=mean_adj_epa, fill=split)) +
  geom_bar(aes(fill = split), position = "dodge",stat="identity", width=0.5) +
  #  geom_bar(stat="identity", width=0.8)+
  theme_minimal() +
  #  theme(legend.position = "none") +
  labs(title = "+/- Adj. EPA by Roster Position",
       fill = "Split") +
  xlab("Position") + 
  ylab("Mean Adjusted EPA")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"))







####    4i technique usage

four_i <- data2 %>% 
  group_by(game_id, event_id) %>% 
  summarise(four_i_usage = ifelse(sum(four_i)> 0,1,0)) %>% 
  ungroup() 

  mean(four_i$four_i_usage)

data2 %>% 
  filter(game_id == 2694 & event_id == 925) %>% 
  select(roster_position, technique_name, four_i)
  



   