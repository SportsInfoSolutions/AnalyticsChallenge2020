library(tidyverse)
library(readr)
library(RCurl)
library(glue)

pbp <- read_csv("Data/AnalyticsChallenge2020Data.csv",
                na = c("NULL", "NA"))

# Clean Data
pbp_clean <- pbp %>%
  # Remove kneels and spikes
  filter(!str_detect(PlayDesc, "kneel") & !str_detect(PlayDesc, "spike")) %>%
  # Classify Scrambles using SIS data (more reliable than GSIS-provided play descriptions)
  left_join(pbp %>%
              group_by(GameID, EventID) %>%
              summarize(run = max(if_else(str_detect(EventType, "designed run"), 1, 0)),
                        pass = max(if_else(str_detect(EventType, "pass"), 1, 0)),
                        pass_rush = max(if_else(IsRushing == 1, 1, 0)),
                        PlayDesc = paste(unique(PlayDesc), collapse = '-')) %>%
              filter((run == 1 & pass_rush == 1)|str_detect(PlayDesc, "scramble")) %>%
              select(GameID, EventID) %>%
              mutate(scramble = 1)) %>%
  mutate(scramble = replace_na(scramble, 0),
         event_type = if_else(str_detect(EventType, "pass") | scramble == 1, "dropback", "designed run"),
         yardline_100 = if_else(SideOfField == "Oppo", StartYard, 100 - StartYard),
         tech_side = case_when(TechniqueName %in% c("0", "Off Ball") ~ TechniqueName,
                               TechniqueName == "Outside" ~ as.character(glue("{SideOfBall}OLB")),
                               TRUE ~ as.character(glue("{SideOfBall}{TechniqueName}"))),
         in_designed_gap = case_when(event_type == "dropback" ~ NA_real_,
                                     RunDirection == "Middle" ~ 
                                       if_else(tech_side %in% c("0", "L1", "R1", "L2i", "R2i"), 1, 0),
                                     RunDirection == "Right A Gap" ~ 
                                       if_else(tech_side %in% c("0", "L1", "L2", "L2i"), 1, 0),
                                     RunDirection == "Left A Gap" ~ 
                                       if_else(tech_side %in% c("0", "R1", "R2", "R2i"), 1, 0),
                                     RunDirection == "Right Off-Tackle B Gap" ~ 
                                       if_else(tech_side %in% c("L2", "L3", "L4", "L4i"), 1, 0),
                                     RunDirection == "Left Off-Tackle B Gap" ~ 
                                       if_else(tech_side %in% c("R2", "R3", "R4", "R4i"), 1, 0),
                                     RunDirection == "Right Off-Tackle C Gap" ~ 
                                       if_else(tech_side %in% c("L4", "L5", "L7", "L6"), 1, 0),
                                     RunDirection == "Left Off-Tackle C Gap" ~ 
                                       if_else(tech_side %in% c("R4", "R5", "R7", "R6"), 1, 0),
                                     RunDirection == "Right D Gap" ~ 
                                       if_else(tech_side %in% c("L6", "L9", "LOLB"), 1, 0),
                                     RunDirection == "Left D Gap" ~ 
                                       if_else(tech_side %in% c("R6", "R9", "ROLB"), 1, 0)),
         run_force = if_else(UsedDesignedGap == 0 & in_designed_gap == 1, 1, 0)
         success = if_else(EPA > 0, 1, 0),
         position_group = case_when(TechniqueName %in% c("7", "Outside", "9", "6", "5") ~ "Edge",
                                    TechniqueName == "Off Ball" ~ TechniqueName,
                                    TRUE ~ "iDL"))

pbp_clean %>%
  filter(event_type == "designed run" & UsedDesignedGap == 0) %>%
  mutate(TimeLeft = glue("{floor(TimeLeft/60)}:{TimeLeft - floor(TimeLeft/60)*60}")) %>%
  select(Week, OffensiveTeam, Quarter, TimeLeft, Down, ToGo, RunDirection) %>%
  distinct()

# # Look at pressure rates from edge players vs interior players
DEDT <- pbp_clean %>% 
  filter (event_type == "dropback" & IsRushing == 1) %>% 
  group_by(PlayerId) %>% 
  summarise(Player = paste(unique(Name), collapse = '-'),
            Position = paste(unique(RosterPosition), collapse = '-'),
            RushSnaps = n(), 
            Pressure = sum(Pressure), 
            PressureRate = Pressure/RushSnaps, 
            meanEPA = mean(EPA),
            EdgeRate = sum(if_else(position_group == "Edge", 1, 0))/RushSnaps,
            EPA = sum(EPA)) %>% 
  ungroup() %>% 
  filter(RushSnaps >= 100) %>%
  mutate(dl_pos = as.factor(if_else(EdgeRate >= 0.5, "Edge", "iDL")))

DEDT %>%
  ggplot(aes(x = PressureRate, fill = dl_pos)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.23), expand = c(0, 0)) +
  labs(x = "Pressure Rate",
       y = "Density",
       title = "Distribution of Player Pressure Rates by Position Group",
       fill = "Position Group") +
  theme_bw() + 
  theme(legend.position = c(0.915, 0.885),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, margin = margin(5, 0, 10, 0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold",
                                  margin = margin(b = 8, unit = "pt")),
        plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic",
                                     margin = margin(b = 16, unit = "pt")),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# How pressure from the edge vs interior affects the play
Pressures <- pbp_clean %>% 
  filter(event_type == "dropback") %>%
  mutate(edge_pressure = if_else(position_group == "Edge" & Pressure == 1, 1, 0),
         idl_pressure = if_else(position_group == "iDL" & Pressure == 1, 1, 0)) %>% 
  group_by(GameID, EventID) %>%
  summarise(edge_p = max(edge_pressure),
            idl_p = max(idl_pressure),
            Complete = paste(unique(Completion), collapse = "-"),
            OffensiveYardage = mean(OffensiveYardage),
            EPA = mean(EPA), 
            success = mean(success)) %>%
  mutate(pressure_type = case_when(edge_p == 1 & idl_p == 1 ~ "Both",
                                   edge_p == 1 ~ "Edge",
                                   idl_p == 1 ~ "iDL",
                                   TRUE ~ "No Pressure"))

Pressures %>%
  filter(pressure_type != "No Pressure") %>%
  ggplot(aes(x = EPA, fill = pressure_type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue", 
                               #"green", 
                               "yellow")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
  labs(x = "EPA",
       y = "Density",
       title = "Distribution of EPA by Pressure Type",
       fill = "Pressure Type") +
  theme_bw() + 
  theme(legend.position = c(0.915, 0.885),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, margin = margin(5, 0, 10, 0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold",
                                  margin = margin(b = 8, unit = "pt")),
        plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic",
                                     margin = margin(b = 16, unit = "pt")),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))



#Look at pressure rates by technique - league wide

techs <- pbp_clean %>% 
  filter(event_type == "dropback" & IsRushing == 1) %>% 
  group_by(tech_side) %>% 
  summarise(RushSnaps = n(), 
            Pressure = sum(Pressure), 
            PressureRate = Pressure/RushSnaps, 
            meanEPA = mean(EPA),
            EPA = sum(EPA)) %>% 
  ungroup()

tech_names <- c("LOLB", "L9","L6", "L7", "L5", "L4", "L4i", "L3", "L2", "L2i", "L1", "0",
                "R1", "R2i", "R2", "R3", "R4i", "R4", "R5", "R7", "R6", "R9", "ROLB")

techs %>%
  filter(tech_side != "Off Ball") %>%
  ggplot(aes(x = tech_side, y = PressureRate)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  scale_x_discrete(limits = tech_names, expand = c(0.025, 0)) +
  scale_y_continuous(limits = c(0, 0.13), expand = c(0, 0)) +
  #scale_x_continuous(limits = c(-5, 5)) +
  labs(x = "Technique",
       y = "Pressure Rate",
       title = "Pressure Rate by DL Technique") +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 11, margin = margin(5, 0, 10, 0)),
        axis.text.y = element_text(size = 12, margin = margin(0, 5, 0, 10)),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold",
                                  margin = margin(b = 8, unit = "pt")),
        plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic",
                                     margin = margin(b = 16, unit = "pt")),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

#Look at whether the presence of 0/1 tech defenders affect the outcome of inside runs

#Create column with DL alignments for each play vs run 

dfrun <- pbp_clean %>% 
  filter(event_type == "designed run") %>% 
  group_by(GameID, EventID) %>%
  mutate(in_designed_gap = max(in_designed_gap)) %>% 
  summarise(Quarter = mean(Quarter), 
            TimeLeft = mean(TimeLeft),
            Down = mean(Down), 
            ToGo = mean(ToGo), 
            StartYard = mean(yardline_100),
            OffensiveYardage = mean(OffensiveYardage), 
            EPA = mean(EPA), 
            success = mean(success),
            RunDirection= paste(unique(RunDirection), collapse = '-'),
            UsedDesignedGap = paste(unique(UsedDesignedGap), collapse = '-'),
            in_designed_gap = paste(unique(in_designed_gap), collapse = '-'),
            PlayDesc = paste(unique(PlayDesc), collapse = '-'),
            DL = paste(unique(tech_side), collapse = '-'))

#Compare distribution of EPA and Offensive Yards Gained and success rate
#of plays with a defender in the run gap and plays without

dfrun %>%
  filter(UsedDesignedGap == 0) %>%
  ggplot(aes(x = EPA, fill = in_designed_gap)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red","blue"))

dfrun %>%
  ggplot(aes(x = OffensiveYardage, fill = in_designed_gap)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red","blue"))

dfrun %>%
  ggplot(aes(x = EPA, fill = UsedDesignedGap)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red","blue"))

# Using the Designed Gap appears to lead to more success for the offense
dfrun %>%
  group_by(UsedDesignedGap) %>%
  summarize(runs = n(),
            epa = mean(EPA, na.rm = T),
            sr = mean(success))

# Having a defender in/next to the designed gap appears to prevent more big plays as measured by EPA
dfrun %>%
  group_by(in_designed_gap) %>%
  summarize(runs = n(),
            epa = mean(EPA, na.rm = T),
            sr = mean(success))

# The most successful run type is when the offense targets an unoccupied gap and is able to run
# through that gap, as the only run type that gains positive EPA on average
dfrun %>%
  group_by(in_designed_gap, UsedDesignedGap) %>%
  summarize(runs = n(),
            epa = mean(EPA, na.rm = T),
            sr = mean(success))

# We can conclude a defensive lineman's ability to force a run away from a designed gap that they
# occupy has a positive effect of ~0.05 epa per run
dfrun %>%
  filter(in_designed_gap == 1) %>%
  group_by(UsedDesignedGap) %>%
  summarize(runs = n(),
            epa = mean(EPA, na.rm = T),
            sr = mean(success))

dfrun %>%
  filter(in_designed_gap == 1) %>%
  ggplot(aes(x = success)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue") + 
  theme_minimal() +
  scale_x_discrete(limits = c(0,1), labels = c(0, 1)) + 
  scale_y_continuous(breaks = seq(0.05, 0.60, 0.05))


### Look at positional leaderboards for pressure rate and forcing runs away from gap
player_leaderboard <- pbp_clean %>%
  group_by(Name, PlayerId) %>%
  summarize(all_snaps = n(),
            edge_snaps = sum(if_else(position_group == "Edge", 1, 0)),
            edge_pct = edge_snaps/all_snaps,
            idl_snaps = sum(if_else(position_group == "iDL", 1, 0)),
            idl_pct = idl_snaps/all_snaps,
            run_snaps = sum(if_else(event_type == "designed run", 1, 0)),
            pass_snaps = all_snaps - run_snaps,
            pass_rushes = sum(if_else(IsRushing == 1, 1, 0), na.rm = T),
            pressures = sum(Pressure, na.rm = T),
            p_rate = pressures/pass_rushes,
            runs_at_player = sum(in_designed_gap, na.rm = T),
            gap_forces = sum(if_else(in_designed_gap == 1 & 
                                       UsedDesignedGap == 0, 1, 0), na.rm = T),
            gap_force_rate = gap_forces/runs_at_player) %>%
  mutate(position = case_when(edge_pct >= 0.5 ~ "Edge",
                              idl_pct >= 0.5 ~ "iDL",
                              TRUE ~ "Other")) %>%
  arrange(desc(all_snaps))


dfrun %>% na.omit()  %>%
  ggplot(aes(x = in_designed_gap, y = EPA, fill = UsedDesignedGap)) + geom_boxplot() 

boxplot(EPA ~ pressure_type, data = Pressures)
