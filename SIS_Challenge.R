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
         gap_force = if_else(UsedDesignedGap == 0 & in_designed_gap == 1, 1, 0),
         success = if_else(EPA > 0, 1, 0),
         position_group = case_when(TechniqueName %in% c("7", "Outside", "9", "6", "5") ~ "Edge",
                                    TechniqueName == "Off Ball" ~ TechniqueName,
                                    TRUE ~ "iDL"),
         dl_tackle = if_else((SoloTackle == 1 | AssistedTackle == 1) & 
                               position_group %in% c("Edge", "iDL") & 
                               (SackOnPlay == 0 | is.na(SackOnPlay)), 1, 0))

# pbp_clean %>%
#   filter(event_type == "designed run" & UsedDesignedGap == 0) %>%
#   mutate(TimeLeft = glue("{floor(TimeLeft/60)}:{TimeLeft - floor(TimeLeft/60)*60}")) %>%
#   select(Week, OffensiveTeam, Quarter, TimeLeft, Down, ToGo, RunDirection) %>%
#   distinct()

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


# Create Dataframe of Pass plays - Scrambles
dfpass <- pbp_clean %>% 
  filter(event_type == "dropback" & scramble == 0) %>%
  mutate(edge_pressure = if_else(position_group == "Edge" & Pressure == 1, 1, 0),
         idl_pressure = if_else(position_group == "iDL" & Pressure == 1, 1, 0)) %>% 
  group_by(GameID, EventID) %>%
  summarise(Quarter = mean(Quarter), 
            TimeLeft = mean(TimeLeft),
            Down = mean(Down), 
            ToGo = mean(ToGo), 
            StartYard = mean(yardline_100),
            Completion = mean(Completion),
            OffensiveYardage = mean(OffensiveYardage), 
            EPA = mean(EPA), 
            success = mean(success),
            Touchdown = mean(Touchdown),
            PressureOnPlay = mean(PressureOnPlay),
            PassBreakupOnPlay = mean(PassBreakupOnPlay),
            SackOnPlay = mean(SackOnPlay),
            dl_tackle = max(dl_tackle),
            edge_p = max(edge_pressure),
            idl_p = max(idl_pressure)) %>%
  mutate(pressure_type = case_when(edge_p == 1 & idl_p == 1 ~ "Both",
                                   edge_p == 1 ~ "Edge",
                                   idl_p == 1 ~ "iDL",
                                   edge_p == 0 & idl_p == 0 & PressureOnPlay == 1 ~ "Other",
                                   TRUE ~ "No Pressure")) %>%
  select(-edge_p, -idl_p) %>% na.omit()

#Look at EPA distributions by Pressure Type
dfpass %>% filter(pressure_type != "Other") %>% 
  ggplot(aes(x = EPA, fill = pressure_type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue", 
                               "green", 
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
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + facet_wrap(~pressure_type) 

#Kolmogorov-Smirnov Test, cannot reject null hyptohesis that two samples come from the same distribution
ks.test(Pressures$EPA[Pressures$pressure_type == "Edge"], Pressures$EPA[Pressures$pressure_type == "iDL"])

#Independent Sammples T-Test, cannot reject null hypothesis that the two sample means are equal
t.test(Pressures$EPA[Pressures$pressure_type == "Edge"], Pressures$EPA[Pressures$pressure_type == "iDL"])


#Create dataframe of run plays
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
            Touchdown = mean(Touchdown),
            RunDirection= paste(unique(RunDirection), collapse = '-'),
            UsedDesignedGap = paste(unique(UsedDesignedGap), collapse = '-'),
            in_designed_gap = paste(unique(in_designed_gap), collapse = '-'),
            gap_force = max(gap_force),
            dl_tackle = max(dl_tackle),
            PlayDesc = paste(unique(PlayDesc), collapse = '-'))


#Look at whether gap forces have a significant effect on run plays
dfrun %>% na.omit() %>% ggplot(aes(x = as.factor(gap_force), y = EPA)) + 
  geom_boxplot() 

ks.test(dfrun$EPA[dfrun$gap_force ==1], dfrun$EPA[dfrun$gap_force == 0])

t.test(dfrun$EPA[dfrun$gap_force ==1], dfrun$EPA[dfrun$gap_force == 0])
#We can conclude gap forces have no significant effect on the outcome of run plays

#Look at whether DL tackles have a significant effect on run plays with tackles by other position groups
# or runner out of bounds
dfrun %>% filter(Touchdown == 0) %>% na.omit() %>% ggplot(aes(x = as.factor(dl_tackle), y = EPA)) + 
  geom_boxplot() 

ks.test(dfrun$EPA[dfrun$dl_tackle ==1], dfrun$EPA[dfrun$dl_tackle == 0 & dfrun$Touchdown == 0])

t.test(dfrun$EPA[dfrun$dl_tackle ==1], dfrun$EPA[dfrun$dl_tackle == 0 & dfrun$Touchdown == 0])

#We can conclude that a DL tackle has a significant effect on the outcome of run plays

#Compare distribution of EPA and Offensive Yards Gained and success rate
#of plays with a defender in the run gap and plays without

dfrun %>%
  ggplot(aes(x = EPA, fill = as.factor(gap_force))) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red","blue"))

dfrun %>%
  ggplot(aes(x = OffensiveYardage, fill = in_designed_gap)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red","blue"))

dfrun %>%
  ggplot(aes(x = EPA, fill = as.factor(dl_tackle))) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red","blue"))



# The most successful run type is when the offense targets an unoccupied gap and is able to run
# through that gap
dfrun %>%
  group_by(in_designed_gap, UsedDesignedGap) %>%
  summarize(runs = n(),
            epa = mean(EPA, na.rm = T),
            sr = mean(success))



#Fit a linear model to determine the effects of pressures, pass breakups on pass plays
#Control for TDs + Downs + Yardline

passmodel <- lm(EPA ~ PassBreakupOnPlay + PressureOnPlay  + Touchdown + as.factor(Down) + StartYard +
                  ToGo, 
                data = dfpass)

PressureValue <- as.numeric(passmodel$coefficients["PressureOnPlay"]) 

PbuValue <- as.numeric(passmodel$coefficients["PassBreakupOnPlay"]) 


#Fit a linear model to determine the effects of DL tackles on run plays
#Control for TDs + Downs + Yardline

runmodel <- lm(EPA ~ dl_tackle + Touchdown + as.facor(Down) + StartYard + ToGo, data = dfrun)


TackleValue <- as.numeric(runmodel$coefficients["dl_tackle"])



pbp_clean$dEPA <- 0
for (i in 1:nrow(pbp_clean)){
  if(pbp_clean$event_type[i] == "dropback" & pbp_clean$scramble[i] == 0) {
    pbp_clean$dEPA[i] <- pbp_clean$Pressure[i] * PressureValue + pbp_clean$PassBreakup[i] * PbuValue
  } else if (pbp_clean$event_type[i] == "designed run") {
    pbp_clean$dEPA[i] <- pbp_clean$SoloTackle[i] * TackleValue + (pbp_clean$AssistedTackle[i] * TackleValue)/2
  } else {
    pbp_clean$dEPA[i] <- 0
  }
}

pbp_clean$dEPA <- pbp_clean$dEPA * -1

TechValues <- pbp_clean %>% 
  group_by(TechniqueName) %>% 
  summarise(Snaps = n(), 
            dEPA = sum(dEPA),
            dEPA100 = (dEPA/Snaps)*100) %>% 
  ungroup()


TalentDistributions <- pbp_clean %>% 
  group_by(TechniqueName, Name, PlayerId) %>% 
  summarise(Snaps = n(), 
            dEPA = sum(dEPA),
            dEPA100 = (dEPA/Snaps)*100) %>% 
  ungroup() %>% filter(Snaps >= 20)


TalentDistributions %>%
  ggplot(aes(x = dEPA100, fill = TechniqueName)) + geom_density(alpha = 0.5) +
  facet_wrap(~TechniqueName)


TalentDistributions %>%
  ggplot(aes(x = dEPA, fill = TechniqueName)) + geom_density(alpha = 0.5) +
  facet_wrap(~TechniqueName)


TechValues %>%
  filter(TechniqueName != "Off Ball") %>%
  ggplot(aes(x = TechniqueName, y = dEPA100)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  scale_x_discrete(limits = c("0","1","2","2i","3","4i","4","5","6","7","9","Outside")) +
  #scale_x_continuous(limits = c(-5, 5)) +
  labs(x = "Technique",
       y = "dEPA/100",
       title = "dEPA per 100 Snaps by DL Technique") +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 11, margin = margin(5, 0, 10, 0)),
        axis.text.y = element_text(size = 12, margin = margin(0, 5, 0, 10)),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold",
                                  margin = margin(b = 8, unit = "pt")),
        plot.subtitle = element_text(size = 14, hjust = 0.5, face = "italic",
                                     margin = margin(b = 16, unit = "pt")),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
