library(tidyverse)
library(dplyr)
library(splitstackshape)
library(ggplot2)
library(RColorBrewer)

read_csv("AnalyticsChallenge2020Data.csv") -> data
data %>% mutate(TotalTackles = ifelse(AssistedTackle == 1, 0.5, ifelse(SoloTackle == 1, 1, 0))) -> data
unique(data$PlayDesc) -> data_snaps

#Filter Data to get only rush attempts
data %>% filter(EventType == "rush" | EventType == "challenge rush") -> rushes
unique(rushes$PlayDesc) -> rushes_snaps

#Mutating Columns
rushes %>% mutate(TackleForLoss = ifelse(OffensiveYardage < 1, 1,0)) -> rushes
rushes %>% mutate(Solo_TackleForLoss = ifelse(TackleForLoss == 1 & SoloTackle == 1, 1,0)) -> rushes
rushes %>% mutate(Assisted_TackleForLoss = ifelse(TackleForLoss == 1 & AssistedTackle == 1, 1,0)) -> rushes
rushes %>% mutate(TotalTFLs = ifelse(Assisted_TackleForLoss == 1, 0.5, ifelse(Solo_TackleForLoss == 1, 1, 0))) -> rushes

write.xlsx(rushes, "SIS_rushes.xlsx")

#Filter Data to only get pass attempts
data %>% filter(EventType == "pass" | EventType == "challenge pass") -> passes
passes %>% mutate(TotalSacks = ifelse(AssistedSack == 1, 0.5, ifelse(SoloSack == 1, 1, 0))) -> passes

write.xlsx(passes, "SIS_passes.xlsx")

unique(passes$PlayDesc) -> passes_snaps


#Making new dataframes for stats
aggregate(x = passes$Pressure, by = list(Season = passes$Season, RosterPosition = passes$RosterPosition), sum) -> df
aggregate(x = passes$SoloSack, by = list(RosterPosition = passes$RosterPosition, Season = passes$Season), sum) -> df2
aggregate(x = passes$AssistedSack, by = list(RosterPosition = passes$RosterPosition, Season = passes$Season), sum) -> df3
aggregate(x = data$TotalTackles, by = list(RosterPosition = data$RosterPosition, Season = data$Season), sum) -> df4
aggregate(x = rushes$Solo_TackleForLoss, by = list(RosterPosition = rushes$RosterPosition, Season = rushes$Season), sum) -> df5
aggregate(x = rushes$Assisted_TackleForLoss, by = list(RosterPosition = rushes$RosterPosition, Season = rushes$Season), sum) -> df6

#DE Pressure %
1446/9848

#DT Pressure %
880/9848

#LB Pressure %
1056/9848

#DE Sack %
(199 + (41/2))/9848

#DT Sack %
(108 + (31/2))/9848

#LB Sack %
(169 + (28/2))/9848

#DE Tackle %
(926 + (686/2))/16788

#DT Tackle %
(909 + (948/2))/16788

#LB Tackle %
(751 + (571/2))/16788

#DE TFL %
(170 + (141/2))/6940

#DT TFL %
(195 + (153/2))/6940

#LB TFL %
(122 + (99/2))/6940


#Pressures Bar Graph
names(df)[3] <- "Pressures"
df[-c(4), ] -> df
ggplot(df)+
  geom_bar(aes(x=RosterPosition,y=Pressures, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Number of Pressures by Position")
  

#Tackles Bar Graph
names(df4)[3] <- "Tackles"
df4[-c(4,7,8), ] -> df4
ggplot(df4)+
  geom_bar(aes(x=RosterPosition,y=Tackles, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Number of Tackles by Position")

#Filtering Data
passes %>% filter(RosterPosition == "DE") -> passes_DE
passes %>% filter(RosterPosition == "DT") -> passes_DT
passes %>% filter(RosterPosition == "LB") -> passes_LB

#DE Density Plot
aggregate(x = passes_DE$Pressure, by = list(RosterPosition = passes_DE$RosterPosition, Season = passes_DE$Season, Name = passes_DE$Name), sum) -> df
names(df)[4] <- "Pressures"
ggplot(df)+
  geom_density(aes(x = Pressures, fill = "red"),scale = "area")+
  theme_bw()+
  ggtitle("Distrbution of Defensive End Pressures")+
  scale_x_continuous(limits=c(-2.5,60))+
  geom_vline(xintercept = 10.79104, size = 2)

mean(df$Pressures)

#LB Density Plot
passes %>% filter(RosterPosition == "LB" & OnFieldPosition == "DL") -> LB_DL
aggregate(x = LB_DL$TotalSacks, by = list(RosterPosition = LB_DL$RosterPosition, Season = LB_DL$Season, Name = LB_DL$Name), sum) -> df
names(df)[4] <- "Sacks"
ggplot(df, aes(x = Sacks))+
  geom_density(color="black", fill ="yellow")+
  theme_bw()+
  ggtitle("Sacks by Linebackers Lining Up on Defensive Line")+
  scale_x_continuous(limits=c(0,11))+
  geom_vline(xintercept = 1.118182, size = 1)

mean(df$Sacks)

#DT Density Plot
aggregate(x = data$TotalTackles, by = list(RosterPosition = data$RosterPosition, Season = data$Season, Name = data$Name), sum) -> df
df %>% filter(RosterPosition == "DT") -> df
names(df)[4] <- "Tackles"
hist(df$Tackles,
     main="Tackles by DTs", 
     xlab="Tackles", 
     ylab = "Number of Players",
     border="black", 
     col="skyblue",
     xlim=c(0,50),
     las=1, 
     breaks=5)

mean(df$Tackles)


#Filtering Data
passes %>% filter(RosterPosition != "S") %>% filter(RosterPosition != "CB") %>% filter(RosterPosition != "T") %>% filter(RosterPosition != "WR") %>% filter(RosterPosition != "FB") -> passes1

#Making boxplot graph
aggregate(x = passes1$TotalSacks, by = list(RosterPosition = passes1$RosterPosition, Season = passes1$Season, Name = passes1$Name), sum) -> df
aggregate(x = passes1$Pressure, by = list(RosterPosition = passes1$RosterPosition, Season = passes1$Season, Name = passes1$Name), sum) -> df2
names(df)[4] <- "Sacks"
df2 %>% filter(x > 3) -> df2
df %>% full_join(df2, by = c("Name" = "Name", "RosterPosition" = "RosterPosition", "Season" = "Season")) -> df
df[!is.na(df$x),] -> df
ggplot(df) + 
  geom_boxplot(aes(x = df$RosterPosition, y = df$Sacks, fill = df$RosterPosition)) +
  theme_bw()+
  scale_y_continuous(name="Sacks")+
  scale_x_discrete(name="Position")+
  scale_fill_discrete(name = "Positions")+
  ggtitle("Sacks by Position")


#Getting statistics for PowerPoint
aggregate(x = df$Sacks, by = list(RosterPosition = df$RosterPosition), mean) -> df2
aggregate(x = df$x, by = list(RosterPosition = df$RosterPosition), mean) -> df3


#Getting statistics for PowerPoint
distinct(rushes, rushes$PlayDesc, .keep_all = TRUE) -> rushes
aggregate(x = rushes$TotalTFLs, by = list(DefensiveTeam = rushes$DefensiveTeam), sum) -> df
aggregate(x = rushes$OffensiveYardage, by = list(DefensiveTeam = rushes$DefensiveTeam), mean) -> df2


#Getting statistics for PowerPoint
aggregate(x = passes_DE$Pressure, by = list(RosterPosition = passes_DE$RosterPosition, Season = passes_DE$Season, Name = passes_DE$Name), sum) -> df
aggregate(x = data$TotalTackles, by = list(RosterPosition = data$RosterPosition, Season = data$Season, Name = data$Name), sum) -> df2
df2 %>% filter(RosterPosition == "DT") -> df2
aggregate(x = passes_LB$Pressure, by = list(RosterPosition = passes_LB$RosterPosition, Season = passes_LB$Season, Name = passes_LB$Name), sum) -> df3
#LB Boxplot
names(df3)[4] <- "Pressures"
df3 %>% filter(Pressures > 3) -> df3
ggplot(df3, aes(x = df3$RosterPosition, y = df3$Pressures)) + 
  geom_boxplot(color = "black", fill = "red") +
  theme_bw()+
  scale_y_continuous(name="Pressures")+
  scale_x_discrete(name="Position")+
  scale_fill_discrete(name = "Positions")+
  ggtitle("Distribution of Linebacker Pressures")

mean(df3$Pressures)

mean(df2$x)



#For Question 3

#Against the Pass

#Steelers
#LB
passes %>% filter(DefensiveTeam == "Steelers") -> steelers
aggregate(x = steelers$TotalSacks, by = list(RosterPosition = steelers$RosterPosition, Season = steelers$Season), sum) -> df
aggregate(x = steelers$Pressure, by = list(RosterPosition = steelers$RosterPosition, Season = steelers$Season), sum) -> df2
names(df)[3] <- "Sacks"
ggplot(df)+
  geom_bar(aes(x=RosterPosition,y=Sacks, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Steelers: Number of Sacks by Position")+
  scale_fill_brewer(palette = "YlOrBr")




#Panthers
#DT and LB
passes %>% filter(DefensiveTeam == "Panthers") -> panthers
aggregate(x = panthers$TotalSacks, by = list(RosterPosition = panthers$RosterPosition, Season = panthers$Season, Name = panthers$Name), sum) -> df
aggregate(x = panthers$Pressure, by = list(RosterPosition = panthers$RosterPosition, Season = panthers$Season), sum) -> df2
names(df)[3] <- "Sacks"
ggplot(df)+
  geom_bar(aes(x=RosterPosition,y=Sacks, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Panthers: Number of Sacks by Position")+
  scale_fill_brewer(palette = "Blues")


#Saints
#DE
passes %>% filter(DefensiveTeam == "Saints") -> saints
aggregate(x = saints$TotalSacks, by = list(RosterPosition = saints$RosterPosition, Season = saints$Season), sum) -> df
aggregate(x = saints$Pressure, by = list(RosterPosition = saints$RosterPosition, Season = saints$Season), sum) -> df2
names(df2)[3] <- "Pressures"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=Pressures, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Saints: Number of Pressures by Position")+
  scale_fill_brewer(palette = "YlOrRd")


#Rams
#LB and DT
passes %>% filter(DefensiveTeam == "Rams") -> rams
aggregate(x = rams$TotalSacks, by = list(RosterPosition = rams$RosterPosition, Season = rams$Season), sum) -> df
aggregate(x = rams$Pressure, by = list(RosterPosition = rams$RosterPosition, Season = rams$Season), sum) -> df2
names(df2)[3] <- "Pressures"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=Pressures, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Rams: Number of Pressures by Position")+
  scale_fill_brewer(palette = "YlGnBu")



#Niners
#DE
passes %>% filter(DefensiveTeam == "49ers") -> niners
aggregate(x = niners$TotalSacks, by = list(RosterPosition = niners$RosterPosition, Season = niners$Season), sum) -> df
aggregate(x = niners$Pressure, by = list(RosterPosition = niners$RosterPosition, Season = niners$Season), sum) -> df2
names(df2)[3] <- "Pressures"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=Pressures, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("49ers: Number of Pressures by Position")+
  scale_fill_brewer(palette = "Reds")

#Against the Run

#Niners
#DT & DE
rushes %>% filter(DefensiveTeam == "49ers") -> niners
aggregate(x = niners$TotalTFLs, by = list(RosterPosition = niners$RosterPosition, Season = niners$Season), sum) -> df2
names(df2)[3] <- "TFLs"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=TFLs, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("49ers: Number of TFLs by Position (run plays only)")+
  scale_fill_brewer(palette = "Reds")

#Panthers
#DT
rushes %>% filter(DefensiveTeam == "Panthers") -> panthers
aggregate(x = panthers$TotalTFLs, by = list(RosterPosition = panthers$RosterPosition, Season = panthers$Season), sum) -> df2
names(df2)[3] <- "TFLs"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=TFLs, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Panthers: Number of TFLs by Position (run plays only)")+
  scale_fill_brewer(palette = "Blues")

#Rams
#DT
rushes %>% filter(DefensiveTeam == "Rams") -> rams
aggregate(x = rams$TotalTFLs, by = list(RosterPosition = rams$RosterPosition, Season = rams$Season), sum) -> df2
names(df2)[3] <- "TFLs"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=TFLs, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Rams: Number of TFLs by Position (run plays only)")+
  scale_fill_brewer(palette = "YlGnBu")

#Saints
#DE & DT
rushes %>% filter(DefensiveTeam == "Saints") -> saints
aggregate(x = saints$TotalTFLs, by = list(RosterPosition = saints$RosterPosition, Season = saints$Season), sum) -> df2
names(df2)[3] <- "TFLs"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=TFLs, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Saints: Number of TFLs by Position (run plays only)")+
  scale_fill_brewer(palette = "YlOrRd")

#Steelers
#LB
rushes %>% filter(DefensiveTeam == "Steelers") -> steelers
aggregate(x = steelers$TotalTFLs, by = list(RosterPosition = steelers$RosterPosition, Season = steelers$Season), sum) -> df2
names(df2)[3] <- "TFLs"
ggplot(df2)+
  geom_bar(aes(x=RosterPosition,y=TFLs, fill = RosterPosition), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Steelers: Number of TFLs by Position (run plays only)")+
  scale_fill_brewer(palette = "YlOrBr")



#Looking at Game Situation

data %>% filter(Quarter == 1 & TimeLeft > 450) -> early_game_situation
passes %>% filter(Quarter == 1 & TimeLeft > 450) -> early_game_situation2
rushes %>% filter(Quarter == 1 & TimeLeft > 450) -> early_game_situation3


data %>% filter(Quarter == 4 & TimeLeft < 450 & OffTeamScoreBefore > DefTeamScoreBefore) -> late_game_situation
passes %>% filter(Quarter == 4 & TimeLeft < 450) -> late_game_situation2
rushes %>% filter(Quarter == 4 & TimeLeft < 450 & OffTeamScoreBefore > DefTeamScoreBefore) -> late_game_situation3


aggregate(x = late_game_situation3$TotalTFLs, by = list(RosterPosition = late_game_situation3$RosterPosition, Season = late_game_situation3$Season), sum) -> df
aggregate(x = early_game_situation3$TotalTFLs, by = list(RosterPosition = early_game_situation3$RosterPosition, Season = early_game_situation3$Season), sum) -> df2


aggregate(x = late_game_situation2$TotalSacks, by = list(RosterPosition = late_game_situation2$RosterPosition, Season = late_game_situation2$Season), sum) -> df
aggregate(x = early_game_situation2$TotalSacks, by = list(RosterPosition = early_game_situation2$RosterPosition, Season = early_game_situation2$Season), sum) -> df2


aggregate(x = late_game_situation2$Pressure, by = list(RosterPosition = late_game_situation2$RosterPosition, Season = late_game_situation2$Season), sum) -> df
aggregate(x = early_game_situation2$Pressure, by = list(RosterPosition = early_game_situation2$RosterPosition, Season = early_game_situation2$Season), sum) -> df2





#Looking at Technique
passes %>% filter(TotalSacks > 0) -> passes
aggregate(x = passes$TotalSacks, by = list(Technique = passes$TechniqueName), sum) -> df
df %>% filter(x > 31) -> df
names(df)[2] <- "Sacks"
ggplot(df)+
  geom_bar(aes(x=Technique,y=Sacks, fill = Technique), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Top Four Techniques in Terms of Sacks")


rushes %>% filter(TotalTackles > 0) -> rushes
aggregate(x = rushes$TotalTackles, by = list(Technique = rushes$TechniqueName), sum) -> df
df %>% filter(x > 180) -> df
names(df)[2] <- "Tackles"
ggplot(df)+
  geom_bar(aes(x=Technique,y=Tackles, fill = Technique), stat="identity", position="dodge")+
  theme_bw()+
  ggtitle("Top Four Techniques in Terms of Tackles (run plays only)")







