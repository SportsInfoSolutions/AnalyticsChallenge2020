library(tidyverse)
library(ggplot2)
library(reshape2)


# get data from SIS github
data_all <- read.csv(url("https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv"))

tech_counts <- data_all %>% group_by(SideOfBall, TechniqueName) %>% summarise(count = n())

data_all <- data_all %>% rename(GameID = ï..GameID)
data_all <- data_all %>% filter(EPA != "NULL")
data_all$EPA <- as.numeric(levels(data_all$EPA)[data_all$EPA])

# create a yards to goal and success column
data_all <- data_all %>% mutate(
              yds_to_goal = ifelse(SideOfField == "Own", 100 - StartYard, StartYard),
              success = ifelse(EPA > 0, 1, 0)
              )

# create a game state column
# 2 = big offensive lead (>10 pts), 1 = slight off lead (>3), 0 = close (within 3), -1 = slight def leaf, -2 = big def lead
data_all$OffGameState <- 0
for (i in 1:nrow(data_all)) {
  if (data_all$OffTeamScoreBefore[i] - data_all$DefTeamScoreBefore[i] > 10) {
    data_all$OffGameState[i] <- 2
  }
  else if (data_all$OffTeamScoreBefore[i] - data_all$DefTeamScoreBefore[i] > 3) {
    data_all$OffGameState[i] <- 1
  }
  else if (data_all$DefTeamScoreBefore[i] - data_all$OffTeamScoreBefore[i] > 10 ) {
    data_all$OffGameState[i] <- -2
  }
  else if (data_all$DefTeamScoreBefore[i] - data_all$OffTeamScoreBefore[i] > 3 ) {
    data_all$OffGameState[i] <- -1
  }
}

# cluster players based on where they line up
player_pos_rates <- data_all %>% group_by(Name) %>% summarise(
  count = n(),
  roster_DL = mean(ifelse(RosterPosition == "DT" | RosterPosition == "DE", 1, 0)),
  pass_rush_rate = sum(ifelse(EventType == "pass" & IsRushing == 1, 1, 0)) / sum(ifelse(EventType == "pass", 1, 0)),
  third_down_rate = sum(ifelse(Down == 3, 1, 0)) / count,
  tech_ob_rate = sum(ifelse(TechniqueName == "Off Ball", 1, 0)) / count,
  tech_0_rate = sum(ifelse(TechniqueName == "0", 1, 0)) / count,
  tech_1_rate = sum(ifelse(TechniqueName == "1", 1, 0)) / count,
  tech_2i_rate = sum(ifelse(TechniqueName == "2i", 1, 0)) / count,
  tech_2_rate = sum(ifelse(TechniqueName == "2", 1, 0)) / count,
  tech_3_rate = sum(ifelse(TechniqueName == "3", 1, 0)) / count,
  tech_4i_rate = sum(ifelse(TechniqueName == "4i", 1, 0)) / count,
  tech_4_rate = sum(ifelse(TechniqueName == "4", 1, 0)) / count,
  tech_5_rate = sum(ifelse(TechniqueName == "5", 1, 0)) / count,
  tech_7_rate = sum(ifelse(TechniqueName == "7", 1, 0)) / count,
  tech_6_rate = sum(ifelse(TechniqueName == "6", 1, 0)) / count,
  tech_9_rate = sum(ifelse(TechniqueName == "9", 1, 0)) / count,
  tech_os_rate = sum(ifelse(TechniqueName == "Outside", 1, 0)) / count,
) %>% filter(count > 50)

player_pos_rates_clusters <- player_pos_rates[, !names(player_pos_rates) %in% c("Name", "count")]


# cluster players
clust_rsq = rep(0, 3)
clusters_pos_3 <- kmeans(player_pos_rates_clusters, centers = 3, nstart = 20)
clust_rsq[1] <- clusters_pos_3$betweenss / clusters_pos_3$totss
clusters_pos_4 <- kmeans(player_pos_rates_clusters, centers = 4, nstart = 20)
clust_rsq[2] <- clusters_pos_4$betweenss / clusters_pos_4$totss
clusters_pos_5 <- kmeans(player_pos_rates_clusters, centers = 5, nstart = 20)
clust_rsq[3] <- clusters_pos_5$betweenss / clusters_pos_5$totss

plot(clust_rsq)

centers_3 <- t(clusters_pos_3$centers)
centers_3
centers_4 <- clusters_pos_4$centers
centers_4
centers_5 <- t(clusters_pos_5$centers)
centers_5

centers_4 <- cbind.data.frame(centers_4, "cluster" = c(1, 2, 3, 4))
centers_4_melt <- melt(centers_4, id.vars = "cluster", measure.vars = c("tech_os_rate", "tech_9_rate", "tech_6_rate",
                                                                        "tech_7_rate", "tech_5_rate", "tech_4_rate",
                                                                        "tech_4i_rate", "tech_3_rate", "tech_2_rate",
                                                                        "tech_2i_rate", "tech_1_rate", "tech_0_rate",
                                                                        "tech_ob_rate"))


ggplot(centers_4_melt, aes(x = variable, y = value, group = as.factor(cluster), colour = as.factor(cluster))) +
  geom_line(size = 2) +
  ggtitle("Cluster Technique Rates") +
  labs(x = "Technique", y = "Frequency", colour = "Cluster") +
  theme(axis.text.x = element_text(size = 10)) +
  scale_x_discrete(labels = c("Outside", "9", "6", "7", "5", "4", "4i", "3", "2", "2i",  "1", "0", "Off-Ball"))
ggsave("Clusters.png")


player_pos_rates$cluster <- clusters_pos_4$cluster
player_pos_rates$assigned_position <- "blank"
player_pos_rates[player_pos_rates$cluster == 1, "assigned_position"] <- "Middle 3 Tech"
player_pos_rates[player_pos_rates$cluster == 2, "assigned_position"] <- "Inside 1 Tech"
player_pos_rates[player_pos_rates$cluster == 3, "assigned_position"] <- "End DL"
player_pos_rates[player_pos_rates$cluster == 4, "assigned_position"] <- "Outside non-DL"

player_pos_slim <- player_pos_rates[, names(player_pos_rates) %in% c("Name", "assigned_position")]
data_all <- left_join(x = data_all, y = player_pos_slim, by = "Name")
data_all[is.na(data_all$assigned_position), "assigned_position"] <- "non-standard"

# create a play level dataframe
data_all$TopRow <- 0
data_all$TopRow[1] <- 1
for (i in 2:nrow(data_all)) {
  if ((data_all$GameID[i] != data_all$GameID[i - 1]) | (data_all$EventID[i] != data_all$EventID[i - 1])) {
    data_all$TopRow[i] <- 1
  }
}

plays_all <- data_all[data_all$TopRow == 1, ]
plays_all <- plays_all[, !names(plays_all) %in% c("PlayerId", "Name", "RosterPosition", "OnFieldPosition", "SideOfBall", "TechniqueName", 
                           "IsRushing", "SoloTackle", "AssistedTackle", "Pressure", "SoloSack", "AssistedSack",
                           "PassBreakup", "Interception", "ForcedFumble", "RecoveredFumble", "TopRow")]

# add columns for defensive positioning
plays_all$Tech_Left_Outside <- 0
plays_all$Tech_Left_Outside_PR <- 0
plays_all$Tech_Left_Outside_ID <- 0
plays_all$Tech_Left_9 <- 0
plays_all$Tech_Left_9_PR <- 0
plays_all$Tech_Left_9_ID <- 0
plays_all$Tech_Left_7 <- 0
plays_all$Tech_Left_7_PR <- 0
plays_all$Tech_Left_7_ID <- 0
plays_all$Tech_Left_6 <- 0
plays_all$Tech_Left_6_PR <- 0
plays_all$Tech_Left_6_ID <- 0
plays_all$Tech_Left_5 <- 0
plays_all$Tech_Left_5_PR <- 0
plays_all$Tech_Left_5_ID <- 0
plays_all$Tech_Left_4 <- 0
plays_all$Tech_Left_4_PR <- 0
plays_all$Tech_Left_4_ID <- 0
plays_all$Tech_Left_4i <- 0
plays_all$Tech_Left_4i_PR <- 0
plays_all$Tech_Left_4i_ID <- 0
plays_all$Tech_Left_3 <- 0
plays_all$Tech_Left_3_PR <- 0
plays_all$Tech_Left_3_ID <- 0
plays_all$Tech_Left_2 <- 0
plays_all$Tech_Left_2_PR <- 0
plays_all$Tech_Left_2_ID <- 0
plays_all$Tech_Left_2i <- 0
plays_all$Tech_Left_2i_PR <- 0
plays_all$Tech_Left_2i_ID <- 0
plays_all$Tech_Left_1 <- 0
plays_all$Tech_Left_1_PR <- 0
plays_all$Tech_Left_1_ID <- 0
plays_all$Tech_0 <- 0
plays_all$Tech_0_PR <- 0
plays_all$Tech_0_ID <- 0
plays_all$Tech_OB <- 0
plays_all$Tech_OB_PR <- 0
plays_all$Tech_OB_ID <- 0
plays_all$Tech_Right_1 <- 0
plays_all$Tech_Right_1_PR <- 0
plays_all$Tech_Right_1_ID <- 0
plays_all$Tech_Right_2i <- 0
plays_all$Tech_Right_2i_PR <- 0
plays_all$Tech_Right_2i_ID <- 0
plays_all$Tech_Right_2 <- 0
plays_all$Tech_Right_2_PR <- 0
plays_all$Tech_Right_2_ID <- 0
plays_all$Tech_Right_3 <- 0
plays_all$Tech_Right_3_PR <- 0
plays_all$Tech_Right_3_ID <- 0
plays_all$Tech_Right_4i <- 0
plays_all$Tech_Right_4i_PR <- 0
plays_all$Tech_Right_4i_ID <- 0
plays_all$Tech_Right_4 <- 0
plays_all$Tech_Right_4_PR <- 0
plays_all$Tech_Right_4_ID <- 0
plays_all$Tech_Right_5 <- 0
plays_all$Tech_Right_5_PR <- 0
plays_all$Tech_Right_5_ID <- 0
plays_all$Tech_Right_6 <- 0
plays_all$Tech_Right_6_PR <- 0
plays_all$Tech_Right_6_ID <- 0
plays_all$Tech_Right_7 <- 0
plays_all$Tech_Right_7_PR <- 0
plays_all$Tech_Right_7_ID <- 0
plays_all$Tech_Right_9 <- 0
plays_all$Tech_Right_9_PR <- 0
plays_all$Tech_Right_9_ID <- 0
plays_all$Tech_Right_Outside <- 0
plays_all$Tech_Right_Outside_PR <- 0
plays_all$Tech_Right_Outside_ID <- 0
plays_all$Inside1 <- 0
plays_all$Middle3 <- 0
plays_all$EndDL <- 0
plays_all$OutsidenonDL <- 0

# loop through plays to identify if there was a player in each technique for that play and then if there was a player
# at each assigned position on the play
for (i in 1:nrow(plays_all)) {
  play <- subset(data_all, GameID == plays_all$GameID[i] & EventID == plays_all$EventID[i])
  for (j in 1:nrow(play)) {
    if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "Outside")) {
      start_col <- 41
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "9")) {
      start_col <- 44
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "7")) {
      start_col <- 47
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "6")) {
      start_col <- 50
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "5")) {
      start_col <- 53
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "4")) {
      start_col <- 56
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "4i")) {
      start_col <- 59
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "3")) {
      start_col <- 62
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "2")) {
      start_col <- 65
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "2i")) {
      start_col <- 68
    }
    else if ((play$SideOfBall[j] == "L") & (play$TechniqueName[j] == "1")) {
      start_col <- 71
    }
    else if ((play$SideOfBall[j] == "NULL") & (play$TechniqueName[j] == "0")) {
      start_col <- 74
    }
    else if ((play$SideOfBall[j] == "NULL") & (play$TechniqueName[j] == "Off Ball")) {
      start_col <- 77
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "1")) {
      start_col <- 80
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "2i")) {
      start_col <- 83
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "2")) {
      start_col <- 86
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "3")) {
      start_col <- 89
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "4i")) {
      start_col <- 92
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "4")) {
      start_col <- 95
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "5")) {
      start_col <- 98
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "6")) {
      start_col <- 101
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "7")) {
      start_col <- 104
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "9")) {
      start_col <- 107
    }
    else if ((play$SideOfBall[j] == "R") & (play$TechniqueName[j] == "Outside")) {
      start_col <- 110
    }
    plays_all[i, start_col] <- plays_all[i, start_col] + 1
    plays_all[i, start_col + 1] <- plays_all[i, start_col + 1] + play$IsRushing[j]
    plays_all[i, start_col + 2] <- play$PlayerId[j]
    
    if (play$assigned_position[j] == "Inside 1 Tech") {
      plays_all$Inside1[i] <- 1
    }
    else if (play$assigned_position[j] == "Middle 3 Tech") {
      plays_all$Middle3[i] <- 1
    }
    else if (play$assigned_position[j] == "End DL") {
      plays_all$EndDL[i] <- 1
    }
    else if (play$assigned_position[j] == "Outside non-DL") {
      plays_all$OutsidenonDL[i] <- 1
    }
  }
}

# create DL summary columns
plays_all <- plays_all %>% mutate(
                totalDL = Tech_Left_Outside + Tech_Left_9 + Tech_Left_7 + Tech_Left_6 + Tech_Left_5 + Tech_Left_4 + 
                          Tech_Left_4i + Tech_Left_3 + Tech_Left_2 + Tech_Left_2i + Tech_Left_1 + Tech_0 + Tech_OB + 
                          Tech_Right_1 + Tech_Right_2i + Tech_Right_2 + Tech_Right_3 + Tech_Right_4i + Tech_Right_4 + 
                          Tech_Right_5 + Tech_Right_6 + Tech_Right_7 + Tech_Right_9 + Tech_Right_Outside,
                totalPR = Tech_Left_Outside_PR + Tech_Left_9_PR + Tech_Left_7_PR + Tech_Left_6_PR + Tech_Left_5_PR + 
                          Tech_Left_4_PR + Tech_Left_4i_PR + Tech_Left_3_PR + Tech_Left_2_PR + Tech_Left_2i_PR + 
                          Tech_Left_1_PR + Tech_0_PR + Tech_OB_PR + Tech_Right_1_PR + Tech_Right_2i_PR + Tech_Right_2_PR + 
                          Tech_Right_3_PR + Tech_Right_4i_PR + Tech_Right_4_PR + Tech_Right_5_PR + Tech_Right_6_PR + 
                          Tech_Right_7_PR + Tech_Right_9_PR + Tech_Right_Outside_PR
)

# split plays into runs and passes
run_plays <- plays_all %>% filter(EventType == "rush")
pass_plays <- plays_all %>% filter(EventType == "pass")

# aggregate plays to see how number of DL impact offensive success
DL_counts_run <- run_plays %>% group_by(totalDL) %>% summarise(
                  count = n(),
                  mean_EPA = mean(EPA),
                  mean_Success = mean(success)
)

DL_counts_pass <- pass_plays %>% group_by(totalDL) %>% summarise(
                    count = n(),
                    mean_EPA = mean(EPA),
                    mean_Success = mean(success)
)

DLR_counts_pass <- pass_plays %>% group_by(totalPR) %>% summarise(
                    count = n(),
                    mean_EPA = mean(EPA),
                    mean_Success = mean(success)
)

# rush analysis
run_used_gap <- run_plays %>% group_by(UsedDesignedGap) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  fumb_rate = sum(Turnover)/count
)

run_used_gap_metrics <- melt(run_used_gap, id.vars = "UsedDesignedGap", measure.vars = c("mean_EPA", "success_rate",
                                                                                         "fumb_rate"))

ggplot(run_used_gap_metrics, aes(fill=UsedDesignedGap, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  ggtitle("Rushing Metrics when Using Designed Gap") +
  labs(x = "Metric", y = "Value", fill = "Used Designed Gap") +
  scale_fill_manual(values=c("#66CC99", "#CC6666"))
ggsave("RunMetricsUsingGap.png")

run_dir <- run_plays %>% group_by(RunDirection) %>% summarise(
            count = n(),
            mean_EPA = mean(EPA),
            success_rate = mean(success),
            used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count,
            fumb_rate = sum(Turnover)/count
)

ggplot(run_dir, aes(x = RunDirection, y = success_rate)) +
  geom_bar(stat = "identity")

run_dir %>% 
  mutate(RunDirection=factor(RunDirection, levels=c("Left D Gap", "Left Off-Tackle C Gap", "Left Off-Tackle B Gap",
                                                    "Left A Gap", "Middle", "Right A Gap", "Right Off-Tackle B Gap",
                                                    "Right Off-Tackle C Gap", "Right D Gap"))) %>%
  ggplot(aes(x = RunDirection, y = mean_EPA)) +
    geom_bar(stat = "identity", colour = "black") +
    ggtitle("Mean EPA per Run Direction") + 
    labs(x = "Run Direction", y = "Mean EPA") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ggsave("EPAByRunDirection.png")

run_dir %>% 
  mutate(RunDirection=factor(RunDirection, levels=c("Left D Gap", "Left Off-Tackle C Gap", "Left Off-Tackle B Gap",
                                                    "Left A Gap", "Middle", "Right A Gap", "Right Off-Tackle B Gap",
                                                    "Right Off-Tackle C Gap", "Right D Gap"))) %>%
  ggplot(aes(x = RunDirection, y = used_gap_rate)) +
    geom_bar(stat = "identity", colour = "black") +
    ggtitle("Gap Usage per Run Direction") + 
    labs(x = "Run Direction", y = "Used Gap Rate") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ggsave("GapUsageByRunDirection.png")

# summarize run direction to gap
run_plays$Gap <- "Middle"
run_plays$Gap <- ifelse(run_plays$RunDirection == "Left A Gap" | run_plays$RunDirection == "Right A Gap", "A", run_plays$Gap)
run_plays$Gap <- ifelse(run_plays$RunDirection == "Left Off-Tackle B Gap" | run_plays$RunDirection == "Right Off-Tackle B Gap", "B", run_plays$Gap)
run_plays$Gap <- ifelse(run_plays$RunDirection == "Left Off-Tackle C Gap" | run_plays$RunDirection == "Right Off-Tackle C Gap", "C", run_plays$Gap)
run_plays$Gap <- ifelse(run_plays$RunDirection == "Left D Gap" | run_plays$RunDirection == "Right D Gap", "D", run_plays$Gap)

run_gap <- run_plays %>% group_by(Gap) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  mean_Success = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count,
  fumb_rate = sum(Turnover)/count
)

run_used_gap_per_gap <- run_plays %>% group_by(Gap, UsedDesignedGap) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  mean_Success = mean(success),
  fumb_rate = sum(Turnover)/count
)

run_used_gap_per_gap %>% 
  mutate(Gap=factor(Gap, levels=c("D", "C", "B", "A", "Middle"))) %>% 
  ggplot(aes(fill=as.factor(UsedDesignedGap), y=mean_EPA, x=Gap)) + 
    geom_bar(position="dodge", stat="identity", colour = "black") +
    ggtitle("Mean EPA by Designed Gap Usage") +
    labs(x = "Gap", y = "Mean EPA", fill = "Used Designed Gap") +
    scale_fill_manual(values=c("#66CC99", "#CC6666"))
ggsave("RushEPAbyGapUsingGap.png")

# now that we see for every gap (besides middle) it's beneficial to force the RB to use a different gap,
# we can test to see if certain techniques tend to force off of gaps more often
# first assign gap opportunity
run_players <- data_all %>% filter(EventType == "rush")
run_players$GapOpportunity <- 0
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Left D Gap" & run_players$SideOfBall == "R" & 
                                      (run_players$TechniqueName == "6" | run_players$TechniqueName == "9" | 
                                        run_players$TechniqueName == "Outside"), 1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Left Off-Tackle C Gap" & run_players$SideOfBall == "R" & 
                                      (run_players$TechniqueName == "4" | run_players$TechniqueName == "5" | 
                                        run_players$TechniqueName == "7" | run_players$TechniqueName == "6"), 
                                      1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Left Off-Tackle B Gap" & run_players$SideOfBall == "R" & 
                                      (run_players$TechniqueName == "2" | run_players$TechniqueName == "3" | 
                                        run_players$TechniqueName == "4i" | run_players$TechniqueName == "4"), 
                                      1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Left A Gap" & ((run_players$SideOfBall == "R" & 
                                      (run_players$TechniqueName == "1" | run_players$TechniqueName == "2i" | 
                                        run_players$TechniqueName == "2")) | (run_players$TechniqueName == "0" | 
                                                                              run_players$TechniqueName == "Off Ball")), 
                                      1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Middle" & (run_players$TechniqueName == "1" | 
                                                                               run_players$TechniqueName == "0" |
                                                                               run_players$TechniqueName == "Off Ball"),
                                     1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Right A Gap" & ((run_players$SideOfBall == "L" & 
                                      (run_players$TechniqueName == "1" | run_players$TechniqueName == "2i" | 
                                        run_players$TechniqueName == "2")) | (run_players$TechniqueName == "0" | 
                                                                              run_players$TechniqueName == "Off Ball")), 
                                     1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Right Off-Tackle B Gap" & run_players$SideOfBall == "L" & 
                                       (run_players$TechniqueName == "2" | run_players$TechniqueName == "3" | 
                                          run_players$TechniqueName == "4i" | run_players$TechniqueName == "4"), 
                                     1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Right Off-Tackle C Gap" & run_players$SideOfBall == "L" & 
                                       (run_players$TechniqueName == "4" | run_players$TechniqueName == "5" | 
                                          run_players$TechniqueName == "7" | run_players$TechniqueName == "6"), 
                                     1, run_players$GapOpportunity)
run_players$GapOpportunity <- ifelse(run_players$RunDirection == "Right D Gap" & run_players$SideOfBall == "L" & 
                                       (run_players$TechniqueName == "6" | run_players$TechniqueName == "9" | 
                                          run_players$TechniqueName == "Outside"), 1, run_players$GapOpportunity)

# aggregate performance for each DL position
run_gap_pos <- run_players %>% group_by(assigned_position) %>% summarise(
  count = n(),
  gap_opps = sum(GapOpportunity),
  gap_opp_rate = gap_opps / count,
  forced_gap_change = sum(ifelse(GapOpportunity == 1 & UsedDesignedGap == "0", 1, 0)),
  forced_gap_change_percent = forced_gap_change / gap_opps,
  forced_gap_EPA = sum(ifelse(GapOpportunity == 1 & UsedDesignedGap == "0", EPA, 0)) / forced_gap_change,
  used_gap_EPA = sum(ifelse(GapOpportunity == 1 & UsedDesignedGap == "1", EPA, 0)) / (gap_opps - forced_gap_change),
  gap_EPA_diff = forced_gap_EPA - used_gap_EPA
)

run_gap_pos %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL", "non-standard"))) %>%
  ggplot(aes(fill = gap_opp_rate, x = assigned_position, y = forced_gap_change_percent)) +
    geom_bar(stat = "identity", colour = "black") +
    ggtitle("Forced Gap Change by Assigned Position") +
    labs(x = "Assigned Position", y = "Forced Gap Change Rate", fill = "Gap Opportunity Rate") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1)
ggsave("ForcedGapChangebyPosition.png")

run_gap_opps_side <- run_players %>% group_by(SideOfBall, TechniqueName) %>% summarise(
  count = n(),
  gap_opps = sum(GapOpportunity),
  forced_gap_change = sum(ifelse(GapOpportunity == 1 & UsedDesignedGap == "0", 1, 0)),
  forced_gap_change_percent = forced_gap_change / gap_opps
)

run_gap_opps <- run_players %>% group_by(TechniqueName) %>% summarise(
  count = n(),
  gap_opps = sum(GapOpportunity),
  gap_opp_percent = gap_opps / count,
  forced_gap_change = sum(ifelse(GapOpportunity == 1 & UsedDesignedGap == "0", 1, 0)),
  forced_gap_change_percent = forced_gap_change / gap_opps
)

ggplot(run_gap_opps, aes(x = TechniqueName, y = forced_gap_change_percent)) +
  geom_bar(stat = "identity")

run_gap_pos_opp <- run_players %>% group_by(assigned_position, GapOpportunity) %>% summarise(
  count = n(),
  tackle_rate = sum(ifelse(SoloTackle == 1 | AssistedTackle == 1, 1, 0)) / count,
  mean_EPA = mean(EPA)
)

# aggregate performance by position for each player
run_player_perf <- run_players %>% group_by(Name, assigned_position) %>% summarise(
  count = n(),
  gap_opps = sum(GapOpportunity),
  gap_opp_percent = gap_opps / count,
  forced_gap_change = sum(ifelse(GapOpportunity == 1 & UsedDesignedGap == "0", 1, 0)),
  forced_gap_change_percent = forced_gap_change / gap_opps
)

ggplot(run_player_perf, aes(count)) +
  geom_histogram(binwidth = 5)

ggplot(run_player_perf, aes(gap_opps)) +
  geom_histogram(binwidth = 2)

# filter to players with enough snaps to be assigned a position
run_player_perf_filtered <- run_player_perf %>% filter(assigned_position != "non-standard")

run_player_perf_filtered %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                            "Outside non-DL"))) %>%
  ggplot(aes(forced_gap_change_percent, colour = assigned_position)) +
    geom_freqpoly(binwidth = 0.1, size = 2) +
    ggtitle("Distribution of Forced Gap Change Rates by Position") +
    labs(x = "Forced Gap Change Rate", y = "Frequency", colour = "Assigned Position")
ggsave("GapChangeRateDistro.png")


# let's look at play level data when runs go at gaps with relevant positions present or not
# for each run play, check if the designed gap was covered
run_plays$GapCovered <- 0
run_plays$GapDefenders <- 1000
for (i in 1:nrow(run_plays)){
  if (run_plays$RunDirection[i] == "Left D Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Right_Outside[i], 
                                     run_plays$Tech_Right_9[i],
                                     run_plays$Tech_Right_6[i])
  }
  else if (run_plays$RunDirection[i] == "Left Off-Tackle C Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Right_6[i],
                                     run_plays$Tech_Right_7[i],
                                     run_plays$Tech_Right_5[i],
                                     run_plays$Tech_Right_4[i])
  }
  else if (run_plays$RunDirection[i] == "Left Off-Tackle B Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Right_4[i],
                                     run_plays$Tech_Right_4i[i],
                                     run_plays$Tech_Right_3[i],
                                     run_plays$Tech_Right_2[i])
  }
  else if (run_plays$RunDirection[i] == "Left A Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Right_2[i],
                                     run_plays$Tech_Right_2i[i],
                                     run_plays$Tech_Right_1[i],
                                     run_plays$Tech_0[i],
                                     run_plays$Tech_OB[i])
  }
  else if (run_plays$RunDirection[i] == "Middle") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Right_1[i],
                                     run_plays$Tech_0[i],
                                     run_plays$Tech_OB[i],
                                     run_plays$Tech_Left_1[i])
  }
  else if (run_plays$RunDirection[i] == "Right A Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Left_2[i],
                                     run_plays$Tech_Left_2i[i],
                                     run_plays$Tech_Left_1[i],
                                     run_plays$Tech_0[i],
                                     run_plays$Tech_OB[i])
  }
  else if (run_plays$RunDirection[i] == "Right Off-Tackle B Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Left_4[i],
                                     run_plays$Tech_Left_4i[i],
                                     run_plays$Tech_Left_3[i],
                                     run_plays$Tech_Left_2[i])
  }
  else if (run_plays$RunDirection[i] == "Right Off-Tackle C Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Left_6[i],
                                     run_plays$Tech_Left_7[i],
                                     run_plays$Tech_Left_5[i],
                                     run_plays$Tech_Left_4[i])
  }
  else if (run_plays$RunDirection[i] == "Right D Gap") {
    run_plays$GapDefenders[i] <- sum(run_plays$Tech_Left_Outside[i], 
                                     run_plays$Tech_Left_9[i],
                                     run_plays$Tech_Left_6[i])
  }
  if (run_plays$GapDefenders[i] > 0) {
    run_plays$GapCovered[i] <- 1
  }
}

# aggregate to see performance per gap depending on if the gap was covered
run_gap_covered <- run_plays %>% group_by(Gap, GapCovered) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count
)

run_gap_covered %>% 
  mutate(Gap=factor(Gap, levels=c("D", "C", "B", "A", "Middle"))) %>%
  ggplot(aes(fill=as.factor(GapCovered), y=mean_EPA, x=Gap)) + 
    geom_bar(position="dodge", stat="identity", colour = "black") +
    ggtitle("Mean EPA per Gap By Gap Coverage") +
    labs(x = "Gap", y = "Mean EPA", fill = "Gap Covered") +
    scale_fill_manual(values=c("#CC6666", "#66CC99"))
ggsave("EPAperGapByGapCoverage.png")

ggplot(run_gap_covered, aes(fill=as.factor(GapCovered), y=success_rate, x=Gap)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(run_gap_covered, aes(fill=as.factor(GapCovered), y=used_gap_rate, x=Gap)) + 
  geom_bar(position="dodge", stat="identity")

run_gap_defenders <- run_plays %>% group_by(Gap, GapDefenders) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count
) %>% filter(count > 50)

run_defenders  <- run_plays %>% group_by(GapDefenders) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count
)

run_gap_defenders %>% 
  mutate(Gap=factor(Gap, levels=c("D", "C", "B", "A", "Middle"))) %>%
  ggplot(aes(fill=as.factor(GapDefenders), y=mean_EPA, x=Gap)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Mean EPA per Gap By No. of Gap Defenders") +
    labs(x = "Gap", y = "Mean EPA", fill = "No. of Gap Defenders") +
    scale_fill_brewer(palette = "Reds")


# plot distribution of EPA for solo tackles and assisted tackles to see if they have different value
ggplot(run_players[run_players$SoloTackle == 1,], aes(EPA)) +
  geom_density()

ggplot(run_players[run_players$AssistedTackle == 1,], aes(EPA)) +
  geom_density()

# aggregate tackles per position
run_tackles_tech <- run_players %>% group_by(TechniqueName) %>% summarise(
  count = n(),
  solo_tackles = sum(SoloTackle),
  solo_tackle_rate = solo_tackles / count,
  solo_tackle_EPA = sum(ifelse(SoloTackle == 1, EPA, 0)) / solo_tackles,
  asst_tackles = sum(AssistedTackle),
  asst_tackle_rate = asst_tackles / count,
  asst_tackle_EPA = sum(ifelse(AssistedTackle == 1, EPA, 0)) / asst_tackles,
  tackles = solo_tackles + asst_tackles,
  tackle_rate = tackles / count,
  tackle_EPA = sum(ifelse(SoloTackle == 1 | AssistedTackle == 1, EPA, 0)) / tackles,
  gap_opps = sum(GapOpportunity),
  gap_tackles = sum(ifelse(GapOpportunity == 1 & (SoloTackle == 1 | AssistedTackle == 1), 1, 0)),
  gap_tackle_rate = gap_tackles / gap_opps,
  gap_tackle_EPA = sum(ifelse(GapOpportunity == 1 & (SoloTackle == 1 | AssistedTackle == 1), EPA, 0)) / gap_tackles,
  out_of_gap_tackles = sum(ifelse(GapOpportunity == 0 & (SoloTackle == 1 | AssistedTackle == 1), 1, 0)),
  out_of_gap_tackle_rate = out_of_gap_tackles / (count - gap_opps),
  out_of_gap_tackle_EPA = sum(ifelse(GapOpportunity == 0 & (SoloTackle == 1 | AssistedTackle == 1), EPA, 0)) / out_of_gap_tackles,
  gap_rate_diff = gap_tackle_rate - out_of_gap_tackle_rate,
  gap_EPA_diff = gap_tackle_EPA - out_of_gap_tackle_EPA
)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = solo_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.1)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = asst_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.1)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = solo_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.5, 0)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = asst_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.5, 0)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = tackle_rate)) +
  geom_bar(stat = "identity")

ggplot(run_tackles_tech, aes(x = TechniqueName, y = tackle_EPA)) +
  geom_bar(stat = "identity")

ggplot(run_tackles_tech, aes(x = TechniqueName, y = gap_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.2)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = out_of_gap_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.2)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = gap_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.6, 0)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = out_of_gap_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.6, 0)

ggplot(run_tackles_tech, aes(x = TechniqueName, y = gap_EPA_diff)) +
  geom_bar(stat = "identity")

# group by assigned position to see tackle stats
run_tackles_pos <- run_players %>% group_by(assigned_position) %>% summarise(
  count = n(),
  solo_tackles = sum(SoloTackle),
  solo_tackle_rate = solo_tackles / count,
  solo_tackle_EPA = sum(ifelse(SoloTackle == 1, EPA, 0)) / solo_tackles,
  asst_tackles = sum(AssistedTackle),
  asst_tackle_rate = asst_tackles / count,
  asst_tackle_EPA = sum(ifelse(AssistedTackle == 1, EPA, 0)) / asst_tackles,
  tackles = solo_tackles + asst_tackles,
  tackle_rate = tackles / count,
  tackle_EPA = sum(ifelse(SoloTackle == 1 | AssistedTackle == 1, EPA, 0)) / tackles,
  gap_opps = sum(GapOpportunity),
  gap_tackles = sum(ifelse(GapOpportunity == 1 & (SoloTackle == 1 | AssistedTackle == 1), 1, 0)),
  gap_tackle_rate = gap_tackles / gap_opps,
  gap_tackle_EPA = sum(ifelse(GapOpportunity == 1 & (SoloTackle == 1 | AssistedTackle == 1), EPA, 0)) / gap_tackles,
  out_of_gap_tackles = sum(ifelse(GapOpportunity == 0 & (SoloTackle == 1 | AssistedTackle == 1), 1, 0)),
  out_of_gap_tackle_rate = out_of_gap_tackles / (count - gap_opps),
  out_of_gap_tackle_EPA = sum(ifelse(GapOpportunity == 0 & (SoloTackle == 1 | AssistedTackle == 1), EPA, 0)) / out_of_gap_tackles,
  gap_rate_diff = gap_tackle_rate - out_of_gap_tackle_rate,
  gap_EPA_diff = gap_tackle_EPA - out_of_gap_tackle_EPA
) %>% filter(assigned_position != "non-standard")

run_tackles_pos_tackle_rates <- melt(run_tackles_pos, id.vars = "assigned_position", 
                                     measure.vars = c("solo_tackle_rate", "asst_tackle_rate", "tackle_rate",
                                                      "gap_tackle_rate", "out_of_gap_tackle_rate"))

run_tackles_pos_tackle_EPAs <- melt(run_tackles_pos, id.vars = "assigned_position", 
                                     measure.vars = c("solo_tackle_EPA", "asst_tackle_EPA", "tackle_EPA",
                                                      "gap_tackle_EPA", "out_of_gap_tackle_EPA"))
run_tackles_pos_tackle_rates %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL"))) %>%
  ggplot(aes(fill=assigned_position, y=value, x=variable)) + 
    geom_bar(position="dodge", stat="identity", colour = "black") +
    ggtitle("Tackle Rates by Assigned Position") +
    labs(x = "Metric", y = "Value", fill = "Assigned Position")
ggsave("TackleRatesbyPos.png")

run_tackles_pos_tackle_EPAs %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL"))) %>%
  ggplot(aes(fill=assigned_position, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
    ggtitle("EPA per Tackle Type by Assigned Position") +
    labs(x = "Metric", y = "Value", fill = "Assigned Position")
ggsave("TackleEPAsbyPos.png")


ggplot(run_tackles_pos, aes(x = assigned_position, y = solo_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.1)

ggplot(run_tackles_pos, aes(x = assigned_position, y = asst_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.1)

ggplot(run_tackles_pos, aes(x = assigned_position, y = solo_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.5, 0)

ggplot(run_tackles_pos, aes(x = assigned_position, y = asst_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.5, 0)

ggplot(run_tackles_pos, aes(x = assigned_position, y = tackle_rate)) +
  geom_bar(stat = "identity")

ggplot(run_tackles_pos, aes(x = assigned_position, y = tackle_EPA)) +
  geom_bar(stat = "identity")

ggplot(run_tackles_pos, aes(x = assigned_position, y = gap_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.2)

ggplot(run_tackles_pos, aes(x = assigned_position, y = out_of_gap_tackle_rate)) +
  geom_bar(stat = "identity") +
  ylim(0, 0.2)

ggplot(run_tackles_pos, aes(x = assigned_position, y = gap_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.6, 0)

ggplot(run_tackles_pos, aes(x = assigned_position, y = out_of_gap_tackle_EPA)) +
  geom_bar(stat = "identity") +
  ylim(-0.6, 0)

ggplot(run_tackles_pos, aes(x = assigned_position, y = gap_EPA_diff)) +
  geom_bar(stat = "identity")

# see if fumbles happen with any kind of pattern
run_fumbles_gap <- run_plays %>% group_by(Gap) %>% summarise(
  count = n(),
  turnovers = sum(Turnover),
  to_rate = turnovers / count
)

# look at how game state affects metrics
run_game_state_plays <- run_plays %>% group_by(OffGameState) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count,
  gap_covered_rate = mean(GapCovered),
  mean_gap_defenders = mean(GapDefenders)
)

ggplot(run_game_state_plays, aes(x = OffGameState, y = mean_EPA, fill = gap_covered_rate)) +
  geom_bar(stat = "identity", colour = "black") +
  ggtitle("Rushing EPA by Offensive Game State") +
  labs(x = "Offensive Game State", y = "Mean EPA", fill = "Gap Coverage Rate") +
  scale_fill_distiller(palette = "YlGn", direction = 1)
ggsave("RushingEPAByGameState.png")

run_game_state_players <- run_players %>% group_by(assigned_position, OffGameState) %>% summarise(
  count = n(),
  solo_tackles = sum(SoloTackle),
  solo_tackle_rate = solo_tackles / count,
  solo_tackle_EPA = sum(ifelse(SoloTackle == 1, EPA, 0)) / solo_tackles,
  asst_tackles = sum(AssistedTackle),
  asst_tackle_rate = asst_tackles / count,
  asst_tackle_EPA = sum(ifelse(AssistedTackle == 1, EPA, 0)) / asst_tackles,
  tackles = solo_tackles + asst_tackles,
  tackle_rate = tackles / count,
  tackle_EPA = sum(ifelse(SoloTackle == 1 | AssistedTackle == 1, EPA, 0)) / tackles,
  gap_opps = sum(GapOpportunity),
  gap_opp_rate = gap_opps / count,
  gap_tackles = sum(ifelse(GapOpportunity == 1 & (SoloTackle == 1 | AssistedTackle == 1), 1, 0)),
  gap_tackle_rate = gap_tackles / gap_opps,
  gap_tackle_EPA = sum(ifelse(GapOpportunity == 1 & (SoloTackle == 1 | AssistedTackle == 1), EPA, 0)) / gap_tackles,
  out_of_gap_tackles = sum(ifelse(GapOpportunity == 0 & (SoloTackle == 1 | AssistedTackle == 1), 1, 0)),
  out_of_gap_tackle_rate = out_of_gap_tackles / (count - gap_opps),
  out_of_gap_tackle_EPA = sum(ifelse(GapOpportunity == 0 & (SoloTackle == 1 | AssistedTackle == 1), EPA, 0)) / out_of_gap_tackles,
  gap_rate_diff = gap_tackle_rate - out_of_gap_tackle_rate,
  gap_EPA_diff = gap_tackle_EPA - out_of_gap_tackle_EPA
) %>% filter(assigned_position != "non-standard")

ggplot(run_game_state_players, aes(fill=as.factor(OffGameState), y=tackle_rate, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(run_game_state_players, aes(fill=as.factor(OffGameState), y=tackle_EPA, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(run_game_state_players, aes(fill=as.factor(OffGameState), y=gap_rate_diff, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(run_game_state_players, aes(fill=as.factor(OffGameState), y=gap_EPA_diff, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(run_game_state_players, aes(fill=as.factor(OffGameState), y=gap_opps, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

run_game_state_players %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL"))) %>%
  ggplot(aes(fill=as.factor(OffGameState), y=gap_opp_rate, x=assigned_position)) + 
    geom_bar(position="dodge", stat="identity", colour = "black") +
    ggtitle("Gap Opportunity Rate By Game State by Position") +
    labs(x = "Assigned Position", y = "Gap Opportunity Rate", fill = "Offesnive Game State") + 
    scale_fill_manual(values = c("#FF3333", "#FFCC66", "#FFFF99", "#99FF99", "#339933"))
ggsave("GapOppsByGameState.png")

# try to create a model to predict rushing success
# first explore the distribution of rushing success
ggplot(run_plays, aes(EPA)) +
  geom_freqpoly()

run_EPA_DL_model <- lm(EPA ~ Tech_Left_Outside + Tech_Left_9 + Tech_Left_6 + Tech_Left_7 + Tech_Left_5 + Tech_Left_4 + Tech_Left_4i + 
                         Tech_Left_3 + Tech_Left_2 + Tech_Left_2i + Tech_Left_1 + Tech_0 + Tech_OB + Tech_Right_1 + 
                         Tech_Right_2i + Tech_Right_2 + Tech_Right_3 + Tech_Right_4i + Tech_Right_4 + Tech_Right_5 + 
                         Tech_Right_7 + Tech_Right_6 + Tech_Right_9 + Tech_Right_Outside, data = run_plays)

summary(run_EPA_DL_model)

run_EPA_total_DL_model <- lm(EPA ~ totalDL + GapDefenders, data = run_plays)

summary(run_EPA_total_DL_model)

run_EPA_DL_fieldpos_model <- lm(EPA ~ totalDL + GapDefenders + Down + ToGo + yds_to_goal, data = run_plays)

summary(run_EPA_DL_fieldpos_model)

# see if the number of DL's by roster position matter for run game
run_plays_pos <- run_players %>% group_by(GameID, EventID) %>% summarise(
  DTs = sum(ifelse(RosterPosition == "DT", 1, 0)),
  DEs = sum(ifelse(RosterPosition == "DE", 1, 0)),
  LBs = sum(ifelse(RosterPosition == "LB", 1, 0)),
  DBs = sum(ifelse(RosterPosition == "S" | RosterPosition == "CB", 1, 0)),
  Others = sum(ifelse(RosterPosition == "FB" | RosterPosition == "WR" | RosterPosition == "T", 1, 0)),
  DLs = DTs + DEs,
  non_DLs = LBs + DBs + Others,
  DL_diff = DLs - non_DLs,
  EPA = mean(EPA),
  success = mean(success),
  used_gap = mean(ifelse(UsedDesignedGap == 1, 1, 0))
)

run_EPA_DL_rostpos_model <- lm(EPA ~ DTs + DEs + LBs + DBs + Others, data = run_plays_pos)
summary(run_EPA_DL_rostpos_model)

# check how DTs affect run success
run_DTs <- run_plays_pos %>% group_by(DTs) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = mean(used_gap)
)

ggplot(run_DTs, aes(x = DTs, y = mean_EPA, fill = count)) +
  geom_bar(stat = "identity", colour = "black") +
  ggtitle("Rushing Success By No. of DTs") +
  labs(x = "No. of DTs according to roster", y ="Mean EPA", fill = "No. of Plays") +
  scale_fill_distiller(palette = "RdYlBu")
ggsave("RushEPAbyDT.png")

ggplot(run_DTs, aes(x = DTs, y = used_gap_rate)) +
  geom_bar(stat = "identity")

# check how DEs affect run success
run_DEs <- run_plays_pos %>% group_by(DEs) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = mean(used_gap)
)

ggplot(run_DEs, aes(x = DEs, y = mean_EPA, fill = count)) +
  geom_bar(stat = "identity", colour = "black") +
  ggtitle("Rushing Success By No. of DEs") +
  labs(x = "No. of DEs according to roster", y ="Mean EPA", fill = "No. of Plays") +
  scale_fill_distiller(palette = "RdYlBu")
ggsave("RushEPAbyDE.png")

ggplot(run_DEs, aes(x = DEs, y = used_gap_rate)) +
  geom_bar(stat = "identity")

# check how other positions affect run success
run_nonDLs <- run_plays_pos %>% group_by(non_DLs) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = mean(used_gap)
) %>% filter(count > 50)

ggplot(run_nonDLs, aes(x = non_DLs, y = mean_EPA, fill = count)) +
  geom_bar(stat = "identity", colour = "black") +
  ggtitle("Rushing Success By No. of Non-DLs") +
  labs(x = "No. of non-DLs according to roster", y ="Mean EPA", fill = "No. of Plays") +
  scale_fill_distiller(palette = "RdYlBu")
ggsave("RushEPAbynonDL.png")

ggplot(run_nonDLs, aes(x = non_DLs, y = used_gap_rate)) +
  geom_bar(stat = "identity")

# check how the difference in roster DLs vs roster non-DLs affects run success
run_DL_diff <- run_plays_pos %>% group_by(DL_diff) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = mean(used_gap)
) %>% filter(count > 50)

ggplot(run_DL_diff, aes(x = DL_diff, y = mean_EPA, fill = count)) +
  geom_bar(stat = "identity", colour = "black") +
  ggtitle("Rushing Success By Difference in DL and Non-DL") +
  labs(x = "No. of DLs - No. of Non-DLs according to roster", y ="Mean EPA", fill = "No. of Plays") +
  scale_fill_distiller(palette = "RdYlBu")
ggsave("RushEPAbyDLDiff.png")

ggplot(run_DL_diff, aes(x = DL_diff, y = used_gap_rate)) +
  geom_bar(stat = "identity")

# check on/off splits for each position
run_inside1 <- run_plays %>% group_by(Inside1) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count,
  gap_covered_rate = mean(GapCovered)
) %>% rename(on_off = Inside1)

run_middle3 <- run_plays %>% group_by(Middle3) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count,
  gap_covered_rate = mean(GapCovered)
) %>% rename(on_off = Middle3)

run_endDL <- run_plays %>% group_by(EndDL) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count,
  gap_covered_rate = mean(GapCovered)
) %>% rename(on_off = EndDL)

run_outside_nonDL <- run_plays %>% group_by(OutsidenonDL) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  used_gap_rate = sum(ifelse(UsedDesignedGap == 1, 1, 0))/count,
  gap_covered_rate = mean(GapCovered)
) %>% rename(on_off = OutsidenonDL)

run_pos_on_off <- rbind(run_inside1, run_middle3, run_endDL, run_outside_nonDL)
run_pos_on_off <- cbind("assigned_position" = c("Inside 1 Tech", "Inside 1 Tech", "Middle 3 Tech", "Middle 3 Tech",
                                                "End DL", "End DL", "Outside non-DL", "Outside non-DL"), run_pos_on_off)

run_pos_on_off %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL"))) %>%
  ggplot(aes(x = assigned_position, y = mean_EPA, fill = as.factor(on_off))) +
    geom_bar(position = "dodge", stat = "identity", colour = "black") +
    ggtitle("Rush EPA when Each Position is On vs Off the Field") +
    labs(x = "Assigned Position", y = "Mean EPA", fill = "On/Off") +
    scale_fill_manual(values=c("#CC6666", "#66CC99"))
ggsave("RushOnOff.png")

# pass analysis
pass_players <- data_all %>% filter(EventType == "pass" & Spike == 0)
pass_plays <- pass_plays %>% filter(Spike == 0)
pass_players$ThrowDepth <- as.numeric(levels(pass_players$ThrowDepth)[pass_players$ThrowDepth])
pass_plays$ThrowDepth <- as.numeric(levels(pass_plays$ThrowDepth)[pass_plays$ThrowDepth])

# analyze passing metrics by number of DLs on the field
pass_num_DL <- pass_plays %>% group_by(totalDL) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  to_rate = mean(Turnover),
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  pressure_rate = sum(ifelse(PressureOnPlay == 1, 1, 0)) / count,
  sack_rate = sum(ifelse(SackOnPlay == 1, 1, 0)) / count,
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count,
  sacks_per_pressure = sum(ifelse(SackOnPlay == 1, 1, 0)) / sum(ifelse(PressureOnPlay == 1, 1, 0))
) %>% filter(count > 50)

ggplot(pass_num_DL, aes(fill=mean_EPA, y=pressure_rate, x=totalDL)) + 
  geom_bar(stat="identity", colour = "black") +
  ggtitle("Pressure Rate and EPA by Number of DL") +
  labs(x = "No. of Defensive Linemen", y = "Pressure Rate", fill = "Mean EPA") +
  scale_fill_gradient(low = "#009933", high = "#33CC66")
ggsave("PressureRateByDL.png")

# analyze passing metrics by number of DLs rushing the passer
pass_num_PR <- pass_plays %>% group_by(totalPR) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  to_rate = mean(Turnover),
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  pressure_rate = sum(ifelse(PressureOnPlay == 1, 1, 0)) / count,
  sack_rate = sum(ifelse(SackOnPlay == 1, 1, 0)) / count,
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count,
  sacks_per_pressure = sum(ifelse(SackOnPlay == 1, 1, 0)) / sum(ifelse(PressureOnPlay == 1, 1, 0))
) %>% filter(count > 50)

ggplot(pass_num_PR, aes(fill=mean_EPA, y=pressure_rate, x=totalPR)) + 
  geom_bar(stat="identity", colour = "black") +
  ggtitle("Pressure Rate and EPA by Number of Pass Rushers") +
  labs(x = "No. of Pass Rushers", y = "Pressure Rate", fill = "Mean EPA") +
  scale_fill_gradient(low = "#006633" , high = "#33CC66")
ggsave("PressureRateByPassRushers.png")

pass_pressure <- pass_plays %>% group_by(PressureOnPlay) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  sack_rate = sum(ifelse(SackOnPlay == 1, 1, 0)) / count,
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  to_rate = mean(Turnover),
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count
)

pass_pressure_metrics = melt(pass_pressure, id.vars = "PressureOnPlay", measure.vars = c("mean_EPA", "success_rate",
                                                                                         "comp_rate", "sack_rate", 
                                                                                         "pbu_rate", "to_rate",
                                                                                         "int_rate", "qb_fum_rate"))

ggplot(pass_pressure_metrics, aes(fill=PressureOnPlay, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  ggtitle("Passing Metrics when Pressured vs Not") +
  labs(x = "Metric", y = "Value", fill = "Pressure on Play") +
  scale_fill_manual(values=c("#CC6666", "#66CC99"))
ggsave("PassingMetricsIfPressure.png")

# pressure is key. which positions get pressures
pass_positions <- pass_players %>% filter(IsRushing == 1) %>% group_by(assigned_position) %>% summarise(
  count = n(),
  pressure_rate = sum(Pressure) / count,
  solo_sack_rate = sum(SoloSack) / count,
  asst_sack_rate = sum(AssistedSack) / count,
  sack_rate = (sum(SoloSack) + sum(AssistedSack)) / count,
  pbu_rate = sum(PassBreakup) / count
)

# which position is it most valuable to get pressure from
pass_pressure_pos <- pass_players %>% filter(Pressure == 1) %>% group_by(assigned_position) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  team_sack = sum(ifelse(SackOnPlay == "1", 1, 0)) / count,
  player_sack = (sum(SoloSack) + sum(AssistedSack)) / count,
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  to_rate = mean(Turnover),
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count
)

pass_pressure_pos_metrics = melt(pass_pressure_pos %>% filter(assigned_position != "non-standard") , 
                                 id.vars = "assigned_position", measure.vars = c("mean_EPA", "success_rate",
                                                                                 "team_sack", "player_sack",
                                                                                 "comp_rate", "pbu_rate", "to_rate", 
                                                                                 "int_rate", "qb_fum_rate"))

pass_pressure_pos_metrics %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                            "Outside non-DL"))) %>%
  ggplot(aes(fill=assigned_position, y=value, x=variable)) + 
    geom_bar(position="dodge", stat="identity", colour = "black") +
    ggtitle("Passing Metrics when Each Position Gets Pressure") +
    labs(x = "Metric", y = "Value", fill = "Assigned Position") +
    theme(axis.text.x = element_text(size = 8))
ggsave("PassingMetricsWhenPressured.png")

# calculate pressure metrics for each player
pass_player_perf <- pass_players %>% filter(IsRushing == 1) %>% group_by(Name, assigned_position) %>% summarise(
  count = n(),
  pressure_rate = sum(Pressure) / count,
  solo_sack_rate = sum(SoloSack) / count,
  asst_sack_rate = sum(AssistedSack) / count,
  sack_rate = (sum(SoloSack) + sum(AssistedSack)) / count,
  pbu_rate = sum(PassBreakup) / count
)

# filter to players with enough snaps to get a position and plot positional distributions
pass_player_perf_filtered <- pass_player_perf %>% filter(assigned_position != "non-standard")

pass_player_perf_filtered %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                            "Outside non-DL"))) %>%
  ggplot(aes(pressure_rate, colour = assigned_position)) +
    geom_freqpoly(binwidth = 0.03, size = 2) +
    ggtitle("Distribution of Pressure Rates by Position") +
    labs(x = "Pressure Rate", y = "Frequency", colour = "Assigned Position")
ggsave("PressureRateDistro.png")

# how does game state affect pressure for each position
pass_positions_game_state <- pass_players %>% filter(IsRushing == 1) %>% group_by(OffGameState, assigned_position) %>% summarise(
  count = n(),
  pressure_rate = sum(Pressure) / count,
  solo_sack_rate = sum(SoloSack) / count,
  asst_sack_rate = sum(AssistedSack) / count,
  sack_rate = (sum(SoloSack) + sum(AssistedSack)) / count,
  pbu_rate = sum(PassBreakup) / count
) %>% filter(assigned_position != "non-standard")

pass_positions_game_state %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL"))) %>%
  ggplot(aes(fill=as.factor(OffGameState), y=pressure_rate, x=assigned_position)) + 
    geom_bar(position="dodge", stat="identity", colour = "black") +
    ggtitle("Pressure Rate by Offensive Game State") +
    scale_fill_manual(values = c("#FF3333", "#FFCC66", "#FFFF99", "#99FF99", "#339933")) +
    labs(x = "Assigned Position", y = "Pressure Rate", fill = "Offensive Game State")
ggsave("PressureRateByGameState.png")

ggplot(pass_positions_game_state, aes(fill=as.factor(OffGameState), y=solo_sack_rate, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(pass_positions_game_state, aes(fill=as.factor(OffGameState), y=sack_rate, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

pass_positions_game_state %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL"))) %>%
  ggplot(aes(fill=as.factor(OffGameState), y=sack_rate, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  ggtitle("Sack Rate by Offensive Game State") +
  scale_fill_manual(values = c("#FF3333", "#FFCC66", "#FFFF99", "#99FF99", "#339933")) +
  labs(x = "Assigned Position", y = "Sack Rate", fill = "Offensive Game State")
ggsave("SackRateByGameState.png")

ggplot(pass_positions_game_state, aes(fill=as.factor(OffGameState), y=pbu_rate, x=assigned_position)) + 
  geom_bar(position="dodge", stat="identity")

# what about plays with multiple pressures
pass_pressure_plays <- pass_players %>% filter(Pressure == 1) %>% group_by(GameID, EventID) %>% summarise(
  pressures = n(),
  inside_pressures = sum(ifelse(assigned_position == "Inside 1 Tech", 1, 0)),
  middle_pressures = sum(ifelse(assigned_position == "Middle 3 Tech", 1, 0)),
  end_pressures = sum(ifelse(assigned_position == "End DL", 1, 0)),
  non_DL_pressures = sum(ifelse(assigned_position == "Outside non-DL", 1, 0)),
  non_standard_pressures = sum(ifelse(assigned_position == "non-standard", 1, 0)),
  EPA = mean(EPA),
  success = mean(success),
  sack = mean(ifelse(SackOnPlay == "1", 1, 0)),
  turnover = mean(Turnover),
  pbu = mean(ifelse(PassBreakupOnPlay == "1", 1, 0)),
  completion = mean(ifelse(Completion == "1", 1, 0)),
  throw_depth = mean(ThrowDepth)
)

pass_num_pressures <- pass_pressure_plays %>% group_by(pressures) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  sack_rate = mean(sack),
  to_rate = mean(turnover),
  comp_rate = mean(completion),
  avg_depth = mean(throw_depth),
  pbu_rate = mean(pbu)
)

pass_num_pressure_metrics = melt(pass_num_pressures %>% filter(pressures < 4) , 
                                 id.vars = "pressures", measure.vars = c("mean_EPA", "success_rate","sack_rate", 
                                                                         "to_rate", "comp_rate", "pbu_rate"))

ggplot(pass_num_pressure_metrics, aes(fill=as.factor(pressures), y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  scale_fill_brewer("No. of Pressures", palette = "YlGn") +
  ggtitle("Passing Metrics by Number of Defenders Getting Pressure") +
  labs(x = "Metric", y = "Value")
ggsave("PassingMetricsByPressures.png")

# calculate ratios of solo to combo pressures for each position
inside_solo_pressures <- nrow(subset(pass_pressure_plays, pressures == 1 & inside_pressures == 1))
inside_combo_pressures <- nrow(subset(pass_pressure_plays, pressures > 1 & inside_pressures > 0))
middle_solo_pressures <- nrow(subset(pass_pressure_plays, pressures == 1 & middle_pressures == 1))
middle_combo_pressures <- nrow(subset(pass_pressure_plays, pressures > 1 & middle_pressures > 0))
end_solo_pressures <- nrow(subset(pass_pressure_plays, pressures == 1 & end_pressures == 1))
end_combo_pressures <- nrow(subset(pass_pressure_plays, pressures > 1 & end_pressures > 0))
non_DL_solo_pressures <- nrow(subset(pass_pressure_plays, pressures == 1 & non_DL_pressures == 1))
non_DL_combo_pressures <- nrow(subset(pass_pressure_plays, pressures > 1 & non_DL_pressures > 0))
non_standard_solo_pressures <- nrow(subset(pass_pressure_plays, pressures == 1 & non_standard_pressures == 1))
non_standard_combo_pressures <- nrow(subset(pass_pressure_plays, pressures > 1 & non_standard_pressures > 0))

pass_pos_pressures <- cbind.data.frame("assigned_position" = c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                               "Outside non-DL", "non-standard"),
                                       "solo_pressures" = c(inside_solo_pressures, middle_solo_pressures,
                                                            end_solo_pressures, non_DL_solo_pressures, non_standard_solo_pressures),
                                       "combo_pressures" = c(inside_combo_pressures, middle_combo_pressures,
                                                             end_combo_pressures, non_DL_combo_pressures,
                                                             non_standard_combo_pressures))
pass_pos_pressures <- pass_pos_pressures %>% mutate(
  solo_pressure_rate = solo_pressures / (solo_pressures + combo_pressures),
  combo_pressure_rate = combo_pressures / (solo_pressures + combo_pressures)
)

pass_pos_pressures_melt <- melt(pass_pos_pressures, id.vars = "assigned_position", measure.vars = c("solo_pressures",
                                                                                                    "combo_pressures"))
pass_pos_pressures_melt %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL", "non-standard"))) %>%
  ggplot(aes(fill=variable, y=value, x=assigned_position)) + 
    geom_bar(position="dodge", stat="identity", colour = "black") +
    scale_fill_manual(values=c("#CC6666", "#66CC99")) +
    ggtitle("Pressures by Position") +
    labs(x = "Assigned Position", y="No. of Pressures", fill = "Pressure Type")
ggsave("PressuresByPosition.png")    

ggplot(pass_pos_pressures, aes(y=solo_pressure_rate, x=assigned_position)) + 
  geom_bar(stat="identity")

# create a field listing each position who pressured per play
pass_pressure_plays$pressure_positions <- "blank"
for (i in 1:nrow(pass_pressure_plays)) {
  vector <- c()
  if (pass_pressure_plays$inside_pressures[[i]] > 0) {
    vector <- c(vector, rep("Inside 1 Tech", pass_pressure_plays$inside_pressures[[i]]))
  }
  if (pass_pressure_plays$middle_pressures[[i]] > 0) {
    vector <- c(vector, rep("Middle 3 Tech", pass_pressure_plays$middle_pressures[[i]]))
  }
  if (pass_pressure_plays$end_pressures[[i]] > 0) {
    vector <- c(vector, rep("End DL", pass_pressure_plays$end_pressures[[i]]))
  }
  if (pass_pressure_plays$non_DL_pressures[[i]] > 0) {
    vector <- c(vector, rep("Outside non_DL", pass_pressure_plays$non_DL_pressures[[i]]))
  }
  if (pass_pressure_plays$non_standard_pressures[[i]] > 0) {
    vector <- c(vector, rep("non_standard", pass_pressure_plays$non_standard_pressures[[i]]))
  }
  pass_pressure_plays$pressure_positions[i] <- paste(vector, collapse = " ")
}

pass_mult_pressures <- pass_pressure_plays %>% group_by(pressure_positions) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  sack_rate = mean(sack),
  to_rate = mean(turnover),
  comp_rate = mean(completion),
  avg_depth = mean(throw_depth),
  pbu_rate = mean(pbu)
)

head(pass_mult_pressures %>% arrange(desc(count)), 10)
pass_top_common_mult_pressures <- head(pass_mult_pressures %>% filter(count < 100) %>% arrange(desc(count)), 5)

pass_top_common_mult_pressures %>% 
  mutate(pressure_positions=factor(pressure_positions, levels=pressure_positions)) %>%
  ggplot(aes(fill=mean_EPA, y=count, x=pressure_positions)) + 
    geom_bar(stat="identity", colour = "black") +
    ggtitle("Most Frequent Combo Pressures") +
    theme(axis.text.x = element_text(size = 8)) +
    scale_fill_distiller(palette = "YlGn") +
    labs(x = "Pressuring Positions", y = "No. of Plays", fill = "Avg EPA")
ggsave("ComboPressuresMost.png")

pass_best_mult_pressures <- head(pass_mult_pressures %>% filter(count > 10) %>% arrange(mean_EPA), 10)

ggplot(pass_best_mult_pressures, aes(y=mean_EPA, x=pressure_positions)) + 
  geom_bar(stat="identity") +
  

pass_best_mult_pressures_sack <- head(pass_mult_pressures %>% filter(count > 10) %>% arrange(desc(sack_rate)), 5)

pass_best_mult_pressures_sack %>% 
  mutate(pressure_positions=factor(pressure_positions, levels=pressure_positions)) %>%
  ggplot(aes(y=sack_rate, x=pressure_positions)) + 
    geom_bar(stat="identity", colour = "black") +
    ggtitle("Top Sack Rates for Combo Pressures") +
    labs(x = "Pressuring Positions", y = "Sack Rate") +
    theme(axis.text.x = element_text(size = 8)) +
    geom_hline(yintercept = sum(pass_pressure_plays$sack) / nrow(pass_pressure_plays), color = "red") +
    geom_text(aes(1,sum(pass_pressure_plays$sack) / nrow(pass_pressure_plays),label = "Avg Sack Rate", vjust = 1))
ggsave("ComboPressureTopSackRate.png")

# check on/off splits for each position
pass_inside1 <- pass_plays %>% group_by(Inside1) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  pressure_rate = sum(ifelse(PressureOnPlay == 1, 1, 0)) / count,
  sack_rate = sum(ifelse(SackOnPlay == 1, 1, 0)) / count,
  sacks_per_pressure = sum(ifelse(SackOnPlay == 1, 1, 0)) / sum(ifelse(PressureOnPlay == 1, 1, 0)),
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  to_rate = mean(Turnover),
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count
) %>% rename(on_off = Inside1)

pass_middle3 <- pass_plays %>% group_by(Middle3) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  pressure_rate = sum(ifelse(PressureOnPlay == 1, 1, 0)) / count,
  sack_rate = sum(ifelse(SackOnPlay == 1, 1, 0)) / count,
  sacks_per_pressure = sum(ifelse(SackOnPlay == 1, 1, 0)) / sum(ifelse(PressureOnPlay == 1, 1, 0)),
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  to_rate = mean(Turnover),
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count
) %>% rename(on_off = Middle3)

pass_EndDL <- pass_plays %>% group_by(EndDL) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  pressure_rate = sum(ifelse(PressureOnPlay == 1, 1, 0)) / count,
  sack_rate = sum(ifelse(SackOnPlay == 1, 1, 0)) / count,
  sacks_per_pressure = sum(ifelse(SackOnPlay == 1, 1, 0)) / sum(ifelse(PressureOnPlay == 1, 1, 0)),
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  to_rate = mean(Turnover),
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count
) %>% rename(on_off = EndDL)

pass_outside_nonDL <- pass_plays %>% group_by(OutsidenonDL) %>% summarise(
  count = n(),
  mean_EPA = mean(EPA),
  success_rate = mean(success),
  comp_rate = sum(ifelse(Completion == 1, 1, 0)) / count,
  avg_depth = mean(ThrowDepth),
  pressure_rate = sum(ifelse(PressureOnPlay == 1, 1, 0)) / count,
  sack_rate = sum(ifelse(SackOnPlay == 1, 1, 0)) / count,
  sacks_per_pressure = sum(ifelse(SackOnPlay == 1, 1, 0)) / sum(ifelse(PressureOnPlay == 1, 1, 0)),
  pbu_rate = sum(ifelse(PassBreakupOnPlay == 1, 1, 0)) / count,
  to_rate = mean(Turnover),
  int_rate = sum(ifelse(InterceptionOnPlay == 1, 1, 0)) / count,
  qb_fum_rate = sum(ifelse(FumbleByPasser == 1, 1, 0)) / count
) %>% rename(on_off = OutsidenonDL)

pass_pos_on_off <- rbind(pass_inside1, pass_middle3, pass_EndDL, pass_outside_nonDL)
pass_pos_on_off <- cbind("assigned_position" = c("Inside 1 Tech", "Inside 1 Tech", "Middle 3 Tech", "Middle 3 Tech",
                                                "End DL", "End DL", "Outside non-DL", "Outside non-DL"), 
                         pass_pos_on_off)

pass_pos_on_off %>% 
  mutate(assigned_position=factor(assigned_position, levels=c("Inside 1 Tech", "Middle 3 Tech", "End DL", 
                                                              "Outside non-DL"))) %>%
  ggplot(aes(x = assigned_position, y = mean_EPA, fill = as.factor(on_off))) +
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  ggtitle("Pass EPA when Each Position is On vs Off the Field") +
  labs(x = "Assigned Position", y = "Mean EPA", fill = "On/Off") +
  scale_fill_manual(values=c("#CC6666", "#66CC99"))
ggsave("PassOnOff.png")
