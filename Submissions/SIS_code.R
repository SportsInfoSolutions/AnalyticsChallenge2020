remove(list = ls())
library(tidyverse)
library(ggrepel)
library(lme4)
library(modeest)
df <- read.csv("~/Downloads/AnalyticsChallenge2020-master/Data/AnalyticsChallenge2020Data.csv", stringsAsFactors=FALSE)
df$EPA <- as.numeric(df$EPA)
df$PressureOnPlay <- as.numeric(df$PressureOnPlay)
df$SackOnPlay <- as.numeric(df$SackOnPlay)

# Fix missing EPA from one play (data from nflfastR)
df$EPA[df$GameID == 2757 & df$EventID == 915] <- -8.13
df$EventType <- sub('challenge ', '', df$EventType)

# Remove kneels and spikes and add binary success variable
df <-
  df %>%
  filter(!str_detect(PlayDesc, 'kneel'), Spike!=1)  %>%
  mutate(success = ifelse(EPA >0, 1, 0))

df$tackles <- df$SoloTackle + .5*df$AssistedTackle

# Get run gap
df$RunGap <- str_sub(df$RunDirection,-5, -5)

# Keep only front seven players
df <-
  df %>% filter(RosterPosition == 'DE' | RosterPosition == 'DT' | RosterPosition == 'LB')

# Group techniques
df$TechniqueCat <- df$TechniqueName
df$TechniqueCat <- sub("9|7|6", "Outside", df$TechniqueCat)
df$TechniqueCat <- sub("0|1|2i|2", "Inside", df$TechniqueCat)
df$TechniqueCat <- sub("4|4i|5|3", "Middle", df$TechniqueCat)

player_snaps <-
  df %>% filter(EventType == 'pass' | EventType == 'rush') %>%
  mutate(n=1) %>%
  group_by(Name, TechniqueCat, EventType) %>%
  summarise(n_snaps = sum(n)) %>%
  pivot_wider(names_from = c(TechniqueCat, EventType), values_from = n_snaps)

pass_rushing_snaps <-
  df %>% filter(EventType == 'pass') %>%
  mutate(n=1) %>%
  group_by(Name) %>%
  summarise(rushing_pct = sum(IsRushing)/sum(n))
player_snaps <- left_join(player_snaps, pass_rushing_snaps, by = 'Name')
player_snaps$total_snaps <- rowSums(player_snaps[,2:9], na.rm = TRUE)
player_snaps[,2:9] <- player_snaps[,2:9] / player_snaps$total_snaps
player_snaps <- player_snaps %>% filter(total_snaps >= 5, rushing_pct > .5)
player_snaps[is.na(player_snaps)] <- 0

# Perform k-means clustering to get position groups
k2 <- kmeans(player_snaps[,2:9], centers = 3, nstart = 40)
player_snaps$cluster <- k2[["cluster"]]
# Rename based on a key player for each group
player_snaps$cluster <- gsub(player_snaps$cluster[player_snaps$Name=='Aaron Donald'], 'Tackle', player_snaps$cluster)
player_snaps$cluster <- gsub(player_snaps$cluster[player_snaps$Name=='Chandler Jones'], 'Edge', player_snaps$cluster)
player_snaps$cluster <- gsub(player_snaps$cluster[player_snaps$Name=='Vita Vea'], 'Nose', player_snaps$cluster)

# Join position clusters to df
df <- left_join(df, player_snaps %>% select(Name, cluster), by = 'Name')

df$cluster = factor(df$cluster, levels = c('Edge', 'Tackle', 'Nose'))

# Plot technique snaps by cluster
cluster_snaps <-
  df %>% filter(!is.na(cluster)) %>%
  mutate(n = 1) %>%
  group_by(cluster, TechniqueName, EventType) %>%
  summarise(n_snaps = sum(n)) %>% 
  left_join(df %>% 
              mutate(n = 1) %>%
              group_by(cluster) %>%
              summarise(cluster_snaps = sum(n)), by = 'cluster')
cluster_snaps$cluster = factor(cluster_snaps$cluster, levels = c('Nose', 'Tackle', 'Edge'))
cluster_snaps$TechniqueName = factor(cluster_snaps$TechniqueName, levels = c('0','1', '2i', '2', '3', '4i', '4', '5', '7', '6', '9', 'Outside', 'Off Ball'))

clutser_plot <- ggplot(cluster_snaps,
                       aes(TechniqueName, n_snaps/cluster_snaps, fill = EventType)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~cluster) +
  ylim(0, .275) +
  scale_fill_brewer(palette = "Set1") + ggtitle("Technique Allignments By Position") +
  xlab("Technqiue") + ylab("Snap Allotment") +
  scale_x_discrete(labels = c('Off Ball' = ' off', 'Outside' = 'olb')) +
  theme(text = element_text(size=rel(5.2)), plot.title = element_text(size = 28, face = "bold"),
        axis.text=element_text(size=21.5), 
        legend.title=element_text(size=19), 
        legend.text=element_text(size=19), legend.position="bottom",
        strip.text = element_text(size = 24), panel.spacing = unit(2, "lines"))

ggsave("cluster_snaps.png", width = 16.5, height = 9.3)

#upload draft and salary data
draft_picks <- read.csv("~/Downloads/draft_picks.csv")
draft_values <- read.csv("~/Downloads/draft_values.csv")
dl_salaries <- read.csv("~/Downloads/dl_salaries.csv")

# Clean team names
draft_picks$team <- sub("SD", "LAC", draft_picks$team)
draft_picks$team <- sub("STL", "LA", draft_picks$team)

# Clean the salary data
dl_salaries$Player <- sub("OUTSIDE LINEBACKER|INSIDE LINEBACKER|LINEBACKER|DEFENSIVE END|DEFENSIVE TACKLE", "", dl_salaries$Player)
dl_salaries$Salary <- as.numeric(gsub(",|\\$", "", dl_salaries$Salary))
dl_salaries$Player <- trimws(dl_salaries$Player)

# Clean up the name differences between sheets
dl_salaries$Player <- sub(" II[I]{0,1}", "", dl_salaries$Player)
df$Name <- sub(" II[I]{0,1}", "", df$Name)
dl_salaries$Player <- sub(" Jr[.]{0,1}", "", dl_salaries$Player)
df$Name <- sub(" Jr[.]{0,1}", "", df$Name)
dl_salaries$Player <- sub(" Sr[.]{0,1}", "", dl_salaries$Player)
df$Name <- sub(" Sr[.]{0,1}", "", df$Name)
draft_picks$full_name <- sub(" Jr[.]{0,1}", "", draft_picks$full_name)
draft_picks$full_name <- sub("Matthew Ioannidis", "Matt Ioannidis", draft_picks$full_name)
draft_picks$full_name <- sub(" IV", "", draft_picks$full_name)
draft_picks$full_name <- sub("Da'Ron Payne", "Daron Payne", draft_picks$full_name)
draft_picks$full_name <- sub("Sebastian Joseph", "Sebastian Joseph-Day", draft_picks$full_name)
draft_picks$full_name <- sub("Pat O'Connor", "Patrick O'Connor", draft_picks$full_name)
dl_salaries$Player <- sub("Daniel McCullers", "Dan McCullers", dl_salaries$Player)
dl_salaries$Player <- sub("Nate Gerry", "Nathan Gerry", dl_salaries$Player)
dl_salaries$Player <- sub("Jon Bostic", "Jonathan Bostic", dl_salaries$Player)
dl_salaries$Player <- sub("Justin March", "Justin March-Lillard", dl_salaries$Player)
dl_salaries$Player <- sub("A.J. Johnson", "Alexander Johnson", dl_salaries$Player)
dl_salaries$Player <- sub("P.J.", "PJ", dl_salaries$Player)
draft_picks$full_name <- sub("P.J.", "PJ", draft_picks$full_name)
dl_salaries$Player <- sub("B.J.", "BJ", dl_salaries$Player)
df$Name <- sub("B.J.", "BJ", df$Name)
draft_picks$full_name <- sub("B.J.", "BJ", draft_picks$full_name)
dl_salaries$Player <- sub("A.J.", "AJ", dl_salaries$Player)
draft_picks$full_name <- sub("A.J", "AJ", draft_picks$full_name)
dl_salaries$Player <- sub("R.J", "RJ", dl_salaries$Player)
draft_picks$full_name <- sub("R.J", "RJ", draft_picks$full_name)
draft_picks$full_name <- sub("RJ.", "RJ", draft_picks$full_name)

# Get only front 7 positions
draft_picks <-
  draft_picks %>% 
  filter(category == 'DL' | category == 'LB', season > 2015 | (season == 2015 & round ==1)) %>%
  select(pick, full_name)

# Bring in nflFastR team names for merging with Lee Sharpe's draft file
seasons <- 2019:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

team_names <-
  inner_join(df, pbp %>% select(defteam, desc), by = c('PlayDesc' = 'desc')) %>%
  group_by(DefensiveTeam, defteam) %>%
  summarise() %>%
  select(DefensiveTeam, defteam)

team_names[nrow(team_names) + 1,] = c("Rams","LA")

df <- left_join(df, team_names, by = 'DefensiveTeam') %>%
  left_join(team_names , by = c('OffensiveTeam' = 'DefensiveTeam')) 
df$DefensiveTeam <- df$defteam.x  
df$OffensiveTeam <- df$defteam.y 

player_names <- left_join(df, team_names, by = 'DefensiveTeam') %>%
  group_by(Name, DefensiveTeam) %>%
  summarise()

#Join with values
draft_picks <- inner_join(draft_picks, player_names, by = c('full_name' = 'Name')) %>%
  left_join(draft_values %>% select(pick, otc), by = 'pick')

# Join with salaries
dl_salaries <-
  left_join(dl_salaries, draft_picks, by = c('Player' = 'full_name'))
# Only keep salaries for players not on rookie deals
dl_salaries <-
  dl_salaries %>% filter(is.na(otc))

# Join to df
df <-
  left_join(df, draft_picks %>% select(full_name, otc), by = c('Name' = 'full_name'))
df <-
  left_join(df, dl_salaries %>% select(Player, Salary), by = c('Name' = 'Player'))

#A couple players missing from salary data who were cut before end of season
df$Salary[df$Name == 'Dion Jordan'] <- 331471
df$Salary[df$Name == 'Devaroe Lawrence'] <- 527348
df$Salary[df$Name == 'Sharif Finch'] <- 502941
df$Salary[df$Name == 'Chris Smith'] <- 2750000
df$Salary[df$Name == 'Joel Heath'] <- 354118
df$Salary[df$Name == 'Albert Huggins'] <- 340000
df$Salary[df$Name == 'Terrell McClain'] <- 443059
df$Salary[df$Name == 'Rob McCray'] <- 495000
df$Salary[df$Name == 'Jayrone Elliott'] <- 328850
df$Salary[df$Name == 'TY McGill'] <- 265558
df$Salary[df$Name == 'Ahmad Gooden' | df$Name == 'James Vaughters' | df$Name == 'Brandin Bryant' | df$Name == 'Carroll Phillips' | df$Name == 'Pete Robertson' | df$Name == 'Ryan Bee'] <- 136000

# Get z-scores
otc_z <- df %>% filter(!is.na(otc)) %>%
  group_by(Name, otc) %>%
  summarise()

salary_z <- df %>% filter(!is.na(Salary)) %>%
  group_by(Name, Salary) %>%
  summarise()

otc_z$draft_z <- scale(otc_z$otc)
salary_z$salary_z <- scale(salary_z$Salary)

# Join z-scores with df
df <-
  left_join(df, otc_z %>% select(Name, draft_z), by = 'Name') %>%
  left_join(salary_z %>% select(Name, salary_z), by = 'Name')

# Get z-scores in one column
df$player_z<-
  as.vector(pmax(df$salary_z, df$draft_z, na.rm = TRUE))

# Average value by cluster
cluster_value <-
  df %>%
  filter(!is.na(cluster)) %>%
  group_by(DefensiveTeam, cluster) %>% 
  summarise(avg_value = mean(player_z)) %>%
  pivot_wider(names_from = cluster, values_from = avg_value)

dat <- data.frame(average_value = c(cluster_value$Tackle, cluster_value$Edge, cluster_value$Nose)
                  , position = rep(c("Tackle", "Edge", 'Nose'), each = 32))

dat$position = factor(dat$position, levels = c('Edge', 'Tackle', 'Nose'))

mu <- dat %>% group_by(position) %>% summarise(avg_value = mean(average_value))

# Plot density
ggplot(dat, aes(x = average_value, fill = position)) +
  geom_density(alpha = 0.5) + 
  geom_vline(data = mu, aes(xintercept=avg_value, color = position)) +
  xlim(-1.5,2.5) + ggtitle("Team Resource Spending by Position") +
  xlab("Avg. Standardized Draft/FA Value by Team") +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))


# Plot pressure by cluster
df$cluster <- as.character(df$cluster)
technique_pressures <-
  df %>% filter(IsRushing == 1, EventType== 'pass') %>%
  filter(!is.na(cluster)) %>%
  mutate(n=1) %>%
  group_by(cluster,  Down) %>%
  summarise(pressures = mean(Pressure),
            sacks = mean(SoloSack)+(.5*mean(AssistedSack)),
            n_snaps = sum(n))

technique_pressures$cluster = factor(technique_pressures$cluster, levels = c('Edge', 'Tackle', 'Nose'))

technique_pressures %>%
  ggplot( aes(x=Down, y=pressures, group=cluster, color=cluster)) +
  geom_point(cex=(technique_pressures$n_snaps/400), alpha=1/2) +
  geom_line() + 
  ylab("Pressures Per Pass Rush Snap") +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))

technique_pressures %>%
  ggplot( aes(x=Down, y=sacks, group=cluster, color=cluster)) +
  geom_point(cex=(technique_pressures$n_snaps/400), alpha=1/2) +
  geom_line() + 
  ylab("Sacks Per Pass Rush Snap")  +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))


df$edge_pressure <- ifelse(df$Pressure == 1 & df$cluster == 'Edge', 1, 0)
df$tackle_pressure <- ifelse(df$Pressure == 1 & df$cluster == 'Tackle', 1, 0)
df$nose_pressure <- ifelse(df$Pressure == 1 & df$cluster == 'Nose', 1, 0)

pressure_model_data <-
  df %>% filter(EventType == 'pass') %>%
  group_by(DefensiveTeam, OffensiveTeam, GameID, EventID, Turnover, Touchdown) %>%
  summarise(EPA = mean(EPA),
            success = mean(success),
            down = mean(Down),
            ToGo = mean(ToGo),
            edge_pressure = max(edge_pressure, na.rm = TRUE),
            tackle_pressure = max(tackle_pressure, na.rm = TRUE),
            nose_pressure = max(nose_pressure, na.rm = TRUE))

lin_model <- lm(EPA ~ edge_pressure + tackle_pressure + nose_pressure + as.factor(down) + ToGo + OffensiveTeam + DefensiveTeam, data = pressure_model_data)
lin_model
summary(lin_model)$r.squared

logit_model <- glm(success ~ edge_pressure + tackle_pressure + nose_pressure + as.factor(down) + ToGo + OffensiveTeam + DefensiveTeam,
                 data = pressure_model_data, family = 'binomial')


# Mixed effects model to get player intercepts and position coefficients
mixed_model <- glmer(Pressure ~ as.factor(Down) + ToGo + TechniqueName + (1|Name),
                    data = df %>% filter(EventType == 'pass', IsRushing == 1, !is.na(cluster)),
                    family=binomial)

intercepts <-data.frame(ranef(mixed_model))

player_pressures <-
  df %>% filter(EventType == 'pass', IsRushing == 1, !is.na(cluster)) %>%
  mutate(n=1) %>%
  group_by(Name, cluster, player_z) %>%
  summarise(n_snaps = sum(n),
            pressure_rate = sum(Pressure)/n_snaps,
            DefensiveTeam = mlv(DefensiveTeam, method='mfv')) %>%
  filter(n_snaps > 25)

intercepts <-
  inner_join(intercepts, player_pressures %>% select(Name, DefensiveTeam, cluster, player_z), by = c('grp' = 'Name')) %>%
  arrange(-condval)

intercepts$grp <- factor(reorder(intercepts$grp, -intercepts$condval))

mu <- intercepts %>% group_by(cluster) %>% summarise(avg_value = mean(condval))
ggplot(intercepts, aes(x = condval, fill = cluster)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mu, aes(xintercept=avg_value, color = cluster)) +
  ggtitle("Mixed Effects Player Intercepts") +
  xlab("Logit Player Intercept") +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))

edge_intercepts <- intercepts %>% filter(cluster == 'Edge')
tackle_intercepts <- intercepts %>% filter(cluster == 'Tackle')
nose_intercepts <- intercepts %>% filter(cluster == 'Nose')
edge_cor <- round(cor(edge_intercepts$condval, edge_intercepts$player_z), 3)
tackle_cor <-round(cor(tackle_intercepts$condval, tackle_intercepts$player_z), 3)
nose_cor <- round(cor(nose_intercepts$condval, nose_intercepts$player_z), 3)

intercepts %>%
  ggplot(aes(x = player_z, y = condval)) +
  geom_point() +
  xlim(-1.24, 3.82) + ylim(-.36, .52) +
  facet_wrap(~cluster, labeller = labeller(cluster = 
                                             c("Edge" = paste('Edge: Cor =', edge_cor),
                                               "Tackle" = paste('Tackle: Cor =', tackle_cor),
                                               "Nose" = paste('Nose: Cor =', nose_cor)))) +
  geom_smooth(method='lm', formula= y~x, color='blue') +
  ylab('Player Intercept') +
  xlab('Standardized Draft/FA Value') +
  theme(text = element_text(size=rel(3.5)), plot.title = element_text(size = 28, face = "bold"),
        axis.text=element_text(size=10.5),
        strip.text = element_text(size = 12), panel.spacing = unit(2, "lines"))

ggsave("intercepts.png", width = 8.25, height = 4.6)


neg_epa_tackles <-
  df %>% filter(EventType == 'rush', EPA < 0, tackles > 0, !is.na(cluster), RunGap != 'i') %>%
  group_by(cluster, Down) %>%
  summarise(tackles = sum(tackles)) %>% 
  left_join(df %>% filter(EventType == 'rush') %>%
              mutate(n=1) %>% group_by(cluster, Down) %>%
              summarise(n_snaps = sum(n)), by = c('cluster', 'Down')) %>%
  mutate(rate = tackles/n_snaps) %>%
  filter(!is.na(cluster))

neg_epa_tackles %>%
  ggplot( aes(x=Down, y=rate, group=cluster, color=cluster)) +
  geom_point(cex=(neg_epa_tackles$n_snaps/400), alpha=1/2) +
  geom_line() + 
  ylab("Negative EPA Tackles Per Rush Snap") +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))

neg_epa_tackles <-
  df %>% filter(EventType == 'rush', EPA < 0, tackles > 0, RunGap != 'i') %>%
  group_by(cluster, Name) %>%
  summarise(tackles = sum(tackles)) %>% 
  left_join(df %>% filter(EventType == 'rush', RunGap != 'i') %>%
              mutate(n=1) %>% group_by(cluster, DefensiveTeam, Name, player_z) %>%
              summarise(n_snaps = sum(n)), by = c('cluster', 'Name')) %>%
  mutate(rate = tackles/n_snaps) %>%
  filter(!is.na(cluster)) %>%
  filter(n_snaps > 40) 

df$tackles_neg <- ifelse(df$EPA < 0, df$tackles, 0)
mixed_model2 <- lmer(tackles_neg ~ as.factor(Down) + ToGo + TechniqueName + (1|Name),
                     data = df %>% filter(!is.na(cluster), EventType == 'rush'))

intercepts2 <-data.frame(ranef(mixed_model2))
intercepts2 <-
  inner_join(intercepts2, neg_epa_tackles %>% select(Name, cluster, player_z), by = c('grp' = 'Name')) %>%
  arrange(-condval)

mu <- intercepts2 %>% group_by(cluster) %>% summarise(avg_value = mean(condval))
ggplot(intercepts2, aes(x = condval, fill = cluster)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mu, aes(xintercept=avg_value, color = cluster)) +
  ggtitle("Mixed Effects Player Intercepts") +
  xlab("Negative EPA Tackle Rate Over Expectation") +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))

edge_intercepts <- intercepts2 %>% filter(cluster == 'Edge')
tackle_intercepts <- intercepts2 %>% filter(cluster == 'Tackle')
nose_intercepts <- intercepts2 %>% filter(cluster == 'Nose')
edge_cor <- round(cor(edge_intercepts$condval, edge_intercepts$player_z), 3)
tackle_cor <-round(cor(tackle_intercepts$condval, tackle_intercepts$player_z), 3)
nose_cor <- round(cor(nose_intercepts$condval, nose_intercepts$player_z), 3)

intercepts2$cluster <- factor(intercepts2$cluster, levels = c('Edge', 'Tackle', 'Nose'))
intercepts2 %>%
  ggplot(aes(x = player_z, y = condval)) +
  geom_point() +
  xlim(-1.24, 3.82) + ylim(-.0175, .0191) +
  facet_wrap(~cluster, labeller = labeller(cluster = 
                                             c("Edge" = paste('Edge: Cor =', edge_cor),
                                               "Tackle" = paste('Tackle: Cor =', tackle_cor),
                                               "Nose" = paste('Nose: Cor =', nose_cor)))) +
  geom_smooth(method='lm', formula= y~x, color='blue') +
  ylab('Player Intercept') +
  xlab('Standardized Draft/FA Value') +
  theme(text = element_text(size=rel(3.5)), plot.title = element_text(size = 28, face = "bold"),
        axis.text=element_text(size=10.5),
        strip.text = element_text(size = 12), panel.spacing = unit(2, "lines"))

ggsave("intercepts2.png", width = 8.25, height = 4.6)


designated_gap <-
  df %>% filter(EventType=='rush', RunGap != 'i') %>%
  group_by(DefensiveTeam, Week, EventID, RunGap) %>%
  summarise(EPA = mean(EPA), UsedDesignedGap = mean(as.numeric(UsedDesignedGap))) %>%
  mutate(n=1) %>%
  group_by(RunGap, UsedDesignedGap) %>%
  summarise(
            EPA = mean(EPA),
            n_snaps = sum(n)) 

  # Binary variable for if the run was toward the defender's side
df$RunSide <- ifelse(substr(df$RunDirection,1, 1)!=substr(df$SideOfBall, 1 , 1) | df$SideOfBall == 'NULL' ,1, 0)

player_designated_gap <-
  df %>% filter(EventType == 'rush', RunSide == 1, RunGap != 'i') %>%
  mutate(n=1) %>% group_by(cluster, Name, player_z) %>%
  summarise(designated_gap = mean(as.numeric(UsedDesignedGap)),
    n_snaps = sum(n), DefensiveTeam = mlv(DefensiveTeam, method='mfv')) %>%
  filter(!is.na(cluster), n_snaps > 30)

mixed_model3 <- glmer(as.numeric(UsedDesignedGap) ~ RunGap + (1|Name),
                       data = df %>% filter(EventType == 'rush', RunSide == 1, RunGap != 'i'),
                     family = 'binomial')

intercepts3 <-data.frame(ranef(mixed_model3))
intercepts3 <-
  inner_join(intercepts3, player_designated_gap %>% select(Name, cluster, player_z), by = c('grp' = 'Name')) %>%
  arrange(-condval)

mu <- intercepts3 %>% group_by(cluster) %>% summarise(avg_value = mean(condval))
ggplot(intercepts3, aes(x = -condval, fill = cluster)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mu, aes(xintercept=avg_value, color = cluster)) +
  ggtitle("Mixed Effects Player Intercepts") +
  xlab("(Negative) Player Intercept") +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))

edge_intercepts <- intercepts3 %>% filter(cluster == 'Edge')
tackle_intercepts <- intercepts3 %>% filter(cluster == 'Tackle')
nose_intercepts <- intercepts3 %>% filter(cluster == 'Nose')
edge_cor <- round(cor(-edge_intercepts$condval, edge_intercepts$player_z), 3)
tackle_cor <-round(cor(-tackle_intercepts$condval, tackle_intercepts$player_z), 3)
nose_cor <- round(cor(-nose_intercepts$condval, nose_intercepts$player_z), 3)

intercepts3$cluster <- factor(intercepts3$cluster, levels = c('Edge', 'Tackle', 'Nose'))
intercepts3 %>%
  ggplot(aes(x = player_z, y = -condval)) +
  geom_point() +
  xlim(-1.24, 3.82) + ylim(-.265, .285) +
  facet_wrap(~cluster, labeller = labeller(cluster = 
                                             c("Edge" = paste('Edge: Cor =', edge_cor),
                                               "Tackle" = paste('Tackle: Cor =', tackle_cor),
                                               "Nose" = paste('Nose: Cor =', nose_cor)))) +
  geom_smooth(method='lm', formula= y~x, color='blue') +
  ylab('(Negative) Player Intercept') +
  xlab('Standardized Draft/FA Value') +
  theme(text = element_text(size=rel(3.5)), plot.title = element_text(size = 28, face = "bold"),
        axis.text=element_text(size=10.5),
        strip.text = element_text(size = 12), panel.spacing = unit(2, "lines"))

ggsave("intercepts3.png", width = 8.25, height = 4.6)


# Negative EPA Rushes

neg_epa_rushes <-
  df %>% filter(EventType == 'rush', RunSide == 1, RunGap != 'i') %>%
  mutate(n=1) %>% group_by(cluster, Name, player_z) %>%
  summarise(success_rate = mean(success), n_snaps = sum(n), DefensiveTeam = mlv(DefensiveTeam, method='mfv')) %>% 
  filter(!is.na(cluster), n_snaps > 40)

mixed_model4 <- glmer(success ~ as.factor(Down) + ToGo + RunGap + (1|OffensiveTeam) + (1|Name),
                     data = df %>% filter(EventType == 'rush', RunSide == 1, RunGap != 'i'), family = 'binomial')

intercepts4 <-data.frame(ranef(mixed_model4))[1:(nrow(data.frame(ranef(mixed_model4)))-32),]
intercepts4 <-
  inner_join(intercepts4, neg_epa_rushes %>% select(Name, cluster, player_z), by = c('grp' = 'Name')) %>%
  arrange(condval)

mu <- intercepts4 %>% group_by(cluster) %>% summarise(avg_value = mean(condval))
ggplot(intercepts4, aes(x = -condval, fill = cluster)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mu, aes(xintercept=avg_value, color = cluster)) +
  ggtitle("Mixed Effects Player Intercepts") +
  xlab("(Negative) Logit Player Intercept") +
  theme(text = element_text(size=rel(3.7)), plot.title = element_text(size = 16, face = "bold"),
        axis.text=element_text(size=12), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=13))

edge_intercepts <- intercepts4 %>% filter(cluster == 'Edge')
tackle_intercepts <- intercepts4 %>% filter(cluster == 'Tackle')
nose_intercepts <- intercepts4 %>% filter(cluster == 'Nose')
edge_cor <- round(cor(-edge_intercepts$condval, edge_intercepts$player_z), 3)
tackle_cor <-round(cor(-tackle_intercepts$condval, tackle_intercepts$player_z), 3)
nose_cor <- round(cor(-nose_intercepts$condval, nose_intercepts$player_z), 3)

intercepts4$cluster <- factor(intercepts4$cluster, levels = c('Edge', 'Tackle', 'Nose'))
intercepts4 %>%
  ggplot(aes(x = player_z, y = -condval)) +
  geom_point() +
  xlim(-1.24, 3.82) + ylim(-.175, .22) +
  facet_wrap(~cluster, labeller = labeller(cluster = 
                                             c("Edge" = paste('Edge: Cor =', edge_cor),
                                               "Tackle" = paste('Tackle: Cor =', tackle_cor),
                                               "Nose" = paste('Nose: Cor =', nose_cor)))) +
  geom_smooth(method='lm', formula= y~x, color='blue') +
  ylab('(Negative) Player Intercept') +
  xlab('Standardized Draft/FA Value') +
  theme(text = element_text(size=rel(3.5)), plot.title = element_text(size = 28, face = "bold"),
        axis.text=element_text(size=10.5),
        strip.text = element_text(size = 12), panel.spacing = unit(2, "lines"))

ggsave("intercepts4.png", width = 8.25, height = 4.6)


