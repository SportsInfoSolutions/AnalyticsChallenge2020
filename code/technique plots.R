setwd('C:/Users/Owner/Documents/GitHub/AnalyticsChallenge2020')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

sis_df <- read.csv('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)

## % of time is DL or LB
## everything else by side of ball

p <- sis_df %>% 
  group_by(TechniqueName) %>% 
  summarize(freq = n()) %>% 
  ggplot(aes(x = TechniqueName, y = freq)) + 
  geom_bar(color = 'darkblue', fill = color_SB[1], size = 0.6, stat = 'identity', show.legend = F) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05)), label = comma) +
  labs(title = 'Technique Count',
       subtitle = 'Number of Times a Player is at Each Technique',
       y = 'Count',
       x = 'Player Technique') +
  theme_SB +
  theme(panel.grid.major.x = element_blank())

brand_plot(p, save_name = 'plots/tech_count.png', data_home = 'Data: SIS')


p <- sis_df %>% 
  mutate(
    RosterPosition = case_when(
      RosterPosition %in% c('FB','WR','T') ~ 'Other',
      RosterPosition %in% c('CB','S') ~ 'DB',
      TRUE ~ RosterPosition
    )
  ) %>% 
  group_by(TechniqueName, RosterPosition) %>% 
  summarize(freq = n()) %>% 
  ggplot(aes(x = TechniqueName, y = freq, group = RosterPosition, fill = RosterPosition)) + 
  geom_bar(color = 'darkblue', position = 'stack', size = 0.6, stat = 'identity') +
  scale_y_continuous(expand = expansion(mult = c(0,0.05)), label = comma) +
  scale_fill_manual(values = color_SB) +
  labs(title = 'Technique Count by RosterPosition',
       subtitle = 'Number of Times a Player is at Each Technique',
       y = 'Count',
       x = 'Player Technique') +
  theme_SB +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = 'bottom'
  )

brand_plot(p, save_name = 'plots/tech_roster_count.png', data_home = 'Data: SIS')





tech_cor_df <- sis_df %>% 
  select(GameID, EventID, TechniqueName) %>% 
  pivot_wider(names_from = TechniqueName, values_from = TechniqueName, values_fn = length, values_fill = 0, names_prefix = 'Tech_') %>% 
  left_join(sis_df) %>% 
  select(GameID, EventID, TechniqueName, starts_with('Tech_')) %>% 
  pivot_longer(starts_with('Tech_'), names_to = 'TeammateTech', values_to = 'TeammateTechCount') %>% 
  mutate(
    TeammateTech = gsub('Tech_','',TeammateTech),
    TeammateTechCount = TeammateTechCount - ifelse(TechniqueName == TeammateTech, 1, 0)
  ) %>% 
  group_by(TechniqueName, TeammateTech) %>% 
  summarize(freq = mean(ifelse(TeammateTechCount > 0, 1, 0))) %>% 
  filter(!(TechniqueName %in% c('Outside','Off Ball')) &  !(TeammateTech %in% c('Outside','Off Ball')))
  
  
p <- ggplot(tech_cor_df, aes(y = TechniqueName, x = TeammateTech, fill = freq)) + 
  geom_tile(color = 'darkblue', size = 0.35, stat = 'identity', show.legend = F) +
  scale_y_discrete(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(expand = expansion(mult = c(0,0))) +
  scale_fill_gradient(low = 'white', high = color_SB[1]) +
  labs(title = 'Technique Correlation Matrix',
       y = 'Player\nTechnique',
       x = 'Teammate Technique') +
  theme_SB

brand_plot(p, save_name = 'plots/tech_heatmap.png', data_home = 'Data: SIS')


### 
basic_tech_ord <- c(1,'2i',2,3,'4i',4,5,7,6,9)
tech_ord_LR <- c(paste0('L ',rev(basic_tech_ord)),'NULL 0',paste0('R ',basic_tech_ord))
tech_ord <- c(paste0('Same ',rev(basic_tech_ord)),'Same 0',paste0('Opposite ',basic_tech_ord))

tech_cor_side_df <- sis_df %>% 
  mutate(TechniqueNameSide = paste0(SideOfBall, ' ', TechniqueName)) %>% 
  select(GameID, EventID, TechniqueNameSide) %>% 
  pivot_wider(names_from = TechniqueNameSide, values_from = TechniqueNameSide, values_fn = length, values_fill = 0, names_prefix = 'Tech_') %>% 
  left_join(sis_df) %>% 
  mutate(TechniqueNameSide = paste0(SideOfBall, ' ', TechniqueName)) %>% 
  select(GameID, EventID, SideOfBall, TechniqueName, TechniqueNameSide, starts_with('Tech_')) %>% 
  pivot_longer(starts_with('Tech_'), names_to = 'TeammateTech', values_to = 'TeammateTechCount', names_prefix	= 'Tech_') %>% 
  mutate(TeammateTech = gsub('Off Ball','Off-Ball',TeammateTech)) %>% 
  separate(TeammateTech, sep = ' ', into = c('TeammateSide_LR', 'TeammateTechOnly')) %>% 
  mutate(
    #TeammateTech = gsub('Tech_','',TeammateTech),
    RelativeSideOfBall = ifelse(SideOfBall==TeammateSide_LR | SideOfBall=='NULL' | TeammateSide_LR=='NULL', 'Same', 'Opposite'),
    #TeammateTechCount = TeammateTechCount - ifelse(TechniqueNameSide == TeammateTech, 1, 0)
    TeammateTechCount = TeammateTechCount - ifelse(RelativeSideOfBall == 'Same' & TechniqueName==TeammateTechOnly, 1, 0),
    TeammateTech = paste0(RelativeSideOfBall, ' ', TeammateTechOnly)
  ) %>% 
  filter(!(TechniqueName %in% c('Outside','Off Ball')) &  !(TeammateTechOnly %in% c('Outside','Off-Ball'))) %>% 
  group_by(TechniqueName, TeammateTech) %>% 
  summarize(freq = mean(ifelse(TeammateTechCount > 0, 1, 0))) %>% 
  ungroup %>% 
  mutate(
    TechniqueName = factor(TechniqueName, c(0,basic_tech_ord)),
    TeammateTech = factor(TeammateTech, tech_ord)
  )


p <- ggplot(tech_cor_side_df, aes(x = TechniqueName, y = TeammateTech, fill = freq, label = percent(freq, accuracy = 1))) + 
  geom_tile(color = 'darkblue', size = 0.35, stat = 'identity', show.legend = F) +
  geom_text(color = 'darkblue', size = 2, family = font_SB) +
  scale_y_discrete(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(expand = expansion(mult = c(0,0))) +
  scale_fill_gradient(low = 'white', high = color_SB[1]) +
  labs(title = 'DL Technique Correlation Matrix',
       subtitle = '% of Time \"Player Technique\" has a Teammate at \"Teammate Technique\"',
       x = 'Player Technique',
       y = 'Teammate\nTechnique') +
  theme_SB + 
  theme(
    panel.grid.major = element_blank()
  )

brand_plot(p, save_name = 'plots/tech_heatmap_side.png', data_home = 'Data: SIS')

