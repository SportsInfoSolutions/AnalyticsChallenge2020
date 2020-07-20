

main_df <- read.csv('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv', stringsAsFactors = F)
play_df <- readRDS(url('https://raw.githubusercontent.com/z-feldman/AnalyticsChallenge2020/master/Data/plays.rds'))


### forced out of designed gap
run_gap_order = c('Left D Gap', 'Left Off-Tackle C Gap', 'Left Off-Tackle B Gap', 'Left A Gap', 'Middle', 'Right A Gap', 'Right Off-Tackle B Gap', 'Right Off-Tackle C Gap', 'Right D Gap')

p <- all_plays %>% 
  filter(RunDirection!='NULL') %>% 
  mutate(
    RunDirection = factor(RunDirection, run_gap_order),
    UsedDesignedGap = ifelse(UsedDesignedGap == 1, 'Designed Gap', ifelse(UsedDesignedGap == 0, 'Other Gap', NA))
  ) %>% 
  group_by(RunDirection, UsedDesignedGap) %>% 
  summarize(
    snaps = n(),
    EPA_success = mean(ifelse(SIS_EPA > 0, 1, 0))
  ) %>% 
  ggplot(aes(x = RunDirection, y = EPA_success, group = UsedDesignedGap, fill = UsedDesignedGap)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.6), show.legend = T, size = 0.5, width = 0.75, color = 'darkblue') +
  geom_text(aes(y = 0.02, label = paste0('n = ',snaps)), hjust = 0, color = 'white', family = font_SB, position = position_dodge(width = 0.6), size = 2) +
  coord_flip() +
  scale_y_continuous(label = percent, expand = expansion(mult = c(0,0.05))) +
  scale_fill_manual(values = color_SB[2:1], name = NULL) +
  labs(title = 'Success Rate by Gap',
       x = NULL,
       y = 'EPA Success Rate') +
  theme_SB + 
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'bottom'
  )
brand_plot(p, save_name = 'used run gap.png', data_home = 'Data: SIS', fade_borders = 'tr', axis_rot = T)



gap_only <- c('D Gap', 'Off-Tackle C Gap', 'Off-Tackle B Gap', 'A Gap', 'Middle')

p <- play_df %>% 
  filter(RunDirection!='NULL' & UsedDesignedGap == 0) %>% 
  mutate(
    RunGap = gsub('Left ','', gsub('Right ','', RunDirection)),
    RunGap = factor(RunGap, gap_only),
    UsedDesignedGap = ifelse(UsedDesignedGap == 1, 'Designed Gap', ifelse(UsedDesignedGap == 0, 'Other Gap', NA))
  ) %>% 
  group_by(RunGap) %>% 
  summarize(
    snaps = n(),
    EPA_success = mean(ifelse(EPA > 0, 1, 0))
  ) %>% 
  ggplot(aes(x = RunGap, y = EPA_success)) + 
  geom_bar(stat = 'identity', size = 1, width = 0.75, color = 'darkblue', fill = color_SB[1]) +
  geom_text(aes(y = 0.02, label = paste0('n = ',snaps)), hjust = 0, color = 'white', family = font_SB, size = 3) +
  coord_flip() +
  scale_y_continuous(label = percent, expand = expansion(mult = c(0,0.05))) +
  labs(title = 'Success Rate by Gap When Not Using\nDesigned Gap',
       x = NULL,
       y = 'EPA Success Rate') +
  theme_SB + 
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'bottom'
  )

brand_plot(p, save_name = 'wrong gap.png', data_home = 'Data: SIS', fade_borders = 'tr', axis_rot = T)



p <- play_df %>% 
  filter(RunDirection!='NULL') %>% 
  mutate(
    RunGap = gsub('Left ','', gsub('Right ','', RunDirection)),
    RunGap = factor(RunGap, gap_only)
  ) %>% 
  group_by(RunGap) %>% 
  summarize(
    snaps = n(),
    designed_gap_freq = mean(as.numeric(UsedDesignedGap), na.rm = T)
  ) %>% 
  ggplot(aes(x = RunGap, y = designed_gap_freq)) + 
  geom_bar(stat = 'identity', size = 1, width = 0.75, color = 'darkblue', fill = color_SB[1]) +
  geom_text(aes(y = 0.02, label = paste0('n = ',snaps)), hjust = 0, color = 'white', family = font_SB, size = 3) +
  coord_flip() +
  scale_y_continuous(label = percent, expand = expansion(mult = c(0,0.05))) +
  labs(title = 'Rate of Running Through Designed Gap',
       x = 'Intended\nGap',
       y = 'Designed Gap %') +
  theme_SB + 
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    #axis.title.y = element_text(angle = 90),
    legend.position = 'bottom'
  )

brand_plot(p, save_name = 'wrong gap pct.png', data_home = 'Data: SIS', fade_borders = 'tr', axis_rot = T)


play_df %>% 
  filter(RunDirection=='Middle' & grepl('sneak', PlayDesc) & !grepl('scrambles', PlayDesc) & !grepl('spike', PlayDesc) & !grepl('kneels', PlayDesc) & !grepl('Aborted', PlayDesc) & !grepl('broken play', PlayDesc)) %>% 
  view

unique(middle_plays$PlayDesc)[1:10]





