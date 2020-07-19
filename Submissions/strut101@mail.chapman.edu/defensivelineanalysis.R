# Load in Packages
pacman::p_load(fastDummies,na.tools,multicon,extrafont, ggridges, lubridate, stringr,tidyverse,ggrepel,ggimage,ggthemes,sf,RANN, magick, ggpubr,RColorBrewer,dplyr,ggplot2,lme4)

#Read In CSV
data <- read_csv('https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv')

######Add Mutations#######
data_prep <- data %>% dplyr::group_by(EventID,GameID) %>% mutate(
  effective_pos = ifelse(RosterPosition %in% c('CB','S'),'DB',
                         ifelse(OnFieldPosition == 'LB' & TechniqueName =='Outside','OLB',
                                ifelse(OnFieldPosition == 'LB' & TechniqueName == 'Off Ball','iLB',
                                       ifelse(OnFieldPosition == 'DL' & TechniqueName %in% c('2','3'),'DT',
                                              ifelse(OnFieldPosition == 'DL'& RosterPosition!="DT" & TechniqueName %in% c('0','1','2i'), 'DT',
                                                     ifelse(OnFieldPosition == 'DL'& RosterPosition=="DT" & TechniqueName %in% c('0','1','2i','2'),'NT', 
                                                            ifelse(OnFieldPosition == 'DL' & TechniqueName %in% c('4i','4','5','7','6','9'),'DE',
                                                                   ifelse(RosterPosition == 'CB'|RosterPosition=='S','DB',RosterPosition)))))))),
  ThrowDepth = as.numeric(na_if(ThrowDepth,'NULL')),
  ThrowDepth = as.numeric(na_if(Spike,'NULL')),
  SoloSack = as.numeric(SoloSack),
  AssistedSack = as.numeric(AssistedSack),
  Pressure = as.numeric(Pressure),
  PressureOnPlay = as.numeric(na_if(PressureOnPlay,'NULL')),
  SackOnPlay = as.numeric(SackOnPlay),
  sack_t =  ifelse(SoloSack ==1 | AssistedSack ==1, 1,0 ),
  Completion = as.numeric(na_if(Completion,'NULL')),
  half = if_else(Quarter > 2,2,1),
  tech_0 = ifelse(TechniqueName=='0',1,0) ,
  dline = ifelse(OnFieldPosition == 'DL',1,0),
  SideOfBall = ifelse(SideOfBall == 'NULL' & TechniqueName == 0,'C',
                      ifelse(SideOfBall == 'NULL' & TechniqueName == 'Off Ball','O',SideOfBall)),
  UsedDesignedGap = as.numeric(na_if(UsedDesignedGap,'NULL')),
  Yardline_100 = if_else(SideOfField == 'Oppo',as.numeric(StartYard), 100-as.numeric(StartYard)),
  side_position = paste(SideOfBall,effective_pos,sep=''),
  total_tackle = if_else(SoloTackle==1,1,if_else(AssistedTackle==1,.5,0)),
  tackle_player_name = ifelse(total_tackle ==1,Name,
                              ifelse(total_tackle == 0.5,Name,"None")),
  tech_name = ifelse(TechniqueName == 'Outside', '9',TechniqueName),
  full_technique = paste0(SideOfBall,tech_name),
  run_num = ifelse(RunDirection=='Right D Gap',1,
                   ifelse(RunDirection=='Right Off-Tackle C Gap',2,
                          ifelse(RunDirection=='Right Off-Tackle B Gap',3,
                                 ifelse(RunDirection=='Right A Gap',4,
                                        ifelse(RunDirection=='Middle',4.5,
                                               ifelse(RunDirection=='Left A Gap',5,
                                                      ifelse(RunDirection=='Left Off-Tackle B Gap',6,
                                                             ifelse(RunDirection=='Left Off-Tackle C Gap',7,
                                                                    ifelse(RunDirection=='Left D Gap',8,NA))))))))),
  tech_num = ifelse(full_technique == 'L9',1,
                    ifelse(full_technique == 'L7',2,
                           ifelse(full_technique == 'L6',1.5,
                                  ifelse(full_technique == 'L5',2,
                                         ifelse(full_technique == 'L4',2.5,
                                                ifelse(full_technique == 'L4i'| full_technique == 'L3', 3, 
                                                       ifelse(full_technique == 'L2',3.5,
                                                              ifelse(full_technique=='L2i' | full_technique=='L1',4,
                                                                     ifelse(full_technique=='C0',4.5,NA)))))))))
  ,
  tech_num = ifelse(full_technique == 'R1' | full_technique == 'R2i',5,
                    ifelse(full_technique == 'R2',5.5,
                           ifelse(full_technique == 'R3' | full_technique == 'R4i',6,
                                  ifelse(full_technique == 'R4',6.5,
                                         ifelse(full_technique == 'R5',7,
                                                ifelse(full_technique == 'R6',7.5,
                                                       ifelse(full_technique == 'R7',7,
                                                              ifelse(full_technique == 'R9',8,tech_num)))))))),
  distance_from_gap = abs(run_num - tech_num),
  RunAtDefender = ifelse(distance_from_gap<=1, 1,0),
  qb_scramble = ifelse(str_detect(PlayDesc,'scrambles'), 1,0),
  qb_sneak = ifelse(str_detect(PlayDesc,'sneak'), 1,0),
  broken_play =ifelse(str_detect(PlayDesc,'broken play'), 1,0),
  tackle_when_run_at = ifelse(RunAtDefender == 1 & total_tackle >0,1,0),
  qb_kneel =ifelse(str_detect(PlayDesc,'kneels'), 1,0),
  EventType = ifelse(qb_kneel == 1 | qb_scramble==1 | broken_play==1, 'pass',EventType),
  run = if_else(EventType == 'rush',1,0) ,
  pass = if_else(EventType == 'pass',1,0),
  distance_DE = ifelse(effective_pos == 'DE',distance_from_gap,NA),
  distance_DT = ifelse(effective_pos == 'DT',distance_from_gap,NA) ,
  distance_NT = ifelse(effective_pos == 'NT',distance_from_gap,NA),
  distance_OLB = ifelse(effective_pos == 'OLB',distance_from_gap,NA),
  distance_iLB = ifelse(effective_pos == 'iLB',distance_from_gap,NA),
  screen = ifelse(str_detect(tolower(PlayDesc),'screen'),1,0) ,
  shotgun = ifelse(str_detect(tolower(PlayDesc),'shotgun'),1,0),
  no_huddle = ifelse(str_detect(tolower(PlayDesc),'no huddle'),1,0),
  sack_effective = ifelse(SoloSack==1 | AssistedSack ==1,1,0),
  tackle_effective = ifelse(SoloTackle ==1 | AssistedTackle ==1,1,0),
  idl = ifelse(effective_pos == 'DT' | effective_pos == 'NT',1,0),
  edge = ifelse(effective_pos == 'DE' | effective_pos == 'OLB',1,0),
  idl_pressure = ifelse(idl==1&Pressure==1,1,0),
  edge_pressure = ifelse(edge==1&Pressure==1,1,0),
  de_pressure = ifelse(effective_pos=='DE' & Pressure==1,1,0),
  olb_pressure = ifelse(effective_pos=='OLB' & Pressure==1,1,0),
  min_dist_DE = min(distance_DE, na.rm = T),
  min_dist_DE = ifelse(min_dist_DE==Inf,NA,min_dist_DE),
  min_dist_DT = min(distance_DT, na.rm = T),
  min_dist_DT = ifelse(min_dist_DT==Inf,NA,min_dist_DT),
  min_dist_NT = min(distance_NT, na.rm = T),
  min_dist_NT = ifelse(min_dist_NT==Inf,NA,min_dist_NT),
  min_dist_OLB = min(distance_OLB, na.rm = T),
  min_dist_OLB = ifelse(min_dist_OLB==Inf,NA,min_dist_OLB),
  min_dist_iLB = min(distance_iLB, na.rm = T),
  min_dist_iLB = ifelse(min_dist_iLB==Inf,NA,min_dist_iLB),
  min_dist = min(distance_from_gap,na.rm = T),
  min_dist_2 = sort(distance_from_gap, decreasing = F,na.last = TRUE)[2],
  mean_dist = mean(distance_from_gap,na.rm=T),
  dist_between_1and2 = min_dist_2 - min_dist,
  tech_0_playing = sum(tech_0,na.rm = T),  
  are_rushing = sum(IsRushing,na.rm = T),
  are_pressuring = sum(Pressure,na.rm = T),
  dline= ifelse(OnFieldPosition == 'DL', 1,0),
  players_in_line = sum(dline), 
  front_3 = ifelse(players_in_line <=3, 1,0),
  front_4 = ifelse(players_in_line == 4,1,0),
  special_front = ifelse(players_in_line >4,1,0),
  ScoreDiff = as.numeric(OffTeamScoreBefore - DefTeamScoreBefore),
  NT = ifelse(effective_pos == 'NT',1,0),
  DT = ifelse(effective_pos == 'DT',1,0),
  OLB = ifelse(effective_pos == 'OLB',1,0),
  DE = ifelse(effective_pos == 'DE',1,0),
  iLB = ifelse(effective_pos == 'iLB',1,0),
  DB = ifelse(effective_pos == 'DB',1,0),
  num_DB =  sum(DB),
  num_DE = sum(DE),
  num_DT = sum(DT),
  num_iLB = sum(iLB),
  num_OLB = sum(OLB),
  num_NT = sum(NT),
  blitz = ifelse(are_rushing >4,1,0),
  edge_pressuring = sum(edge_pressure,na.rm = T),
  de_pressuring = sum(de_pressure,na.rm = T),
  olb_pressuring = sum(olb_pressure,na.rm = T),
  idl_pressuring = sum(idl_pressure,na.rm = T),
  ) 


#Create Distance Density Plot
distance <- data_prep %>% 
  filter(qb_kneel == 0, qb_scramble ==0, broken_play == 0, EventType == 'rush'|EventType == 'challenge rush') %>% 
  group_by(Name,RosterPosition,DefensiveTeam,effective_pos) %>% 
  summarize(plays = n(),mean_distance=  mean(distance_from_gap, na.rm = TRUE)) %>% 
  filter(plays >10)
distance %>% filter(effective_pos!='iLB') %>%  ggplot(aes(x = mean_distance, fill = effective_pos)) + geom_density_ridges(aes(y = effective_pos)) +
  theme_clean() +
  theme(text = element_text(),
        plot.title = element_text(size = 12, family = "Trebuchet MS", hjust =.5),
        plot.subtitle = element_text(size = 12,family = "Trebuchet MS",
                                     color = "grey20", hjust =.5),
        axis.title = element_text(size = 12,family = "Trebuchet MS",color = "grey20"),
        axis.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.position = 'none', 
        plot.caption = element_text(size = 10,family = "Trebuchet MS",
                                    color = "grey20", hjust = .5)) +
  labs(title = "How Far Away is the Defensive Line Position from Run Gaps?",
       subtitle = "2019 (Weeks 9 - 17)",
       x = "Player Distance from Run Gap",
       y = NULL
  ) 
ggsave('documents/player_distances.png',dpi =1200)

#Tackle Rate Plot Based on Distance From Gap
tackle_rate <- data_prep %>% filter(!is.na(distance_from_gap), EventType == 'rush'|EventType == 'challenge rush') %>% 
  group_by(distance_from_gap) %>% 
  summarize(plays = n(), tackles = sum(total_tackle), tackle_rate = tackles/plays)
tackle_rate %>% ggplot(aes(x = distance_from_gap, y = tackle_rate)) +
  geom_point(alpha = .6) +
  geom_smooth(se = FALSE,color = 'black', method ='lm') +
  theme_clean()+
  theme(text = element_text(),
        plot.title = element_text(size = 12, family = "Trebuchet MS", hjust =.5),
        plot.subtitle = element_text(size = 12,family = "Trebuchet MS",
                                     color = "grey20", hjust =.5),
        axis.title = element_text(size = 12,family = "Trebuchet MS",color = "grey20"),
        axis.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.position = 'none', 
        plot.caption = element_text(size = 10,family = "Trebuchet MS",
                                    color = "grey20", hjust = .5)) +
  labs(title = "Harder To Be Involved When You Are Further Away",
       subtitle = "2019 (Weeks 9 - 17)",
       x = "Player Distance from Run Gap",
       y = "Tackle Rate"
  ) +
  scale_y_continuous(labels = scales::percent)
ggsave('documents/tackle_rate.png',dpi=1200)


#######Create Run Defense Models and Graphs#######
#####Front 3 Run Defense######

#Scale Variables
data_prep_run <- data_prep %>%  filter(EventType %in% c('rush','challenge rush'))
data_prep_run$scorediff_s<-data_prep_run$ScoreDiff %>% scale() 
data_prep_run$yardline_s<-data_prep_run$Yardline_100 %>% scale() 


front3 <- data_prep_run %>% 
  dplyr::filter(front_3==1)

#DE#
pos_player_de3<-front3 %>% 
  filter(effective_pos=='DE' & !is.na(OffensiveYardage)) %>% mutate(
    distance  = ifelse(distance_from_gap%in%c(0,0.5,1,1.5),'close',
                       ifelse(distance_from_gap%in%c(2,2.5,3,3.5),'mid',
                              ifelse(distance_from_gap%in%c(4,4.5,5,5.5),'mid-far','far'
                              ))))
mixed_de3<-pos_player_de3 %>% lmer(formula=EPA ~  
                                     min_dist +
                                     min_dist_2 + 
                                     mean_dist + 
                                     num_DT + 
                                     num_NT +
                                     num_DE +
                                     num_OLB +
                                     yardline_s + 
                                     scorediff_s + 
                                     distance_from_gap+
                                     (1|PlayerId:distance),
                                   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
de_3front<-broom.mixed::tidy(mixed_de3,effects="ran_vals") %>% 
  filter(group=='PlayerId:distance')%>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         distance = sub(".*:", "",level))

de_3front_mean<-de_3front %>% 
  filter(distance %in% c('close','mid')) %>%
  group_by(Player_id) %>% 
  summarise(EPA = mean(estimate)) %>% 
  mutate(Position='DE')

#iDL#
pos_player_idl3<-front3 %>% 
  filter(idl==1 & !is.na(OffensiveYardage)) %>% mutate(
    distance  = ifelse(distance_from_gap%in%c(0,0.5,1,1.5),'close',
                       ifelse(distance_from_gap%in%c(2,2.5,3,3.5),'mid','far'))
  )

mixed_idl3<-pos_player_idl3 %>% lmer(formula=EPA ~  
                                       min_dist +
                                       min_dist_2 + 
                                       mean_dist + 
                                       num_DT + 
                                       num_NT +
                                       num_DE +
                                       num_OLB +
                                       yardline_s + 
                                       scorediff_s + 
                                       distance_from_gap+
                                       (1|PlayerId:distance),
                                     control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

idl_3front<-broom.mixed::tidy(mixed_idl3,effects="ran_vals") %>% 
  filter(group=='PlayerId:distance') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         distance = sub(".*:", "",level))

idl_3front_mean<-idl_3front%>% 
  filter(distance %in% c('close','mid')) %>%
  group_by(Player_id) %>% 
  summarise(EPA = mean(estimate)) %>% 
  mutate(Position='iDL')  
#OLB
pos_player_olb3<-front3 %>% 
  filter(effective_pos=='OLB' & !is.na(OffensiveYardage)) %>% mutate(
    distance  = ifelse(distance_from_gap%in%c(0,0.5,1,1.5),'close',
                       ifelse(distance_from_gap%in%c(2,2.5,3,3.5),'mid','far'))
  )

mixed_olb3<-pos_player_olb3 %>% lmer(formula=EPA ~  
                                       min_dist +
                                       min_dist_2 + 
                                       mean_dist + 
                                       num_DT + 
                                       num_NT +
                                       num_DE +
                                       num_OLB +
                                       yardline_s + 
                                       scorediff_s +  
                                       distance_from_gap+
                                       (1|PlayerId:distance),
                                     control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)


olb_3front<-broom.mixed::tidy(mixed_olb3,effects="ran_vals") %>% 
  filter(group=='PlayerId:distance') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         distance = sub(".*:", "",level))

olb_3front_mean<-olb_3front%>% 
  filter(distance %in% c('close','mid')) %>%
  group_by(Player_id) %>% 
  summarise(EPA = mean(estimate)) %>% 
  mutate(Position='OLB') 

# Plots 
plot1<-rbind(de_3front_mean,idl_3front_mean)
plot2<-rbind(de_3front_mean,olb_3front_mean)
plot3<-rbind(idl_3front_mean,olb_3front_mean)

pl1<-plot1%>%
  ggplot(aes(x=EPA, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+
  labs(x='Run cDiPA', y='Density')+ scale_fill_manual(values = c("#8DD3C7","#E41A1C"))+
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.caption = element_text(size=12)
  )
pl1 + labs(title='In front 3 defenses: iDL has more run cDiPA variance than DE',caption = "Data: Sports Info Solutions")

pl2<-plot2%>%
  ggplot(aes(x=EPA, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+ 
  labs(x='Run cDiPA', y='Density')+ scale_fill_manual(values = c("#8DD3C7","#377EB8"))+
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    plot.caption = element_text(size=12)
  )
pl2 + labs(title='In front 3 defenses: OLB has more run cDiPA variance than DE',caption = "Data: Sports Info Solutions")

pl3<-plot3%>%
  ggplot(aes(x=EPA, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+ 
  labs(x='Run cDiPA', y='Density')+ scale_fill_manual(values = c("#E41A1C","#377EB8"))+
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    plot.caption = element_text(size=12)
  )
pl3 + labs(title='In front 3 defenses: iDL has more run cDiPA variance than OLB',
           subtitle = 'OLB variance: 2.992e-04  |  iDL variance: 4.673e-04',
           caption = "Data: Sports Info Solutions")

olb_3front_mean$EPA %>% var()
idl_3front_mean$EPA %>% var()
de_3front_mean$EPA %>% var()

ggpubr::ggarrange(pl1,pl2,pl3) %>% annotate_figure(
  top = text_grob("Credited Defensive iPA Distributions - Front 3 - Run Plays", face = "bold", size = 14),
  bottom = text_grob("Data source: Sports Info Solutions",hjust = 1.5, x = 1, face = "italic", size = 12))

#####Front 4 Run Defense######
front4 <- data_prep_run %>% 
  dplyr::filter(front_4==1)

pos_player_idl4<-front4 %>% 
  filter(idl==1 & !is.na(OffensiveYardage)) %>% mutate(
    distance  = ifelse(distance_from_gap%in%c(0,0.5,1,1.5,2),'close',
                       ifelse(distance_from_gap%in%c(2.5,3,3.5,4),'mid','far'))
  )

mixed_idl4<-pos_player_idl4 %>% lmer(formula= EPA ~
                                       min_dist +
                                       min_dist_2 + 
                                       mean_dist + 
                                       num_DT + 
                                       num_NT +
                                       num_DE +
                                       num_OLB +
                                       yardline_s + 
                                       scorediff_s +  
                                       distance_from_gap+
                                       (1|PlayerId:distance),
                                     control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

idl_4front<-broom.mixed::tidy(mixed_idl4,effects="ran_vals") %>% 
  filter(group=='PlayerId:distance') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         distance = sub(".*:", "",level))

idl_4front_mean<-idl_4front%>% 
  filter(distance %in% c('close','mid')) %>%
  group_by(Player_id) %>% 
  summarise(EPA = mean(estimate)) %>% 
  mutate(Position='iDL')  

# DE
pos_player_edge4<-front4 %>% 
  filter(effective_pos=='DE' & !is.na(OffensiveYardage)) %>% mutate(
    distance  = ifelse(distance_from_gap%in%c(0,0.5,1,1.5,2),'close',
                       ifelse(distance_from_gap%in%c(2.5,3,3.5,4),'mid','far'))
  )

mixed_edge4<-pos_player_edge4 %>% lmer(formula=EPA ~  
                                         min_dist +
                                         min_dist_2 + 
                                         mean_dist + 
                                         num_DT + 
                                         num_NT +
                                         num_DE +
                                         num_OLB +
                                         yardline_s + 
                                         scorediff_s +  
                                         distance_from_gap+
                                         (1|PlayerId:distance),
                                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

edge_4front<-broom.mixed::tidy(mixed_edge4,effects="ran_vals") %>% 
  filter(group=='PlayerId:distance') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         distance = sub(".*:", "",level))

edge_4front_mean<-edge_4front%>% 
  filter(distance %in% c('close','mid')) %>%
  group_by(Player_id) %>% 
  summarise(EPA = mean(estimate)) %>% 
  mutate(Position='DE') 

plot4<-rbind(idl_4front_mean,edge_4front_mean)
plot4%>%
  ggplot(aes(x=EPA, fill=Position)) + 
  geom_density(alpha=0.6) + theme_bw()+ 
  scale_fill_brewer(palette = 'Set1')+
  labs(x='cDiPA', y='Density',
       title = "In front 4 defenses: DE has more run cDiPA variance than iDL",
       subtitle = 'EDGE variance: 1.682e-05  |  iDL variance: 6.611e-07',caption = "Data: Sports Info Solutions")+
  theme(text = element_text(),
        plot.title = element_text(size = 16 ),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14), 
        plot.caption = element_text(size=12)
  )

edge_4front_mean$EPA %>% var()
idl_4front_mean$EPA%>% var()

# Front 3 EDGE vs iDL for Comparison

pos_player_idl3<-front3 %>% 
  filter(idl==1 & !is.na(OffensiveYardage)) %>% mutate(
    distance  = ifelse(distance_from_gap%in%c(0,0.5,1,1.5),'close',
                       ifelse(distance_from_gap%in%c(2,2.5,3,3.5),'mid','far'))
  )


mixed_idl3<-pos_player_idl3 %>% lmer(formula= EPA ~
                                       min_dist +
                                       min_dist_2 + 
                                       mean_dist + 
                                       num_DT + 
                                       num_NT +
                                       num_DE +
                                       num_OLB +
                                       yardline_s + 
                                       scorediff_s + 
                                       distance_from_gap+
                                       (1|PlayerId:distance),
                                     control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)


idl_3front<-broom.mixed::tidy(mixed_idl3,effects="ran_vals") %>% 
  filter(group=='PlayerId:distance') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         distance = sub(".*:", "",level))

idl_3front_mean<-idl_3front%>% 
  filter(distance %in% c('close','mid')) %>%
  group_by(Player_id) %>% 
  summarise(EPA = mean(estimate)) %>% 
  mutate(Position='iDL')  

# EDGE
pos_player_edge3<-front3 %>% 
  filter(edge ==1 & !is.na(OffensiveYardage)) %>% mutate(
    distance  = ifelse(distance_from_gap%in%c(0,0.5,1,1.5),'close',
                       ifelse(distance_from_gap%in%c(2,2.5,3,3.5),'mid',
                              ifelse(distance_from_gap%in%c(4,4.5,5,5.5),'mid-far','far'
                              )))
  )

mixed_edge3<-pos_player_edge3 %>% lmer(formula=EPA ~  
                                         min_dist +
                                         min_dist_2 + 
                                         mean_dist + 
                                         num_DT + 
                                         num_NT +
                                         num_DE +
                                         num_OLB +
                                         yardline_s + 
                                         scorediff_s + 
                                         distance_from_gap+
                                         (1|PlayerId:distance),
                                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

edge_3front<-broom.mixed::tidy(mixed_edge3,effects="ran_vals") %>% 
  filter(group=='PlayerId:distance') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         distance = sub(".*:", "",level))

edge_3front_mean<-edge_3front%>% 
  filter(distance %in% c('close','mid')) %>%
  group_by(Player_id) %>% 
  summarise(EPA = mean(estimate)) %>% 
  mutate(Position='EDGE') 

# Plots 
plot5<-rbind(idl_3front_mean,edge_3front_mean)

plot5%>%
  ggplot(aes(x=EPA, fill=Position)) + 
  geom_density(alpha=0.6) + theme_bw()+ 
  scale_fill_brewer(palette = 'Set1')+
  labs(x='Run cDiPA', y='Density',
       title = "In front 3 defenses: iDL has slightly more run cDiPA variance than EDGE",
       subtitle = 'EDGE variance: 3.915-04  |  iDL variance: 4.673e-04',
       caption = "Data: Sports Info Solutions")+
  theme(text = element_text(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14), 
        plot.caption = element_text(size=12)
  )

edge_3front_mean$EPA %>% var()
idl_3front_mean$EPA%>% var()





#######Create Pass Defense Models and Graphs#######
#####Front 3 Pass Defense#####
data_prep_pass <-  data_prep %>% 
  filter(EventType %in% c('pass','challenge pass'),
         qb_kneel ==0,
         Spike == 0,
  )
data_prep_pass$scorediff_s<-data_prep_pass$ScoreDiff %>% scale() 
data_prep_pass$yardline_s<-data_prep_pass$Yardline_100 %>% scale() 

front3_pass <- data_prep_pass %>% filter(front_3==1)
idl3_pass <- front3_pass %>% 
  filter(idl ==1)
# iDL

pressure_logmix_idl3<- idl3_pass %>%
  glmer(formula=
          Pressure ~
          ToGo*as.factor(Down) + 
          yardline_s + 
          as.factor(Down)+
          shotgun +
          blitz +
          scorediff_s + 
          olb_pressuring +
          de_pressuring +
          (1|PlayerId:IsRushing),
        family = binomial,
        control=glmerControl(optimizer="bobyqa"),
        nAGQ = 10)

pressure_logmix_idl3 %>% summary()

idl_dist_3<-broom.mixed::tidy(pressure_logmix_idl3,effects="ran_vals") %>% 
  filter(group=='PlayerId:IsRushing') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         rushing = sub(".*:", "",level))%>% 
  filter(rushing==1) %>%
  group_by(Player_id) %>% 
  summarise(log_ods = mean(estimate)) %>% 
  mutate(Position='iDL') 
# DE
de3_pass <-  front3_pass %>% 
  filter(effective_pos == 'DE') 

pressure_logmix_de3<- de3_pass %>%
  glmer(formula=
          Pressure ~
          ToGo*as.factor(Down) + 
          yardline_s + 
          as.factor(Down)+
          shotgun +
          blitz +
          scorediff_s + 
          olb_pressuring +
          idl_pressuring +
          (1|PlayerId:IsRushing),
        family = binomial,
        control=glmerControl(optimizer="bobyqa"),
        nAGQ = 10)

pressure_logmix_de3 %>% summary()
de_dist_3<-broom.mixed::tidy(pressure_logmix_de3,effects="ran_vals") %>% 
  filter(group=='PlayerId:IsRushing') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         rushing = sub(".*:", "",level))%>% 
  filter(rushing==1) %>%
  group_by(Player_id) %>% 
  summarise(log_ods = mean(estimate)) %>% 
  mutate(Position='DE') 

# OLB
olb3_pass <-  front3_pass %>% 
  filter(effective_pos == 'OLB') 

pressure_logmix_olb3<- olb3_pass %>%
  glmer(formula=
          Pressure ~
          ToGo*as.factor(Down) + 
          yardline_s + 
          as.factor(Down)+
          shotgun +
          blitz +
          scorediff_s + 
          idl_pressuring +
          de_pressuring +
          (1|PlayerId:IsRushing),
        family = binomial,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
        nAGQ = 10)

olb_dist_3<-broom.mixed::tidy(pressure_logmix_olb3,effects="ran_vals") %>% 
  filter(group=='PlayerId:IsRushing') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         rushing = sub(".*:", "",level))%>% 
  filter(rushing==1) %>%
  group_by(Player_id) %>% 
  summarise(log_ods = mean(estimate)) %>% 
  mutate(Position='OLB') 

# Save data
write.csv(olb_dist_3,'documents/olb_pass_front3_dist.csv',row.names = F)
write.csv(idl_dist_3,'documents/idl_pass_front3_dist.csv',row.names = F)
write.csv(de_dist_3,'documents/de_pass_front3_dist.csv',row.names = F)
# Read Data
olb_dist_3<-read.csv('documents/olb_pass_front3_dist.csv')
idl_dist_3<-read.csv('documents/idl_pass_front3_dist.csv')
de_dist_3<-read.csv('documents/de_pass_front3_dist.csv')

#### Plots 

plotp1<-rbind(idl_dist_3,olb_dist_3)
plotp2<-rbind(de_dist_3,idl_dist_3)
plotp3<-rbind(olb_dist_3,de_dist_3)

pl1p<-plotp1%>%
  ggplot(aes(x=log_ods, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+
  labs(x='iLog-Odds of Pressure', y='Density')+ scale_fill_manual(values = c("#8DD3C7","#E41A1C"))+
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    plot.caption = element_text(size=12)
  )



pl1p+ labs(title='In front 3 defenses: OLB has more iLog-Odds-of-Pressure variance than iDL',
           subtitle = 'OLB variance: 0.130  |  iDL variance: .022',caption = "Data: Sports Info Solutions")

pl2p<-plotp2%>%
  ggplot(aes(x=log_ods, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+ 
  labs(x='iLog-Odds of Pressure', y='Density')+ scale_fill_manual(values = c("#377EB8","#8DD3C7"))+
  theme(
    plot.title = element_text(size = 16, hjust =.5),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    plot.caption = element_text(size=12)
  )
pl2p+ labs(title='In front 3 defenses: DE has more iLog-Odds-of-Pressure variance than iDL',
           subtitle = 'DE variance: 0.068  |  iDL variance: .022',caption = "Data: Sports Info Solutions")

pl3p<-plotp3%>%
  ggplot(aes(x=log_ods, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+ 
  labs(x='iLog-Odds of Pressure', y='Density')+ scale_fill_manual(values = c("#377EB8","#E41A1C"))+
  theme(
    plot.title = element_text(size = 16, hjust =.5),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    plot.caption = element_text(size=12)
  )


pl3p + labs(title='In front 3 defenses: OLB has more iLog-Odds-of-Pressure variance than DE',
            subtitle = 'OLB variance: 0.130  |  DE variance: 0.068',caption = "Data: Sports Info Solutions")

ggpubr::ggarrange(pl1p,pl2p,pl3p) %>% annotate_figure(
  top = text_grob("Distribution of iLog-Odds of QB Pressure when Rushing - Pass Plays | Front 3", size = 18),
  bottom = text_grob("Data source: Sports Info Solutions",hjust = 1.5, x = 1, face = "italic", size = 14))

olb_dist_3$log_ods %>% var()
de_dist_3$log_ods%>% var()
idl_dist_3$log_ods%>% var()

###########Front 4 Pass Defense########
front4_pass <- data_prep_pass %>% filter(front_4 ==1)
#  iDL
idl4_pass <-  front4_pass %>% 
  filter(idl ==1) 

pressure_logmix_idl4<- idl4_pass %>%
  glmer(formula=
          Pressure ~
          ToGo*as.factor(Down) + 
          yardline_s + 
          as.factor(Down)+
          shotgun +
          blitz +
          scorediff_s + 
          edge_pressuring +
          (1|PlayerId:IsRushing),
        family = binomial,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
        nAGQ = 10)

idl_dist_4<-broom.mixed::tidy(pressure_logmix_idl4,effects="ran_vals") %>% 
  filter(group=='PlayerId:IsRushing') %>%
  mutate(
    Player_id = sub(":.*", "",level)
    ,
    rushing = sub(".*:", "",level)
  )%>% 
  filter(rushing==1) %>%
  group_by(Player_id) %>% 
  summarise(log_ods = mean(estimate)) %>% 
  mutate(Position='iDL') 

write.csv(idl_dist_4,'documents/idl_pass_front4_dist.csv',row.names = F)

# DE

edge4_pass <-  front4_pass%>% 
  filter(effective_pos == 'DE') 

pressure_logmix_edge4<- edge4_pass %>%
  glmer(formula=
          Pressure ~
          ToGo*as.factor(Down) + 
          yardline_s + 
          as.factor(Down)+
          shotgun +
          blitz +
          scorediff_s + 
          idl_pressuring +
          olb_pressuring + 
          (1|PlayerId:IsRushing),
        family = binomial,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
        nAGQ = 10)

de_dist_4<-broom.mixed::tidy(pressure_logmix_edge4,effects="ran_vals") %>% 
  filter(group=='PlayerId:IsRushing') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         rushing = sub(".*:", "",level))%>% 
  filter(rushing==1) %>%
  group_by(Player_id) %>% 
  summarise(log_ods = mean(estimate)) %>% 
  mutate(Position='DE') 

write.csv(de_dist_4,'documents/de_pass_front4_dist.csv',row.names = F)

idl_dist_4<-read.csv('documents/idl_pass_front4_dist.csv')
de_dist_4<-read.csv('documents/de_pass_front4_dist.csv')

plotp24<-rbind(de_dist_4,idl_dist_4)
de_dist_4$log_ods%>% var()
idl_dist_4$log_ods%>% var()

plotp24%>%
  ggplot(aes(x=log_ods, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+ 
  labs(
    title='In front 4 defenses: iDL has slightly more iLog-Odds-of-Pressure variance than DE',
    subtitle = 'DE variance: 3.259e-03  | iDL variance: 5.089e-03',
    caption = "Data: Sports Info Solutions",
    x='iLog-Odds of Pressure',
    y='Density')+ 
  scale_fill_manual(values = c("#FCCDE5","#8DD3C7"))+
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    plot.caption = element_text(size=12)
  )

######Front 3 Pass Edge Comparison####
# EDGE front 3
edge3_pass <-  front3_pass%>% 
  filter(edge ==1) 

pressure_logmix_edge3<- edge3_pass %>%
  glmer(formula=
          Pressure ~
          ToGo*as.factor(Down) + 
          yardline_s + 
          as.factor(Down)+
          shotgun +
          blitz +
          scorediff_s + 
          idl_pressuring +
          (1|PlayerId:IsRushing),
        family = binomial,
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
        nAGQ = 10)

edge_dist_3<-broom.mixed::tidy(pressure_logmix_edge3,effects="ran_vals") %>% 
  filter(group=='PlayerId:IsRushing') %>%
  mutate(Player_id = sub(":.*", "",level)
         ,
         rushing = sub(".*:", "",level))%>% 
  filter(rushing==1) %>%
  group_by(Player_id) %>% 
  summarise(log_ods = mean(estimate)) %>% 
  mutate(Position='EDGE') 

write.csv(edge_dist_3,'documents/edge_pass_front3_dist.csv',row.names = F)

edge_dist_3 <- read.csv('documents/edge_pass_front3_dist.csv') 

edge_dist_3$log_ods %>% var()
idl_dist_3$log_ods %>% var()
##Plots

edge_vs_idl_3<-rbind(edge_dist_3,idl_dist_3)

edge_vs_idl_3%>%
  ggplot(aes(x=log_ods, fill=Position)) + 
  geom_density(alpha=0.4) + theme_bw()+ 
  labs(
    title='In front 3 defenses: EDGE has more iLog-Odds-of-Pressure variance than iDL',
    subtitle = 'EDGE variance: 0.040  | iDL variance: 0.022',
    caption = "Data: Sports Info Solutions",
    x='iLog-Odds of Pressure',
    y='Density')+ 
  scale_fill_manual(values = c("#FCCDE5","#8DD3C7"))+ 
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14), 
    plot.caption = element_text(size=12)
  )


