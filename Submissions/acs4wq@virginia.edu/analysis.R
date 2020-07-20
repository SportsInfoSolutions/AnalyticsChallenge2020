sis <- read_csv(url("https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv"))
# write_csv(sis, 'sis_data.csv')
setwd("~/Desktop/projects/sis_comp")

library(lme4)
library(merTools)
library(tidyverse)
library(ggthemes)
require(gridExtra)

# read data
sis <- read.csv('sis_data.csv') %>%
  # create distinct play IDs
  mutate(playID = paste0(as.character(GameID), '_', as.character(Quarter), '_', as.character(TimeLeft))) %>%
  filter(EventType=='pass' | EventType=='rush') %>%
  mutate(isRush=ifelse(EventType=='rush',1,0),
         isPass=abs(1-isRush)) %>%
  # only keep those listed as DE or DT on roster
  filter(RosterPosition=='DE' | RosterPosition=='DT') %>%
  mutate(isTackle=ifelse(RosterPosition=='DT',1,0),
         isEnd=abs(1-isTackle)) %>%
  # remove spike plays
  filter(Spike!=1) %>%
  # identify home teams
  mutate(isHome = case_when(
    OffensiveTeam=='Broncos' & StadiumName=='Broncos Stadium at Mile High' ~ 1,
    OffensiveTeam=='Vikings' & StadiumName=='U.S. Bank Stadium' ~ 1,
    OffensiveTeam=='Saints' & StadiumName=='Mercedes-Benz Superdome' ~ 1,
    OffensiveTeam=='Cardinals' & StadiumName=='State Farm Stadium' ~ 1,
    OffensiveTeam=='Bills' & StadiumName=='New Era Field' ~ 1,
    OffensiveTeam=='Chiefs' & StadiumName=='Arrowhead Stadium' ~ 1,
    OffensiveTeam=='Dolphins' & StadiumName=='Hard Rock Stadium' ~ 1,
    OffensiveTeam=='Eagles' & StadiumName=='Lincoln Financial Field' ~ 1,
    OffensiveTeam=='Steelers' & StadiumName=='Heinz Field' ~ 1,
    OffensiveTeam=='Panthers' & StadiumName=='Bank of America Stadium' ~ 1,
    OffensiveTeam=='Raiders' & StadiumName=='O.co Coliseum' ~ 1,
    OffensiveTeam=='Chargers' & StadiumName=='Dignity Health Sports Park' ~ 1,
    OffensiveTeam=='Seahawks' & StadiumName=='CenturyLink Field' ~ 1,
    OffensiveTeam=='Ravens' & StadiumName=='M&T bank Stadium' ~ 1,
    OffensiveTeam%in%c('Jets', 'Giants') & StadiumName=='MetLife Stadium' ~ 1,
    OffensiveTeam=='Bengals' & StadiumName=='Paul Brown Stadium' ~ 1,
    OffensiveTeam=='Bears' & StadiumName=='Soldier Field' ~ 1,
    OffensiveTeam=='Browns' & StadiumName=='FirstEnergy Stadium' ~ 1,
    OffensiveTeam=='Packers' & StadiumName=='Lambeau Field' ~ 1,
    OffensiveTeam=='Titans' & StadiumName=='Nissan Stadium' ~ 1,
    OffensiveTeam=='Cowboys' & StadiumName=='AT&T Stadium' ~ 1,
    OffensiveTeam=='Colts' & StadiumName=='Lucas Oil Stadium' ~ 1,
    OffensiveTeam=='Buccaneers' & StadiumName=='Raymond James Stadium' ~ 1,
    OffensiveTeam=='49ers' & StadiumName=="Levi's Stadium" ~ 1,
    OffensiveTeam=='Lions' & StadiumName=='Ford Field' ~ 1,
    OffensiveTeam=='Redskins' & StadiumName=='FedEx Field' ~ 1,
    OffensiveTeam=='Rams' & StadiumName=='Los Angeles Memorial Coliseum' ~ 1,
    OffensiveTeam=='Texans' & StadiumName=='NRG Stadium' ~ 1,
    OffensiveTeam=='Falcons' & StadiumName=='Mercedes-Benz Stadium' ~ 1,
    OffensiveTeam=='Patriots' & StadiumName=='Gillette Stadium' ~ 1,
    OffensiveTeam=='Jaguars' & StadiumName=='TIAA Bank Field' ~ 1,
    TRUE ~ 0
  ))

# data exploration
length(unique(sis$playID))

names(sis)
unique(sis$OffensiveYardage)
unique(sis$Spike)

head(sis$OnFieldPosition,10)
head(sis$TimeLeft,10)
head(sis$SideOfBall,10)
head(sis$SideOfField,10)
summary(sis$OnFieldPosition)
unique(sis$EventType)
sis %>%
 filter(OnFieldPosition=='DL') %>%
  count(RosterPosition)

sis %>%
  count(EventType)

# positional breakdown graphic
sis %>%
  ggplot(aes(x=RosterPosition, fill=OnFieldPosition)) +
  geom_bar(position = position_dodge()) + 
  theme_minimal() + 
  labs(x='Roster Position',
       y='Count',
       title='When do linemen assume a three-point stance?') + 
  scale_fill_viridis_d(name='On Field Position') + 
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        axis.title=element_text(face='bold'),
       #  legend.position='bottom',
        legend.title=element_text(face='bold'))


# what gaps to RBs go through
pass <- sis %>% filter(isPass==1)
runs <- sis %>% filter(isRush==1) 
unique(runs$RunDirection)
# rename gaps
runs <- runs %>% 
  mutate(gap = ifelse(RunDirection=='Left D Gap', 'LD',
                      ifelse(RunDirection=='Left Off-Tackle C Gap', 'LC',
                             ifelse(RunDirection=='Left Off-Tackle B Gap', 'LB',
                                    ifelse(RunDirection=='Right Off-Tackle B Gap', 'RB',
                                           ifelse(RunDirection=='Right Off-Tackle C Gap','RC',
                                                  ifelse(RunDirection=='Right D Gap','RD', 'M')))))))


# notes:
# int (and more generally, whether or not the passer throws/gets sacked, isn't really within the D-lineman's control)
# turnovers are harder to control and should be accounted for in the model 
# whereas pressures, which are all that matters when it comes to DL,
# should be accounted for by the random effects (individual players, on passing plays)


unique(runs$TechniqueName)
unique(runs$SideOfBall)

# define gapMatch feature
runs2 <- runs %>%
  mutate(gapMatch = case_when(
    gap=='M' & TechniqueName=='0' ~ 1,
    gap=='M' & TechniqueName=='1' ~ 1,
    gap=='M' & TechniqueName=='2i' ~ 1,
    gap=='RD' & TechniqueName=='6' & SideOfBall=='L' ~ 1,
    gap=='RD' & TechniqueName=='9' & SideOfBall=='L' ~ 1,
    gap=='LD' & TechniqueName=='6' & SideOfBall=='R' ~ 1,
    gap=='LD' & TechniqueName=='9' & SideOfBall=='R' ~ 1,
    # 4, 5, 7, 6
    gap=='RC' & TechniqueName%in%c('4','5','7','6') & SideOfBall=='L' ~ 1,
    gap=='LC' & TechniqueName%in%c('4','5','7','6') & SideOfBall=='R' ~ 1,
    # 2, 3, 4i, 4
    gap=='RB' & TechniqueName%in%c('2','3','4i','4') & SideOfBall=='L' ~ 1,
    gap=='LB' & TechniqueName%in%c('2','3','4i','4') & SideOfBall=='R' ~ 1,
    TRUE ~ 0
  ))

# player name lookup table
name_lookup <- sis %>%
  dplyr::select(PlayerId, Name, RosterPosition) %>%
  distinct()

# nDT lookup
runs_nTackle <- runs2 %>% 
  select(playID, RosterPosition) %>%
  group_by(playID, RosterPosition) %>%
  summarise(numDT = n()) %>%
  filter(RosterPosition=='DT') %>%
  select(-RosterPosition)

# nDE lookup
runs_nEnd <- runs2 %>% 
  select(playID, RosterPosition) %>%
  group_by(playID, RosterPosition) %>%
  summarise(numDE = n()) %>%
  filter(RosterPosition=='DE') %>%
  select(-RosterPosition)

# run_epa lookup
run_epa_lookup <- runs2 %>% 
  group_by(DefensiveTeam) %>%
  summarise(run_epa = mean(EPA, na.rm=TRUE))

# pass_epa lookup
pass_epa_lookup <- pass %>%
  group_by(DefensiveTeam) %>%
  summarise(pass_epa = mean(EPA, na.rm=TRUE))

# merge
runs3 <- runs2 %>%
  left_join(runs_nEnd, by='playID') %>%
  left_join(runs_nTackle, by='playID') %>%
  left_join(pass_epa_lookup, by='DefensiveTeam') 

# same as above, for pass plays
pass_nTackle <- pass %>% 
  select(playID, RosterPosition) %>%
  group_by(playID, RosterPosition) %>%
  summarise(numDT = n()) %>%
  filter(RosterPosition=='DT') %>%
  select(-RosterPosition)

pass_nEnd <- pass %>% 
  select(playID, RosterPosition) %>%
  group_by(playID, RosterPosition) %>%
  summarise(numDE = n()) %>%
  filter(RosterPosition=='DE') %>%
  select(-RosterPosition)

pass2 <- pass %>% 
  left_join(pass_nEnd, by='playID') %>%
  left_join(pass_nTackle, by='playID') %>%
  left_join(run_epa_lookup, by='DefensiveTeam')

# subset
pass_sub <- pass2 %>%
  dplyr::mutate(nDE = ifelse(is.na(numDE), 0, numDE),
         nDT = ifelse(is.na(numDT), 0, numDT),
         nDL = nDE + nDT) %>%
  dplyr::select(EPA, PlayerId, OffensiveTeam, 
         isHome, run_epa, nDT, nDE, isEnd, ThrowDepth, Completion, InterceptionOnPlay,
         SackOnPlay, FumbleByPasser, FumbleByReceiver, Pressure, PassBreakupOnPlay, Turnover)

# subset
runs_sub <- runs3 %>%
  mutate(team_gap = paste0(OffensiveTeam, ' ', gap),
         nDE = ifelse(is.na(numDE), 0, numDE),
         nDT = ifelse(is.na(numDT), 0, numDT),
         nDL = nDE + nDT) %>%
  dplyr::select(EPA, PlayerId, OffensiveTeam, team_gap, 
         gapMatch, nDE, nDT, isEnd, isHome, pass_epa, Turnover)


# MODELING

rush_formula <- as.formula(EPA ~ gapMatch + isHome + pass_epa + nDT + nDE + Turnover + 
                             (1|PlayerId) + (1|OffensiveTeam) + (1|team_gap))
environment(rush_formula) <- list2env(runs_sub)
rush_lmer <- lmer(rush_formula)
rush_re <- ranef(rush_lmer)

pass_formula <- as.formula(EPA ~ isHome + run_epa + nDT + nDE + 
                             FumbleByReceiver + Turnover + 
                             ThrowDepth + Completion + # InterceptionOnPlay + 
                             (1|PlayerId) + (1|OffensiveTeam))
environment(pass_formula) <- list2env(pass_sub)
pass_lmer <- lmer(pass_formula)
pass_re <- ranef(pass_lmer)

rush_table <- as.data.frame(rush_re$PlayerId) %>%
  mutate(Player_Model_ID_Rush = rownames(rush_re$PlayerId)) %>%
  rename(rush_iPA = '(Intercept)')

pass_table <- as.data.frame(pass_re$PlayerId) %>%
  mutate(Player_Model_ID_Pass = rownames(pass_re$PlayerId)) %>%
  rename(pass_iPA = '(Intercept)')

# get std deviations
sim_runs <- REsim(rush_lmer, n.sims=1000) %>%
  dplyr::select(groupFctr, groupID, sd) %>%
  rename(Player_Model_ID_Rush = groupID, Sim_rush_iPA_SD = sd) %>%
  group_by(groupFctr) %>%
  nest()
sim_pass <- REsim(pass_lmer, n.sims=1000) %>%
  dplyr::select(groupFctr, groupID, sd) %>%
  rename(Player_Model_ID = groupID, Sim_pass_iPA_SD = sd) %>%
  group_by(groupFctr) %>%
  nest()

# merge player names and EPA added per play
player_table <- name_lookup %>%
  mutate(PlayerId = as.character(PlayerId)) %>%
  left_join(rush_table, by=c('PlayerId' = 'Player_Model_ID_Rush')) %>%
  left_join(pass_table, by=c('PlayerId' = 'Player_Model_ID_Pass')) %>%
  left_join(sim_runs$data[[which(sim_runs$groupFctr == 'PlayerId')]],
            by=c('PlayerId' = 'Player_Model_ID_Rush')) %>%
  left_join(sim_pass$data[[which(sim_pass$groupFctr == 'PlayerId')]],
            by=c('PlayerId' = 'Player_Model_ID')) %>%
  mutate(Sim_rush_iPA_SD = ifelse(is.na(Sim_rush_iPA_SD), 0, Sim_rush_iPA_SD),
         Sim_pass_iPA_SD = ifelse(is.na(Sim_pass_iPA_SD), 0, Sim_pass_iPA_SD))

# remove NAs (only 1)
player_table <- player_table[complete.cases(player_table),]
head(player_table)

passchart8 <- player_table %>%
  ggplot(aes(x=pass_iPA, fill=RosterPosition)) + 
  geom_density(alpha=0.4)

sub_de <- player_table %>%
  filter(RosterPosition=='DE')

sub_dt <- player_table %>% 
  filter(RosterPosition=='DT')


plota <- player_table %>%
  ggplot(aes(x=(-1*pass_iPA), fill=RosterPosition)) + 
  geom_density(alpha=0.4) + 
  scale_fill_brewer(palette = 'Set1', name='Roster Position') + 
  labs(x='-Pass iPA', y='Density',
       title='Pass Plays') + 
  ylim(0,70) + 
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        axis.title=element_text(face='bold'),
        legend.position='bottom',
        legend.title=element_text(face='bold')) + 
  geom_vline(aes(xintercept=mean(sub_de$pass_iPA)), linetype='dashed', alpha=0.7) + 
  geom_vline(aes(xintercept=mean(sub_dt$pass_iPA)), linetype='dashed', alpha=0.7)

plotb <- player_table %>%
  ggplot(aes(x=(-1*rush_iPA), fill=RosterPosition)) + 
  geom_density(alpha=0.4) + 
  scale_fill_brewer(palette = 'Set1', name='Roster Position') + 
  labs(x='-Run iPA', y='Density',
       title='Run Plays') + 
  ylim(0,70) + 
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        axis.title=element_text(face='bold')) + # legend.position='none')  + 
  geom_vline(aes(xintercept=mean(sub_dt$rush_iPA)), linetype='dashed', alpha=0.7) + 
  geom_vline(aes(xintercept=mean(sub_de$rush_iPA)), linetype='dashed', alpha=0.7)

# function from stackOverflow
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend<-g_legend(plota)
p3 <- grid.arrange(arrangeGrob(plota + theme(legend.position="none"),
                               plotb + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))




# other stats

sis %>%
  filter(!is.na(PressureOnPlay), isPass==1) %>%
  group_by(PressureOnPlay) %>%
  summarise(mean(EPA, na.rm=TRUE))

sis %>%
  group_by(isRush) %>%
  summarise(mean(EPA, na.rm=TRUE))

player_table %>%
  group_by(RosterPosition) %>%
  summarise(mean(rush_iPA))


