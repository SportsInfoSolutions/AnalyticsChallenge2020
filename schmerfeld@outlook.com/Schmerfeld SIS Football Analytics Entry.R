#David Schmerfeld
#SIS Football Analytics Challenge - R Code
#July 2020

library(dplyr)

#Load raw data into data frame
data <- read.csv('c:/Users/David/Desktop/SIS Football Challenge Raw Data.csv')

#Question 1

#Determine which roster positions are in the data
summary(data$RosterPosition)

#Create subset that only includes DLs in both RosterPosition & OnFieldPosition
datasub <- data[data$RosterPosition=="DE"|data$RosterPosition=="DT",]
datasub <- datasub[datasub$OnFieldPosition=="DL",]

#Delete "NULL" entries in Expected Points Added (EPA) & make EPA numeric
datasub <- datasub[datasub$EPA!="NULL",]
datasub$EPA <- as.character(datasub$EPA)
datasub$EPA <- as.numeric(datasub$EPA)

#Create subset with negative EPAs only (positive defensive plays)
datasubpos <- datasub[datasub$EPA<0,]

#Determine which techniques are included in the data
summary(datasub$TechniqueName)

#Assign positions based on each technique:
#Nose Guard ("NG") = 0 & 1 (set as default)
datasub$DLPosition <- "NG"

#Defensive Tackle ("DT") = 2, 2i, & 3
datasub$DLPosition[datasub$TechniqueName=="2"|datasub$TechniqueName=="2i"|
                  datasub$TechniqueName=="3"] <- "DT"

#Defensive End ("DE") = 4i, 4, 5, 6, 7, & 9
datasub$DLPosition[datasub$TechniqueName=="4i"|datasub$TechniqueName=="4"|
                     datasub$TechniqueName=="5"|datasub$TechniqueName=="6"|
                     datasub$TechniqueName=="7"|datasub$TechniqueName=="7"|
                     datasub$TechniqueName=="9"] <- "DE"

#Also add column to indicate side ("RDT," "LDT," "RDE," "LDE")
datasub$DLPositionSide <- ifelse(datasub$DLPosition=="DT"|datasub$DLPosition=="DE",
                                 paste(datasub$SideOfBall,datasub$DLPosition,sep=""),
                                 "NG")
datasub$DLPositionSide <- factor(datasub$DLPositionSide)

#Determine # of plays per each position
summary(datasub$DLPositionSide)

#Summarize "Pass Rushing" statistics by position per 10,000 plays
DLPositionsPassRush <- summarize(group_by(datasub, datasub$DLPositionSide), 
                                 SoloSacks=mean(SoloSack)*10000, 
                                 AssistedSacks=mean(AssistedSack)*10000,
                                 Pressures=(mean(Pressure)-mean(SoloSack)-
                                              mean(AssistedSack))*10000)
DLPositionsPassRush

#Summarize "Play Ending" statistics by position per 10,000 plays
DLPositionsPlayEnd <- summarize(group_by(datasub, datasub$DLPositionSide), 
                                SoloTackles=(mean(SoloTackle)-
                                               mean(SoloSack))*10000, 
                                AssistedTackles=(mean(AssistedTackle)-
                                  mean(AssistedSack))*10000,
                                PassBreakup=(mean(PassBreakup)-
                                               mean(Interception))*10000)
DLPositionsPlayEnd

#Summarize "Create Turnover" statistics by position per 10,000 plays
DLPositionsTurnover <- summarize(group_by(datasub, datasub$DLPositionSide), 
                                Interceptions=mean(Interception)*10000, 
                                ForcedFumbles=mean(ForcedFumble)*10000,
                                RecoveredFumbles=mean(RecoveredFumble)*10000)
DLPositionsTurnover

#EPAs (all plays & only positive plays) of "Pass Rushing" statistics
SoloSackEPA <- mean(datasub$EPA[datasub$SoloSack==1])
SoloSackEPApos <- mean(datasubpos$EPA[datasubpos$SoloSack==1])

AssistedSackEPA <- mean(datasub$EPA[datasub$AssistedSack==1])
AssistedSackEPApos <- mean(datasubpos$EPA[datasubpos$AssistedSack==1])

PressureEPA <- mean(datasub$EPA[datasub$Pressure==1 & datasub$SoloSack==0 &
                                  datasub$AssistedSack==0])
PressureEPApos <- mean(datasubpos$EPA[datasubpos$Pressure==1 & 
                                        datasubpos$SoloSack==0 & 
                                        datasubpos$AssistedSack==0])

#EPAs (all plays & only positive plays) of "Play Ending" statistics
SoloTackleEPA <- mean(datasub$EPA[datasub$SoloTackle==1 & datasub$SoloSack==0])
SoloTackleEPApos <- mean(datasubpos$EPA[datasubpos$SoloTackle==1 & 
                                          datasubpos$SoloSack==0])

AssistedTackleEPA <- mean(datasub$EPA[datasub$AssistedTackle==1 &
                                        datasub$AssistedSack==0])
AssistedTackleEPApos <- mean(datasubpos$EPA[datasubpos$AssistedTackle==1 &
                                              datasubpos$AssistedSack==0])

PassBreakupEPA <- mean(datasub$EPA[datasub$PassBreakup==1 & 
                                     datasub$Interception==0])
PassBreakupEPApos <- mean(datasubpos$EPA[datasubpos$PassBreakup==1 & 
                                     datasubpos$Interception==0])

#EPAs (all plays & only positive plays) of "Create Turnover" statistics
InterceptionEPA <- mean(datasub$EPA[datasub$Interception==1])
InterceptionEPApos <- mean(datasubpos$EPA[datasubpos$Interception==1])

ForcedFumbleEPA <- mean(datasub$EPA[datasub$ForcedFumble==1])
ForcedFumbleEPApos <- mean(datasubpos$EPA[datasubpos$ForcedFumble==1])

RecoveredFumbleEPA <- mean(datasub$EPA[datasub$RecoveredFumble==1])
RecoveredFumbleEPApos <- mean(datasubpos$EPA[datasubpos$RecoveredFumble==1])

#Table of all EPAs
EPATable <- matrix(c(SoloSackEPA, SoloSackEPApos, 
                     AssistedSackEPA,AssistedSackEPApos, 
                     PressureEPA, PressureEPApos,
                     SoloTackleEPA, SoloTackleEPApos, 
                     AssistedTackleEPA, AssistedTackleEPApos, 
                     PassBreakupEPA, PassBreakupEPApos,
                     InterceptionEPA, InterceptionEPApos, 
                     ForcedFumbleEPA,ForcedFumbleEPApos, 
                     RecoveredFumbleEPA, RecoveredFumbleEPApos), 
                   ncol=2, byrow=TRUE)
rownames(EPATable) <- c("Solo Sack", "Assisted Sack", "Pressure (No Sack)",
                        "Solo Tackle", "Assisted Tackle", 
                        "Pass Breakup (No INT)", "Interception",
                        "Forced Fumble", "Recovered Fumble")
colnames(EPATable) <- c("EPA (all plays)", "EPA (+ plays only)")
EPATable <- as.table(EPATable)
EPATable

#Create "Indirect" statistic based on times run play was in direction of a
#D-Lineman but a teammate made a play

#First, add "DLDirection" column, in which running play to:
#Middle is in direction of NG
#A Gap is in direction of NGs & DTs
#B Gap is in direction of DTs & DEs in 4i, 4, 5, & 7 techniques
#C Gap is in direction of DE
#D Gap is in direction of DEs in 6 & 9 techniques

datasub <- datasub %>%
  mutate(DLDirection = case_when(
    RunDirection=="Middle" & DLPositionSide=="NG" ~ 1,
    RunDirection=="Left A Gap" & DLPositionSide=="NG" ~ 1, 
    RunDirection=="Left A Gap" & DLPositionSide=="RDT" ~ 1,
    RunDirection=="Right A Gap" & DLPositionSide=="NG" ~ 1,
    RunDirection=="Right A Gap" & DLPositionSide=="LDT" ~ 1,
    RunDirection=="Left Off-Tackle B Gap" & DLPositionSide=="RDT" ~ 1,
    RunDirection=="Left Off-Tackle B Gap" & TechniqueName=="4i" & DLPositionSide=="RDE" ~ 1,
    RunDirection=="Left Off-Tackle B Gap" & TechniqueName==4 & DLPositionSide=="RDE" ~ 1,
    RunDirection=="Left Off-Tackle B Gap" & TechniqueName==5 & DLPositionSide=="RDE" ~ 1,
    RunDirection=="Left Off-Tackle B Gap" & TechniqueName==7 & DLPositionSide=="RDE" ~ 1,
    RunDirection=="Right Off-Tackle B Gap" & DLPositionSide=="LDT" ~ 1,
    RunDirection=="Right Off-Tackle B Gap" & TechniqueName=="4i" & DLPositionSide=="LDE" ~ 1,
    RunDirection=="Right Off-Tackle B Gap" & TechniqueName==4 & DLPositionSide=="LDE" ~ 1,
    RunDirection=="Right Off-Tackle B Gap" & TechniqueName==5 & DLPositionSide=="LDE" ~ 1,
    RunDirection=="Right Off-Tackle B Gap" & TechniqueName==7 & DLPositionSide=="LDE" ~ 1,
    RunDirection=="Left Off-Tackle C Gap" & DLPositionSide=="RDE" ~ 1,
    RunDirection=="Right Off-Tackle C Gap" & DLPositionSide=="LDE" ~ 1,
    RunDirection=="Left D Gap" & TechniqueName==6 & DLPositionSide=="RDE" ~ 1,
    RunDirection=="Left D Gap" & TechniqueName==9 & DLPositionSide=="RDE" ~ 1,
    RunDirection=="Right D Gap" & TechniqueName==6 & DLPositionSide=="LDE" ~ 1,
    RunDirection=="Right D Gap" & TechniqueName==9 & DLPositionSide=="LDE" ~ 1
  ))
datasub$DLDirection[is.na(datasub$DLDirection)] <- 0

#Second, add "Indirect" column -- 
#Plays in a Lineman's direction where the Lineman did not make a tackle, etc.
datasub$Indirect <- ifelse(datasub$DLDirection==1 & datasub$SoloTackle==0 &
                             datasub$AssistedTackle==0 & datasub$Pressure==0 &
                             datasub$SoloSack==0 & datasub$AssistedSack==0 &
                             datasub$PassBreakup==0 & datasub$Interception==0 &
                             datasub$ForcedFumble==0 & datasub$RecoveredFumble==0,
                           1,0)

#Summarize "Indirect" statistic by position per 10,000 plays
DLPositionsIndirect <- summarize(group_by(datasub, datasub$DLPositionSide), 
                                 Indirect=mean(Indirect)*10000)
DLPositionsIndirect

#Create a subset of all indirect plays
datasubIndirectPlays <- datasub[datasub$Indirect=="1",]

#EPAs by position for indirect plays
IndirectEPA <- summarize(group_by(datasubIndirectPlays,
                                  datasubIndirectPlays$DLPositionSide), 
                         EPAavg=mean(EPA), EPAsum=sum(EPA))
IndirectEPA

#Question 2

#Create data subsets for each position
datasubDE <- datasub[datasub$DLPosition=="DE",]
datasubDT <- datasub[datasub$DLPosition=="DT",]
datasubNG <- datasub[datasub$DLPosition=="NG",]

#EPA values for sack, tackle, & pass breakup for DEs
datasubDE <- datasubDE %>%
  mutate(EPAplay = case_when(
    SoloSack==1 ~ SoloSackEPApos,
    AssistedSack==1 ~ AssistedSackEPApos, 
    SoloSack==0 & AssistedSack==0 & Pressure==1 ~ PressureEPApos,
    SoloSack==0 & SoloTackle==1 ~ SoloTackleEPApos,
    AssistedSack==0 & AssistedTackle==1 ~ AssistedTackleEPApos,
    PassBreakup==1 & Interception==0 ~ PassBreakupEPApos))

datasubDE$EPAplay[is.na(datasubDE$EPAplay)] <- 0

#EPA values for sack, tackle, & pass breakup for DTs
datasubDT <- datasubDT %>%
  mutate(EPAplay = case_when(
    SoloSack==1 ~ SoloSackEPApos,
    AssistedSack==1 ~ AssistedSackEPApos, 
    SoloSack==0 & AssistedSack==0 & Pressure==1 ~ PressureEPApos,
    SoloSack==0 & SoloTackle==1 ~ SoloTackleEPApos,
    AssistedSack==0 & AssistedTackle==1 ~ AssistedTackleEPApos,
    PassBreakup==1 & Interception==0 ~ PassBreakupEPApos))

datasubDT$EPAplay[is.na(datasubDT$EPAplay)] <- 0

#EPA values for sack, tackle, & pass breakup for NGs
datasubNG <- datasubNG %>%
  mutate(EPAplay = case_when(
    SoloSack==1 ~ SoloSackEPApos,
    AssistedSack==1 ~ AssistedSackEPApos, 
    SoloSack==0 & AssistedSack==0 & Pressure==1 ~ PressureEPApos,
    SoloSack==0 & SoloTackle==1 ~ SoloTackleEPApos,
    AssistedSack==0 & AssistedTackle==1 ~ AssistedTackleEPApos,
    PassBreakup==1 & Interception==0 ~ PassBreakupEPApos))

datasubNG$EPAplay[is.na(datasubNG$EPAplay)] <- 0

#EPA values for each turnover for DEs
datasubDE <- datasubDE %>%
  mutate(EPAturnover = case_when(
    Interception==1 ~ InterceptionEPApos,
    ForcedFumble==1 & RecoveredFumble==0 ~ ForcedFumbleEPApos, 
    ForcedFumble==0 & RecoveredFumble==1 ~ RecoveredFumbleEPApos,
    ForcedFumble==1 & RecoveredFumble==1 ~ ForcedFumbleEPApos+RecoveredFumbleEPApos,
    ))

datasubDE$EPAturnover[is.na(datasubDE$EPAturnover)] <- 0

#EPA values for each turnover for DTs
datasubDT <- datasubDT %>%
  mutate(EPAturnover = case_when(
    Interception==1 ~ InterceptionEPApos,
    ForcedFumble==1 & RecoveredFumble==0 ~ ForcedFumbleEPApos, 
    ForcedFumble==0 & RecoveredFumble==1 ~ RecoveredFumbleEPApos,
    ForcedFumble==1 & RecoveredFumble==1 ~ ForcedFumbleEPApos+RecoveredFumbleEPApos,
  ))

datasubDT$EPAturnover[is.na(datasubDT$EPAturnover)] <- 0

#EPA values for each turnover for NGs
datasubNG <- datasubNG %>%
  mutate(EPAturnover = case_when(
    Interception==1 ~ InterceptionEPApos,
    ForcedFumble==1 & RecoveredFumble==0 ~ ForcedFumbleEPApos, 
    ForcedFumble==0 & RecoveredFumble==1 ~ RecoveredFumbleEPApos,
    ForcedFumble==1 & RecoveredFumble==1 ~ ForcedFumbleEPApos+RecoveredFumbleEPApos,
  ))

datasubNG$EPAturnover[is.na(datasubNG$EPAturnover)] <- 0

#Calculate total EPA values for each position
datasubDE$EPAMeas <- datasubDE$EPAplay + datasubDE$EPAturnover
datasubDT$EPAMeas <- datasubDT$EPAplay + datasubDT$EPAturnover
datasubNG$EPAMeas <- datasubNG$EPAplay + datasubNG$EPAturnover

#EPA values for each indirect play for each position
datasubDE$EPAIndirect <- ifelse(datasubDE$Indirect==1,datasubDE$EPA,0)
datasubDT$EPAIndirect <- ifelse(datasubDT$Indirect==1,datasubDE$EPA,0)
datasubNG$EPAIndirect <- ifelse(datasubNG$Indirect==1,datasubNG$EPA,0)

#At each position, determine EPA for each player
datasubDEEPA <- summarize(group_by(datasubDE, PlayerId), 
                         Name=Name[which.max(PlayerId)],
                         Team=DefensiveTeam[which.max(PlayerId)],
                         Position=DLPositionSide[which.max(PlayerId)],
                         Plays=length(PlayerId),
                         IndirectPlays=sum(Indirect),
                         EPAMeasAll=sum(EPAMeas),
                         EPAIndirectAll=sum(EPAIndirect))

datasubDTEPA <- summarize(group_by(datasubDT, PlayerId), 
                          Name=Name[which.max(PlayerId)],
                          Team=DefensiveTeam[which.max(PlayerId)],
                          Position=DLPositionSide[which.max(PlayerId)],
                          Plays=length(PlayerId),
                          IndirectPlays=sum(Indirect),
                          EPAMeasAll=sum(EPAMeas),
                          EPAIndirectAll=sum(EPAIndirect))

datasubNGEPA <- summarize(group_by(datasubNG, PlayerId), 
                          Name=Name[which.max(PlayerId)],
                          Team=DefensiveTeam[which.max(PlayerId)],
                          Position=DLPositionSide[which.max(PlayerId)],
                          Plays=length(PlayerId),
                          IndirectPlays=sum(Indirect),
                          EPAMeasAll=sum(EPAMeas),
                          EPAIndirectAll=sum(EPAIndirect))

#Total overall EPA at each position & order players from best to worst
datasubDEEPA$EPAtotal <- datasubDEEPA$EPAMeasAll+datasubDEEPA$EPAIndirectAll
datasubDEEPA <- datasubDEEPA[order(datasubDEEPA$EPAtotal),]

datasubDTEPA$EPAtotal <- datasubDTEPA$EPAMeasAll+datasubDTEPA$EPAIndirectAll
datasubDTEPA <- datasubDTEPA[order(datasubDTEPA$EPAtotal),]

datasubNGEPA$EPAtotal <- datasubNGEPA$EPAMeasAll+datasubNGEPA$EPAIndirectAll
datasubNGEPA <- datasubNGEPA[order(datasubNGEPA$EPAtotal),]

#Distributions of EPAs at each position
datasubDEEPAtop <- datasubDEEPA[1:60,]
hist((-1*datasubDEEPAtop$EPAtotal),col="lightblue",
     main="Top 60 DEs\nWeeks 9-17 (2019)",
     xlab="Value (Measurable+Indirect)", ylab="# of DEs")

datasubDTEPAtop <- datasubDTEPA[1:60,]
hist((-1*datasubDTEPAtop$EPAtotal),col="lightblue",
     main="Top 60 DTs\nWeeks 9-17 (2019)",
     xlab="Value (Measurable+Indirect)", ylab="# of DTs")

datasubNGEPAtop <- datasubNGEPA[1:30,]
hist((-1*datasubNGEPAtop$EPAtotal),col="lightblue",
     main="Top 30 NGs\nWeeks 9-17 (2019)",
     xlab="Value (Measurable+Indirect)", ylab="# of NGs")

#Question 3

#For DEs, determine score differentials
datasubDE$ScoreDiff <- datasubDE$OffTeamScoreBefore-datasubDE$DefTeamScoreBefore

#For DEs, determine total EPA at each score differential
datasubDE$EPAPlay <- datasubDE$EPAMeas+datasubDE$EPAIndirect

#For DEs, average by total EPA value at each score differential
datasubDEEPAscore <- summarize(group_by(datasubDE, ScoreDiff), 
                              EPA=-mean(EPAPlay))

#For DTs, determine score differentials
datasubDT$ScoreDiff <- datasubDT$OffTeamScoreBefore-datasubDT$DefTeamScoreBefore

#For DTs, determine total EPA at each score differential
datasubDT$EPAPlay <- datasubDT$EPAMeas+datasubDT$EPAIndirect

#For DTs, average by total EPA value at each score differential
datasubDTEPAscore <- summarize(group_by(datasubDT, ScoreDiff), 
                              EPA=-mean(EPAPlay))

#For NGs, determine score differentials
datasubNG$ScoreDiff <- datasubNG$OffTeamScoreBefore-datasubNG$DefTeamScoreBefore

#For NGs, determine total EPA at each score differential
datasubNG$EPAPlay <- datasubNG$EPAMeas+datasubNG$EPAIndirect

#For NGs, average by total EPA value at each score differential
datasubNGEPAscore <- summarize(group_by(datasubNG, ScoreDiff), 
                              EPA=-mean(EPAPlay))

#Average by total EPA at each yard-to-go for each position
datasubDEEPAytg <- summarize(group_by(datasubDE, ToGo), 
                              EPA=-mean(EPAPlay))

datasubDTEPAytg <- summarize(group_by(datasubDT, ToGo), 
                             EPA=-mean(EPAPlay))

datasubNGEPAytg <- summarize(group_by(datasubNG, ToGo), 
                             EPA=-mean(EPAPlay))