library(tidyverse)
library(glue)

pbp <- read_csv("Data/AnalyticsChallenge2020Data.csv",
                na = c("NULL", "NA"))

unique(pbp$TechniqueName)

# Clean Data
pbp <- pbp %>%
    mutate(event_type = if_else(str_detect(EventType, "pass"), "pass", "rush"),
           yardline_100 = if_else(SideOfField == "Oppo", StartYard, 100 - StartYard),
           tech_side = case_when(TechniqueName %in% c("0", "Off Ball") ~ TechniqueName,
                                 TechniqueName == "Outside" ~ as.character(glue("{SideOfBall}OLB")),
                                 TRUE ~ as.character(glue("{SideOfBall}{TechniqueName}"))),
           in_designed_gap = case_when(event_type == "pass" ~ NA_real_,
                                       RunDirection == "Middle" ~ 
                                           if_else(tech_side %in% c("0", "L1", "R1", "L2i", "R2i"), 1, 0),
                                       RunDirection == "Left A Gap" ~ 
                                           if_else(tech_side %in% c("0", "L1", "L2", "L2i"), 1, 0),
                                       RunDirection == "Right A Gap" ~ 
                                           if_else(tech_side %in% c("0", "R1", "R2", "R2i"), 1, 0),
                                       RunDirection == "Left Off-Tackle B Gap" ~ 
                                           if_else(tech_side %in% c("L2", "L3", "L4", "L4i"), 1, 0),
                                       RunDirection == "Right Off-Tackle B Gap" ~ 
                                           if_else(tech_side %in% c("R2", "R3", "R4", "R4i"), 1, 0),
                                       RunDirection == "Left Off-Tackle C Gap" ~ 
                                           if_else(tech_side %in% c("L4", "L5", "L7", "L6"), 1, 0),
                                       RunDirection == "Right Off-Tackle C Gap" ~ 
                                           if_else(tech_side %in% c("R4", "R5", "R7", "R6"), 1, 0),
                                       RunDirection == "Left D Gap" ~ 
                                           if_else(tech_side %in% c("L6", "L9", "LOLB"), 1, 0),
                                       RunDirection == "Right D Gap" ~ 
                                           if_else(tech_side %in% c("R6", "R9", "ROLB"), 1, 0)),
           success = if_else(EPA > 0, 1, 0))
# %>%
#     select(RunDirection, tech_side, in_designed_gap) %>%
#     distinct() %>%
#     arrange(desc(in_designed_gap), RunDirection) %>%
#     View()

