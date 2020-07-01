library(tidyverse)
library(magrittr)
library(jsonlite)
library(janitor)


url <- "https://www.easports.com/madden-nfl/ratings/service/data?entityType=madden19_player&filter=iteration:9%20AND%20position:(LE%20OR%20DT%20OR%20RE%20OR%20LOLB%20OR%20MLB%20OR%20ROLB%20OR%20CB%20OR%20FS%20OR%20SS%20OR%20FB)&sort=ovr_rating:DESC,%20lastName:ASC&limit=100000"
json <- jsonlite::fromJSON(url)

madden_ratings <- purrr::pluck(json, "docs") %>% tibble()
madden_ratings %<>% janitor::clean_names()
madden_ratings %<>% select(first_name, last_name, position, ovr_rating, everything())

saveRDS(madden_ratings, "Data/madden_ratings.rds")
