library(tidyverse)
library(magrittr)
library(jsonlite)
library(janitor)

# iteration is the week/period, i believe the number is the ratings after the previous week
# so 1 is the initial ratings, 2 is after week 1, 3 after week 2 etc.
# this is me just getting defensive positions, you can do whatever filters you want with their gui and then copy the url, or just manually change the parameters if you want to loop over
# make sure to change the limit at the end so you get more than the top 25 or 50 or whatever the default is

url <- "https://www.easports.com/madden-nfl/ratings/service/data?entityType=madden19_player&filter=iteration:9%20AND%20position:(LE%20OR%20DT%20OR%20RE%20OR%20LOLB%20OR%20MLB%20OR%20ROLB%20OR%20CB%20OR%20FS%20OR%20SS%20OR%20FB)&sort=ovr_rating:DESC,%20lastName:ASC&limit=100000"
json <- jsonlite::fromJSON(url)

madden_ratings <- purrr::pluck(json, "docs") %>% tibble()
madden_ratings %<>% janitor::clean_names()
madden_ratings %<>% select(first_name, last_name, position, ovr_rating, everything())

saveRDS(madden_ratings, "Data/madden_ratings.rds")
