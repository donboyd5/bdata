
# Get and save longitude and latitude points for labeling a US map with state labels.


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library("hms") # hms, for times.
library("stringr") # stringr, for strings.
library("lubridate") # lubridate, for date/times.
library("forcats") # forcats, for factors.
library("vctrs")

#****************************************************************************************************
#                get and save the state map label points ####
#****************************************************************************************************

df <- read_csv(here::here("data_raw", "us_states_centroids.csv"))
df
statemap_labels <- df %>% 
  rename(stabbr=abbr, stname=full)
statemap_labels
# check how the labeling looks on a U.S. map

p <- ggplot(data = usmap::us_map(), aes(x = x, y = y)) +
  geom_polygon(aes(group=group), fill="white", color = "gray90", size = 0.1, na.rm=TRUE) +
  coord_equal()
p

p + geom_text(data = statemap_labels, aes(x, y, label = stabbr), size = 2)
p + geom_text(data = statemap_labels, aes(x, y, label = stname), size = 2)

usethis::use_data(statemap_labels, overwrite=TRUE)
