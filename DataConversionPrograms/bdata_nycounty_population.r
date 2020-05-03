
# https://data.ny.gov/Government-Finance/Annual-Population-Estimates-for-New-York-State-and/krt9-ym2k/data


# alt-o, shift-alt-o
# alt-l, shift-alt-l
# alt-r


# libraries ----
library(magrittr)
library(plyr) # needed for ldply; must be loaded BEFORE dplyr
library(tidyverse)
options(tibble.print_max = 65, tibble.print_min = 65) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library(readxl)

library(scales)
library(hms) # hms, for times
library(lubridate) # lubridate, for date/times
library(vctrs)

library(grDevices)
library(knitr)
library(kableExtra)

library(btools)
library(bdata)

# get data ----
url <- "https://data.ny.gov/api/views/krt9-ym2k/rows.csv?accessType=DOWNLOAD&sorting=true"
download.file(url, here::here("data_raw", "nycountypop_history.csv"), mode = "wb")

df <- read_csv(here::here("data_raw", "nycountypop_history.csv"))
max(df$Year)
df2 <- df %>%
  setNames(c("fips", "geoname", "year", "program", "value")) %>%
  mutate(fips_co=str_sub(fips, 3, 5), name="pop")
glimpse(df2)
count(df2, fips_co, geoname) # 63: 57 counties, 5 boroughs NYC, New York State
count(df2, program)
count(df2, year)
count(df2, year, program) %>% pivot_wider(names_from = program, values_from = n)
# decennial years have base plus intercensal
# looks like seamless is intercensal or postcensal, whichever is not na, as we only ever have one

df3 <- df2 %>%
  mutate(usethis=case_when(str_detect(program, "Intercensal") & !is.na(value) ~ TRUE,
                           str_detect(program, "Postcensal") & !is.na(value) ~ TRUE,
                           TRUE ~ FALSE))
count(df3 %>% filter(usethis), year, program) %>% pivot_wider(names_from = program, values_from = n)

nycfips <- c("005", "047", "061", "081", "085")
pop_hist <- df3 %>%
  filter(usethis) %>%
  select(year, fips_co, geoname, name, value) %>%
  mutate(rectype=case_when(fips_co=="000" ~ "NYS",
                           fips_co %in% nycfips ~ "NYCBoro",
                           TRUE ~ "County"))
count(pop_hist, rectype, geoname)
glimpse(pop_hist)

# let's add ROS and NYC records and create uni_code and rectype
popupdown <- pop_hist %>%
  filter(fips_co != "000") %>%
  group_by(year, name, rectype) %>%
  summarise(value=sum(value)) %>%
  ungroup %>%
  mutate(rectype=ifelse(rectype=="NYCBoro", "NYC", "ROS"))
ht(popupdown)

pop_hist_save <- pop_hist %>%
  bind_rows(popupdown) %>%
  mutate(uni_code=ifelse(rectype %in% c("County", "NYCBoro"), fips_co, rectype)) %>%
  select(year, uni_code, rectype, fips_co, geoname, name, value)

glimpse(pop_hist_save)
summary(pop_hist_save)
count(pop_hist_save, uni_code, rectype, fips_co, geoname)
count(pop_hist_save, name)

pop_hist_save %>%
  filter(uni_code == "ROS") %>%
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point()

nycounty_pop <- pop_hist_save %>%
  select(-name, -fips_co) %>%
  arrange(rectype, uni_code, geoname, year)
glimpse(nycounty_pop)
summary(nycounty_pop)

usethis::use_data(nycounty_pop, overwrite=TRUE)
