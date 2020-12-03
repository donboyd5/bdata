# bdata_recessions.r
# Don Boyd
# 5/2/2020

# Run this once to create data for the package btools

# bdata.r has @import dplyr so that we can use dplyr and "%>%" without always using "dplyr::"

# libs ----
library(magrittr)
library(plyr) # needed for ldply; must be loaded BEFORE dplyr
library(tidyverse)
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library(lubridate) # lubridate, for date/times

library(btools)


# set up recession dates for indexing and for recession bands on graphs ----

# data frame to be used for recession bands in graphs, etc.
# do NOT put spaces between names or they will have to be trimmed
recessions <- read_csv(
 "peak, trough
  1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01
  2020-02-01,",
 col_types="DD")
ht(recessions)

recessions <- recessions %>% 
  mutate(rec_year=lubridate::year(peak),
         peak_decimal=decimal_date(peak),
         trough_decimal=decimal_date(trough),
         # first date of quarter in which recession started and ended
         peak_quarter=btools::fdoq(peak), 
         trough_quarter=btools::fdoq(trough)) %>%
  select(rec_year, peak, trough, peak_decimal, trough_decimal, peak_quarter, trough_quarter)
recessions

usethis::use_data(recessions, overwrite=TRUE)

# saveRDS(recessions, "./data/recessions.rds")  # this file format not allowed in packages
# save(recessions, file="./data/recessions.rda")

