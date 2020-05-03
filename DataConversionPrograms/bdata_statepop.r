# bdata_statepop.r
# Don Boyd
# 5/3/2020

# create file with annual state population from 1900 forward

#  Sources:
#  http://www.census.gov/popest/  for data in general
#  http://www2.census.gov/programs-surveys/popest/datasets/ to get straight to files

#  for 2000-2010: # the data on the web ALSO are Sep 2011 so continue to use the downloaded file
#  http://www.census.gov/popest/data/intercensal/state/tables/ST-EST00INT-01.csv now (Feb 2013) appears to be latest
#  http://www.census.gov/popest/intercensal/state/state2010.html ST-EST00INT-01.csv 2000-2010  released Sep 2011
#  http://www.census.gov/popest/archives/2000s/vintage_2001/CO-EST2001-12/CO-EST2001-12-00.html 1990-2000 Release Date: April 11, 2002
#  http://www.census.gov/popest/archives/1990s/  1990-2000
#  http://www.census.gov/popest/archives/1980s/80s_st_totals.html   1900-1990

#  NOTES:
#  st0009ts, st1019ts, st2029ts, st3039ts, st4049:
#     only have the intercensal estimates. AK and HI are not in the files - not states yet
#  st5060ts, st6070ts, st6070ts, st8090ts:
#     have Apr and July for decade start year, but only Apr for start of next decade
# CO-EST2001-12-00.csv
# ST-EST00INT-01.csv 2000-2010

# CAUTION: we do not have intercensal data for 1970, 1980 and must use decennial for those years(???)


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("devtools")

library(magrittr)
library(plyr) # needed for ldply; must be loaded BEFORE dplyr
library(tidyverse)
options(tibble.print_max = 110, tibble.print_min = 110) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library(lubridate) # lubridate, for date/times

library(btools) # library that I created (install from github)
library(bdata)

library(readxl) # readxl, for .xls and .xlsx files.
library(haven) # haven, for SPSS, SAS and Stata files.
library(vctrs)

library(grDevices)
library(knitr)

library(zoo) # for rollapply

# Steps: ----
# 1. upate and save the most recent data, currently postcensal 2010+
# 2. assemble the previously prepared files
# 3. save

# Assemble previously prepared files ----
pop_1900_1990 <- readRDS(here::here("data_intermediate", "popfiles", "pop_1900_1990.rds"))
glimpse(pop_1900_1990)
ht(pop_1900_1990)

pop_1990_2000 <- readRDS(here::here("data_intermediate", "popfiles", "pop_1990_2000.rds"))
glimpse(pop_1990_2000)

pop_2000_2010 <- readRDS(here::here("data_intermediate", "popfiles", "pop_2000_2010.rds"))
glimpse(pop_2000_2010)

pop_2010plus <- readRDS(here::here("data_intermediate", "popfiles", "pop_2010plus.rds"))
glimpse(pop_2010plus)

tpop <- bind_rows(pop_1900_1990, pop_1990_2000, pop_2000_2010, pop_2010plus)
glimpse(tpop)
count(tpop, type)
count(tpop, fname)

# note that for 1970 and 1980 we only have census, and that for 1990 we have two census values
tpop %>% filter(stabbr=="NY", year %in% c(1960, 1970, 1980, 1990)) %>% arrange(year, type)

# get latest value per year per type, then establish priority for seamless data
tpop %>%
  filter(type != "base") %>%
  group_by(stabbr, type, year) %>%
  summarise(n=n()) %>%
  filter(n > 1) %>%
  group_by(type, year) %>%
  summarise(nrecs=n())
# we want the latest record for each of these

# order file names by priority, from most recent to oldest
fnames <- tpop$fname %>% unique %>% sort
fpriority <- c(3, 1, 2, length(fnames):4)
fdf <- tibble(fname=fnames, fpriority) %>% arrange(fpriority)
fdf

check <- tpop %>%
  left_join(fdf, by = "fname") %>%
  arrange(stabbr, type, year, fpriority) %>%
  group_by(stabbr, type, year) %>%
  mutate(n=n()) %>%
  filter(n>1)
check

# good, now we are ready
upop <- tpop %>%
  left_join(fdf, by = "fname") %>%
  arrange(stabbr, type, year, fpriority) %>%
  group_by(stabbr, type, year) %>%
  filter(fpriority==min(fpriority)) %>%
  ungroup %>%
  select(-fpriority)
upop

# note that now we only have 1 census value for 1990 and of course still no intercensal values for 1970 and 1980
upop %>% filter(stabbr=="NY", year %in% c(1960, 1970, 1980, 1990)) %>% arrange(year, type)
# are there any years for which we have NO values for a  state (other than AK, HI?)
# for which years do we not have intercensal?
stub <- expand_grid(stabbr=unique(tpop$stabbr), year=unique(tpop$year), type=unique(tpop$type)) %>%
  left_join(upop, by = c("stabbr", "year", "type"))
stub

# any states that don't have ANY value for a year?
stub %>%
  group_by(stabbr, year) %>%
  summarise(n.notna=sum(!is.na(value))) %>%
  ungroup %>%
  filter(n.notna==0) %>%
  count(stabbr)
# good, just AK and HI

# any years for which we don't have intercensal or postcensal?
stub %>%
  filter(type %in% c("intercensal", "postcensal")) %>%
  group_by(stabbr, year) %>%
  summarise(n.notna=sum(!is.na(value))) %>%
  ungroup %>%
  filter(n.notna==0) %>%
  filter(!(stabbr %in% c("AK", "HI") & year < 1950)) %>%
  count(year)
# just 1970 and 1980

# save 2 files:
#   allpop all of upop
#   spop.a a unique set of data -- either intercensal or postcensal
allpop <- upop
usethis::use_data(allpop, overwrite=TRUE)

spop.a <- upop %>%
  filter((type %in% c("intercensal", "popstcensal")) |
           (year %in% c(1970, 1980) & type=="census")) %>%
  select(-fname) %>%
  arrange(stabbr, year)
glimpse(spop.a)
summary(spop.a)
anyDuplicated(spop.a %>% select(stabbr, year))
count(spop.a, stabbr)
count(spop.a, year)
count(spop.a, type)

spop.a %>%
  filter(stabbr=="LA") %>%
  mutate(lvalue=log(value)) %>%
  ggplot(aes(year, lvalue)) +
  geom_line()

# looks good
usethis::use_data(spop.a, overwrite=TRUE)



# ONETIME: 2010+ Get and save most recent file, currently postcensal 2010-2019 ----
# These data are not located where you'd expect -- they're in a national directory.
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv
ufn2010 <- "nst-est2019-alldata.csv"
urlbase <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/"
download.file(paste0(urlbase, ufn2010), here::here("data_raw", "popraw", ufn2010), mode="wb")

tpop <- read_csv(here::here("data_raw", "popraw", ufn2010), col_names=TRUE)
tpop # 151 columns; 50 states, DC, US, PR, regions; web page shows date of December 30, 2019

# we only need first 17 columns, subtracting the first 4 (a net of 13 columns)
vnames <- c("stname", "2010cen", "2010base", paste0(2010:2019, "post"))
tpop2 <- tpop %>%
  select(5:17) %>%
  setNames(vnames) %>%
  filter(stname %in% c(state.name, "District of Columbia", "United States")) %>% # drop Puerto Rico
  mutate(stabbr=bdata::stcodes$stabbr[match(stname, bdata::stcodes$stname %>% as.character)] %>% as.character)
tpop2

pop_2010plus <- tpop2 %>%
  select(-stname) %>%
  pivot_longer(-stabbr, names_to = "year") %>%
  mutate(type=case_when(str_detect(year, "cen") ~ "census",
                        str_detect(year, "base") ~ "base",
                        str_detect(year, "post") ~ "postcensal"),
         year=str_sub(year, 1, 4) %>% as.integer,
         fname=ufn2010)
glimpse(pop_2010plus)
summary(pop_2010plus)
ht(pop_2010plus)

saveRDS(pop_2010plus, here::here("data_intermediate", "popfiles", "pop_2010plus.rds"))



# ONETIME get and save pop data for recent decades ----
#.. 1990-2000 ----
# pdf can be found here: https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/st-co/co-est2001-12-00.pdf
# I work with a previously downloaded text file, Release Date: April 11, 2002
# it includes 1990cen, 1990-1999 intercensal, 2000cen
# 50 states, DC, US

fn <- "CO-EST2001-12-00.csv"
# use locale to read the state name - spaces are encoded differently than normal file
# Unicode Character 'NO-BREAK SPACE' (U+00A0)  (i.e., NBSP)

tpop <- read_csv(here::here("data_raw", "popraw", fn),
                 col_names=FALSE, skip=3, locale=locale(encoding="latin1"))
tpop # 50 states, DC, US
# tpop <- read_csv(here::here("data_raw", "popraw", fn),
#                  col_names=FALSE, skip=3)
# tpop <- tpop %>%
#   mutate(X1=str_replace_all(X1, "\\s", " ")) # replace white spaces inside string with space
# iconvlist()
vnames <- c("stname", "1990cen", as.character(1990:1999), "2000cen")
pop_1990_2000 <- tpop %>% setNames(vnames) %>%
  mutate(stname=str_squish(stname), # replace white space with space
         stname=ifelse(stname=="USA", "United States", stname),
         stabbr=bdata::stcodes$stabbr[match(stname, bdata::stcodes$stname %>% as.character)] %>% as.character) %>%
  filter(stname %in% bdata::stcodes$stname) %>%
  select(-stname) %>%
  pivot_longer(-stabbr, names_to = "year") %>%
  mutate(type=ifelse(str_detect(year, "cen"), "census", "intercensal"),
         year=str_sub(year, 1, 4) %>% as.integer, fname=fn)
count(pop_1990_2000, type)
count(pop_1990_2000, year)
count(pop_1990_2000, stabbr)
ht(pop_1990_2000)

saveRDS(pop_1990_2000, here::here("data_intermediate", "popfiles", "pop_1990_2000.rds"))



#.. 2000-2010 ----
ufn2000 <- "st-est00int-01.csv"  # Census APPEARS to have updated this 24-Aug-2016
urlbase <- "https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/state/"
download.file(paste0(urlbase, ufn2000), here::here("data_raw", "popraw", ufn2000), mode="wb")

tpop <- read_csv(here::here("data_raw", "popraw", ufn2000), col_names=FALSE, skip=3)
tpop # 14 columns; 50 states, DC, US, PR, regions; Release Date: September 2011 

vnames <- c("stname", "2000cen", as.character(2000:2009), "2010cen", "2010")
tpop2 <- tpop %>%
  setNames(vnames) %>%
  mutate(stname=str_remove(stname, "\\.")) %>%
  filter(stname %in% c(state.name, "District of Columbia", "United States")) %>% # drop Puerto Rico
  mutate(stabbr=stcodes$stabbr[match(stname, stcodes$stname %>% as.character)] %>% as.character)

pop_2000_2010 <- tpop2 %>%
  select(-stname) %>%
  pivot_longer(-stabbr, names_to = "year") %>%
  mutate(type=ifelse(str_detect(year, "cen"), "census", "intercensal"),
         year=str_sub(year, 1, 4) %>% as.integer,
         fname=ufn2000)
glimpse(pop_2000_2010)
summary(pop_2000_2010)
ht(pop_2000_2010)

saveRDS(pop_2000_2010, here::here("data_intermediate", "popfiles", "pop_2000_2010.rds"))


# ONETIME get and save pop data for 1900-1990 ----
(ygroups1 <- seq(0009, 4049, by=1010) %>% str_pad(width=4, side="left", pad="0"))
(ygroups2 <- seq(5060, 8090, by=1010) %>% str_pad(width=4, side="left", pad="0"))
(ygroups <- c(ygroups1, ygroups2))
(fnames <- paste0("st", ygroups, "ts.txt"))


get_rtypes <- function(df, y1){
  # this works for 
  # identify record types, and first and second group of years, and keep desired recs
  fix1970 <- function(text, y1){
    if(y1==1970) str_sub(text, 5, -1) else(text)
  }
  
  df2 <- df %>%
    mutate(text=fix1970(text, y1), # remove first 4 chars
           rn=row_number(),
           rtype=case_when(str_sub(text, 1, 3) %in% c("U.S", "US ") ~ "US",
                           str_sub(text, 1, 2) %in% c(state.abb, "DC") ~ "state",
                           TRUE ~ "other"),
           rtype=case_when(rtype=="other" & y1!=1970 & rtype[rn + 1]=="US" ~ "years",
                           rtype=="other" & y1==1970 & str_sub(text[rn + 1], 1, 2)=="AL" ~ "years",
                           TRUE ~ rtype),
           group2start=which(rtype=="years")[2],
           group=ifelse(rn < group2start, 1, 2)) %>%
    filter(rtype %in% c("US", "state", "years")) %>%
    select(rn, rtype, group, text)
  
  # clean file before splitting it into individual years
  getrow <- function(y1, grp){
    text <- case_when(
      y1 %in% c(1900, 1910, 1920, 1930, 1940) & grp==1 ~
        paste0("stabbr ", paste(y1:(y1 + 5), collapse=" ")),
      y1 %in% c(1900, 1910, 1920, 1930, 1940) & grp==2 ~
        paste0("stabbr ", paste((y1 + 6):(y1 + 9), collapse=" ")),
      y1 %in% c(1950, 1960) & grp==1 ~
        paste0("stabbr ", 
               paste0(y1, "cen "),
               paste(y1:(y1 + 4), collapse=" ")),
      y1 %in% c(1950, 1960) & grp==2 ~
        paste0("stabbr ",
               paste((y1 + 5):(y1 + 9), collapse=" "),
               paste0(" ", y1 + 10, "cen")),
      y1 %in% c(1970) & grp==1 ~
        paste0("stabbr ", 
               paste0(y1, "cen "),
               paste((y1 + 1):(y1 + 5), collapse=" ")),
      y1 %in% c(1970) & grp==2 ~
        paste0("stabbr ",
               paste((y1+6):(y1 + 9), collapse=" "),
               paste0(" ", y1 + 10, "cen")),
      y1 %in% c(1980) & grp==1 ~
        paste0("stabbr ", 
               paste0(y1, "cen "),
               paste(paste0((y1 + 1):(y1 + 4)), collapse=" ")),
      y1 %in% c(1980) & grp==2 ~
        paste0("stabbr ",
               paste((y1 + 5):(y1 + 9), collapse=" "),
               paste0(" ", y1 + 10, "cen")),
      TRUE ~ "junk")
    return(text)
    }
    
    year_rows <- expand_grid(year1=seq(1900, 1990, by=10), group=c(1:2)) %>%
      rowwise() %>%
      mutate(text=getrow(year1, group))
    
    fixyears <- function(y1, grp){
      year_rows %>%
        filter(year1==y1, group==grp) %>%
        .$text
    }
    
    fixus <- function(text) {
      text <- str_remove(text, "U.S.")
      text <- ifelse(str_sub(text, 1, 2) != "US", paste0("US ", text), text)
      text
    }
  
  df3 <- df2 %>%
    rowwise() %>%
    mutate(text=ifelse(rtype=="years", fixyears(y1, group), text),
           text=ifelse(rtype=="US", fixus(text), text))
  df3
}


get_pop_history <- function(fname){
  # Important changes over time:
  #   Files for 1940-49 and earlier all have July 1 intercensal estimates only and have approx same format
  #   1950-60 has census April 1 for 1950, July 1 intercensal for 1950-1959 (1950 both ways), then April census for 1960
  #   1960-70 has same year arrangement as 1950-60 but format differs; 1960 is in this file and in prior - use this
  #   1970-80 has 1970 Census, then 1971-1979 intercensal, then 1980 census
  #   1980-90 has 1980 Census, then 1981-1989 intercensal, then 1990 census; format differs
  
  # read the file as one text variable per row
  df <- read_delim(here::here("data_raw", "popraw", fname),
                   delim="\n", 
                   col_names="text", 
                   skip_empty_rows = TRUE)
  
  y1 <- paste0("19", str_sub(fname, 3, 4)) %>% as.numeric
  df2 <- get_rtypes(df, y1)
  
  todf <- function(text){
    tmat <- str_split(text, boundary("word"), simplify=TRUE)
    cnames <- tmat[1, ]
    tmat <- tmat[-1, ]
    colnames(tmat) <- cnames
    tdf <- as_tibble(tmat) %>%
      pivot_longer(-stabbr, names_to = "year")
    tdf
  }
  
  group1 <- df2 %>% filter(group==1) %>% .$text
  group2 <- df2 %>% filter(group==2) %>% .$text
  
  df3 <- bind_rows(todf(group1), todf(group2))
  
  df4 <- df3 %>%
    mutate(type=ifelse(str_detect(year, "cen"), "census", "intercensal"),
           year=str_sub(year, 1, 4) %>% as.integer(),
           value=parse_number(value),
           fname=fname) %>%
    arrange(stabbr, year, type) %>%
    distinct()
  df4
}

tmp <- get_pop_history(fnames[2]) %>% count(year)

pop1 <- ldply(fnames, get_pop_history)
glimpse(pop1)
summary(pop1)
count(pop1, type)
count(pop1, year)
count(pop1 %>% filter(type=="intercensal"), year)
count(pop1 %>% filter(type=="census"), year)
pop1 %>% filter(type=="census", year==1970) %>% arrange(stabbr)
count(pop1)
pop1 %>% filter(stabbr=="NY")

# keep better census population when there are 2 records and then convert units
pop2 <- pop1 %>%
  group_by(stabbr, year, type) %>%
  arrange(fname) %>%
  slice(n()) %>% # gets last observation
  mutate(value=ifelse(year < 1970, value * 1000, value)) %>%
  ungroup %>%
  arrange(stabbr, type, year)
anyDuplicated(pop2 %>% select(stabbr, year, type))

pop2 %>%
  filter(stabbr=="NY")
  
pop2 %>%
  filter(stabbr=="NY") %>%
  ggplot(aes(year, value)) +
  geom_line()

saveRDS(pop2, here::here("data_intermediate", "popfiles", "pop_1900_1990.rds"))


# parse_number("-$1,234,567.78") # bad!!

