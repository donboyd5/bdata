# bdata_states.R
# Don Boyd
# 7/31/2021

# Create stcodes data file that has various state codes available


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
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)


#****************************************************************************************************
#                Data ####
#****************************************************************************************************

# base data ----
stabbr <- c('US','AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
            'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY',
            'PR','VI')

stname <- c('United States','Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','District of Columbia',
            'Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland',
            'Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey',
            'New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
            'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming',
            'Puerto Rico','Virgin Islands')

stfips <- c('00','01','02','04','05','06','08','09','10','11','12','13','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30',
            '31','32','33','34','35','36','37','38','39','40','41','42','44','45','46','47','48','49','50','51','53','54','55','56',
            '72','78')

stcen <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27',
           '28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50','51',
           '72','78') # I made this up for territories

stcodes <- as_tibble(list(stabbr=stabbr, stfips=stfips, stcen=stcen, stname=stname))


# BEA Regions ----
neng.bea <- c("CT", "MA", "ME", "NH", "RI", "VT")
mideast.bea <- c("DE", "DC", "MD", "NJ", "NY", "PA") # BEA calls this mideast but Lucy uses Mid Atlantic
glr.bea <- c("IL", "IN", "MI", "OH", "WI")
plr.bea <- c("IA", "KS", "MN", "MO", "NE", "ND", "SD")
ser.bea <- c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", "WV")
swr.bea <- c("AZ", "NM", "OK", "TX")
rmr.bea <- c("CO", "ID", "MT", "UT", "WY")
fwr.bea <- c("AK", "CA", "HI", "NV", "OR", "WA")

stcodes <- stcodes %>%
  mutate(beargn=case_when(stabbr %in% neng.bea ~ "neng",
                               stabbr %in% mideast.bea ~ "mideast",
                               stabbr %in% glr.bea ~ "glr",
                               stabbr %in% plr.bea ~ "plr",
                               stabbr %in% ser.bea ~ "ser",
                               stabbr %in% swr.bea ~ "swr",
                               stabbr %in% rmr.bea ~ "rmr",
                               stabbr %in% fwr.bea ~ "fwr",
                               stabbr == "US" ~ "US"),
         beargn.name=case_when(stabbr %in% neng.bea ~ "New England",
                               stabbr %in% mideast.bea ~ "Mideast",
                               stabbr %in% glr.bea ~ "Great Lakes",
                               stabbr %in% plr.bea ~ "Plains",
                               stabbr %in% ser.bea ~ "Southeast",
                               stabbr %in% swr.bea ~ "Southwest",
                               stabbr %in% rmr.bea ~ "Rocky Mountain",
                               stabbr %in% fwr.bea ~ "Far West",
                               stabbr == "US" ~ "United States"))


# stcodes$beargn <- ""
# stcodes$beargn.name <- ""
# 
# stcodes$beargn[stcodes$stabbr %in% neng.b] <- "neng"
# stcodes$beargn.name[stcodes$stabbr %in% neng.b] <- "New England"
# 
# stcodes$beargn[stcodes$stabbr %in% mdatl.b] <- "mdatl"
# stcodes$beargn.name[stcodes$stabbr %in% mdatl.b] <- "Mid Atlantic"
# 
# stcodes$beargn[stcodes$stabbr %in% glr.b] <- "glr"
# stcodes$beargn.name[stcodes$stabbr %in% glr.b] <- "Great Lakes"
# 
# stcodes$beargn[stcodes$stabbr %in% plr.b] <- "plr"
# stcodes$beargn.name[stcodes$stabbr %in% plr.b] <- "Plains"
# 
# stcodes$beargn[stcodes$stabbr %in% ser.b] <- "ser"
# stcodes$beargn.name[stcodes$stabbr %in% ser.b] <- "Southeast"
# 
# stcodes$beargn[stcodes$stabbr %in% swr.b] <- "swr"
# stcodes$beargn.name[stcodes$stabbr %in% swr.b] <- "Southwest"
# 
# stcodes$beargn[stcodes$stabbr %in% rmr.b] <- "rmr"
# stcodes$beargn.name[stcodes$stabbr %in% rmr.b] <- "Rocky Mountain"
# 
# stcodes$beargn[stcodes$stabbr %in% fwr.b] <- "fwr"
# stcodes$beargn.name[stcodes$stabbr %in% fwr.b] <- "Far West"
# 
# stcodes$beargn[stcodes$stabbr %in% c("US")] <- "usr"
# stcodes$beargn.name[stcodes$stabbr %in% c("US")] <- "United States"


# Census regions ----
# see list of states and regions in the notes section
# fips codes for states
ne.cen <- c(9, 23, 25, 33, 44, 50, 34, 36, 42)
mw.cen <- c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46)
south.cen <- c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48)
west.cen <- c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53)

stcodes <- stcodes %>%
  mutate(ifips=as.integer(stfips),
         cenrgn=case_when(ifips %in% ne.cen ~ "ne",
                          ifips %in% mw.cen ~ "mw",
                          ifips %in% south.cen ~ "south",
                          ifips %in% west.cen ~ "west",
                          ifips == 0 ~ "US"),
         cenrgn.name=case_when(cenrgn=="ne" ~ "Northeast",
                               cenrgn=="mw" ~ "Midwest",
                               cenrgn=="south" ~ "South",
                               cenrgn=="west" ~ "West",
                               cenrgn=="US" ~ "United States")) %>%
  select(-ifips)


# check and save ----
stcodes
count(stcodes, beargn, beargn.name)
count(stcodes, cenrgn, cenrgn.name)
count(stcodes, cenrgn.name, stname)
count(stcodes, beargn.name, stname)

usethis::use_data(stcodes, overwrite=TRUE)


# notes to self ----
# ne.cen <- c(9, 23, 25, 33, 44, 50, 34, 36, 42)
# mw.cen <- c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46)
# south.cen <- c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48)
# west.cen <- c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53)
# 
# stcodes2 <- stcodes %>%
#   mutate(ifips=as.integer(stfips),
#          cenrgn=case_when(ifips %in% ne.cen ~ "ne.cen",
#                           ifips %in% mw.cen ~ "mw.cen",
#                           ifips %in% south.cen ~ "south.cen",
#                           ifips %in% west.cen ~ "west.cen",
#                           ifips == 0 ~ "US"),
#          cenrgn.name=case_when(cenrgn=="ne.cen" ~ "Northeast",
#                                cenrgn=="mw.cen" ~ "Midwest",
#                                cenrgn=="south.cen" ~ "South",

# https://www2.census.gov/geo/docs/maps-data/maps/reg_div.txt
# census regions
# Census Bureau Regions and Divisions with State FIPS Codes
#
# REGION I: NORTHEAST
#
# Division I: New England
# Connecticut     (09)
# Maine           (23)
# Massachusetts   (25)
# New Hampshire   (33)
# Rhode Island    (44)
# Vermont         (50)
#
# Division 2: Middle Atlantic
# New Jersey      (34)
# New York        (36)
# Pennsylvania    (42)
#
# ne.cen <- c(9, 23, 25, 33, 44, 50, 34, 36, 42)
#
# REGION 2: MIDWEST*
#  
#   Division 3:  East North Central
# Illinois        (17)
# Indiana         (18)
# Michigan        (26)
# Ohio            (39)
# Wisconsin       (55)
#
#
# Division 4:  West North Central
# Iowa            (19)
# Kansas          (20)
# Minnesota       (27)
# Missouri        (29)
# Nebraska        (31)
# North Dakota    (38)
# South Dakota    (46)
# c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46)
#
# REGION 3: SOUTH
#
# Division 5: South Atlantic
# Delaware        (10)
# District of Columbia (11)
# Florida         (12)
# Georgia         (13)
# Maryland        (24)
# North Carolina  (37)
# South Carolina  (45)
# Virginia        (51)
# West Virginia   (54)
#
# Division 6: East South Central
# Alabama         (01)
# Kentucky        (21)
# Mississippi     (28)
# Tennessee       (47)
#
# Division 7:  West South Central
# Arkansas        (05)
# Louisiana       (22)
# Oklahoma        (40)
# Texas           (48)
#
# south.cen <- c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48)
#
# REGION 4: WEST
#
# Division 8:  Mountain
# Arizona         (04)
# Colorado        (08)
# Idaho           (16)
# Montana         (30)
# Nevada          (32)
# New Mexico      (35)
# Utah            (49)
# Wyoming         (56)
#
#
# Division 9: Pacific
# Alaska          (02)
# California      (06)
# Hawaii          (15)
# Oregon          (41)
# Washington      (53)
#
# west.cen <- c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53)



