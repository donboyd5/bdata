# Get CPS, ACS, and SAIPE state-level poverty rates
# Also get CPS US poverty rates
# Combine in a single file

# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# http://www.census.gov/hhes/www/poverty/histpov/footnotes.html


#****************************************************************************************************
#                load packages ####
#****************************************************************************************************

library("devtools")
library("ggplot2")
library("scales") # so we can use scales with ggplot2
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("reshape2")
library("magrittr")
library("tidyr")
library("dplyr") # always load AFTER plyr
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library("knitr")
library("lubridate")
library("stringr")
library("grDevices")
library("readr")
library("readxl")

library("btools") # library that I created (install from github)
library("bdata")
library("pdata")
library("fof")
library("apitools")


# data(package="bdata")
# glimpse(spovrates)
# glimpse(saipe)
# count(saipe, variable)



#****************************************************************************************************
#                define globals, directories, and files ####
#****************************************************************************************************
censusapikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"

# Table 21. Number of Poor and Poverty Rate, by State [XLS – 125k]
# http://www.census.gov/hhes/www/poverty/data/historical/hstpov21.xls

# Small Area Income and Poverty Estimates (saipe)
# http://www.census.gov/did/www/saipe/data/statecounty/data/index.html

adir <- "D:/Data/bdata_package_sourcedata/census_poverty/acs/"

povd2 <- "D:/Data/bdata_package_sourcedata/census_poverty/"
saiped <- paste0(povd2, "saipe/")


#****************************************************************************************************
#                Get CPS poverty rates from the webfile and save ####
#****************************************************************************************************
# df <- read_excel("http://www.census.gov/hhes/www/poverty/data/historical/hstpov21.xls")
fn <- "hstpov21.xls"
url <- paste0("http://www.census.gov/hhes/www/poverty/data/historical/", fn)
dlfile <- paste0("./data_intermediate/", fn)

# RUN NEXT LINE ONLY WHEN DATA ARE UPDATED
# download.file(url, dlfile, mode="wb")
# END DOWNLOAD

df <- read_excel(dlfile)

df2 <- df
vnames <- c("stname", "totpop", "povpop", "pov.se", "povrate", "povrate.se")
names(df2) <- vnames
count(df2, stname)
# library(zoo)
# make sure we extract the info for which 2013 ests we have
df3 <- df2 %>% mutate(yeartmp=cton(str_sub(stname, 1, 4)),
                      year=as.integer(zoo::na.locf(yeartmp, na.rm=FALSE)),
                      stabbr=as.character(stcodes$stabbr[match(stname, stcodes$stname)]),
                      stabbr=ifelse(stname %in% c("DC", "D.C."), "DC", stabbr)) %>%
  mutate(note2013=ifelse(str_detect(stname, "2013 19"), "note19", NA),
         note2013=ifelse(str_detect(stname, "2013 18"), "note18", note2013),
         note2013=ifelse(year==2013, zoo::na.locf(note2013, na.rm=FALSE), NA)) %>%
  filter(!is.na(stabbr)) %>%
  select(-stname, -yeartmp) %>%
  gather(variable, value, -year, -stabbr, -note2013) %>%
  mutate(value=cton(value))

# # I think we prob should use the note18 table for 2013
saveRDS(df3, "./data_intermediate/cps_poverty.rds")


#****************************************************************************************************
#                Get ACS state povrates from downloaded American Fact Finder files ####
#****************************************************************************************************
# ACS_05_EST_S1701_with_ann.csv
# ACS_07_1YR_S1701_with_ann.csv
adir <- "D:/Data/bdata_package_sourcedata/census_poverty/acs/"
filesa <- paste0("ACS_", str_pad(5:6, 2, "left", pad="0"), "_EST_S1701_with_ann.csv")
filesb <- paste0("ACS_", str_pad(7:14, 2, "left", pad="0"), "_1YR_S1701_with_ann.csv")
files <- c(filesa, filesb)

# povrate is HC03_EST_VC01

f <- function(file) {
  year <- 2000 + as.integer(str_sub(file, 5, 6))
  print(year)
  df <- read_csv(paste0(adir, file)) %>% mutate(year=as.integer(year))
  return(df)
}

df <- f(files[10])
df <- ldply(files, f, .progress="text")
glimpse(df)
names(df)
count(df, year)
count(df, year, is.na(HC03_EST_VC01))
count(df, GEO.id, GEO.id2, `GEO.display-label`)

dfl <- df
names(dfl)[1:3] <- c("geoid", "stfips", "stname")
dfl <- dfl %>% mutate(stabbr=stcodes$stabbr[match(stfips, stcodes$stfips)])
count(dfl, geoid, stfips, stabbr, stname)
dfl2 <- dfl %>% select(-geoid, -stfips, -stname) %>%
  gather(variable, value, -stabbr, -year) %>%
  mutate(value=cton(value)) %>%
  filter(!is.na(value))
glimpse(dfl2)

acspov <- dfl2 %>% filter(variable=="HC03_EST_VC01") %>%
  mutate(stabbr=as.character(stabbr)) %>%
  select(year, stabbr, povrate=value)
saveRDS(acspov, "./data_intermediate/acs_povrate.rds")

#****************************************************************************************************
#                Get SAIPE state povrates from API ####
#****************************************************************************************************
povurl <- "http://api.census.gov/data/timeseries/poverty/saipe?"
vars <- "get=STABREV,NAME,SAEPOVRTALL_PT"
geo <- "&for=state:*"
years <- "&time=from+1995+to+2014"
key <- "&key="
request <- paste0(povurl, vars, geo, years, key, censusapikey)

result <- jsonlite::fromJSON(request) %>% as.data.frame
head(result)
names(result) <- t(result[1, ])
glimpse(result)

df <- result[-1, ]
vnames <- c("stabbr", "stname", "povrate", "year", "stfips")
names(df) <- vnames
df <- df %>% select(-stfips) %>% mutate(year=as.integer(as.character(year)), povrate=as.numeric(as.character(povrate)))
glimpse(df)
count(df, stabbr, stname)
count(df, year)
df2 <- df %>% mutate(stabbr=as.character(stabbr)) %>% select(-stname)
glimpse(df2)
saveRDS(df2, "./data_intermediate/saipe_povrate.rds")


#****************************************************************************************************
#                Create a single povrate file with data from all soruces as well as "best" ####
#****************************************************************************************************
pcps <- readRDS("./data_intermediate/cps_poverty.rds") %>%
  filter(variable=="povrate", !(year==2013 & note2013=="note19")) %>%
  select(stabbr, year, povrate=value) %>%
  mutate(type="cps")

pacs <- readRDS("./data_intermediate/acs_povrate.rds") %>% mutate(type="acs")

psaipe <- readRDS("./data_intermediate/saipe_povrate.rds") %>% mutate(type="saipe")

povrates1 <- bind_rows(pcps, pacs, psaipe)
glimpse(povrates1)
count(povrates1, stabbr)
count(povrates1, year)
count(povrates1, type)

qplot(year, povrate, data=filter(povrates1, stabbr=="NY"), colour=type, geom=c("point", "line"))

# best: 2005+ acs; 1995-2004 saipe, don't give prior to 1995!
t3 <- expression(year>=2005 & type=="acs")
t2 <- expression(year %in% 1995:2004 & type=="saipe")
# t1 <- expression(year %in% 1995:2004 & type=="cps")
pbest <- povrates1 %>% filter(with(., eval(t3) | eval(t2)))
count(pbest, year, type)

spovrates <- bind_rows(povrates1, mutate(pbest, type="best"))
glimpse(spovrates)
comment(spovrates) <- "type=='best' uses ACS for 2005+ and SAIPE for 1995-2004"
comment(spovrates)
use_data(spovrates, overwrite = TRUE)


#****************************************************************************************************
#                OLD STUFF BELOW HERE ####
#****************************************************************************************************

# # get the subset, calculate US povrate, and combine
# 
# df4 <- df3 %>% filter(!(year==2013 & note2013=="note19"))
# 
# uspov <- df4 %>% filter(variable %in% c("totpop", "povpop")) %>%
#   group_by(year, variable) %>%
#   summarise(value=sum(value)) %>%
#   spread(variable, value) %>%
#   mutate(povrate=povpop / totpop * 100,
#          stabbr="US")
# 
# df4a <- filter(df4, variable=="povrate") %>% 
#   select(stabbr, year, povrate=value)
# 
# povall <- bind_rows(df4a, select(uspov, stabbr, year, povrate)) %>% arrange(stabbr, year)
# 
# qplot(year, povrate, data=filter(povall, year>=2000), geom=c("point", "line")) + facet_wrap(~stabbr, ncol=6, scales="free_y")


#****************************************************************************************************
#                download saipe data ####
#****************************************************************************************************
# get the saipe state data for 2003-plus, which are all in the same format
# they have poverty nums, pov rate, breakdown by ages, and median hh income

dlyear <- function(year) {
  # example:
  # http://www.census.gov/did/www/saipe/downloads/estmod14/est14US.xls
  pre <- "http://www.census.gov/did/www/saipe/downloads/estmod"
  y2 <- str_sub(year, 3, 4)
  fn <- paste0("est", y2, "US.xls")
  fget <- paste0(pre, y2, "/", fn)
  fput <- paste0(saiped, fn)
  download.file(fget, fput, mode="wb")
  return(NULL)
}

l_ply(2003:2014, dlyear)
dlyear(2014)

#****************************************************************************************************
#                read and save saipe data ####
#****************************************************************************************************
year <- 2003
y2 <- str_sub(year, 3, 4)
fn <- paste0("est", y2, "US.xls")
fdat <- paste0(saiped, fn)
df <- read_excel(fdat)
vnames <- c("stfips", "stabbr", "stname", 
            "povnum_all", "povnum_lb90_all", "povnum_ub90_all", "povrate_all", "povrate_lb90_all", "povrate_ub90_all",
            "povnum_age017", "povnum_lb90_age017", "povnum_ub90_age017", "povrate_age017", "povrate_lb90_age017", "povrate_ub90_age017",
            "povnum_age517", "povnum_lb90_age517", "povnum_ub90_age517", "povrate_age517", "povrate_lb90_age517", "povrate_ub90_age517",
            "mhhi_all", "mhhi_lb90_all", "mhhi_ub90_all",
            "povnum_age04", "povnum_lb90_age04", "povnum_ub90_age04", "povrate_age04", "povrate_lb90_age04", "povrate_ub90_age04")
ncol(df); length(vnames)
names(df) <- vnames
dfl <- df %>% select(-stfips, -stname) %>%
  gather(variable, value, -stabbr) %>%
  mutate(year=year,
         value=cton(value)) %>%
  filter(!is.na(value)) %>%
  select(stabbr, year, variable, value)

f <- function(year) {
  y2 <- str_sub(year, 3, 4)
  fn <- paste0("est", y2, "US.xls")
  fdat <- paste0(saiped, fn)
  df <- read_excel(fdat)
  vnames <- c("stfips", "stabbr", "stname", 
              "povnum_all", "povnum_lb90_all", "povnum_ub90_all", "povrate_all", "povrate_lb90_all", "povrate_ub90_all",
              "povnum_age017", "povnum_lb90_age017", "povnum_ub90_age017", "povrate_age017", "povrate_lb90_age017", "povrate_ub90_age017",
              "povnum_age517", "povnum_lb90_age517", "povnum_ub90_age517", "povrate_age517", "povrate_lb90_age517", "povrate_ub90_age517",
              "mhhi_all", "mhhi_lb90_all", "mhhi_ub90_all",
              "povnum_age04", "povnum_lb90_age04", "povnum_ub90_age04", "povrate_age04", "povrate_lb90_age04", "povrate_ub90_age04")
  ncol(df); length(vnames)
  names(df) <- vnames
  dfl <- df %>% select(-stfips, -stname) %>%
    gather(variable, value, -stabbr) %>%
    mutate(year=as.integer(year),
           value=cton(value)) %>%
    filter(!is.na(value)) %>%
    select(stabbr, year, variable, value)
  return(dfl)
}

df <- ldply(2003:2014, f, .progress="text")
warnings()
count(df, year)
count(df, stabbr)
count(df, variable)

df %>% filter(stabbr=="LA", variable=="povrate_all") %>% qplot(year, value, data=., geom=c("point", "line"))

vars <- paste0("povrate_", c("", "lb90_", "ub90_"), "all")
vars <- paste0("povrate_", c("", "lb90_", "ub90_"), "age017")
st <- "TX"
df %>% filter(stabbr==st, variable %in% vars) %>% 
  qplot(year, value, data=., colour=variable, geom=c("point", "line")) 

sts <- c("CA", "AZ", "NM", "NY", "US")
df %>% filter(stabbr %in% sts, variable=="povrate_age517") %>% 
  qplot(year, value, data=., colour=stabbr, geom=c("point", "line")) 

saipe <- df
comment(saipe) <- "State poverty and median household income from SAIPE program, annual"
use_data(saipe, overwrite = TRUE)



#****************************************************************************************************
#                read and save Lucy's data from Census hstpov21.xls ####
#****************************************************************************************************
df <- read_excel(paste0(povd, povfn), sheet="PovertyRate")
glimpse(df)

df2 <- df
vnames <- c("stname", "stabbr", "xstatus", "gamstatus", "region", 2000:2014)
names(df2)[1:length(vnames)] <- vnames
df2 <- df2[, vnames]
glimpse(df2)

dfl <- df2 %>% select(-xstatus, -gamstatus, -region, -stname) %>%
  gather(year, povrate, -stabbr, convert=TRUE) %>%
  mutate(povrate=cton(povrate)) %>%
  filter(!is.na(povrate), !is.na(stabbr))
glimpse(dfl)
filter(dfl, is.na(stabbr))


qplot(year, povrate, data=filter(dfl, stabbr %in% c("US", "NY")), colour=stabbr, geom=c("point", "line"))

qplot(year, povrate, data=dfl, geom=c("point", "line")) + facet_wrap(~stabbr, ncol=6, scales="free_y")

spovrates <- dfl
comment(spovrates) <- "State poverty rates, annual"
use_data(spovrates, overwrite = TRUE)


#****************************************************************************************************
#                alternatively get from the web data ####
#****************************************************************************************************
# # df <- read_excel("http://www.census.gov/hhes/www/poverty/data/historical/hstpov21.xls")
# download.file("http://www.census.gov/hhes/www/poverty/data/historical/hstpov21.xls", paste0("./data-raw/hstpov21.xls"), mode="wb")
# 
# df <- read_excel(paste0("./data-raw/hstpov21.xls"))
# df2 <- df
# vnames <- c("stname", "totpop", "povpop", "pov.se", "povrate", "povrate.se")
# names(df2) <- vnames
# count(df2, stname)
# library(zoo)
# df3 <- df2 %>% mutate(year=cton(str_sub(stname, 1, 4)),
#                       year2=na.locf(year, na.rm=FALSE))
# start2013 <- which(df3$year==2013)
# df3 <- df3 %>% mutate(note2013=ifelse(year2==2013 & (row_number()<start2013[2]), "note19", NA),
#                       note2013=ifelse(year2==2013 & (row_number()>=start2013[2]), "note18", note2013),
#                       stabbr=as.character(stcodes$stabbr[match(stname, stcodes$stname)]),
#                       stabbr=ifelse(stname %in% c("DC", "D.C."), "DC", stabbr)) %>%
#   select(-year, -stname) %>%
#   select(year=year2, stabbr, note2013, everything()) %>%
#   gather(variable, value, -year, -stabbr, -note2013) %>%
#   mutate(value=cton(value)) %>%
#   filter(!is.na(value), !is.na(stabbr))
# count(df3, year)
# 
# # I think we prob should use the note18 table for 2013
# 
# # get the subset, calculate US povrate, and combine
# 
# df4 <- df3 %>% filter(!(year==2013 & note2013=="note19"))
# 
# uspov <- df4 %>% filter(variable %in% c("totpop", "povpop")) %>%
#   group_by(year, variable) %>%
#   summarise(value=sum(value)) %>%
#   spread(variable, value) %>%
#   mutate(povrate=povpop / totpop * 100,
#          stabbr="US")
# 
# df4a <- filter(df4, variable=="povrate") %>% 
#   select(stabbr, year, povrate=value)
# 
# povall <- bind_rows(df4a, select(uspov, stabbr, year, povrate)) %>% arrange(stabbr, year)
# 
# qplot(year, povrate, data=filter(povall, year>=2000), geom=c("point", "line")) + facet_wrap(~stabbr, ncol=6, scales="free_y")

