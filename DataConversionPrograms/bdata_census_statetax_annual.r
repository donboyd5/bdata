# Create a data file with tax revenue in $ thousands, by state, year, and type of tax.

# Steps:
#   1. Get historical database 1902-2010
#   2. Get recent ic data 2010-2016
#   3. Conform and combine files

# Notes:
# - state-by-state data begin in 1942. Before that we only have US data.
# - before 1952, the details only add to about 90% of the total - cannot calculate aggregates such as selective sales before that

#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library("scales")
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

library("btools")
library("bdata") # so we have stcodes

# library(gdata) # for trim


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
stax_d <- "D:/Data/CensusFinanceData/StateTax/"
stax_db <- paste0(stax_d, "HistoricalDB/")


#****************************************************************************************************
#                Map item codes to variable names ####
#****************************************************************************************************
# create mapping
icodes <- read_csv(
"ic, variable, vname
T00, Total Taxes (T00), tottax
T01, Property Tax (T01), proptax
T09, Total Gen Sales Tax (T09), gst
T10, Alcoholic Beverage Tax (T10), abt
T11, Amusement Tax (T11), amusetax
T12, Insurance Premium Tax (T12), inspremtax
T13, Motor Fuels Tax (T13), mft
T14, Parimutuels Tax (T14), pmt
T15, Public Utility Tax (T15), utiltax
T16, Tobacco Tax (T16), cigtax
T19, Other Select Sales Tax (T19), othrselsalestax
T20, Alcoholic Beverage Lic (T20), ablic
T21, Amusement License (T21), amuselic
T22, Corporation License (T22), corplic
T23, Hunt and Fish License (T23), huntfishlic
T24, Motor Vehicle License (T24), mvlic
T25, Motor Veh Oper License (T25), mvoplic
T27, Public Utility License (T27), utillic
T28, Occup and Bus Lic NEC (T28), occbuslic
T29, Other License Taxes (T29), othrlic
T40, Individual Income Tax (T40), iit
T41, Corp Net Income Tax (T41), cit
T50, Death and Gift Tax (T50), egt
T51, Docum and Stock Tr Tax (T51), stt
T53, Severance Tax (T53), sevtax
T99, Taxes NEC (T99), nectax"
)
icodes


#****************************************************************************************************
#                Get latest historical database ####
#****************************************************************************************************
#stfn <- "STC_Historical_DB.xls" # do NOT use this
stfn <- "STC_Historical_DB_djb.xlsx" # use this -- only difference is that I saved as xlsx

df <- read_excel(paste0(stax_db, stfn))
problems(df)
glimpse(df)
count(df, Name)
# note that there are exhibit values for AK, HI for some years

df2 <- df %>% rename(year=Year, stcen=State, stname=Name, fye=`FY Ending Date`) %>%
  mutate(stabbr=str_sub(stname, 1, 2),
         stabbr.check=stcodes$stabbr[match(stcen, stcodes$stcen)])
glimpse(df2)

# reading the xls file causes some problems -- it misreads the various exhibit records
# such as stname:  1955 HI STATE GOVT (exhibit) -- see check below
# however, if I save as xlsx then it reads properly

check <- df2 %>% 
  filter(stabbr != stabbr.check) %>%
  select(year, stcen, stname, stabbr, stabbr.check, fye)
check

icodes.vars <- tibble(ic=names(df2), variable=as.character(df2[1, ])) %>%
  filter(!(is.na(variable) | variable=="NA"))
icodes.vars

df3 <- df2 %>% 
  filter(row_number() > 1) %>%
  select(-stcen, -fye, -stabbr.check, -stname) %>%
  gather(ic, value, -year, -stabbr)
glimpse(df3)
count(df3, stabbr) # includes US, does not include DC
count(df3, year)


df4 <- df3 %>%
  mutate(year=as.integer(year),
         value=ifelse(str_detect(value, "-11111"), NA, value),
         value=as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  mutate(variable=icodes.vars$variable[match(ic, icodes.vars$ic)]) %>%
  mutate(ic=ifelse(ic=="C105", "T00", ic))
glimpse(df4)
count(df4, ic, variable)
count(df4, stabbr)
# note that I have created T00 - it is not in the data

# add item codes from the mapping above
df5 <- df4 %>% 
  # use the ifelse so that the item codes that start with "C" -- which are some sort of Census consolidated
  # totals -- have variable even though I don't give them vname
  mutate(variable=ifelse(ic %in% icodes$ic,
                         icodes$variable[match(ic, icodes$ic)], variable),
         vname=icodes$vname[match(ic, icodes$ic)]) %>%
  select(stabbr, year, ic, vname, variable, value)
glimpse(df5)
count(df5, ic, vname, variable)
count(df5, stabbr) # good, DC not in the data

# check to see if US totals are correct for 2013+ -- in the past they have erroneously included DC
df5 %>% 
  filter(vname=="tottax") %>%
  mutate(rectype=case_when(.$stabbr %in% state.abb ~ "state",
                           .$stabbr=="DC" ~ "DC",
                           .$stabbr=="US" ~ "US"
                           )) %>%
  group_by(year, rectype) %>%
  summarise(tot=sum(value, na.rm=TRUE) / 1e3) %>%
  spread(rectype, tot) %>%
  mutate(stdc=state + DC,
         diff=US - stdc) %>%
  filter(year>=1970)

# plausible??
df5 %>% filter(stabbr=="US", vname=="tottax") %>%
  ggplot(aes(year, value)) + geom_line()

df.hist <- df5
glimpse(df.hist)

# sgtax.a <- df5
# devtools::use_data(sgtax.a, overwrite=TRUE)

#****************************************************************************************************
#                Get recent data ####
#****************************************************************************************************
# http://www2.census.gov/govs/statetax/14staxcd.txt
# http://www2.census.gov/govs/statetax/15staxcd.txt

urlbase <- "http://www2.census.gov/govs/statetax/"

# TRY to download the latest data from Census but their urls are messed up
dlyear <- function(year){
  fn <- paste0(str_sub(year, 3, 4), "staxcd.txt")
  url <- paste0(urlbase, fn)
  print(url)
  download.file(url, paste0(stax_d, fn), mode="wb")
  return()
}
dlyear(2014)
dlyear(2015)
dlyear(2016) # does not exist

# Get 2016, which of course is in a different format
fn <- "2016_STC_Detailed.xls"
url <- paste0(urlbase, fn)
url
download.file(url, paste0(stax_d, fn), mode="wb")


#****************************************************************************************************
#                Parse recent data ####
#****************************************************************************************************
# Cautions: 
# 1. CHECK whether (a) DC is in the data, and (b) regardless of whether DC is in the data, whether
#    US totals include DC values. Census has made both mistakes.
# 2. convert xls files to xlsx outside of R. It read_excel is not reading the Census xls files correctly.


# ..first, the old-style data ####
year <- 2014

getyear <- function(year) {
  fn <- paste0(str_sub(year, 3, 4), "staxcd.txt")
  df <- read_csv(paste0(stax_d, fn))
  df2 <- df %>% rename(ic=X1) %>%
    gather(stabbr, value, -ic) %>%
    mutate(year=as.integer(year),
           value=as.numeric(value)) %>%
    filter(!is.na(value))
  
  # put total tax on the file, after checking
  if((df2 %>% filter(ic=="T00") %>% nrow)>0) stop("FILE HAS TOTAL TAX IN IT!!")
  
  # add totals to the file
  tot <- df2 %>% 
    group_by(stabbr, year) %>%
    summarise(value=sum(value, na.rm=TRUE)) %>%
    mutate(ic="T00")
  
  df3 <- bind_rows(df2, tot)
  return(df3)
}

df2014 <- getyear(2014)
df2015 <- getyear(2015)

# ..now the newer data ####
# fn <- "2016_STC_Detailed.xls"
fn <- "2016_STC_Detailed_djb.xlsx" # AGAIN I had to save the xls file as xlsx!!
df <- read_excel(paste0(stax_d, fn))
glimpse(df)
df2 <- df %>% 
  rename(stname=State_Name, stcen=state_code, variable=ITEM_NAME, ic=item, value=AMOUNT) %>%
  mutate(stcen=str_pad(stcen, 2, side="left", pad="0"),
         stabbr=stcodes$stabbr[match(stcen, stcodes$stcen)],
         value2=cton(value))

# check the results before going further
count(df2, stabbr, stname, stcen) # note that DC is in the data, so must check if it is in US total
check <- df2 %>%
  filter(is.na(value2)) %>%
  select(stabbr, variable, ic, value, value2)
glimpse(check)
count(check, value) # good, all are "X"

# is DC in the US total??
df2 %>% 
  mutate(rectype=case_when(.$stabbr %in% state.abb ~ "state",
                           .$stabbr=="DC" ~ "DC",
                           .$stabbr=="US" ~ "US"
  )) %>%
  group_by(variable, rectype) %>%
  summarise(tot=sum(value2, na.rm=TRUE) / 1e3) %>%
  spread(rectype, tot) %>%
  mutate(stdc=state + DC,
         diff=US - stdc) %>%
  kable(digits=2) # BAD BAD BAD -- US total is states + DC
count(df2, ic, variable)


glimpse(df2)
df2016 <- df2 %>% select(stabbr, ic, variable, value=value2)  %>%
  mutate(year=as.integer(2016))

# ..combine the new data
df.new <- bind_rows(df2014, df2015, df2016)
glimpse(df.new)
count(df.new, year)
count(df.new, stabbr) # includes US and DC
count(df.new, ic)
count(df.new, ic, variable)


#****************************************************************************************************
#                3. Combine latest data and historical database, both created below ####
#****************************************************************************************************
# dfhist <- readRDS(paste0(stax_d, "sgtaxes.histdb.rds"))
# dfnew <- readRDS(paste0(stax_d, "sgtaxes.latest.rds"))
# drop subtotals (other than grand total), drop DC, drop US, and recalculate US
# drop vname, variable, and add vname and variable to the file

glimpse(df.hist)
glimpse(df.new)

count(df.hist, us=stabbr=="US", year) %>% spread(us, n) %>% data.frame
df.hist %>% filter(year==1940) # 1902 1913 1922 1927 1932 1934 1936 1938 1940 are the US-only years

us.hist <- df.hist %>% 
  filter(year<=1940, stabbr=="US", str_sub(ic, 1, 1)=="T") %>%
  select(ic, stabbr, year, value)
us.hist

# prepare each file for combination
dfhist2 <- df.hist %>%
  filter(stabbr %in% state.abb, # bring back the US totals later
         str_sub(ic, 1, 1)=="T") %>%
  select(stabbr, year, ic, value)
dfhist2 %>% filter(is.na(value)) # good

dfnew2 <- df.new %>%
  filter(stabbr %in% state.abb,
         str_sub(ic, 2, 2)!="A") %>% 
  select(stabbr, year, ic, value)
dfnew2 %>% filter(is.na(value)) # good


# put history and recent together
lastoldyear <- 2013
dfall <- bind_rows(dfhist2 %>% filter(year <= lastoldyear),
                   dfnew2 %>% filter(year > lastoldyear))
# make sure neither US nor DC is in the data, then add calculated US
glimpse(dfall)
count(dfall, stabbr)

dfall2 <- dfall %>%
  group_by(year, ic) %>%
  summarise(value=sum(value, na.rm=TRUE), stabbr="US") %>%
  bind_rows(dfall, us.hist)

# check
dfall2 %>% group_by(year, usrec=stabbr=="US") %>%
  summarise(n=n()) %>%
  spread(usrec, n) %>%
  ht(20)

# good - add vname and variable, then end
dfall3 <- dfall2 %>%
  mutate(variable=icodes$variable[match(ic, icodes$ic)],
         vname=icodes$vname[match(ic, icodes$ic)]) %>%
  select(stabbr, year, ic, vname, value, variable) %>%
  ungroup
glimpse(dfall3)
count(dfall3, ic, vname, variable)
count(dfall3, ic, variable, vname)

dfall3 %>%
  filter(stabbr=="US", ic=="T00") %>%
  ggplot(aes(year, value)) + geom_line()

# create final file for the package
sgtax.a <- dfall3
count(sgtax.a, ic, vname, variable)

usethis::use_data(sgtax.a, overwrite=TRUE)

