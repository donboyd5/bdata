# Create a data file with tax revenue in $ thousands, by state, year, and type of tax.

# Steps:
#   1. Get historical database 1902-2010
#   2. Get recent ic data 2010-2015
#   3. Conform and combine files

# This program is set up on the assumption that 1 and 2 have been done, so it starts with 3. If 1 or 2 have been
# updated, go further below and rerun those steps, then run 3.

# Notes:
# - state-by-state data begin in 1942. Before that we only have US data.
# - before 1952, the details only add to about 90% of the total - cannot calculate aggregates such as selective sales before that
# - for now (4/30/2015) I use the reported total aggregate for history but do not bother with reported other aggregates

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

library("grDevices")
library("knitr")

library("btools")
library("bdata") # so we have stcodes

# library(gdata) # for trim


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
# D:\Data\bdata_package_sourcedata\census_sgtax_annual\

# stax_d <- paste0("./data-raw/census_sgtax_annual/")
stax_d <- paste0("D:/Data/bdata_package_sourcedata/census_sgtax_annual/")



#****************************************************************************************************
#                3. Combine latest data and historical database, both created below ####
#****************************************************************************************************
# create mapping
# note that I have created T00 - it is not in the data
icodes <- read_csv("ic, variable, vname
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
names(icodes)


dfhist <- readRDS(paste0(stax_d, "sgtaxes.histdb.rds"))
dfnew <- readRDS(paste0(stax_d, "sgtaxes.latest.rds"))
glimpse(dfhist)
glimpse(dfnew)

count(dfhist, us=stabbr=="US", year) %>% spread(us, n) %>% data.frame
dfhist %>% filter(year==1940) # 1902 1913 1922 1927 1932 1934 1936 1938 1940 are the US-only years

count(dfnew, us=stabbr=="US", year) %>% spread(us, n) %>% data.frame # new data does not have US summary

# prepare each file for combination
dfhist2 <- dfhist %>% mutate(ic=ifelse(variable=="Total Taxes", "T00", ic)) %>%
  filter(!is.na(ic), year<min(dfnew$year)) %>% # only keep details, not aggregates (other than T00)
  select(-variable) %>%
  mutate(value=ifelse((value > -11111*1.01) & (value < -11111*.99), NA, value)) %>% # convert the Census NA coding
  filter(!is.na(value))

# create US summary for the new data
dfnewus <- dfnew %>% group_by(year, ic) %>% summarise(value=sum(value, na.rm=TRUE)) %>% mutate(stabbr="US")
dfnew2 <- bind_rows(dfnew, dfnewus)

# create total tax T00 and add to the new data
newtot <- dfnew2 %>% group_by(year, stabbr) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  mutate(ic="T00")
dfnew3 <- bind_rows(dfnew2, newtot)

# put history and recent together
dfall <- bind_rows(dfhist2, dfnew3) %>%
  mutate(variable=icodes$variable[match(ic, icodes$ic)],
         vname=icodes$vname[match(ic, icodes$ic)])
glimpse(dfall)

count(dfall, stabbr, year) %>% filter(stabbr=="US") %>% data.frame
count(dfall, ic, variable, vname)

qplot(year, value, data=filter(dfall, stabbr=="US", ic=="T00"), geom=c("point", "line")) # make sure units from 2 files are the same

# now we need selected totals - CANNOT CALCULATE THESE BEFORE 1952!!!!
recipes <- read_csv("vname, ic
selsalestax, T10
selsalestax, T11
selsalestax, T12
selsalestax, T13
selsalestax, T14
selsalestax, T15
selsalestax, T16
selsalestax, T19
lictax, T20
lictax, T21
lictax, T22
lictax, T23
lictax, T24
lictax, T25
lictax, T27
lictax, T28
")
recipes
varsums <- recipes %>% left_join(select(filter(dfall, year>=1952), -vname)) %>%
  group_by(stabbr, year, vname) %>%
  summarise(value=sum(value, na.rm=TRUE))

# create final file for the package
sgtax.a <- bind_rows(dfall, varsums) %>% arrange(stabbr, vname, year)
count(sgtax.a, ic, vname, variable)

devtools::use_data(sgtax.a, overwrite=TRUE)


#****************************************************************************************************
#                2. get recent data ####
#****************************************************************************************************
# http://www2.census.gov/govs/statetax/14staxcd.txt
# http://www2.census.gov/govs/statetax/15staxcd.txt

getyear <- function(year) {
  yy <- str_sub(as.character(year), 3, 4)
  url <- paste0("http://www2.census.gov/govs/statetax/", yy, "staxcd.txt")
  df <- read_csv(url)
  names(df)[1] <- "ic"
  names(df) <- str_sub(names(df), 1, 2)
  okcols <- c("ic", as.character(stcodes$stabbr))
  df2 <- df[, which(names(df) %in% okcols)]
  dfl <- df2 %>% gather(stabbr, value, -ic) %>%
    mutate(value=as.numeric(value))
  return(dfl)
}


dfl <- data_frame(year=2010:2015) %>%
  group_by(year) %>%
  do(getyear(.$year))

ht(dfl)

saveRDS(dfl, paste0(stax_d, "sgtaxes.latest.rds"))

# readRDS(paste0(stax_d, "sgtaxes.latest.rds"))


#****************************************************************************************************
#                1. get data from historical database ####
#****************************************************************************************************
df <- read_excel(paste0(stax_d, "STC_Historical_DB.xls")) # state tax data for 1902:2010
glimpse(df)
count(df, State, Name)

# create long file
dfl <- df %>% mutate(stabbr=bdata::stcodes$stabbr[match(State, bdata::stcodes$stcen)]) %>% # trust Census code rather than name for state abbreviation
  select(year=Year, everything(), -Name, -State, -`FY Ending Date`) %>%
  gather(variable, value, -year, -stabbr)


# Extract ic from variable
pattern <- "\\([^)]*\\)" # regex pattern expression for finding text between ( and ) - WoW!
m <- regexpr(pattern, dfl$variable)  # find which values have "()" and where they start
# m is a vector with one element per row: -1, or else the col in which the parentheses start
table(m)
dd <- regmatches(dfl$variable, m) # retrieves the "()" including text within, for just the relevant rows
table(dd)
dd <- gsub("[()]","", dd) # remove ()
dfl$ic[which(m!=-1)] <- dd # put data back into this var
count(dfl, ic, variable)

dfl <- dfl %>% select(year, stabbr, variable, ic, value)
c1 <- "Census annual state government tax collections by state and tax type from the Bureau's historical database (STC_Historical_DB.xls) as downloaded on 6/26/2014 and dated 3/18/2014."
c2 <- "Database generally covers fiscal years 1942, 1944, 1946, 1948, and 1950 (limited detail) and 1951 to 2012. See database documentation for details."
comment(dfl) <- c(c1, c2)
saveRDS(dfl, paste0(stax_d, "sgtaxes.histdb.rds"))




# create variable names

