# bdata_stateSOIdata.r
# Don Boyd
# 12/8/2016

# GOAL:  create a single time-series cross-section SOI data file that has the following columns
#   year int tax year
#   stabbr chr state abbreviation
#   incgrp chr income group - may NOT be uniform from year t year
#   vname chr variable name - MY variable name,
#   value dbl
# in the process, create an rds file for each year, maybe

# Keep all original-source xls, xlsx, csv, and .sas7bdat files in the SOI data directory on hard drive
# keep derived rds files in the data-raw subdirectory of this project directory

# TODO:
# - investigate whether older data from Leslie McGranahan can be brought in


# Approach:
# from 2012 (forward?) SOI has created computer-friendly csv files
# 2011 prior requires pulling from xlsx and xls files
# get 2004-2011 and "freeze" in an RDS file
# pre 2004 - pull from old SAS files
# then tack on 2012 and later

# This is good for 2004-2009. 2003 and earlier are different, and different from each other, and will require more work
# there is preliminary commented-out code way below to read some of the pre-2004 years, but it is not ready

# CAUTION: There are two 2007 files: the one that was normally created, plus an adjusted file that SOI/IRS put together
# that attempts to net out impact of returns filed simply for ARRA tax credit.
# I have called the latter file soi2007adj.Rdata

# current SOI main page:
# https://www.irs.gov/uac/SOI-Tax-Stats-Historic-Table-2

# static download locations 1997-2009 as of 9/25/2011:
# http://www.irs.gov/pub/irs-soi/09in54cm.xls
# http://www.irs.gov/pub/irs-soi/07in54cm.xls
# http://www.irs.gov/pub/irs-soi/97in54cm.xls

# also see
# http://www.irs.gov/taxstats/article/0,,id=171535,00.html

# Steps:
# get and save the 2012 and later data
# get and save the 2004-2011 data in reverse order
# combine
# save as a file for the bdata package

#****************************************************************************************************
#                Load packages ####
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

library("zoo") # for rollapply

library("btools") # library that I created (install from github)

# library("ggplot2")
# library("scales") # so we can use scales with ggplot2
# library("plyr") # needed for ddply; must be loaded BEFORE dplyr
# library("reshape2")
# library("magrittr")
# library("tidyr")
# library("dplyr") # always load AFTER plyr
# options(dplyr.print_min = 60) # default is 10
# options(dplyr.print_max = 60) # default is 20
# library("knitr")
# library("lubridate")
# library("stringr")
# library("grDevices")
# library("readr")
# library("readxl")
# library("gdata")
# library("haven")
# 
# library("bdata")
# library("btools")



#****************************************************************************************************
#                Directories and files ####
#****************************************************************************************************

# web data directories and files
websoidir <- "http://www.irs.gov/pub/irs-soi/"

# data directories and files
soitsd <- "D:/Data/SOI Data/Time Series/"
# soidir <- "./data-raw/soi/"
soidir <- "D:/Data/bdata_package_sourcedata/soi/"
interim <- paste0(soidir, "IntermediateRfiles/")
raw <- paste0(soidir, "OriginalSOIfiles/")

soisfx <- "in54cm.xls" # filename suffix

source("./DataConversionPrograms/bdata_StateSOIdataFunctions.r")

# NOTE: for "system", command is parsed as a command plus arguments separated by spaces. So if the path to the
# command (or an argument) contains spaces, it must be quoted e.g. by shQuote. Only double quotes are allowed on Windows


#****************************************************************************************************
#                Constants ####
#****************************************************************************************************
# stlist<-c("US","AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
#           "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","OA")
# define the income ranges used for each year
incgrpdesc2009prior <- c("all", "<$50k", "$50k-<75k", "$75k-<100k", "$100k-<200k", "$200k+")
incgrpdescs20102011 <- c("all", "<$1", "$1-<25k", "$25k-<50k", "$50k-<75k", "$75k-<100k", "$100k-<200k", "$200k-<500k", "$500k-<1m", "$1m+")
incgrpdescs20122014 <- c("all", "<$1", "$1-<10k", "$10k-<25k", "$25k-<50k", "$50k-<75k", "$75k-<100k", "$100k-<200k", "$200-<500k", "$500-<1m", "$1m+")

# tax-year-specific info
# 2014 agi stub
# 0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $10,000'
# 3 = '$10,000 under $25,000'
# 4 = '$25,000 under $50,000'
# 5 = '$50,000 under $75,000'
# 6 = '$75,000 under $100,000'
# 7 = '$100,000 under $200,000'
# 8 = ‘$200,000 under $500,000’
# 9 = ‘$500,000 under $1,000,000’
# 10 = ‘$1,000,000 or more’	
# https://www.irs.gov/pub/irs-soi/14in54cmcsv.csv



#****************************************************************************************************
#                CAUTION: RUN ONCE: Download annual SOI data ####
#****************************************************************************************************
# for(yr in 1997:2009){ # one-time only download of all data
#   y2 <- substr(as.character(yr), 3, 4)
#   fn <- paste0(y2, soisfx)
#   dlfn <- paste0(websoidir, fn)
#   download.file(dlfn, paste0(soidir, fn), mode="wb")
#   # print(dlfn)
# }

# dfl <- get_soi2011()
# saveRDS(dfl, file=paste0(soidir,"soi2011.rds"))



#****************************************************************************************************
#                Combine previously created r files ####
#****************************************************************************************************
df1 <- readRDS(file=paste0(interim, "soifromSAS_1997to2007.rds"))
df2 <- readRDS(file=paste0(interim, "soi2004to2011.rds"))
df3 <- readRDS(file=paste0(interim, "soi2012to2013.rds"))
df4 <- readRDS(file=paste0(interim, "soi2014.rds"))

head(df1)
head(df2)
count(df2, year)
count(df2, item, lineno) %>% data.frame


# create a unifying variable, vname, on each file
# this is a tool to find which line numbers correspond to which variables in which years
# tmp <- df1 %>% count(year, item, lineno) %>%
#   select(-n) %>%
#   spread(year, lineno)
# tmp %>% filter(grepl("social", item, ignore.case=TRUE))


# First, prepare the earlier data
vars2 <- read_csv("vname, year, lineno
  nret, 2003, 1
  nret, 2004, 1
  nret, 2005, 1
  nret, 2006, 1
  nret, 2007, 1
  nret, 2008, 1
  nret, 2009, 1
  nret, 2010, 1
  nret, 2011, 1

  agi, 2003, 5
  agi, 2004, 5
  agi, 2005, 5
  agi, 2006, 5
  agi, 2007, 5
  agi, 2008, 5
  agi, 2009, 6
  agi, 2010, 6
  agi, 2011, 6

  wages, 2003, 7
  wages, 2004, 7
  wages, 2005, 7
  wages, 2006, 7
  wages, 2007, 7
  wages, 2008, 7
  wages, 2009, 10
  wages, 2010, 10
  wages, 2011, 10

  txblinterest, 2003, 9
  txblinterest, 2004, 9
  txblinterest, 2005, 9
  txblinterest, 2006, 9
  txblinterest, 2007, 9
  txblinterest, 2008, 9
  txblinterest, 2009, 12
  txblinterest, 2010, 12
  txblinterest, 2011, 12

  odividends, 2003, 11
  odividends, 2004, 11
  odividends, 2005, 11
  odividends, 2006, 11
  odividends, 2007, 11
  odividends, 2008, 11
  odividends, 2009, 14
  odividends, 2010, 14
  odividends, 2011, 14

  netcgll, 2003, 17
  netcgll, 2004, 16
  netcgll, 2005, 16
  netcgll, 2006, 16
  netcgll, 2007, 16
  netcgll, 2008, 16
  netcgll, 2009, 22
  netcgll, 2010, 22
  netcgll, 2011, 22

  netcgllnum, 2004, 15
  netcgllnum, 2005, 15
  netcgllnum, 2006, 15
  netcgllnum, 2007, 15
  netcgllnum, 2008, 15
  netcgllnum, 2009, 21
  netcgllnum, 2010, 21
  netcgllnum, 2011, 21

  busprofinc, 2004, 13
  busprofinc, 2005, 13
  busprofinc, 2006, 13
  busprofinc, 2007, 13
  busprofinc, 2008, 13
  busprofinc, 2009, 20
  busprofinc, 2010, 20
  busprofinc, 2011, 20

  irapay, 2004, 18
  irapay, 2005, 18
  irapay, 2006, 18
  irapay, 2007, 18
  irapay, 2008, 18
  irapay, 2009, 24
  irapay, 2010, 24
  irapay, 2011, 24

  txblpension, 2004, 20
  txblpension, 2005, 20
  txblpension, 2006, 20
  txblpension, 2007, 20
  txblpension, 2008, 20
  txblpension, 2009, 26
  txblpension, 2010, 26
  txblpension, 2011, 26

  txblsocsec, 2004, 24
  txblsocsec, 2005, 24
  txblsocsec, 2006, 24
  txblsocsec, 2007, 24
  txblsocsec, 2008, 24
  txblsocsec, 2009, 31
  txblsocsec, 2010, 31
  txblsocsec, 2011, 31

  itemded, 2003, 30
  itemded, 2004, 28
  itemded, 2005, 28
  itemded, 2006, 28
  itemded, 2007, 28
  itemded, 2008, 28
  itemded, 2009, 51
  itemded, 2010, 51
  itemded, 2011, 51

  taxinc, 2003, 42
  taxinc, 2004, 42
  taxinc, 2005, 42
  taxinc, 2006, 42
  taxinc, 2007, 42
  taxinc, 2008, 42
  taxinc, 2009, 65
  taxinc, 2010, 65
  taxinc, 2011, 65")
vars2
varsdf2 <- vars2 %>% filter(!is.na(year), year!=2003) # 2003 not yet ready for prime time
varsdf2
count(varsdf2, vname)

# put vname on df2 and only keep those vars
df2a <- left_join(varsdf2, df2) %>% mutate(year=as.integer(year))
ht(df2a)
# fix the income groupings
count(df2a, incgrp, incgrpdesc)
count(df2a, year, incgrpdesc)
df2b <- df2a %>% rename(incgrpnum=incgrp, incgrp=incgrpdesc) %>%
  mutate(incgrp=ifelse(incgrp=="$50-<75k", "$50k-<75k", incgrp),
         incgrp=ifelse(incgrp=="$75-<100k", "$75k-<100k", incgrp),
         incgrp=ifelse(incgrp=="$100-<200k", "$100k-<200k", incgrp))
glimpse(df2b)
count(df2b, incgrp, year)

# now create the crosswalk for df2
# tool to find the variable name (varcsv) for a given concept
# head(df2)
# tmp <- count(df2, varcsv, varcsvdesc)
# tmp %>% filter(grepl("pension", varcsvdesc, ignore.case=TRUE))


# Prepare the later data
vars3 <- read_csv("vname, varcsv
  nret, N1
  agi, A00100
  wages, A00200
  txblinterest, A00300
  odividends, A00600
  netcgll, A01000
  netcgllnum, N01000
  busprofinc, A00900
  irapay, A01400
  txblpension, A01700
  txblsocsec, A02500
  itemded, A04470
  taxinc, A04800")
varsdf3 <- vars3 %>% filter(!is.na(vname))

# get ALL of the new data, and put vname on selected items
# put vname on df1 and only keep those vars
df3a <- df3 %>% mutate(vname=varsdf3$vname[match(variable, varsdf3$varcsv)],
                       incgrp=as.character(incgrp)) %>%
  filter(!is.na(vname))
ht(df3a)

ht(df4)
df4a <- df4 %>% mutate(vname=varsdf3$vname[match(variable, varsdf3$varcsv)],
                       incgrp=as.character(incgrp)) %>%
  filter(!is.na(vname))
ht(df4a)

# stack the files, examine, and save
glimpse(df2b)
glimpse(df3a)
glimpse(df4a)
keepvars <- c("year", "stabbr", "vname","incgrp", "value")
soiall <- bind_rows(df2b, df3a, df4a) %>% select(one_of(keepvars))
glimpse(soiall)
count(soiall, year)
count(soiall, vname)
count(soiall, stabbr)
count(soiall, incgrp)
count(soiall, incgrp, year) %>% spread(year, n)
qplot(year, value, data=filter(soiall, vname=="netcgll", incgrp=="all", stabbr=="US"), geom=c("point", "line"))

# saveRDS(soiall, file=paste0("./data/", "soiall.rds"))
# save the data for purposes of the package ####
devtools::use_data(soiall, overwrite=TRUE)


#****************************************************************************************************
#                Master vnames 2012+ ####
#****************************************************************************************************
# first, get descriptions for the variable names
library(XLConnect) # slow but convenient because it reads ranges
vnames <- readWorksheetFromFile(paste0(soidir, "SOIDocumentationandUSTotalsSOI(11).xlsx"),
                                sheet="csvvars_masterlist", header=TRUE, region="B3:C138")
# now clean up vnames
pattern <- "\\[[^)]*\\]" # regex pattern expression for finding text between [ and ]
vnames <- vnames %>% filter(!is.na(vname)) %>%
  mutate(vdesc=str_trim(str_replace(vdesc, pattern, "")))
vnames

# caution: change vname ending 85330 in 2013 and earlier to ending 85530


#****************************************************************************************************
#                Get 2014 data PAY ATTENTION TO UNITS ####
#****************************************************************************************************
# get 2014
df2014 <- read_csv(paste0(raw, "14in54cmcsv.csv"))
glimpse(df2014) # good, all but first are numeric
# setNames(str_to_lower(names(.))) 

incgrps <- c("all", "<$1", "$1-<10k", "$10k-<25k", "$25k-<50k", "$50k-<75k",
             "$75k-<100k", "$100k-<200k", "$200k-<500k", "$500k-<1m", "$1m+")
dfl <- df2014 %>% rename(agi_stub=AGI_STUB) %>%
  mutate(year=as.integer(2014)) %>%
  rename(stabbr=STATE) %>%
  gather(variable, value, -year, -stabbr, -agi_stub) %>%
  mutate(vdesc=vnames$vdesc[match(variable, vnames$vname)],
         incgrp=factor(agi_stub, levels=0:10, labels=incgrps)) %>%
  select(year, stabbr, agi_stub, incgrp, variable, vdesc, value) %>%
  filter(!is.na(stabbr))
saveRDS(dfl, paste0(interim, "soi2014.rds"))


#****************************************************************************************************
#                Get 2012-2013 data PAY ATTENTION TO UNITS ####
#****************************************************************************************************
# get 2013
df2013 <- read_excel(paste0(raw, "13in54cmcsv.xlsx"))
warnings()
glimpse(df2013)
# all but first should be numeric, so reread
ctypes <- c("text", rep("numeric", ncol(df2013)-1))
df2013 <- read_excel(paste0(raw, "13in54cmcsv.xlsx"), col_types = ctypes)
warnings()
glimpse(df2013)


# now 2012
df2012 <- read_csv(paste0(raw, "12in54cmcsv.csv"))
glimpse(df2012) # good, all but first are numeric

# combine, make long, add variable descriptions, save
df <- bind_rows(mutate(df2012, year=as.integer(2012)), mutate(df2013, year=as.integer(2013)))
glimpse(df)

incgrps <- c("all", "<$1", "$1-<10k", "$10k-<25k", "$25k-<50k", "$50k-<75k",
            "$75k-<100k", "$100k-<200k", "$200k-<500k", "$500k-<1m", "$1m+")
dfl <- df %>% mutate(agi_stub=as.integer(agi_stub)) %>%
  rename(stabbr=State) %>%
  gather(variable, value, -year, -stabbr, -agi_stub) %>%
  mutate(vdesc=vnames$vdesc[match(variable, vnames$vname)],
         incgrp=factor(agi_stub, levels=0:10, labels=incgrps)) %>%
  select(year, stabbr, agi_stub, incgrp, variable, vdesc, value) %>%
  filter(!is.na(stabbr))

saveRDS(dfl, paste0(interim, "soi2012to2013.rds"))



#****************************************************************************************************
#                Stack soi files through 2011 ####
#****************************************************************************************************
soi2011 <- readRDS(paste0(soidir, "soi2011.rds"))
soi2010 <- readRDS(paste0(soidir, "soi2010.rds"))
load(paste(soidir, "soi2009", ".RData", sep=""))
load(paste(soidir,"soi2008",".RData",sep=""))
# load(paste(soidir,"soi2007",".RData",sep=""))
load(paste(soidir,"soi2007adj",".RData",sep=""))  # see CAUTION note above
load(paste(soidir,"soi2006",".RData",sep=""))
load(paste(soidir,"soi2005",".RData",sep=""))
load(paste(soidir,"soi2004",".RData",sep=""))
load(paste(soidir,"soi2003",".RData",sep=""))

# load(paste0(soidir, "soi1997.RData"))
# count(soi1997, item, itemvar)

ht(soi2003)
ht(soi2004)
ht(soi2005)
ht(soi2006)
ht(soi2007)
ht(soi2008)
ht(soi2009)
ht(soi2010)
ht(soi2011)

dfnlist <- as.list(paste0("soi", 2004:2011)) # create list of data frame names - don't do 2003 yet; it does not have US; investigate
stack <- bind_rows(lapply(dfnlist, get)) # put the dfs into a list and then stack
glimpse(stack)
count(stack, year, incgrp) %>% spread(year, n)
ht(stack)
stack$incgrp <- stack$incgrp - 1 # zero-base the income group numbers
count(stack, incgrp, incgrpdesc)
saveRDS(stack, paste0(soidir, "soi2004to2011.rds"))

count(filter(stack, stabbr=="US"), year)

#****************************************************************************************************
#                Read soi by year ####
#****************************************************************************************************

# for each year, create an Rdata file with a data frame:
# $ item      : chr  "Number of returns" "Number of joint returns" "Number with paid preparer's signature" "Number of exemptions" ...
# $ lineno    : num  1 2 3 4 5 6 7 8 9 10 ...
# $ value     : num  1.41e+08 5.38e+07 8.10e+07 2.86e+08 9.84e+07 ...
# $ stabbr    : chr  "US" "US" "US" "US" ...
# $ incgrp    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ incgrpdesc: chr  "all" "all" "all" "all" ...
# $ year      : num  2009 2009 2009 2009 2009 ...


# notes:
# 2004-2009:
# 2 blocks of data: US-SC in top block, followed below by SD-WY, OtherAreas
# within each block, cols for a state are all, <$50k, <75, <100, <200, 200+ (6 cols)
# and rows are unnumbered but labeled return items -- 107 in 2009, 65 in 2008, other # rows in other years

# 2010
# xls has 3 blocks, US-MN on top, MS-WV, then WI to WY
# xlsx has US-OA in one block
# within each block (xls or xlsx), cols for a state are all, <$1, <25k, <50, <75, <100, <200, <500, <1m, 1m+ (10 cols)

# 2011
# xlsx has US-OA in one block
# within each block, cols for a state are all, <$1, <25k, <50, <75, <100, <200, <500, <1m, 1m+ (10 cols)

# 2012
# xlsx has US-OA in one block
# within each block, cols for a state are all, <$1, <10k, <25, <50, <75, <100, <200, <500, <1m, 1m+ (11 cols)

# income groupings in 2009:
# incgrp incgrpdesc    n
# 1        all
# 2      <$50k
# 3   $50-<75k
# 4  $75-<100k
# 5 $100-<200k
# 6     $200k+

# income groupings in 2010-11:
# incgrp incgrpdesc    n
#  1        all
#  2        <$1
#  3    $1-<25k
#  4   $25-<50k
#  5   $50-<75k
#  6  $75-<100k
#  7 $100-<200k
#  8 $200-<500k
#  9   $500-<1m
# 10       $1m+

incgrpdescs20102011
######################################## read 2011 soi ##############################################
fn <- paste0(soidir, "11in54cm.xlsx")
df <- read.xls(fn, colClasses="character")
head(df)
stcodes$stabbr <- as.character(stcodes$stabbr) # note that this creates a copy of stcodes in the workspace, does not overwrite btools
stcodes$stname <- trim(as.character(stcodes$stname))
stnames <- df[2, ] %>% gather(variable, stname) %>%
  filter(!is.na(stname), stname!="", stname!="Item") %>%
  mutate(stname=trim(stname),
         stabbr=ifelse(stname %in% toupper(stcodes$stname), stcodes$stabbr[match(toupper(stcodes$stname), stname)], ""),
         stabbr=ifelse(grepl("OTHER AREAS", stname), "OA", stabbr))
stnames

# now modify the stname row of df

df2 <- df
# first, put state abbreviation in each relevant column - put into row 1 so we can easily compare to names already in row 2; start in col 2 as first col is a stub
# inserts <- rep(stnames$stname, each=10)
inserts <- rep(stnames$stabbr, each=10)
df2[1, 2:(length(inserts)+1)] <- inserts
df2[1:3, ]

# now income group - put into row 3 so easily can compare to descriptions in row 4
inserts <- rep(incgrpdescs20102011, 53)
df2[3, 2:(length(inserts)+1)] <- inserts
df2[1:6, ]

# new var names so that we can gather the data
vnames <- c("item", paste(df2[1, 2:531], df2[3, 2:531], sep="zzz"))
vnames
names(df2) <- vnames
row1 <- which(df2$item=="Number of returns")
df2 <- df2 %>% mutate(lineno=row_number() - row1 + 1)

dfl <- df2 %>% gather(stabbr.incgrp, value, -item, -lineno) %>%
  mutate(item=trim(item), value=as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  group_by(stabbr.incgrp) %>%
  arrange(lineno) %>%
  mutate(item2=ifelse(item=="Amount", paste0(trim(gsub("Number", "", lag(item))), ".Amount"), item)) %>%
  separate(stabbr.incgrp, c("stabbr", "incgrpdesc"), sep="zzz") %>%
  mutate(incgrp=factor(incgrpdesc, levels=incgrpdescs20102011, labels=1:length(incgrpdescs20102011)),
         year=2011) %>%
  select(-item) %>%
  select(stabbr, year, incgrp, incgrpdesc, lineno, item=item2, value)
ht(dfl)
count(dfl, item, lineno)

saveRDS(dfl, file=paste0(soidir,"soi2011.rds"))

count(dfl, lineno, item) %>% data.frame
count(dfl, incgrp, incgrpdesc) %>% data.frame



######################################## read 2010 soi ##############################################
count(soi2009, incgrp, incgrpdesc)
count(soi2005, incgrp, incgrpdesc)

fn <- paste0(soidir, "10in54cm.xlsx")
df <- read.xls(fn, colClasses="character")
head(df)
stcodes$stabbr <- as.character(stcodes$stabbr) # note that this creates a copy of stcodes in the workspace, does not overwrite btools
stnames <- df[2, ] %>% gather(variable, stname) %>%
  filter(!is.na(stname), stname!="") %>%
  mutate(stname=trim(stname),
         stabbr=ifelse(stname %in% toupper(stcodes$stname), stcodes$stabbr[match(toupper(stcodes$stname), stname)], ""),
         stabbr=ifelse(grepl("OTHER AREAS", stname), "OA", stabbr))
stnames

# now modify the stname row of df

df2 <- df
# first, put state abbreviation in each relevant column - put into row 1 so we can easily compare to names already in row 2; start in col 2 as first col is a stub
# inserts <- rep(stnames$stname, each=10)
inserts <- rep(stnames$stabbr, each=10)
df2[1, 2:(length(inserts)+1)] <- inserts
df2[1:3, ]

# now income group - put into row 3 so easily can compare to descriptions in row 4
inserts <- rep(incgrpdescs20102011, 53)
df2[3, 2:(length(inserts)+1)] <- inserts
df2[1:6, ]

# new var names so that we can gather the data
vnames <- c("item", paste(df2[1, 2:531], df2[3, 2:531], sep="zzz"))
vnames
names(df2) <- vnames
row1 <- which(df2$item=="Number of returns")
df2 <- df2 %>% mutate(lineno=row_number() - row1 + 1)

dfl <- df2 %>% gather(stabbr.incgrp, value, -item, -lineno) %>%
  mutate(item=trim(item),
         value=as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  group_by(stabbr.incgrp) %>%
  arrange(lineno) %>%
  mutate(item2=ifelse(item=="Amount", paste0(trim(gsub("Number", "", lag(item))), ".Amount"), item)) %>%
  separate(stabbr.incgrp, c("stabbr", "incgrpdesc"), sep="zzz") %>%
  mutate(incgrp=factor(incgrpdesc, levels=incgrpdescs20102011, labels=1:length(incgrpdescs20102011)),
         year=2010) %>%
  select(-item) %>%
  select(stabbr, year, incgrp, incgrpdesc, lineno, item=item2, value)
ht(dfl)

count(dfl, item, lineno)

saveRDS(dfl, file=paste0(soidir,"soi2010.rds"))


######################################## read 2009 soi ##############################################
fn<-paste(soidir,"09",soisfx,sep="")

# 2009: get the first block
soi2009a<-read.xls(fn, skip=8, nrows=107, colClasses="character")
head(soi2009a[1:2]); tail(soi2009a[1:2])
head(soi2009a[c(1,2,253:ncol(soi2009a))]) # look for where the data ends
soi2009a<-subset(soi2009a, select=-c(254:ncol(soi2009a))) # drop empty columns (determined by inspection)
soi2009a[c(1:2,ncol(soi2009a))] # look at all rows. Good.
names(soi2009a)[1]<-"item"
names(soi2009a)[2:ncol(soi2009a)]<-paste("X",2:ncol(soi2009a),sep="")
soi2009a$lineno<-as.numeric(row.names(soi2009a))
names(soi2009a)


# 2009: now the second block
soi2009b<-read.xls(fn, skip=125, nrows=107, colClasses="character")
head(soi2009b[1:2]); tail(soi2009b[1:2])
head(soi2009b[c(1,2,64:ncol(soi2009b))]) # look for where the data ends
soi2009b<-subset(soi2009b, select=-c(68:ncol(soi2009b))) # drop empty columns (determined by inspection)
soi2009b[c(1:2,ncol(soi2009b))] # look at all rows. Good.
names(soi2009b)[1]<-"item"
# now determine the names we need for the b block
lasta<-ncol(soi2009a)-1 # determine the last column of a
bcols<-(lasta+1):(ncol(soi2009b)+lasta-1)
names(soi2009b)[2:ncol(soi2009b)]<-paste("X",bcols,sep="")
soi2009b$lineno<-as.numeric(row.names(soi2009b))
names(soi2009b)


# put the two blocks together
# str(soi2009a); str(soi2009b)
soi2009w<-merge(soi2009a, subset(soi2009b, select=-c(item)), by="lineno")
# clean the combined file
x<-trim(sapply(strsplit(soi2009w$item, ":"), "[", 1)) # get the first part of item
tempitem<-soi2009w$item
soi2009w$item[6]<-"Adjusted gross income (AGI)"
for(i in 7:nrow(soi2009w)) {if(tempitem[i]=="Amount") soi2009w$item[i]<-paste(x[i-1],": Amount",sep="") else soi2009w$item[i]<-paste(x[i],": Number",sep="")}
soi2009w$item<-trim(soi2009w$item)
rm(tempitem)

# make a long file
soi2009<-melt(soi2009w,id=c("item","lineno"))

# now put stabbr on the file, then save
vnum<-as.numeric(substr(soi2009$variable,2,5))-1
# easiest to do incgrp first
incgrp<-((vnum-1) %% 6)+1
soi2009$incgrp<-incgrp
soi2009$incgrpdesc<-incgrpdesc[incgrp]
# now stnum
stnum<-(vnum-incgrp)/6+1
table(stnum)
head(stnum,200)
soi2009$stabbr<-stlist[stnum]

soi2009$year<-2009
soi2009$value<-ctov(soi2009$value)
soi2009$variable<-NULL
save(soi2009, file=paste(soidir,"soi2009",".RData",sep=""))

load(paste(soidir,"soi2009",".RData",sep=""))
head(soi2009); tail(soi2009)
linkcodes(soi2009, "lineno","item")


######################################## read 2008 soi ##############################################
fn<-paste(soidir,"08",soisfx,sep="")
fn

# 2008: get the first block
soi2008a<-read.xls(fn, skip=8, nrows=66, colClasses="character")
head(soi2008a[1:2]); tail(soi2008a[1:2])
head(soi2008a[c(1,2,253:ncol(soi2008a))]) # look for where the data ends
soi2008a<-subset(soi2008a, select=-c(254:ncol(soi2008a))) # drop empty columns (determined by inspection)
soi2008a[c(1:2,ncol(soi2008b))] # look at all rows. Good.
names(soi2008a)[1]<-"item"
names(soi2008a)[2:ncol(soi2008a)]<-paste("X",2:ncol(soi2008a),sep="")
soi2008a$lineno<-as.numeric(row.names(soi2008a))
names(soi2008a)


# 2008: now the second block
soi2008b<-read.xls(fn, skip=84, nrows=66, colClasses="character")
head(soi2008b[1:2]); tail(soi2008b[1:2])
head(soi2008b[c(1,2,64:ncol(soi2008b))]) # look for where the data ends
soi2008b<-subset(soi2008b, select=-c(68:ncol(soi2008b))) # drop empty columns (determined by inspection)
soi2008b[c(1:2,ncol(soi2008b))] # look at all rows. Good.
names(soi2008b)[1]<-"item"
# now determine the names we need for the b block
lasta<-ncol(soi2008a)-1 # determine the last column of a
bcols<-(lasta+1):(ncol(soi2008b)+lasta-1)
names(soi2008b)[2:ncol(soi2008b)]<-paste("X",bcols,sep="")
soi2008b$lineno<-as.numeric(row.names(soi2008b))
names(soi2008b)


# put the two blocks together
# str(soi2008a); str(soi2008b)
soi2008w<-merge(soi2008a, subset(soi2008b, select=-c(item)), by="lineno")
# clean the combined file
x<-trim(sapply(strsplit(soi2008w$item, ":"), "[", 1)) # get the first part of item
tempitem<-soi2008w$item
soi2008w$item[5]<-"Adjusted gross income (AGI)"
soi2008w$item[6]<-"Salaries and wages in AGI: Number"
for(i in 7:nrow(soi2008w)) {if(tempitem[i]=="Amount") soi2008w$item[i]<-paste(x[i-1],": Amount",sep="") else soi2008w$item[i]<-paste(x[i],": Number",sep="")}
soi2008w$item<-trim(soi2008w$item)
rm(tempitem)

# make a long file
soi2008<-melt(soi2008w,id=c("item","lineno"))

# now put stabbr on the file, then save
vnum<-as.numeric(substr(soi2008$variable,2,5))-1
# easiest to do incgrp first
incgrp<-((vnum-1) %% 6)+1
soi2008$incgrp<-incgrp
soi2008$incgrpdesc<-incgrpdesc[incgrp]
# now stnum
stnum<-(vnum-incgrp)/6+1
table(stnum)
head(stnum,200)
soi2008$stabbr<-stlist[stnum]

soi2008$year<-2008
soi2008$value<-ctov(soi2008$value)
soi2008$variable<-NULL
save(soi2008, file=paste(soidir,"soi2008",".RData",sep=""))


######################################## read 2007 soi ##############################################
fn<-paste(soidir,"07",soisfx,sep="") # the regular file
fn<-paste(soidir,"07in01st.xls",sep="") # a special file SOI created to net out returns filed simply for ARRA tax credit
fn

# 2007: get the first block
soi2007a<-read.xls(fn, skip=8, nrows=64, colClasses="character")
head(soi2007a[1:2]); tail(soi2007a[1:2])
head(soi2007a[c(1,2,253:ncol(soi2007a))]) # look for where the data ends
soi2007a<-subset(soi2007a, select=-c(254:ncol(soi2007a))) # drop empty columns (determined by inspection)
soi2007a[c(1:2,ncol(soi2007a))] # look at all rows. Good.
names(soi2007a)[1]<-"item"
names(soi2007a)[2:ncol(soi2007a)]<-paste("X",2:ncol(soi2007a),sep="")
soi2007a$lineno<-as.numeric(row.names(soi2007a))
names(soi2007a)


# 2007: now the second block
soi2007b<-read.xls(fn, skip=82, nrows=64, colClasses="character")
head(soi2007b[1:2]); tail(soi2007b[1:2])
head(soi2007b[c(1,2,64:ncol(soi2007b))]) # look for where the data ends
soi2007b<-subset(soi2007b, select=-c(68:ncol(soi2007b))) # drop empty columns (determined by inspection)
soi2007b[c(1:2,ncol(soi2007b))] # look at all rows. Good.
names(soi2007b)[1]<-"item"
# now determine the names we need for the b block
lasta<-ncol(soi2007a)-1 # determine the last column of a
bcols<-(lasta+1):(ncol(soi2007b)+lasta-1)
names(soi2007b)[2:ncol(soi2007b)]<-paste("X",bcols,sep="")
soi2007b$lineno<-as.numeric(row.names(soi2007b))
names(soi2007b)


# put the two blocks together
# str(soi2007a); str(soi2007b)
soi2007w<-merge(soi2007a, subset(soi2007b, select=-c(item)), by="lineno")
# clean the combined file
x<-trim(sapply(strsplit(soi2007w$item, ":"), "[", 1)) # get the first part of item
tempitem<-soi2007w$item
soi2007w$item[5]<-"Adjusted gross income (AGI)"
soi2007w$item[6]<-"Salaries and wages in AGI: Number"
for(i in 7:nrow(soi2007w)) {if(tempitem[i]=="Amount") soi2007w$item[i]<-paste(x[i-1],": Amount",sep="") else soi2007w$item[i]<-paste(x[i],": Number",sep="")}
soi2007w$item<-trim(soi2007w$item)
rm(tempitem)

# make a long file
soi2007<-melt(soi2007w,id=c("item","lineno"))

# now put stabbr on the file, then save
vnum<-as.numeric(substr(soi2007$variable,2,5))-1
# easiest to do incgrp first
incgrp<-((vnum-1) %% 6)+1
soi2007$incgrp<-incgrp
soi2007$incgrpdesc<-incgrpdesc[incgrp]
# now stnum
stnum<-(vnum-incgrp)/6+1
table(stnum)
head(stnum,200)
soi2007$stabbr<-stlist[stnum]

soi2007$year<-2007
soi2007$value<-ctov(soi2007$value)
soi2007$variable<-NULL
save(soi2007, file=paste(soidir,"soi2007",".RData",sep=""))



######################################## read 2006 soi ##############################################
fn<-paste(soidir,"06",soisfx,sep="")
fn

# 2006: get the first block
soi2006a<-read.xls(fn, skip=8, nrows=64, colClasses="character")
head(soi2006a[1:2]); tail(soi2006a[1:2])
head(soi2006a[c(1,2,253:ncol(soi2006a))]) # look for where the data ends
soi2006a<-subset(soi2006a, select=-c(254:ncol(soi2006a))) # drop empty columns (determined by inspection)
soi2006a[c(1:2,ncol(soi2006a))] # look at all rows. Good.
names(soi2006a)[1]<-"item"
names(soi2006a)[2:ncol(soi2006a)]<-paste("X",2:ncol(soi2006a),sep="")
soi2006a$lineno<-as.numeric(row.names(soi2006a))
names(soi2006a)


# 2006: now the second block
soi2006b<-read.xls(fn, skip=82, nrows=64, colClasses="character")
head(soi2006b[1:2]); tail(soi2006b[1:2])
head(soi2006b[c(1,2,64:ncol(soi2006b))]) # look for where the data ends
soi2006b<-subset(soi2006b, select=-c(68:ncol(soi2006b))) # drop empty columns (determined by inspection)
soi2006b[c(1:2,ncol(soi2006b))] # look at all rows. Good.
names(soi2006b)[1]<-"item"
# now determine the names we need for the b block
lasta<-ncol(soi2006a)-1 # determine the last column of a
bcols<-(lasta+1):(ncol(soi2006b)+lasta-1)
names(soi2006b)[2:ncol(soi2006b)]<-paste("X",bcols,sep="")
soi2006b$lineno<-as.numeric(row.names(soi2006b))
names(soi2006b)


# put the two blocks together
# str(soi2006a); str(soi2006b)
soi2006w<-merge(soi2006a, subset(soi2006b, select=-c(item)), by="lineno")
# clean the combined file
x<-trim(sapply(strsplit(soi2006w$item, ":"), "[", 1)) # get the first part of item
tempitem<-soi2006w$item
soi2006w$item[5]<-"Adjusted gross income (AGI)"
soi2006w$item[6]<-"Salaries and wages in AGI: Number"
for(i in 7:nrow(soi2006w)) {if(tempitem[i]=="Amount") soi2006w$item[i]<-paste(x[i-1],": Amount",sep="") else soi2006w$item[i]<-paste(x[i],": Number",sep="")}
soi2006w$item<-trim(soi2006w$item)
rm(tempitem)

# make a long file
soi2006<-melt(soi2006w,id=c("item","lineno"))

# now put stabbr on the file, then save
vnum<-as.numeric(substr(soi2006$variable,2,5))-1
# easiest to do incgrp first
incgrp<-((vnum-1) %% 6)+1
soi2006$incgrp<-incgrp
soi2006$incgrpdesc<-incgrpdesc[incgrp]
# now stnum
stnum<-(vnum-incgrp)/6+1
table(stnum)
head(stnum,200)
soi2006$stabbr<-stlist[stnum]

soi2006$year<-2006
soi2006$value<-ctov(soi2006$value)
soi2006$variable<-NULL
save(soi2006, file=paste(soidir,"soi2006",".RData",sep=""))



######################################## read 2005 soi ##############################################
fn<-paste(soidir,"05",soisfx,sep="")
fn

# 2005: get the first block
soi2005a<-read.xls(fn, skip=6, nrows=62, colClasses="character")
head(soi2005a[1:2]); tail(soi2005a[1:2])
head(soi2005a[c(1,2,253:ncol(soi2005a))]) # look for where the data ends
soi2005a<-subset(soi2005a, select=-c(254:ncol(soi2005a))) # drop empty columns (determined by inspection)
soi2005a[c(1:2,ncol(soi2005a))] # look at all rows. Good.
names(soi2005a)[1]<-"item"
names(soi2005a)[2:ncol(soi2005a)]<-paste("X",2:ncol(soi2005a),sep="")
soi2005a$lineno<-as.numeric(row.names(soi2005a))
names(soi2005a)


# 2005: now the second block
soi2005b<-read.xls(fn, skip=75, nrows=62, colClasses="character")
head(soi2005b[1:2]); tail(soi2005b[1:2])
head(soi2005b[c(1,2,64:ncol(soi2005b))]) # look for where the data ends
soi2005b<-subset(soi2005b, select=-c(68:ncol(soi2005b))) # drop empty columns (determined by inspection)
soi2005b[c(1:2,ncol(soi2005b))] # look at all rows. Good.
names(soi2005b)[1]<-"item"
# now determine the names we need for the b block
lasta<-ncol(soi2005a)-1 # determine the last column of a
bcols<-(lasta+1):(ncol(soi2005b)+lasta-1)
names(soi2005b)[2:ncol(soi2005b)]<-paste("X",bcols,sep="")
soi2005b$lineno<-as.numeric(row.names(soi2005b))
names(soi2005b)


# put the two blocks together
# str(soi2005a); str(soi2005b)
soi2005w<-merge(soi2005a, subset(soi2005b, select=-c(item)), by="lineno")
# clean the combined file
x<-trim(sapply(strsplit(soi2005w$item, ":"), "[", 1)) # get the first part of item
tempitem<-soi2005w$item
soi2005w$item[1:10]
soi2005w$item[5]<-"Adjusted gross income (AGI)"
soi2005w$item[6]<-"Salaries and wages in AGI: Number"
for(i in 7:nrow(soi2005w)) {if(tempitem[i]=="Amount") soi2005w$item[i]<-paste(x[i-1],": Amount",sep="") else soi2005w$item[i]<-paste(x[i],": Number",sep="")}
soi2005w$item<-trim(soi2005w$item)
rm(tempitem)

# make a long file
soi2005<-melt(soi2005w,id=c("item","lineno"))

# now put stabbr on the file, then save
vnum<-as.numeric(substr(soi2005$variable,2,5))-1
# easiest to do incgrp first
incgrp<-((vnum-1) %% 6)+1
soi2005$incgrp<-incgrp
soi2005$incgrpdesc<-incgrpdesc[incgrp]
# now stnum
stnum<-(vnum-incgrp)/6+1
table(stnum)
head(stnum,200)
soi2005$stabbr<-stlist[stnum]

soi2005$year<-2005
soi2005$value<-ctov(soi2005$value)
soi2005$variable<-NULL
save(soi2005, file=paste(soidir,"soi2005",".RData",sep=""))


######################################## read 2004 soi ##############################################
fn<-paste(soidir,"04",soisfx,sep="")
fn

# 2004: get the first block
soi2004a<-read.xls(fn, skip=6, nrows=62, colClasses="character")
head(soi2004a[1:2]); tail(soi2004a[1:2])
head(soi2004a[c(1,2,253:ncol(soi2004a))]) # look for where the data ends
soi2004a<-subset(soi2004a, select=-c(254:ncol(soi2004a))) # drop empty columns (determined by inspection)
soi2004a[c(1:2,ncol(soi2004a))] # look at all rows. Good.
names(soi2004a)[1]<-"item"
names(soi2004a)[2:ncol(soi2004a)]<-paste("X",2:ncol(soi2004a),sep="")
soi2004a$lineno<-as.numeric(row.names(soi2004a))
names(soi2004a)


# 2004: now the second block
soi2004b<-read.xls(fn, skip=75, nrows=62, colClasses="character")
head(soi2004b[1:2]); tail(soi2004b[1:2])
head(soi2004b[c(1,2,64:ncol(soi2004b))]) # look for where the data ends
soi2004b<-subset(soi2004b, select=-c(68:ncol(soi2004b))) # drop empty columns (determined by inspection)
soi2004b[c(1:2,ncol(soi2004b))] # look at all rows. Good.
names(soi2004b)[1]<-"item"
# now determine the names we need for the b block
lasta<-ncol(soi2004a)-1 # determine the last column of a
bcols<-(lasta+1):(ncol(soi2004b)+lasta-1)
names(soi2004b)[2:ncol(soi2004b)]<-paste("X",bcols,sep="")
soi2004b$lineno<-as.numeric(row.names(soi2004b))
names(soi2004b)


# put the two blocks together
# str(soi2004a); str(soi2004b)
soi2004w<-merge(soi2004a, subset(soi2004b, select=-c(item)), by="lineno")
# clean the combined file
x<-trim(sapply(strsplit(soi2004w$item, ":"), "[", 1)) # get the first part of item
tempitem<-soi2004w$item
soi2004w$item[1:10]
soi2004w$item[5]<-"Adjusted gross income (AGI)"
soi2004w$item[6]<-"Salaries and wages in AGI: Number"
for(i in 7:nrow(soi2004w)) {if(tempitem[i]=="Amount") soi2004w$item[i]<-paste(x[i-1],": Amount",sep="") else soi2004w$item[i]<-paste(x[i],": Number",sep="")}
soi2004w$item<-trim(soi2004w$item)
rm(tempitem)

# make a long file
soi2004<-melt(soi2004w,id=c("item","lineno"))

# now put stabbr on the file, then save
vnum<-as.numeric(substr(soi2004$variable,2,5))-1
# easiest to do incgrp first
incgrp<-((vnum-1) %% 6)+1
soi2004$incgrp<-incgrp
soi2004$incgrpdesc<-incgrpdesc[incgrp]
# now stnum
stnum<-(vnum-incgrp)/6+1
table(stnum)
head(stnum,200)
soi2004$stabbr<-stlist[stnum]

soi2004$year<-2004
soi2004$value<-ctov(soi2004$value)
soi2004$variable<-NULL
save(soi2004, file=paste(soidir,"soi2004",".RData",sep=""))

#****************************************************************************************************
#                Convert old sas versions to R ####
#****************************************************************************************************
# requires haven
# I have SAS files from 1997-2007
sastor <- function(year) {
  df <- read_sas(paste0(soitsd, "OldSASfiles/", "soi", year, ".sas7bdat"))
  return(df)
}

ht(df)
glimpse(df)

df <- ldply(1997:2007, sastor, .progress = "text")
ht(df)
count(df, year)
count(df, stabbr)
fn <- "soifromSAS_1997to2007.rds"
saveRDS(df, paste0(soitsd, fn))
saveRDS(df, paste0(interim, fn))

# make a long file suitable for saving for the long run




#****************************************************************************************************
#                Examine previously created data ####
#****************************************************************************************************
df <- readRDS(file=paste0(soidir, "soiall.rds"))
glimpse(df)
count(df, year, vname) %>% spread(year, n)
df %<>% filter(!is.na(vname))
count(df, year, incgrp) %>% spread(year, n)

df %>% filter(stabbr=="US", incgrp==0) %>%
  select(vname, year, value) %>%
  spread(vname, value)
# good, all of the units are the same - actual # for nret, and $000 for $ items

df %>% filter(stabbr=="US", incgrp==0, vname!="nret") %>%
  select(vname, year, value) %>%
  spread(vname, value) %>%
  mutate_each(funs(./agi*100), itemded, netcgll, odividends, taxinc, irapay, txblinterest, wages)


dfpch <- df %>% filter(incgrp==0) %>%
  select(stabbr, vname, year, value) %>%
  spread(vname, value) %>%
  mutate(otheragi=agi - wages - odividends - txblinterest - netcgll - busprofinc) %>%
  gather(vname, value, -stabbr, -year) %>%
  group_by(stabbr, vname) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100)
dfpch %>% filter(stabbr=="US") %>%
  select(-value) %>%
  spread(vname, pch) %>%
  kable(digits=2)
dfpch %>% filter(vname=="netcgll") %>%
  select(-value) %>%
  spread(year, pch) %>%
  arrange(`2008`)

dfshare <- df %>% filter(incgrp==0, !vname %in% c("nret", "itemded")) %>%
  select(stabbr, vname, year, value) %>%
  spread(vname, value) %>%
  mutate(otheragi=agi - wages - odividends - txblinterest - netcgll - busprofinc,
         cgshare=netcgll / agi * 100) %>%
  select(stabbr, year, cgshare) %>%
  filter(year %in% c(2007, 2009, 2012)) %>%
  spread(year, cgshare) %>%
  mutate(down=`2009`-`2007`,
         up=`2012` - `2009`,
         netchg=`2012`-`2007`
  ) %>%
  arrange(down)
dfshare %>% filter(!stabbr %in% c("NV", "FL", "OA", "NH", "TX", "TN", "SD")) %>% kable(digits=2)


df1 %>% filter()

dfa <- df %>% filter(incgrp==0) %>%
  select(stabbr, vname, year, value) %>%
  spread(vname, value) %>%
  mutate(cgpct=netcgll / agi * 100) %>%
  filter(year )
gather(vname, value, -stabbr, -year) %>%
  select()
group_by(stabbr, vname) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100) %>%
  select(-value) %>%
  spread(vname, pch)





# ######################################## read earlier SOIs -- DIFFERENT FORMAT ##############################################
# ## read 2003 soi
# fn<-paste(soidir,"03",soisfx,sep="")
# fn
# system.time(tmp<-read.xls(fn, colClasses="character",row.names=NULL))
# head(tmp,9)
# soi2003a<-tmp
# soi2003a<-soi2003a[7:nrow(soi2003a),]
# str(soi2003a)
# names(soi2003a)[1]<-"item"
# names(soi2003a)[2:ncol(soi2003a)]<-paste("V",2:ncol(soi2003a),sep="")
# soi2003a$item<-sub("[14]", "", soi2003a$item, fixed=TRUE) # special treatment for OTHER AREAS [14]
# soi2003a$item<-trim(soi2003a$item) # to prepare us to determine state names
# # head(soi2003a); tail(soi2003a)
#
# # determine where the start of each state is, and which state it is. State names are in ALL CAPS so this makes them easy to find
# sts<-data.frame(V1=soi2003a$item)
# sts$stname<-gsub("[^A-Z ]", "", sts$V1) # keep just upper case letters and spaces
# sts<-subset(sts, V1==stname & (V1 != "") & !is.na(V1)) # now all we have left are state names
# table(sts$stname)
# sts$rownum<-row.names(sts) # rownum gives the rownum in CURRENT version of soi2003a that has the state name
# head(sts); tail(sts)
# soi2003a[3220,]
# soi2003a[1:15,]
#
#
#
# # this is not efficent but it works
# a<-proc.time()
# soi2003a$stname<-""
# soi2003a$lineno<--1
# for(i in 1:nrow(soi2003a)){
#   if(soi2003a$item[i] %in% sts$stname){
#     soi2003a$stname[i]<-soi2003a$item[i]
#     soi2003a$lineno[i]<-0
#   } else {
#     soi2003a$stname[i]<-soi2003a$stname[i-1]
#     soi2003a$lineno[i]<-soi2003a$lineno[i-1]+1
#   }
# }
# proc.time()-a
#
# soi2003a<-subset(soi2003a, lineno>=1 & lineno<=61) # drop unnecessary lines
#
# # prepare to put stabbr on file -- add OTHER AREAS to the mix
# soistname<-c(toupper(stname),"OTHER AREAS")
# soistabbr<-c(stabbr,"OA")
# soistname; soistabbr
# soi2003a$stabbr<-as.character(factor(soi2003a$stname,levels=soistname, labels=soistabbr))
# soi2003a[1:20,]
# linkcodes(soi2003a, "stabbr", "stname") # make sure states are matched properly
#
# # there were a few items where the description was on multiple lines
# soi2003a[1:65,c(1:2,11)]
# # allow dropping of lineno 12 18 51
# soi2003a$item[soi2003a$lineno==13]<-"Business or profession net income (less loss):  Number"
# soi2003a$item[soi2003a$lineno==19]<-"Taxable Individual Retirement Arrangements distributions:  Number"
# soi2003a$item[soi2003a$lineno==52]<-"Excess earned income credit (refundable):  Number"
# soi2003a<-subset(soi2003a, !(lineno %in% c(12,18,51))) # drop the bad lines
# soi2003a$item<-trim(gsub("[^A-Za-z(): ]", "", soi2003a$item)) # keep letters, parentheses, spaces, and colons, but nothing else
# soi2003a$item<-trim(gsub("  ", " ", soi2003a$item)) # change double spaces to single space
# # now we are ready to loop through and put full title on those that only say amount
# for(i in 2:nrow(soi2003a)) if(soi2003a$item[i]=="Amount") soi2003a$item[i]<-paste(sub(": Number", ": ", soi2003a$item[i-1]), "Amount",sep="")
#
# # FINALLY WE ARE READY TO MELT THE DATA
# ncol(soi2003a)
# head(soi2003a)
# soi2003<-melt(soi2003a, id=c("stabbr","lineno","item"),meas=(paste("V",2:8,sep="")))
# soi2003$value<-ctov(soi2003$value)
# incgrp<-as.numeric(substr(soi2003$variable,2,5))-1
# soi2003$incgrp<-ifelse(incgrp>2, incgrp-1, incgrp) # 2003 has 2 groups that we will collapes into <$50k
# table(soi2003$incgrp)
# soi2003$incgrpdesc<-incgrpdesc[soi2003$incgrp]
# soi2003$variable<-NULL
# soi2003$year<-2003
# soi2003<-soi2003[order(soi2003$stabbr,soi2003$lineno,soi2003$incgrp),]
# save(soi2003, file=paste(soidir,"soi2003",".RData",sep=""))
#
#
#
#
# head(soi2003); tail(soi2003)
# table(soi2003$stabbr)
# table(soi2003$variable)
# table(soi2003$incgrpdesc)
#
# soi2003[soi2003$stabbr=="AK",]
#
#
#
#
# ls()
# table(soi2003a$stname)
#
# linkcodes(soi2003a,"stabbr","stname")
#
# soi2003a$stname<-trim(gsub("[^A-Z ]", "", soi2003a$V1))
#
# soi2003a$stname<-ifelse(
# head(sts,80)
#
# head(soi2003a$stname,100)
#
# head(match(soi2003a[,1], sts$stname),140)
#
# # match(sts$stname, soi2003a[,1]) # find where the state names are in the main data frame - not needed because we have rowname
#
# # here is the range for a given state
# soi2003a[986+1,]
# soi2003a[986+61,] # 61 linecodes
#
#
#
# "WEST VIRGINIA" %in% sts$stname
# match("WEST VIRGINIA",sts$stname)
# sts$stname[49]
#
# str(sts)
# head(sts); tail(sts)
#
# nchar(st)==nchar(gsub("[^A-Z ]", "", st))
#
# st<-"ALABAMA"
# st<-"alabama"
# st<-"NORTH CAROLINA"
# st
# nchar(st)
# substr(st,1,1) %in% LETTERS
#
# gsub("[^LETTERS]", "", st)
#
# nchar(gsub("[^A-Z ]", "", st)) # this will be zero if no upper case
#
#
# pattern = r'[^\.a-z0-9]'
#     if re.search(pattern, test_str):
#
# st[1] %in% LETTERS
# for(i in 1:nchar(st)){
#   if(substr(st,i,i) %in% LETTERS) print("in LETTERS")
# }
#
# head(soi2004a[1:2]); tail(soi2004a[1:2])
#



#
#
#
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
#
#
# ######################################## read earlier sois IN PROGRESS ##############################################
# fn<-paste(soidir,"05",soisfx,sep="")
# fn
#
# getsoi<-function(){
#   # get the first block
#   soia<-read.xls(fn, skip=8, nrows=64, colClasses="character")
#   head(soi2007a[1:2]); tail(soi2007a[1:2])
# head(soi2007a[c(1,2,253:ncol(soi2007a))]) # look for where the data ends
# soi2007a<-subset(soi2007a, select=-c(254:ncol(soi2007a))) # drop empty columns (determined by inspection)
# soi2007a[c(1:2,ncol(soi2007a))] # look at all rows. Good.
# names(soi2007a)[1]<-"item"
# names(soi2007a)[2:ncol(soi2007a)]<-paste("X",2:ncol(soi2007a),sep="")
# soi2007a$lineno<-as.numeric(row.names(soi2007a))
# names(soi2007a)
#
#
# # 2007: now the second block
# soi2007b<-read.xls(fn, skip=82, nrows=64, colClasses="character")
# head(soi2007b[1:2]); tail(soi2007b[1:2])
# head(soi2007b[c(1,2,64:ncol(soi2007b))]) # look for where the data ends
# soi2007b<-subset(soi2007b, select=-c(68:ncol(soi2007b))) # drop empty columns (determined by inspection)
# soi2007b[c(1:2,ncol(soi2007b))] # look at all rows. Good.
# names(soi2007b)[1]<-"item"
# # now determine the names we need for the b block
# lasta<-ncol(soi2007a)-1 # determine the last column of a
# bcols<-(lasta+1):(ncol(soi2007b)+lasta-1)
# names(soi2007b)[2:ncol(soi2007b)]<-paste("X",bcols,sep="")
# soi2007b$lineno<-as.numeric(row.names(soi2007b))
# names(soi2007b)
#
#
# # put the two blocks together
# # str(soi2007a); str(soi2007b)
# soi2007w<-merge(soi2007a, subset(soi2007b, select=-c(item)), by="lineno")
# # clean the combined file
# x<-trim(sapply(strsplit(soi2007w$item, ":"), "[", 1)) # get the first part of item
# tempitem<-soi2007w$item
# soi2007w$item[5]<-"Adjusted gross income (AGI)"
# soi2007w$item[6]<-"Salaries and wages in AGI: Number"
# for(i in 7:nrow(soi2007w)) {if(tempitem[i]=="Amount") soi2007w$item[i]<-paste(x[i-1],": Amount",sep="") else soi2007w$item[i]<-paste(x[i],": Number",sep="")}
# soi2007w$item<-trim(soi2007w$item)
# rm(tempitem)
#
# # make a long file
# soi2007<-melt(soi2007w,id=c("item","lineno"))
#
# # now put stabbr on the file, then save
# vnum<-as.numeric(substr(soi2007$variable,2,5))-1
# # easiest to do incgrp first
# incgrp<-((vnum-1) %% 6)+1
# soi2007$incgrp<-incgrp
# soi2007$incgrpdesc<-incgrpdesc[incgrp]
# # now stnum
# stnum<-(vnum-incgrp)/6+1
# table(stnum)
# head(stnum,200)
# soi2007$stabbr<-stlist[stnum]
#
# soi2007$year<-2007
# soi2007$value<-ctov(soi2007$value)
# soi2007$variable<-NULL
# save(soi2007, file=paste(soidir,"soi2007",".RData",sep=""))
#
#












