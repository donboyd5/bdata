
# Updated: 3/9/2021

# Special60 is now: https://www.census.gov/programs-surveys/gov-finances/data/historical-data.html


#****************************************************************************************************
#                Overview ####
#****************************************************************************************************

# Purpose: construct a single state-local government finance file with one record:
#   per year
#   per state (including DC, and US total - 52 groups), using state postal abbreviation
#   per level of government (state-local, state, local)
#   per variable - aggregated variable of interest, defined by me, based on Census categories
# for as many years as possible, focusing on the most recent years first.


# There are two main Census sources:
# 1) Annual item-code files for 1992-2012 (21 years) that are already aggregated by type of government (from individual
#    govt units in the Census Bureau surveys).
# - The item code data must be aggregated into more-meaningful variables using "recipes" based on the Census
#   Bureau's classification manuals
# - Always need to check for presence of DC and of US totals in the data.
# - The files are in differing formats. More recent years are available in an easy-to-read "35-character public use"
#   format (don't know why it's called public use). These are fixed width files that are easy to read and are in
#   the same format from year to eyar. It looks (?) like they begin in 1994. The file names appear to have stabilized
#   into a common format from ~1998 forward where the first 2 digits signify year and standard text
#   follows, such as: 12statetypepu.zip

# 2) Census Bureau historical databases that are not well-exposed to the public and that require extra work.
#   Definitions, samples, and survey procedures have changed over time so it is important to read the documentation.
#   In general, I am only using this for MAJOR aggregated variables (e.g., total taxes, total general expenditure)
#   where I can be pretty sure I understand the data.

#   The main directory is:  http://www2.census.gov/pub/outgoing/govs/special60/
#   There are 3 main sources of historical data already summarized at the state-local level, by state:
#   a) hist_fin.zip (4mb):
#     - an Access database with tables for state-local, state, local, and major types of local (county, ...)
#     - each table has years as columns, 1902-2008
#     - finance items as rows: item-code detail, plus various aggregates (e.g., total taxes), plus some population data
#     Note that because of R difficulties in reading Access databases (have to switch to 32-bit R), to the extent I
#     use these data I will export the tables to csv or xls files.
#   b) Govt_Finances.zip (15mb):
#     - Access database with tables for revenue, expenditures, and debt-and-cash
#     - each table has finance items as columns: item code detail plus various aggregates
#     - rows are year-govtlevel -- state-local, state, and local - no subtypes of local govt
#     This looks like the easiest to work with so I am using it. I tried to export to text (csv) format from
#     Access but it failed repeatedly. Thus I have exported to Excel.
#   c) rex-dac.zip (24mb):
#     - Appears similar to Govt_Finances.zip, but with multiple files per category (rex1...rexn, dac1...dacn)


# Census urls:
# http://www.census.gov/govs/local/historical_data.html   Main page for recent SLG finance data
#
# 35-character files for each year from 1992-2012:
# http://www2.census.gov/govs/local/12statetypepu.zip
# ...
# http://www2.census.gov/govs/local/00statetypepu.zip

# http://www2.census.gov/govs/local/98statetypepu.zip
# http://www2.census.gov/govs/local/97censusstatetypepu.zip
# http://www2.census.gov/govs/local/fin96est.zip
# http://www2.census.gov/govs/local/fin95est.zip
# http://www2.census.gov/govs/local/fin94est.zip # 35-character format?
# http://www2.census.gov/govs/local/93stlest.txt  a wide file with 3 levels x 52 states as columns
# http://www2.census.gov/govs/local/92censusstatetypepu.zip

# Many or perhaps all of the individual-year files above also can be found at http://www2.census.gov/govs/estimate/
# but I use the "local" subdirectory because that is where the main Census web page points to.

# Historical data, generally for periods before 1992, are in various database files. According to the
# Census website: Contact the Outreach and Education Branch by e-mail or by phone at 1 (800) 242-2184.
# The finance staff are reachable at:
# govs.finstaff@census.gov, 1-800-242-4523

## NOTE ####
# I have deleted the original zip files after extracting needed files, and much related documentation,
# in the interest of keeping the package directories and the package itself smaller than they otherwise
# would be. The zip files can be re-downloaded using code below.


#****************************************************************************************************
#                load packages ####
#****************************************************************************************************

library(magrittr)
library(plyr) # needed for ldply; must be loaded BEFORE dplyr
library(tidyverse)
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library(scales)
library(hms) # hms, for times.
library(stringr) # stringr, for strings.
library(lubridate) # lubridate, for date/times.
library(forcats) # forcats, for factors.
library(readxl) # readxl, for .xls and .xlsx files.
library(haven) # haven, for SPSS, SAS and Stata files.
library(vctrs)
library(precis)

library(grDevices)
library(knitr)

library(zoo) # for rollapply

library(btools) # library that I created (install from github)
library(bdata)


#****************************************************************************************************
#                define directories and files ####
#****************************************************************************************************
# cslgdir <- paste0("./data-raw/census_slgfin_annual/")
cslgdir <- "D:/Data/bdata_package_sourcedata/census_slgfin_annual/"

d35 <- paste0(cslgdir, "data35/") # store 35-character item code data here
spec60 <- paste0(cslgdir, "special60/") # data from Census Bureau's special60 directory goes here
gfd <- paste0(spec60, "Govt_Finances/")


#****************************************************************************************************
#                functions ####
#****************************************************************************************************
get.inparens <- function(cvec) {
  # get text within parentheses of strings
  cvecout <- vector(mode="character", length=length(cvec))
  pattern <- "\\([^)]*\\)" # regex pattern expression for finding text between ( and )
  locations <- regexpr(pattern, cvec) # find which vars have "()" and where they start
  text <- regmatches(cvec, locations) # retrieve the "()" including text within - only as long as # of items with ()
  text <- gsub("[()]", "", text) # remove ()
  cvecout[which(locations != -1)] <- text
  return(cvecout)
}

# get.inparens(c("Monday (Mon)", "Tuesday", "Wednesday (Wed)"))


#****************************************************************************************************
#                download item code files as needed and save in a project subdirectory ####
#****************************************************************************************************
# Hard code file names and locations because census has made such a mess of them
#*
# https://www2.census.gov/programs-surveys/gov-finances/tables/2018/2018_Individual_Unit_File.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2017/public-use-datasets/2017_individual_unit_file.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2016/public-use-datasets/2016_Individual_Unit_file.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2015/public-use-datasets/2015-individual-unit-file.zip
# http://www2.census.gov/programs-surveys/gov-finances/datasets/2014/public-use-datasets/2014-individual-unit-file.zip
# https://www2.census.gov/programs-surveys/gov-finances/tables/2013/summary-tables/2013_Individual_Unit_file.zip
# https://www2.census.gov/programs-surveys/gov-finances/tables/2012/summary-tables/2012_Individual_Unit_file.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2011/public-use-datasets/11statetypepu.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2010/public-use-datasets/10statetypepu.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2009/public-use-datasets/09statetypepu.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2008/public-use-datasets/08statetypepu.zip
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2007/public-use-datasets/07statetypepu.zip

ic_files <- tribble(
  ~ year, ~dir, ~fn,
  2007, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2007/public-use-datasets/", "07statetypepu.zip",
  2008, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2008/public-use-datasets/", "08statetypepu.zip",
  2009, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2009/public-use-datasets/", "09statetypepu.zip",
  2010, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2010/public-use-datasets/", "10statetypepu.zip",
  2011, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2011/public-use-datasets/", "11statetypepu.zip",
  2012, "https://www2.census.gov/programs-surveys/gov-finances/tables/2012/summary-tables/", "2012_Individual_Unit_file.zip",
  2013, "https://www2.census.gov/programs-surveys/gov-finances/tables/2013/summary-tables/", "2013_Individual_Unit_file.zip",
  2014, "http://www2.census.gov/programs-surveys/gov-finances/datasets/2014/public-use-datasets/", "2014-individual-unit-file.zip",
  2015, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2015/public-use-datasets/", "2015-individual-unit-file.zip",
  2016, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2016/public-use-datasets/", "2016_Individual_Unit_file.zip",
  2017, "https://www2.census.gov/programs-surveys/gov-finances/datasets/2017/public-use-datasets/", "2017_individual_unit_file.zip",
  2018, "https://www2.census.gov/programs-surveys/gov-finances/tables/2018/", "2018_Individual_Unit_File.zip",
)
ic_files

# http://www2.census.gov/govs/local/00statetypepu.zip

yrs <- 2013:2018
fnames <- ic_files %>% 
  filter(year %in% yrs) %>%
  mutate(url=paste0(dir, fn),
         loc=paste0(d35, fn),
         puname=paste0(str_sub(year, 3, 4), "statetypepu.txt"))

# download the files
walk2(fnames$url, fnames$loc, function(url, loc) download.file(url, loc, mode="wb"))
# extract JUST the 35-character public use file of interest
walk2(fnames$loc, fnames$puname, function(loc, puname) unzip(zipfile=loc, files=puname, exdir=str_sub(d35, 1, -2)))



# older approach
# https://www2.census.gov/programs-surveys/gov-finances/datasets/2016/public-use-datasets/2016_Individual_Unit_file.zip
# for(yr in yrs)  {
#   y2 <- str_sub(yr, 3, 4)
#   if(yr %in% c(2001, 2003)) fn <- paste0(y2, "statetypepu.txt") else
#     fn <- paste0(y2, "statetypepu.zip")
#   url <- paste0("http://www2.census.gov/govs/local/", fn)
#   print(url)
#   download.file(url, paste0(d35, fn), mode="wb")
# }

# Note that the 2013 data do not appear to be here!!!
# Randy Moore, Chief of the Census Local Governments Branch, provided it directly to me by email


#****************************************************************************************************
#                Get historical databases ????? OLD?? ####
#****************************************************************************************************

# get the various historical databases (note that I store these elsewhere on my computer)
# Special60 is now: https://www.census.gov/programs-surveys/gov-finances/data/historical-data.html
# https://www2.census.gov/programs-surveys/gov-finances/datasets/historical/Govt_Finances.zip
# urlbase <- "http://www2.census.gov/pub/outgoing/govs/special60/"
# urlbase <- "https://www2.census.gov/programs-surveys/gov-finances/datasets/historical/"
# fn <- "Govt_Finances.zip" # Govt_Finances.zip; also of interest: hist_fin.zip, rex-dac.zip
# download.file(paste0(urlbase, fn), paste0(spec60, fn), mode="wb")


#****************************************************************************************************
#                Individual year files 2000 through most recent year (2018 as of 3/9/2021) ####
#****************************************************************************************************
# Work SLOWLY, getting only a few variables at a time
# itemfiles <- character(13)
# for(yr in 2000:2013) {
#   i <- yr - 1999
#   if(yr==2000) itemfiles[i] <- "00statetypepu_1108.TXT" else
#     if(yr==2002) itemfiles[i] <- "2002State_By_type_Summaries24.txt" else
#       itemfiles[i] <- paste0(str_sub(yr, 3, 4), "statetypepu.txt")
# }

# itemfiles <- c(list.files(d35, pattern="statetypepu"), "2002State_By_type_Summaries24.txt")
# itemfiles
fnamedf <- data.frame(fname=c(list.files(d35, pattern="statetypepu"),
                                           "2002State_By_type_Summaries24.txt")) %>%
  mutate(year=2000 + ifelse(str_sub(fname, 1, 4)=="2002", 02, as.integer(str_sub(fname, 1, 2)))) %>%
  arrange(year)
fnamedf

# now read the item files
# 2002 is a different format than the other years

# 12/4/2015 read_fwf is broken (issue #300); workaround below adds a final junk character column that I drop
# https://github.com/hadley/readr/issues/300
# year <- 2000
f <- function(year) {
  fname <- fnamedf$fname[match(year, fnamedf$year)]
  print(fname)
  starts <- c(1, 3, 5, 9, 21)
  ends <- c(2, 3, 8, 20, 21)
  if(year==2002) {
    starts <- c(1, 3, 15, 19, 30)
    ends <- c(2, 3, 17, 29, 30)
  }
  df <- read_fwf(paste0(d35, fname), fwf_positions(starts, ends, col_names=c("stcode", "level", "ic", "value", "junk")),
           col_types="cicdc", n_max=-1) %>% select(-junk)
  df$year <- year
  return(df)
}
# df <- ldply(2000:2018, f)
df <- map_dfr(2000:2018, f)
summary(df)
ht(df)
count(df, year)
# df %>% filter(year==2000, stcode=="00", level==1, ic=="19A")

# slight cleaning, then save
df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(stcode, stcodes$stcen)])
count(df2, stcode, stabbr) # note that there are both DC and US records
# check DC and US by year
count(df2, year, usrec=stabbr=="US") %>% spread(usrec, n) # note that there are no non-US recs in 2001, 2003
count(df2, year, dcrec=stabbr=="DC") %>% spread(dcrec, n) # note that there are no DC recs in 2001, 2003
count(filter(df2, stabbr!="US"), year)
count(df2, ic) %>% data.frame
count(df2, level, year) %>% spread(level, n)
df3 <- df2 %>% filter(level %in% 1:3) %>%
  select(year, stabbr, level, ic, value) %>%
  arrange(stabbr, level, ic, year)
# one last check, for duplicates
anyDuplicated(select(df3, year, stabbr, level, ic)) # good, no dups
count(df3, year)

saveRDS(df3, file=paste0(d35, "finrecent.rds"))
rm(df, df2, df3)


#****************************************************************************************************
#                ONETIME: Get recipe data frame for creating aggregates from item code data ####
#****************************************************************************************************
# get recipe data frame
# fn <- "SLGFinAggregationAnalysis(31).xlsx" # broke out the property tax in version 1
fn <- "SLGFinAggregationAnalysis(32).xlsx" # 3/9/2021 removed A54 in 2005-plus
rdf <- read_excel(paste0(cslgdir, fn), sheet="recipesLong", col_names = FALSE)
rdf[1:10, 1:ncol(rdf)]
names(rdf) <- paste(rdf[5, ], rdf[4, ], sep=".")

vnames <- paste(rdf[5, ], rdf[4, ], sep="_")

rdfl <- rdf %>%
  setNames(vnames) %>%
  filter(row_number() >= 6) %>%
  pivot_longer(cols=everything(),
               names_to = "variable",
               values_to = "ic") %>%
  filter(ic !="") %>%
  separate(variable, into=c("aggvar", "recipegroup"), sep="_", remove = FALSE) %>%
  mutate(indicator=1) %>%
  arrange(variable, ic)

# rdfl <- rdf[-c(1:4), ] %>% 
#   gather(variable, ic) %>%
#   filter(ic !="") %>%
#   separate(variable, c("aggvar", "recipegroup"), sep=-7, remove=FALSE) %>%
#   mutate(recipegroup=gsub(".", "", recipegroup, fixed=TRUE),
#          indicator=1) %>%
#   arrange(variable, ic)

ht(rdfl)
saveRDS(rdfl, paste0(d35, "rdfl.rds"))

# count(rdfl, recipegroup)
# count(rdfl, aggvar, recipegroup) %>% spread(recipegroup, n) %>% data.frame


#****************************************************************************************************
#                With each new year: Create aggregates from recent item code data ####
#****************************************************************************************************
rdfl <- readRDS(paste0(d35, "rdfl.rds"))

df <- readRDS(file=paste0(d35, "finrecent.rds"))

tmp <- df %>%
  filter(stabbr=="CA", year==2016, level==2, str_sub(ic, 1, 1)=="T")
sum(tmp$value)

# prepare the data file
df2 <- df %>% 
  mutate(recipegroup = ifelse(year < 2005, "2004m", "2005p"))

df3 <- inner_join(select(rdfl, -variable), df2, by=c("ic", "recipegroup")) %>%  # merge a few secs faster than inner_join but stick with ij
  group_by(stabbr, level, year, aggvar) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  ungroup

df3 %>%
  filter(stabbr=="CA", year==2016, level==2, aggvar=="tottax")

count(df3, aggvar) %>% data.frame
count(df3, year) %>% data.frame

saveRDS(df3, file=paste0(d35, "finrecent_agg.rds"))
rm(df, df2, df3)

# df3 %>%
#   filter(year==2018, aggvar=="osr", level==2)


#****************************************************************************************************
#                ONETIME: Get historical database ####
#****************************************************************************************************
# Work SLOWLY, getting only a few variables at a time

# Historical database - revenues ####
revfn <- "1_Revenues.xlsx"

df <- read_excel(paste0(gfd, revfn))
glimpse(df)
count(df, `Type Code`) # not sure why this is read as character
count(df, `State Code`, `Type Code`, ID, Name) %>% data.frame
# the file is in pretty good shape; begin cleaning

# do a few checks:
bad <- filter(df, str_sub(ID, 1, 2) != `State Code`) # good, 0 obs
bad <- filter(df, str_sub(ID, 4, 4) != `Type Code`) # good, 0 obs
temp <- df %>% select(year=Year4, `Type Code`, stcode=`State Code`, Name) %>%
  mutate(stabbr=stcodes$stabbr[match(stcode, stcodes$stcen)],
         stabbrnm=str_sub(Name, 1, 2),
         good=stabbr==stabbrnm)
count(temp, good)
count(filter(temp, is.na(good)), `Type Code`, stcode, Name, stabbr, stabbrnm)
rm(temp, bad)
# the only problem is with stcode 52, `Type Code` 0 and 4, and we'll be dropping those anyway

# clean up the wide file
dfw <- df %>% rename(year=Year4) %>%
  mutate(level=as.numeric(`Type Code`),
         stabbr=stcodes$stabbr[match(`State Code`, stcodes$stcen)]) %>%
  filter(level %in% 1:3) %>%
  select(-c(Sort_Code, `Survey Year`, ID, `State Code`, `Type Code`, Name, Version,
            Source, `Revise Date`, `Data Flag`), -contains("Population"),
         -contains("Personal Income"), -contains("Year of"))
glimpse(dfw)
count(dfw, level)
count(dfw, stabbr)
count(dfw, year)

# create and clean a long file


# extract item code from variable, and create vnames as needed
dfl <- dfw %>% gather(variable, value, -level, -stabbr, -year) %>%
  mutate(ic=get.inparens(variable))
glimpse(dfl)
ht(dfl)
count(dfl, variable)

saveRDS(dfl, file=paste0(gfd, "revhist_clean.rds"))


# Historical database - expenditures ####
expfn <- "2_Expenditures.xlsx"

df <- read_excel(paste0(gfd, expfn))
glimpse(df)
count(df, `Type Code`) # not sure why this is read as character; 0-9
count(df, `State Code`, `Type Code`, ID, Name) %>% data.frame
# the file is in pretty good shape; begin cleaning

# do a few checks:
bad <- filter(df, str_sub(ID, 1, 2) != `State Code`) # good, 0 obs
bad <- filter(df, str_sub(ID, 4, 4) != `Type Code`) # good, 0 obs
temp <- df %>% select(year=Year4, `Type Code`, stcode=`State Code`, Name) %>%
  mutate(stabbr=stcodes$stabbr[match(stcode, stcodes$stcen)],
         stabbrnm=str_sub(Name, 1, 2),
         good=stabbr==stabbrnm)
count(temp, good) # 116 obs where not good
count(filter(temp, is.na(good)), `Type Code`, stcode, Name, stabbr, stabbrnm)
rm(temp, bad)
# the only problem is with stcode 52, `Type Code` 0 and 4, and we'll be dropping those anyway

# clean up the wide file
names(df)
dfw <- df %>% rename(year=Year4) %>%
  mutate(level=as.numeric(`Type Code`),
         stabbr=stcodes$stabbr[match(`State Code`, stcodes$stcen)]) %>%
  filter(level %in% 1:3) %>%
  select(-c(Sort_Code, `Survey Year`, ID, `State Code`, `Type Code`, Name))
glimpse(dfw)
count(dfw, level)
count(dfw, stabbr)
count(dfw, year) %>% data.frame

# create and clean a long file
# extract item code from variable, and create vnames as needed
dfl <- dfw %>% gather(variable, value, -level, -stabbr, -year) %>%
  mutate(ic=get.inparens(variable))
glimpse(dfl)
ht(dfl)
count(dfl, variable)
saveRDS(dfl, file=paste0(gfd, "exphist_clean.rds"))



# Historical database - debt and cash ####
debtcashfn <- "3_Debt_and_Cash_and_Securities.xlsx"

df <- read_excel(paste0(gfd, debtcashfn))
glimpse(df)
count(df, `Type Code`) # not sure why this is read as character; 0-9
count(df, `State Code`, `Type Code`, ID, Name) %>% data.frame
# the file is in pretty good shape; begin cleaning

# do a few checks:
bad <- filter(df, str_sub(ID, 1, 2) != `State Code`) # good, 0 obs
bad <- filter(df, str_sub(ID, 4, 4) != `Type Code`) # good, 0 obs
temp <- df %>% select(year=Year4, `Type Code`, stcode=`State Code`, Name) %>%
  mutate(stabbr=stcodes$stabbr[match(stcode, stcodes$stcen)],
         stabbrnm=str_sub(Name, 1, 2),
         good=stabbr==stabbrnm)
count(temp, good) # 116 obs where not good
count(filter(temp, is.na(good)), `Type Code`, stcode, Name, stabbr, stabbrnm)
rm(temp, bad)
# the only problem is with stcode 52, `Type Code` 0 and 4, and we'll be dropping those anyway

# clean up the wide file
names(df)
dfw <- df %>% rename(year=Year4) %>%
  mutate(level=as.numeric(`Type Code`),
         stabbr=stcodes$stabbr[match(`State Code`, stcodes$stcen)]) %>%
  filter(level %in% 1:3) %>%
  select(-c(Sort_Code, `Survey Year`, ID, `State Code`, `Type Code`, Name))
glimpse(dfw)
count(dfw, level)
count(dfw, stabbr)
count(dfw, year) %>% data.frame

# create and clean a long file
# extract item code from variable, and create vnames as needed
dfl <- dfw %>% gather(variable, value, -level, -stabbr, -year) %>%
  mutate(ic=get.inparens(variable))
glimpse(dfl)
ht(dfl)
count(dfl, variable)
saveRDS(dfl, file=paste0(gfd, "debtcashhist_clean.rds"))

# Combine the revenue, expenditure, and debtcash historical files, fix NAs, and save ####
dfr <- readRDS(file=paste0(gfd, "revhist_clean.rds"))
dfx <- readRDS(file=paste0(gfd, "exphist_clean.rds"))
dfd <- readRDS(file=paste0(gfd, "debtcashhist_clean.rds"))
# NA value:  -11111
df <- bind_rows(dfr, dfx, dfd)
glimpse(df)
count(df, nna=value==-11111) # 437,554
count(df, nna=round(value)==-11111) # 437,554
count(df, nna=is.na(value))
df2 <- df %>% mutate(value=ifelse(round(value == -11111), NA, value)) %>%
  filter(!is.na(value))
saveRDS(df2, file=paste0(gfd, "finhist_clean.rds"))



#****************************************************************************************************
#                Get selected aggregates from historical database using my aggnames ####
#****************************************************************************************************
dfl <- readRDS(file=paste0(gfd, "finhist_clean.rds"))

vars <- count(dfl, variable)
grep("fed", vars$variable, ignore.case=TRUE, value=TRUE)

# create recipe for vnames of selected variables
# DO NOT PUT SPACES BETWEEN variable names, or will have to trim them
recipe.s <- "aggvar,variable,ic
totrev.tot, Total Revenue,
totrev.gen, General Revenue,
fedigr, Total Fed IG Revenue,
osr, Gen Rev-Own Sources,
tottax, Total Taxes,
proptax, , T01
gst, , T09
iit, , T40
capoutx.gen, General Capital Outlay,
totx.gen, General Expenditure,
"
recipe <- read_csv(recipe.s) %>% mutate_all(str_trim)
recipe

# put vname on file using recipe
recic <- filter(recipe, ic!="")
recvar <- filter(recipe, variable!="")

finhist <- dfl %>% mutate(aggvaric=recic$aggvar[match(ic, recic$ic)],
                          aggvarvar=recvar$aggvar[match(variable, recvar$variable)],
                          aggvar=ifelse(!is.na(aggvaric), aggvaric, aggvarvar)) %>%
  select(-aggvaric, -aggvarvar) %>%
  select(year, level, stabbr, variable, aggvar, ic, value)
count(finhist, aggvar)
glimpse(finhist)

saveRDS(finhist, file=paste0(gfd, "finhist_agg.rds"))



#****************************************************************************************************
#                Combine history and recent to create final slgfin file ####
#****************************************************************************************************
# levf=factor(level, levels=1:3, labels=c("State-local", "State", "Local")),

finhist_agg <- readRDS(file=paste0(gfd, "finhist_agg.rds"))
finrecent_agg <- readRDS(file=paste0(d35, "finrecent_agg.rds"))
# saveRDS(df3, file=paste0(d35, "finrecent.rds"))

ht(finhist_agg)
ht(finrecent_agg)
summary(finrecent_agg)

count(finhist_agg, aggvar)
count(finrecent_agg, aggvar) %>% data.frame

count(filter(finhist_agg, !is.na(aggvar)), year) %>% data.frame
count(finrecent_agg, year) %>% data.frame

# ok, let's use recent for 2004+ (look at this decision more carefully, eventually)
fh.stack <- filter(finhist_agg, !is.na(aggvar), year<2004) %>% select(-variable) # reduce file before stacking
ht(fh.stack)
fr.stack <- filter(finrecent_agg, year>=2004)

# do preliminary stack, check for dups and oddities, then finalize
stack <- bind_rows(fh.stack, fr.stack)
ht(stack) # note different treatment of ic = "" or NA
anyDuplicated(select(stack, year, level, stabbr, aggvar)) # good no dups
glimpse(stack)
count(stack, year) %>% data.frame

var <- "tottax" # proptax totx.gen tottax
stack %>% 
  filter(level==1, stabbr=="US", aggvar==var, year>=2000) %>%
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point() 

count(stack, aggvar) %>% data.frame
count(stack, stabbr)
count(stack, level)
count(stack, ic) # we have both "" and NA

# ok, now finalize
slgfin <- stack %>% 
  mutate(levf=factor(level, levels=1:3, labels=c("State-local", "State", "Local")),
         ic=ifelse(ic=="", NA, ic)) %>%
  select(stabbr, level, levf, aggvar, ic, year, value) %>%
  arrange(stabbr, level, levf, aggvar, ic, year)
# check the changed vars
count(slgfin, level, levf)
count(slgfin, ic)

usethis::use_data(slgfin, overwrite = TRUE)


