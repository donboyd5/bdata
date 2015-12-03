# 12/3/2015


#****************************************************************************************************
#                Overview ####
#****************************************************************************************************

# Purpose: construct a single state-local government finance file with one record:
#   per year
#   per state (including DC, and US total - 52 groups), using state postal abbreviation
#   per level of governemnt (state-local, state, local)
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

library(btools)
library(bdata) # is it ok to use bdata when developing bdata?
library(devtools)
library(plyr)
library(dplyr)
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library(ggplot2)
library(magrittr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)



#****************************************************************************************************
#                define directories and files ####
#****************************************************************************************************
cslgdir <- paste0("./data-raw/census_slgfin_annual/")
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
#                download files as needed and save in a project subdirectory ####
#****************************************************************************************************
# http://www2.census.gov/govs/local/00statetypepu.zip
for(yr in 2000:2012)  {
  y2 <- str_sub(yr, 3, 4)
  if(yr %in% c(2001, 2003)) fn <- paste0(y2, "statetypepu.txt") else
    fn <- paste0(y2, "statetypepu.zip")
  url <- paste0("http://www2.census.gov/govs/local/", fn)
  print(url)
  download.file(url, paste0(d35, fn), mode="wb")
}

# Note that the 2013 data do not appear to be here!!!


# get the various historical databases (note that I store these elsewhere on my computer)
urlbase <- "http://www2.census.gov/pub/outgoing/govs/special60/"
fn <- "Govt_Finances.zip" # Govt_Finances.zip; also of interest: hist_fin.zip, rex-dac.zip
download.file(paste0(urlbase, fn), paste0(spec60, fn), mode="wb")




#****************************************************************************************************
#                Individual year files ####
#****************************************************************************************************
# Work SLOWLY, getting only a few variables at a time
itemfiles <- character(13)
for(yr in 2000:2012) {
  i <- yr - 1999
  if(yr==2000) itemfiles[i] <- "00statetypepu_1108.TXT" else
    if(yr==2002) itemfiles[i] <- "2002State_By_type_Summaries24.txt" else
      itemfiles[i] <- paste0(str_sub(yr, 3, 4), "statetypepu.txt")
}
itemfiles

# now read the item files
# 2002 is a different format than the other years
f <- function(yr) {
  fn <- itemfiles[yr - 1999]
  starts <- c(1, 3, 5, 9)
  ends <- c(2, 3, 8, 20)
  if(yr==2002) {
    starts <- c(1, 3, 15, 19)
    ends <- c(2, 3, 17, 29)
  }
  df <- read_fwf(paste0(d35, fn), fwf_positions(starts, ends, col_names=c("stcode", "level", "ic", "value")),
           col_types="cicd")
  df$year <- yr
  return(df)
}
df <- ldply(2000:2012, f)
ht(df)

# slight cleaning, then save
df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(stcode, stcodes$stcen)])
count(df2, stcode, stabbr) # note that there are both DC and US records
# check DC and US by year
count(df2, year, usrec=stabbr=="US") %>% spread(usrec, n) # note that there are not non-US recs in 2001, 2003
count(df2, year, dcrec=stabbr=="DC") %>% spread(dcrec, n) # note that there are no DC recs in 2001, 2003
count(filter(df2, stabbr!="US"), year)
count(df2, ic) %>% data.frame
count(df2, level, year) %>% spread(level, n)
df3 <- df2 %>% filter(level %in% 1:3) %>%
  select(year, stabbr, level, ic, value) %>%
  arrange(stabbr, level, ic, year)
# one last check, for duplicates
anyDuplicated(select(df3, year, stabbr, level, ic)) # good, no dups

saveRDS(df3, file=paste0(d35, "finrecent.rds"))
rm(df, df2, df3)


#****************************************************************************************************
#
#                Get recipe data frame for creating aggregates from item code data ####
#
#****************************************************************************************************
# get recipe data frame
fn <- "SLGFinAggregationAnalysis(30).xlsx"
rdf <- read_excel(paste0(cslgdir, fn), sheet="recipesLong", col_names = FALSE)
rdf[1:10, 1:ncol(rdf)]
names(rdf) <- paste(rdf[5, ], rdf[4, ], sep=".")

rdfl <- rdf[-c(1:4), ] %>% gather(variable, ic) %>%
  filter(ic !="") %>%
  separate(variable, c("aggvar", "recipegroup"), sep=-7, remove=FALSE) %>%
  mutate(recipegroup=gsub(".", "", recipegroup, fixed=TRUE),
         indicator=1)

ht(rdfl)
# count(rdfl, recipegroup)
# count(rdfl, aggvar, recipegroup) %>% spread(recipegroup, n) %>% data.frame


#****************************************************************************************************
#
#                Create aggregates from recent item code data ####
#
#****************************************************************************************************
df <- readRDS(file=paste0(d35, "finrecent.rds"))

# prepare the data file
df2 <- df %>% mutate(recipegroup=ifelse(year<2005, "2004m", "2005p"))

df3 <- inner_join(select(rdfl, -variable), df2, by=c("ic", "recipegroup")) %>%  # merge a few secs faster than inner_join but stick with ij
  group_by(stabbr, level, year, aggvar) %>%
  summarise(value=sum(value, na.rm=TRUE))

count(df3, aggvar) %>% data.frame

saveRDS(df3, file=paste0(d35, "finrecent_agg.rds"))
rm(df, df2, df3)



#****************************************************************************************************
#
#                Get historical database ####
#
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
#
#                Get selected aggregates from historical database using my aggnames ####
#
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
recipe <- read_csv(recipe.s) %>% mutate_each(funs(str_trim))
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

# save(revhist, file=paste0(gfd, "revhist.rda"), compress="bzip2") # this is slower and takes more space than rds

saveRDS(finhist, file=paste0(gfd, "finhist_agg.rds"))



#****************************************************************************************************
#
#                Combined file ####
#
#****************************************************************************************************
# levf=factor(level, levels=1:3, labels=c("State-local", "State", "Local")),

finhist_agg <- readRDS(file=paste0(gfd, "finhist_agg.rds"))
finrecent_agg <- readRDS(file=paste0(d35, "finrecent_agg.rds"))

ht(finhist_agg)
ht(finrecent_agg)

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
qplot(year, value, data=filter(stack, level==1, stabbr=="US", aggvar=="totx.gen"), geom=c("point", "line")) # units look the same
count(stack, aggvar) %>% data.frame
count(stack, stabbr)
count(stack, level)
count(stack, ic) # we have both "" and NA

# ok, now finalize
slgfin <- stack %>% mutate(levf=factor(level, levels=1:3, labels=c("State-local", "State", "Local")),
                           ic=ifelse(ic=="", NA, ic)) %>%
  select(stabbr, level, levf, aggvar, ic, year, value) %>%
  arrange(stabbr, level, levf, aggvar, ic, year)
# check the changed vars
count(slgfin, level, levf)
count(slgfin, ic)

use_data(slgfin, overwrite = TRUE)

