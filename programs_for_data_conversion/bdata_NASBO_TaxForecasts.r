

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
nasbod <- paste0("D:/Dropbox/NASBO/LATEST Data/")
fssd <- "./data-raw/NASBO_TaxForecasts/"

fn <- "NASBO-FallFSS_12-10-2014.xlsx"
path <- paste0(nasbod, fn)

#****************************************************************************************************
#                read and save data ####
#****************************************************************************************************
# NASBO data:
#   - original revenue estimates (oe) and preliminary actuals (ce - current estimate) for pit, cit, and sales tax, 
# from Fall Fiscal Survey of the States for a given year.
# Conveniently, the "year" of the fall survey is also the fiscal year ending in that year. For example,
# the 2008 Fiscal Survey (December 2008) reports on projections and actuals for the 2007-08 fiscal year.

# sheets are named 1987-F to 2009-F
# each has columns:
# State  StAbbr	STOE	STCE	PITOE	PITCE	CITOE	CITCE	TRC
# ST=sales tax, PIT=personal income tax, and CIT is corporate income tax
# TRC is an indicator as to whether actual was above or below projection - we do not need it
# OE is original estimate and CE is "current" (final) estimate
# there is no CIT data until 1990
# every sheet ends with stabbr=US so we'll use that to decide what data to keep

vnames<-c("stname", "stabbr", "stoe", "stce", "pitoe", "pitce", "citoe", "citce", "trc")

excel_sheets(path)

readyear<-function(year) { # Read a single year of data. Call this repeatedly to get all years
  print(year)
  df <- read_excel(path, sheet=paste0(year, "-F")) # note that I had to remove space after F for 1987
  df <- df[, 1:length(vnames)] # drop stray columns
  names(df) <- vnames
  lastrow <- which(df$stabbr=="US" & str_detect(df$stname, "United States")) # identify the last row of data, uniquely
  df <- df[1:lastrow, ]
  dfl <- df %>% gather(variable, value, -stname, -stabbr) %>%
    mutate(value=cton(value),
           year=year)
  return(dfl)
}
# df <- readyear(1987)

df <- ldply(1987:2014, readyear, .progress="text")
glimpse(df)
count(df, year)
count(df, variable)
count(df, stabbr, stname)
count(df, nchar(stabbr))
df2 <- df %>% mutate(variable=as.character(variable)) %>%
  select(stabbr, year, variable, value) %>%
  filter(variable!="trc")
glimpse(df2)
count(df2, stabbr)

saveRDS(df2, file=paste0(fssd, "nasbodataraw.rds"))


#****************************************************************************************************
#                    Replace selected California data with Lucy-collected data, save revised R file    ####
#****************************************************************************************************
# get the revised/improved California data, which includes 2001 and 2009, and compare with California in the raw data
# once we are convinced it is good, save to R file for LATER combination with the raw data

calraw <- filter(readRDS(file=paste0(fssd, "nasbodataraw.rds")), stabbr=="CA") %>% mutate(type="raw")
glimpse(calraw) # note that all are char except for fyear

# create a similar file with the info obtained by Lucy from California DOF
#          first two sets of numbers should be same as NASBO, next 2 are DOF estimates of the "current estimate"
# i.e., the actual collections -- we want _dofest -- even though not the latest, they are comparable to how we are measuring
# other states
vnames <- c("year", "stoe_nasbo", "pitoe_nasbo", "citoe_nasbo", "sumoe_nasbo", "junk1",
                  "stce_nasbo", "pitce_nasbo", "citce_nasbo", "sumce_nasbo", "junk2",
                  "stce_dofest", "pitce_dofest", "citce_dofest", "sumce_dofest", "junk3",
                  "stce_doffinal", "pitce_doffinal", "citce_doffinal", "sumce_doffinal")
calnew <- read_excel(path, sheet="California", col_names=vnames, col_types=rep("text", length(vnames)))

calnew <- calnew %>% mutate(year=as.integer(str_sub(year, 1, 4)) + 1) %>%
  filter(year %in% 1998:2012) %>% # we only have CA dat from CA for these years
  select(-contains("junk"), -contains("sum")) %>%
  gather(variable, value, -year) %>%
  separate(variable, c("variable", "type")) %>%
  mutate(year=as.integer(year),
         value=cton(value), 
         stabbr="CA")
glimpse(calnew)
count(calnew, variable)

# now compare raw and updated data; if they are good, then replace the cal data in the file with calnew
comp <- bind_rows(calraw, calnew) %>%
  spread(type, value)
comp

# compare the original estimates in the raw data and in the calnew data
comp %>% select(-doffinal, -dofest) %>%
  filter(str_sub(variable, -2, -1)=="oe", !is.na(nasbo), variable!="sumoe") %>%
  mutate(diff=nasbo - raw) %>%
  filter(diff!=0)
# good the dof-nasbo data match the nasbo-raw data for oe

# now compare the current estimates in the raw data to the dof final estimates
comp %>% filter(str_sub(variable, -2, -1)=="ce", !(is.na(dofest) & is.na(doffinal))) %>%
  mutate(diff=nasbo - raw,
         diff2=dofest - raw) %>%
  arrange(-abs(diff)) # ok, all the data match except that 2001 and 2009 current ests had 0 in the revised rather than NA

# now create revised data with the good new california data -- dofest
glimpse(calnew)
calnewkeep <- calnew %>% filter(type=="dofest") %>% rename(value.new=value)

df <- readRDS(file=paste0(fssd, "nasbodataraw.rds"))
glimpse(df)
count(df, variable)

df2 <- full_join(df, calnewkeep)
# filter(df2, stabbr=="CA") %>% data.frame
df2 <- full_join(df, calnewkeep) %>%
  mutate(value=ifelse(type=="dofest" & !is.na(value.new), value.new, value)) %>% # MUST include na check for value.new!!!
  select(-type, -value.new)
glimpse(df2)

df %>% filter(stabbr=="AL", year==1987)
df2 %>% filter(stabbr=="AL", year==1987)

saveRDS(df2, file=paste0(fssd, "nasbodataraw_revised.rds"))

#****************************************************************************************************
#                Data cleaning ####
#****************************************************************************************************
# Data cleaning steps
# 0) remove all US records and all NA records
# 1) if original OR current estimate is missing for a tax in a given year-state, drop the observation
# 2) if original=current (perfect forecast) for 2 or more taxes in a given state-year,
#    assume the state provided NASBO with erroneous data drop ALL taxes for that state-year
# 3) calculate absolute forecast errors, find top 1% for each tax, drop observations where absolute forecast error
#    is an outlier
# tax        p01      p99
# (chr)      (dbl)    (dbl)
# 1   cit 0.00000000 97.29189
# 2   pit 0.07084706 29.87611
# 3    st 0.00000000 20.11019
# 4) calculate sums of taxes, US records, and forecast errors

# get the data
df <- readRDS(file=paste0(fssd, "nasbodataraw_revised.rds"))
glimpse(df)
anyDuplicated(select(df, stabbr, year, variable)) # good, no duplicate stabbr-year-variable combinations
count(df, stabbr)
count(df, is.na(value))
count(df, variable)

# 0) remove all US records and all NA records
df0 <- df %>% filter(stabbr!="US", !is.na(value))

# we only want to use data where BOTH the original estimate and the current estimate are non-missing
# 1) if original OR current estimate is missing for a tax in a given year-state, drop the observation
df1 <- df0 %>% separate(variable, c("tax", "esttype"), -3) %>%
  spread(esttype, value) %>%
  filter(!(is.na(ce) | is.na(oe)))
glimpse(df1)

# 2) if original=current (perfect forecast) for 2 or more taxes in a given state-year,
#    assume the state provided NASBO with erroneous data drop ALL taxes for that state-year
perfect <- df1 %>% group_by(stabbr, year) %>%
  summarise(n=n(), nperfect=sum(ce==oe)) %>%
  filter(nperfect>=2)
perfect
# df1 %>% filter(stabbr=="AL", year==1989) # take a look at one or more "perfect" forecasts
# df1 %>% filter(stabbr=="ID", year==2007) # take a look at one or more "perfect" forecasts

df2 <- anti_join(df1, select(perfect, stabbr, year)) # drop state-year combinations that had perfect forecasts

# 3) calculate absolute forecast errors, find top 1% for each tax, drop observations where absolute forecast error
#    is an outlier (PIT: >28.9%, ST: >20.0%; CIT: >94.3% -- based on
#             calculations I had done of the top 1% of absolute errors) and
#     therefore potentially a reporting error, then set current AND original
#     estimates for that tax-state-year to missing
glimpse(df2)
df3 <- df2 %>% mutate(err=ce - oe,
                      pcterr=err / ce * 100,
                      abspcterr=abs(pcterr))
pany <- function(x, p) {as.numeric(quantile(x, p, na.rm=TRUE))}
outlierpcts <- df3 %>% group_by(tax) %>%
  summarise(p01=pany(abspcterr, .01),
            p99=pany(abspcterr, .99))
idx <- match(df3$tax, outlierpcts$tax)
df3a <- df3 %>% mutate(outlierpct=outlierpcts$p99[idx]) %>%
  filter(abspcterr<=outlierpct) %>%
  select(stabbr, year, tax, ce, oe)
  
# 4) calculate sums of taxes, US records, and forecast errors
glimpse(df3a)
usrecs <- df3a %>% group_by(year, tax) %>% 
  summarise(ce=sum(ce), oe=sum(oe)) %>%
  mutate(stabbr="US")
allrecs <- bind_rows(df3a, usrecs) %>%
  gather(esttype, value, ce, oe)
sumrecs <- allrecs %>% group_by(stabbr, year, esttype) %>%
  summarise(value=sum(value)) %>%
  mutate(tax="sumbig3")
nasbofcerr <- bind_rows(allrecs, sumrecs) %>% 
  spread(esttype, value) %>%
  mutate(err=ce - oe,
         pcterr=err / ce * 100,
         abspcterr=abs(pcterr))
glimpse(nasbofcerr)

use_data(nasbofcerr, overwrite = TRUE)




