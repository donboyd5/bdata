# bdata_statepop.r
# Don Boyd
# 12/2/2015

# create file with annual state population from 1900 forward

#  Sources:
#  http://www.census.gov/popest/  for data in general
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

library(dplyr)
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library(tidyr)
library(gdata)
library(readr)
library(bdata)

# library(btools)


# popdir <- paste0("./data-raw/statepop/")
popdir <- "D:/Data/bdata_package_sourcedata/statepop/"

# CAUTION: we do not have intercensal data for 1970, 1980 and must use decennial for those years(???)

getpop <- function(fname){
  firstyear <- 1900 + as.numeric(substr(fname, 3, 4))

  # define widths and column names
  if(firstyear %in% seq(1900, 1940, 10)) { # set column widths and column names for the a and b parts of the file
      wida <- c(17,9,9,9,9,9,9)
      cna <- c("state",firstyear:(firstyear+5)) # col names for a part
      widb <- c(17,9,9,9,9)
      cnb <- c("state",(firstyear+6):(firstyear+9))
    } else if(firstyear %in% c(1950)) { # now we include the Census data as well as intercensal
      wida <- c(16,8,8,9,9,8,9)
      cna <- c("state",paste(firstyear,"decennial1",sep=""),firstyear:(firstyear+4)) # April data at start
      widb <- c(16,8,8,9,9,8,9)
      cnb <- c("state",(firstyear+5):(firstyear+9),paste(firstyear+10,"decennial2",sep="")) # April data at end
    } else if(firstyear %in% c(1960)) { # now we include the Census data as well as intercensal
      wida <- c(12,9,9,9,9,9,9)
      cna <- c("state",paste(firstyear,"decennial1",sep=""),firstyear:(firstyear+4)) # April data at start
      widb <- c(12,9,9,9,9,9,9)
      cnb <- c("state",(firstyear+5):(firstyear+9),paste(firstyear+10,"decennial2",sep="")) # April data at end
    } else if(firstyear %in% c(1970)) { # now we include the Census data as well as intercensal
      wida <- c(-4,2,10,10,10,10,10,10)
      cna <- c("state",paste(firstyear,"decennial1",sep=""),(firstyear+1):(firstyear+5)) # April data at start
      widb <- c(-4,2,10,10,10,10,10)
      cnb <- c("state",(firstyear+6):(firstyear+9),paste(firstyear+10,"decennial2",sep="")) # April data at end
    } else if(firstyear %in% c(1980)) { # now we include the Census data as well as intercensal
      wida <- c(2,-1,10,10,10,10,10)
      cna <- c("state",paste(firstyear,"decennial1",sep=""),(firstyear+1):(firstyear+4)) # April data at start
      widb <- c(2,-1,10,10,10,10,10,10)
      cnb <- c("state",(firstyear+5):(firstyear+9),paste(firstyear+10,"decennial2",sep="")) # April data at end
    }

  # define start1, start2, and nrows for each. important to set blank.lines.skip=FALSE so we count lines properly
  if(firstyear %in% c(1900, 1910, 1920)) {start1<-18; start2<-76
    } else if(firstyear %in% c(1930))  {start1<-18; start2<-77
    } else if(firstyear %in% c(1940))  {start1<-16; start2<-74
    } else if(firstyear %in% c(1950))  {start1<-19; start2<-84
    } else if(firstyear %in% c(1960))  {start1<-19; start2<-81
    } else if(firstyear %in% c(1970))  {start1<-15; start2<-68
    } else if(firstyear %in% c(1980))  {start1<-11; start2<-70
    }

  nrow <- start2 - start1 # this is an overestimate of what to read - we will have to drop some
  fullname <- paste0(popdir, fname, ".txt")
  a <- read.fwf(fullname, widths=wida, col.names=cna, blank.lines.skip=FALSE, skip=start1-1, n=nrow, colClasses="character")
  b <- read.fwf(fullname, widths=widb, col.names=cnb, blank.lines.skip=FALSE, skip=start2-1, n=nrow, colClasses="character")
  ab <- bind_rows(gather(a, variable, value, -state), gather(b, variable, value, -state))
  # ab <- rbind(melt(a,id="state"),melt(b,id="state"))
  # do some minor cleanup
  ab$value <- btools::cton(ab$value)
  ab <- subset(ab, !is.na(ab$value))
  if(firstyear>=1970) ab$value <- ab$value / 1000
  ab$state <- gdata::trim(ab$state)
  ab <- subset(ab, !(substr(state,1,2) %in% c("","  ")))
  ab$state[ab$state=="U.S."] <- "US"
  ab$stabbr <- ab$state
  ab$state <- NULL
  ab$variable <- as.character(ab$variable)
  ab$year <- as.numeric(substr(ab$variable, 2, 5))
  ab <- subset(ab, !is.na(year))
  ab$esttype <- ifelse(grepl("decennial",ab$variable), substr(ab$variable,6,nchar(ab$variable)), "intercensal")
  print(head(ab)); print(tail(ab)); print(count(ab, stabbr)); print(count(ab, variable)) # debug - otherwise comment out
  return(ab)
}

pop1900 <- getpop("st0009ts") # pop for the decade beginning 1900...
pop1910 <- getpop("st1019ts")
pop1920 <- getpop("st2029ts")
pop1930 <- getpop("st3039ts")
pop1940 <- getpop("st4049ts")
pop1950 <- getpop("st5060ts") # note new file format and naming
pop1960 <- getpop("st6070ts")
pop1970 <- getpop("st7080ts")
pop1980 <- getpop("st8090ts")


# later files have different formats
# get 1990-2000
fn <- "CO-EST2001-12-00.csv"
tpop <- read.csv(paste0(popdir, fn), header=FALSE, skip=3, colClasses="character")
names(tpop) <- c("state", "1990decennial1", as.character(1990:1999), "2000decennial2")
tpop$state <- trim(gsub("\\s"," ",tpop$state)) # replace goofy ascii characters with spaces so we can work with state names
tpopl <- gather(tpop, variable, value, -state)
tpopl$stabbr <- factor(tpopl$state, levels=stcodes$stname, labels=stcodes$stabbr)
tpopl$stabbr[tpopl$state=="USA"] <- "US"
tpopl$year <- as.numeric(substr(tpopl$variable,1,4))
tpopl$value <- btools::cton(tpopl$value)/1000
tpopl <- filter(tpopl, !(is.na(value) | is.na(year)))
tpopl$variable <- as.character(tpopl$variable)
tpopl$esttype <- ifelse(grepl("decennial",tpopl$variable), substr(tpopl$variable,5,nchar(tpopl$variable)), "intercensal")
count(tpopl, year)
count(tpopl, state, stabbr)
tpopl$state <- NULL
head(tpopl); tail(tpopl)
pop1990 <- tpopl
head(pop1990)


# get 2000-2010
fn <- "ST-EST00INT-01.csv" # this was created Sep 2011 and is most current as of 2015-04-21
tpop <- read.csv(paste0(popdir, fn), header=FALSE, skip=4, colClasses="character")
names(tpop) <- c("state", "2000decennial1", as.character(2000:2009), "2010decennial2", "2010")

pop2000 <- tpop %>% mutate(state=trim(gsub("\\s", " ", state)),  # replace goofy ascii characters with spaces so we can work with state names
                        state=gsub(".", "", tpop$state, fixed=TRUE),
                        stabbr=factor(state, levels=stcodes$stname, labels=stcodes$stabbr)) %>%
  filter(stabbr %in% stcodes$stabbr) %>%
  select(-state) %>%
  gather(variable, value, -stabbr) %>%
  mutate(variable=as.character(variable),
         esttype=ifelse(grepl("decennial", variable), substr(variable, 5, nchar(variable)), "intercensal"),
         value=btools::cton(value)/1000,
         year=as.numeric(substr(variable, 1, 4)))  %>%
  filter(year!=2010) %>% # drop 2010 intercensal from pop2000 since we have it in the 2010 file
  select(-variable)
glimpse(pop2000)


# get 2010-2014 (and beyond)...get the latest file from the web
# url<-"http://www.census.gov/popest/data/national/totals/2013/files/NST_EST2013_ALLDATA.csv"
# fn<-"NST_EST2011_ALLDATA.csv" #
# tpop <- read.csv(paste0(popdir, fn), header=TRUE, skip=0, colClasses="character")
# url <- "http://www.census.gov/popest/data/state/totals/2014/tables/NST-EST2014-01.csv"
# tpopold <- read.csv(url, header=TRUE, skip=0, colClasses="character")
# tpop <- read_csv(url)

tpop <- read_csv(paste0(popdir, "NST-EST2015-01.csv"))
glimpse(tpop)
names(tpop) <- c("state", "2010decennial1", "2010estbase", as.character(2010:2015))
# decennial1 means it is the only decennial census after the year in question
head(tpop)
pop2010 <- tpop %>% select(-`2010decennial1`, -`2010estbase`) %>%
  mutate(state=gsub(".", "", state, fixed=TRUE),
         stabbr=factor(state, levels=stcodes$stname, labels=stcodes$stabbr)) %>%
  filter(stabbr %in% stcodes$stabbr) %>%
  select(-state) %>%
  gather(year, value, -stabbr) %>%
  mutate(year=as.numeric(gsub("[[:alpha:]]", "", year)),
         esttype="intercensal",
         value=btools::cton(value)/1e3) # put in thousands
head(pop2010)
tail(pop2010)
count(pop2010, year)


# combine files, finish cleaning data, save results
popall <- bind_rows(pop1900, pop1910, pop1920, pop1930, pop1940, pop1950, pop1960, pop1970, pop1980, pop1990, pop2000, pop2010) %>%
  select(-variable) %>%
  filter(stabbr %in% stcodes$stabbr)
glimpse(popall)
count(popall, year)
count(popall, stabbr)


# make final adjustments so that we have intercensal where possible, and decennial otherwise
# CAUTION: we do not have intercensal data for 1970, 1980 and must use decennial for those years (???)
spop.a <- filter(popall, esttype=="intercensal" | (year %in% c(1970, 1980) & esttype=="decennial1")) %>%
  select(stabbr, year, value, esttype) %>%
  arrange(stabbr, year)
comment(spop.a) <- c("Census pop, state and year 1900-2015, thousands, (July - except 1970 and 1980 where April decennial is used)")

devtools::use_data(spop.a, overwrite=TRUE)

# ALL DONE!! ####

spop.a %>% filter(stabbr=="CA", year>=1975) %>% data.frame

library(ggplot2)
qplot(year, log(value), data=filter(spop.a, stabbr %in% c("CA","FL","IL","NY","TX","VA")), colour=stabbr, geom=c("point","line"))

qplot(year, log(value), data=filter(spop.a, stabbr=="US" & esttype=="intercensal"), geom=c("point","line"))

qplot(year, log(value), data=filter(spop.a, stabbr=="US" & grepl("decennial",esttype)), geom=c("point","line"))


# Note: The April 1, 2000 Population Estimates base reflects changes to the Census 2000 population from the Count Question Resolution
# program and geographic program revisions.  See Geographic Terms and Definitions at http://www.census.gov/popest/geographic/ for a
# list of the states that are included in each region.
# Suggested Citation:
# Table 1: Annual Estimates of the Population for the United States, Regions, and States and for Puerto Rico: April 1, 2000 to July 1, 2006 (NST-EST2006-01)
# Source: Population Division, U.S. Census Bureau
# Release Date: December 22, 2006

# Note: The estimates are based on the 2010 Census and reflect changes to the April 1, 2010 population due to the Count Question Resolution program and geographic program revisions. See Geographic Terms and Definitions at http://www.census.gov/popest/about/geo/terms.html for a list of the states that are included in each region.  All geographic boundaries for the 2014 population estimates series are defined as of January 1, 2014.  For population estimates methodology statements, see http://www.census.gov/popest/methodology/index.html.
# Suggested Citation:
#   Table 1. Annual Estimates of the Resident Population for the United States, Regions, States, and Puerto Rico: April 1, 2010 to July 1, 2014 (NST-EST2014-01)
# Source: U.S. Census Bureau, Population Division
# Release Date: December 2014



