# 12/3/2015


#****************************************************************************************************
#                Overview ####
#****************************************************************************************************

# For now, simply read the existing file and save it.



#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
datdir <- "d:/data/"
edir <- paste0(datdir, "CensusEmploymentData/")


#****************************************************************************************************
#                load packages ####
#****************************************************************************************************


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

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")

library("stringi")

#****************************************************************************************************
#                Get previously cleaned 2010-and-earlier data ####
#****************************************************************************************************
edir <- "D:/Data/CensusEmploymentData/"

pub <- paste0(edir, "Special60\\Public_Emp\\")

df <- readRDS(paste0(edir, "pubemp_upto2010.rds"))
glimpse(df)

olddf <- readRDS(paste0(edir, "pub_emp_clean_trim.rds"))
glimpse(olddf)
count(olddf, vtype)

# hist_emp has national totals, not states
s60hist <- "D:/Data/CensusEmploymentData/Special60/hist_emp/"
s60histfn <- "3-State & Local Governments.csv" 

df <- read_csv(paste0(s60hist, s60histfn))
glimpse(df)


# emp_est has state by state
s60est <- "D:/Data/CensusEmploymentData/Special60/emp_est/"
s60estfn <- "Aggregates.csv" 

df <- read_csv(paste0(s60est, s60estfn))
glimpse(df)
count(df, Version)
count(df, Source)
count(df, ID, State, Type)
count(df, Month)
df %>% filter(Month=="VAR")
stcodes

df2 <- df %>% filter(State!="52", Type %in% 1:3) %>%
  mutate(stabbr=stcodes$stabbr[match(State, stcodes$stcen)],
         stabbr2=str_sub(Name, 1, 2),
         Month=ifelse(Month=="Mar", "MAR", Month)) %>%
  rename(year=Year4, month=Month, type=Type) %>%
  select(-Sort_Code, -ID, -Name, -Version, -Source, -Population)
glimpse(df2)
count(df2, State, stabbr, stabbr2) # check before deleting
count(df2, year, month) # 1957 APR; 1962-1995 OCT; 1997-2010 MAR

dfl <- df2 %>% select(-State, -stabbr2) %>%
  gather(variable, value, -stabbr, -year, -month, -type)
glimpse(dfl)
tmp <- count(dfl, variable) # FT_Emp, FT_Equiv, FT_Pay, Total_Emp, Total_Pay
# ft.emp ft.pay  tot.emp tot.pay fte.emp
vtypes <- c("FT_Emp", "FT_Pay", "Total_Emp", "Total_Pay", "FT_Equiv")
totals <- c("Total_Full_Time_Employees", "Total_Full_Time_Payrolls", "Total_Employees", "Total_Payrolls", "Total_Full_Time_Equivalent")
vtotals <- paste0("Total_", vtypes)
vtypes2 <- str_replace(vtypes, "_", "")
vtypef <- c("ft.emp", "ft.pay", "tot.emp", "tot.pay", "fte.emp")

replacepos <- function(s, pos, replace="") {str_sub(s, pos, pos) <- replace; return(s)}
replacepos("abcd", 3)
replacepos("abcd", 3, "z")

dfl2 <- dfl %>% mutate(v2=factor(variable, levels=totals, labels=vtotals) %>% as.character,
                       v3=ifelse(is.na(v2), variable, v2),
                       last=stri_locate_last_regex(v3, "\\_")[, 1],
                       v4=replacepos(v3, last),
                       last=stri_locate_last_regex(v4, "\\_")[, 1],
                       v5=replacepos(v4, last, ".")) %>%
  select(-v2, -v3, -v4, -last, -variable) %>%
  rename(variable=v5)
glimpse(dfl2)
tmp2 <- count(dfl2, variable)

dfl3 <- dfl2 %>%  separate(variable, c("activity", "vtype"), sep="\\.") %>%
  mutate(vtype=factor(vtype, levels=vtypes2, labels=vtypef))
count(dfl3, vtype)
count(dfl3, activity)
glimpse(dfl3)

saveRDS(dfl3, paste0(edir, "empest_2016.rds"))



#****************************************************************************************************
#                Get 2011+ data ####
#****************************************************************************************************


#****************************************************************************************************
#                Combine 2010-and-earlier data with 2011+ data, and save ####
#****************************************************************************************************
empest <- readRDS(paste0(edir, "empest_2016.rds"))
glimpse(empest)



#****************************************************************************************************
#                get and save data for the good years ####
#****************************************************************************************************
slgemp <- readRDS(paste0(edir, "empest_goodyears.rds"))
glimpse(slgemp)

use_data(slgemp, overwrite = TRUE)



#****************************************************************************************************
#                ONETIME: Get 2010 and prior ####
#****************************************************************************************************
# Public_Emp
pefn <- "Pub_Employ.csv"
system.time(df <- read.csv(paste0(pub, pefn), colClasses="character")) # 10 secs
head(df)
df[1:5, 1:16]
count(df, Year4) # %>% data.frame # note that there are more records in the census years
count(df, Month)
count(df, ID) # weird - when there are more than dplyr.print_max recs, it only prints 10 unless I change dplyr.print_min as well
count(df, State) # none missing
count(df, FIPS_State_Code) # 580 are missing
count(df, Type_Code)
count(df, Name)
count(df, Version) # A 10,768  B 222
count(df, Source)
d <- count(df, Type_Code, Name)

# Type_Code
# 0 National total
# 1 State-local
# 2 State
# 3 Local
# 4 Federal
# 5 County
# 6 Municipality/city
# 7 Township
# 8 Special District
# 9 School District

# clean the data
# CAUTION: stabbr in the Name does not always match true stabbr. Get it both ways, compare, and fix.
df2 <- df %>% mutate(Name=trim(Name),
                     nstabbr=str_sub(Name, 1, 2),
                     nstabbr=ifelse(nstabbr=="NA", "NATL", nstabbr),
                     cstabbr=as.character(factor(State, levels=stcodes$stcen, labels=stcodes$stabbr)))
count(df2, cstabbr, nstabbr, State, ID, Name) %>% data.frame
df2 %>% filter(cstabbr!=nstabbr) %>% select(Sort_Code, Year4, ID, State, Name, Version, Source, Population, Total_Employees, cstabbr) %>% arrange(Sort_Code)
df2[1:5, 1:18]
str(df2)

# pull a subset out, write to csv, and examine there
glimpse(df2)
tmp <- df2 %>% filter(cstabbr=="US", Type_Code=="1") %>% select(Year4, Type_Code, cstabbr, Name, Total_Employees, contains("_Total_emp")) %>%
  arrange(-Year4)
write.csv(tmp, paste0(pub, "slgall.csv"), row.names = FALSE)


# clean further and make a long file
# set up type factor
vtypef <- c("National", "State-local", "State", "Local", "Federal", "County", "Municipality/city", "Township", "Special District", "School District")
df3 <- df2 %>% select(-Sort_Code, -Year, -Month, -ID, -State, -Name, -Version, -Source, -Zero_Data_Flag, -Last_Date_Revised, -Data_Flag, -Population, -nstabbr) %>%
  select(stabbr=cstabbr, year=Year4, fipsst=FIPS_State_Code, type=Type_Code, everything()) %>%
  mutate_each(funs(as.numeric), year, fipsst, type) %>%
  mutate(typef=factor(type, levels=0:9, labels=vtypef))
glimpse(df3)
count(df3, type, typef)

# navals <- c("-11111", "-22222", "-33333", "-66666")
navals <- c(-11111, -22222, -33333, -66666)
dfl <- df3 %>% gather(variable, value, -year, -stabbr, -fipsst, -type, -typef) %>%
  mutate(value=as.numeric(value),
         value=ifelse(round(value) %in% navals, NA, value)) # this takes a little while  
ht(dfl)
filter(dfl, stabbr=="US", year==1944, type==2, variable=="Total_Employees")
filter(df3, stabbr=="US", year==1944, type==2)
glimpse(df3)
saveRDS(dfl, file=paste0(edir, "pubemp_upto2010.rds"))


#****************************************************************************************************
#
#                RUN ONCE: Clean 2010 and prior ####
#
#****************************************************************************************************
# Create a version of the data that is state-local, state, and local only, and that only has "elemental" categories - i.e.,
# categories at the lowest level of detail
pubemp <- readRDS(paste0(edir, "pubemp_upto2010.rds"))
head(pubemp)

# create a dataframe with variable, vtype, and activity, which we will use to create factors in the big data file
getvtypelong <- function(variable) {
  sufx <- c("_Total_Emp", "_Total_Pay", "_FT_Emp", "_FT_Pay", "_FT_Equiv", 
            "Total_Employees", "Total_Payrolls", "Total_Full_Time_Employees", "Total_Full_Time_Payrolls", "Total_Full_Time_Equivalent",
            "Avg_Monthly_Earn")
  vtypelong <- rep(NA, length(variable))
  for(i in 1:length(sufx)) vtypelong <- ifelse(grepl(sufx[i], variable), sufx[i], vtypelong)
  return(vtypelong)  # if we get here, there is nothing to return
}

getact <- function(variable, vtypelong){
  pos <- str_locate(variable, vtypelong) # find starting and ending positions
  activity <- ifelse(pos[, "start"]==1, "All", str_sub(variable, 1, pos[, "start"]-1) )
  activity <- ifelse(grepl("Avg_Monthly_Earn_", variable), str_replace(variable, "Avg_Monthly_Earn_", ""), activity)
  return(activity)
}

getvtype <- function(vtypelong) {
  sufx <- c("_Total_Emp", "_Total_Pay", "_FT_Emp", "_FT_Pay", "_FT_Equiv", 
            "Total_Employees", "Total_Payrolls", "Total_Full_Time_Employees", "Total_Full_Time_Payrolls", "Total_Full_Time_Equivalent",
            "Avg_Monthly_Earn")
  vtypes <- c("tot.emp", "tot.pay", "ft.emp", "ft.pay", "fte.emp", "tot.emp", "tot.pay", "ft.emp", "ft.pay", "fte.emp", "earn")
  vtype=as.character(factor(vtypelong, levels=sufx, labels=vtypes))
  return(vtype)
}

dfv <- count(pubemp, variable) %>%
  mutate(vtypelong=getvtypelong(variable), activity=getact(variable, vtypelong), vtype=getvtype(vtypelong))
glimpse(dfv)

# now use dfv to get vtype and activity for pubemp
glimpse(pubemp)


pubempa <- pubemp %>% select(-fipsst) %>% filter(!is.na(value))
idx <- match(pubempa$variable, dfv$variable)
pubempa %<>% mutate(stabbr=ifelse(is.na(stabbr), toupper(str_sub(typef, 1, 3)), stabbr),
                    vtype=as.character(dfv$vtype[idx]),
                    activity=dfv$activity[idx]) %>%
  select(stabbr, year, type, typef, variable, activity, vtype, value)

ht(pubempa)
count(pubempa, is.na(value))
count(pubempa, value==0) # 2m zeros, can I delete them?
count(pubempa, stabbr)
count(pubempa, vtype)
count(pubempa, activity)
count(pubempa, variable, vtype, activity) %>% data.frame
glimpse(pubempa)

# variable     n
# 1            Total_Employees 21490
# 2             Total_Payrolls 21490
# 3  Total_Full_Time_Employees 20876
# 4   Total_Full_Time_Payrolls 13752
# 5 Total_Full_Time_Equivalent 19817
# 6       Avg_Monthly_Earn_All 17223
# 7  Avg_Monthly_Earn_Teachers 13671
# 8     Avg_Monthly_Earn_Other 16201

saveRDS(pubempa, paste0(edir, "pub_emp_clean.rds"))

# finally, create slimmed down file with calc'd variables for other police, fire - by subtraction
df <- readRDS(paste0(edir, "pub_emp_clean.rds"))
df <- df %>% filter(type %in% 1:3, !stabbr %in% c("NAT", "FED"), vtype!="earn") %>% select(-variable)
glimpse(df)
count(df, vtype)
count(df, activity)
pfactivities <- c("Police_Protection", "Police_Officers", "Fire_Protection", "Firefighters")
pfvars <- df %>% filter(activity %in% pfactivities) %>%
  spread(activity, value) %>%
  mutate(Other_Fire_Employees=ifelse(year>=1977, Fire_Protection - Firefighters, NA),
         Other_Police_Employees=ifelse(year>=1979, Police_Protection - Police_Officers, NA)) %>%
  select(-one_of(pfactivities)) %>%
  gather(activity, value, Other_Fire_Employees, Other_Police_Employees) %>% 
  filter(!is.na(value))

df2 <- rbind_list(df, pfvars)

saveRDS(df2, paste0(edir, "pub_emp_clean_trim.rds"))


# # mess around a little
# d1 <- readRDS(paste0(edir, "pubemp_upto2010.rds"))
# ht(d1)
# tmp <- d1 %>% filter(type==2, variable=="Total_Employees", !is.na(value)) %>% arrange(stabbr, variable, year)
# tmp %>% filter(stabbr=="US")
# qplot(year, value, data=tmp %>% filter(stabbr=="US"), geom=c("point", "line"))
# 
# 
# # write out some info on var types
# d2 <- count(d1, variable)
# badsufx <- c("_Total_Emp", "_Total_Pay", "_FT_Emp", "_FT_Pay", "_FT_Equiv")
# for(sfx in badsufx) d2$variable <- gsub(sfx, "", d2$variable)
# d3 <- count(d2, variable)
# d3 %>% write.csv(paste0(edir, "pevars.csv"), row.names=FALSE)



