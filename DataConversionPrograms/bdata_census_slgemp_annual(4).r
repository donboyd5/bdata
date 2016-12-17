# 12/17/2016


#****************************************************************************************************
#                Overview ####
#****************************************************************************************************

# https://factfinder.census.gov/faces/nav/jsf/pages/download_center.xhtml


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("devtools")

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
#                Globals ####
#****************************************************************************************************
edir <- "D:/Data/CensusEmploymentData/"
rfiles <- "D:/Data/CensusEmploymentData/rfiles/"

dpre2011 <- "D:/Data/CensusEmploymentData/Special60/emp_est/" # 2010 and earlier
d20112012 <- paste0(edir, "emp20112012/") # 2011-2012
d2013plus <- paste0(edir, "emp2013plus_AFF_Files/") # 2013 +

vtypef.all <- c("emp.ft", "pay.ft", "emp.tot", "pay.tot", "emp.fte", "emp.pt", "pay.pt", "hrs.pt")


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
replacepos <- function(s, pos, replace="") {str_sub(s, pos, pos) <- replace; return(s)}
replacepos("abcd", 3)
replacepos("abcd", 3, "z")


#****************************************************************************************************
#                ONETIME: Get 2010-and-earlier data ####
#****************************************************************************************************
# SOURCE:
# use emp_est, not hist_emp
# hist_emp has national totals, not states
# s60hist <- "D:/Data/CensusEmploymentData/Special60/hist_emp/"
# s60histfn <- "3-State & Local Governments.csv" 

# ftp://ftp.census.gov/pub/outgoing/govs/special60/emp_est.zip
# Open mdb file, save as new mdb file, then export the table "Aggregates" as csv

# emp_est has state by state
pre2011fn <- "Aggregates.csv" 

df <- read_csv(paste0(dpre2011, pre2011fn))
glimpse(df)
count(df, Version)
count(df, Source)
count(df, ID, State, Type)
count(df, Month)
count(df, Type)
df %>% filter(Month=="VAR")
stcodes

df2 <- df %>% filter(State!="52", Type %in% 1:3) %>%
  mutate(stabbr=stcodes$stabbr[match(State, stcodes$stcen)],
         stabbr2=str_sub(Name, 1, 2),
         level=factor(Type, levels=1:3, labels=c("slg", "sg", "lg"))) %>%
  rename(year=Year4) %>%
  select(-Sort_Code, -ID, -Name, -Version, -Source, -Population, -Month, -Type)
glimpse(df2)
count(df2, State, stabbr, stabbr2) # check before deleting
# count(df2, year, month) # 1957 APR; 1962-1995 OCT; 1997-2010 MAR

dfl <- df2 %>% select(-State, -stabbr2) %>%
  gather(variable, value, -stabbr, -year, -level)
glimpse(dfl)

tmp <- count(dfl, variable) # FT_Emp, FT_Equiv, FT_Pay, Total_Emp, Total_Pay

vtypes <- c("FT_Emp", "FT_Pay", "Total_Emp", "Total_Pay", "FT_Equiv")
totals <- c("Total_Full_Time_Employees", "Total_Full_Time_Payrolls", "Total_Employees", "Total_Payrolls", "Total_Full_Time_Equivalent")
vtotals <- paste0("Total_", vtypes)
vtypes2 <- str_replace(vtypes, "_", "")
vtypef <- setdiff(vtypef.all, c("emp.pt", "pay.pt", "hrs.pt"))

cbind(vtypes2, vtypef) # verify proper alignment

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
tmp <- dfl2 %>% filter(value<0)
count(tmp, value)

# do final cleaning, including missing values and create an fcode
dfl3 <- dfl2 %>%  separate(variable, c("fname", "vtype"), sep="\\.") %>%
  mutate(vtype=factor(vtype, levels=vtypes2, labels=vtypef)) %>%
  select(stabbr, year, level, fname, vtype, value) %>%
  mutate(value=ifelse(value %in% c(-11111, -22222), NA, value))
count(dfl3, vtype)
count(dfl3, fname)
glimpse(dfl3)
fnames <- unique(dfl3$fname)
fcodes <- 1:length(fnames) %>% as.character

dfl4 <- dfl3 %>% mutate(fcode=factor(fname, levels=fnames, labels=fcodes),
                        fcode=as.character(fcode)) %>%
  select(stabbr, year, level, fcode, fname, vtype, value)
count(dfl4, fcode, fname)
glimpse(dfl4)

comment(dfl4) <- "Survey month: 1957 APR; 1962-1995 OCT; 1997-2010 MAR"
saveRDS(dfl4, paste0(rfiles, "emp_pre2011.rds"))

# type sb level slg sg lg; drop month but add comment; activity sb fname
# 1957 APR; 1962-1995 OCT; 1997-2010 MAR

#****************************************************************************************************
#                ONETIME: Get 2011-2012 data, which is not in AFF ####
#****************************************************************************************************

# AS NEEDED: download files ####
urlbase <- "http://www2.census.gov/govs/apes/"

fn <- "11empest.dat"
download.file(paste0(urlbase, fn), paste0(d20112012, fn), mode="wb")

fn <- "12emptot.dat"
download.file(paste0(urlbase, fn), paste0(d20112012, fn), mode="wb")


# read files ####
# https://www.census.gov//govs/apes/total_estimates_id_layout_02_11.html
# State code (states & DC in alpha sequence where '00' denotes United States)	2	1-2
# Type of summary total code
# 0 = US Total
# 1 = state and local total
# 2 = state only total
# 3 = local only total
# 5 = county only total
# 6 = municipality only total
# 7 = township only total
# 8 = special district only total
# 9 = school district only total

# 1	3-3
# FILLER	11	4-14
# Name of Government/Political description	64	15-78
# Census Region Code	1	79-79
# Summary Total Description	30	80-109
# FILLER	88	110-197
# Year of Data	2	198-199

# get function codes
fcodes <- read_csv("fcode, fname
000, Totals for Government
001, Airports
002, Space Research & Technology (Federal)
005, Correction
006, National Defense and International Relations (Federal)
012, Elementary and Secondary - Instruction 
112, Elementary and Secondary - Other Total 
014, Postal Service (Federal) 
016, Higher Education - Other 
018, Higher Education - Instructional 
021, Other Education (State) 
022, Social Insurance Administration (State) 
023, Financial Administration 
024, Firefighters 
124, Fire - Other 
025, Judical & Legal 
029, Other Government Administration 
032, Health 
040, Hospitals 
044, Streets & Highways 
050, Housing & Community Development (Local) 
052, Local Libraries 
059, Natural Resources 
061, Parks & Recreation 
062, Police Protection - Officers 
162, Police-Other 
079, Welfare 
080, Sewerage 
081, Solid Waste Management 
087, Water Transport & Terminals
089, Other & Unallocable
090, Liquor Stores (State)
091, Water Supply
092, Electric Power
093, Gas Supply
094, Transit")
fcodes <- fcodes %>% mutate(fname=str_trim(fname))
fcodes 



# 1	3-3
# FILLER	11	4-14
# Name of Government/Political description	64	15-78
# Census Region Code	1	79-79
# Summary Total Description	30	80-109
# FILLER	88	110-197
# Year of Data	2	198-199

# State code (states & DC in alpha sequence where '00' denotes United States)	2	1-2
# Type of summary total code
# 0 = US Total 
# 1 = state and local total
# 2 = state only total
# 3 = local only total
# 5 = county only total
# 6 = municipality only total
# 7 = township only total
# 8 = special district only total
# 9 = school district only total
# 1	3-3
# FILLER	14	4-17
# Data Function (Item) Codes	3	18-20
# Full-Time Employees	10	21-30
# Full-Time Payroll	12	31-42
# Part-Time Employees	10	43-52
# Part-Time Payroll	12	53-64
# Part-Time Hours	10	65-74
# Full-Time Equivalent Employees	10	75-84
# Total Employment	10	85-94
# Total Payroll	12	95-106
vnames <- c("st", "govtype", "fcode", "emp.ft", "pay.ft", "emp.pt", "pay.pt", "hrs.pt", "emp.fte", "emp.tot", "pay.tot")
starts <- c(1, 3, 18, 21, 31, 43, 53, 65, 75, 85, 95)
ends <- c(2, 3, 20, 30, 42, 52, 64, 74, 84, 94, 106)

df2011 <- read_fwf(paste0(d20112012, "11empest.dat"), fwf_positions(starts, ends, col_names=vnames))
glimpse(df2011)
count(df2011, govtype)
count(df2011, st)
df2011a <- df2011 %>% mutate_at(vars(-st, -govtype, -fcode), funs(as.numeric)) %>%
  mutate(year=as.integer(2011))
glimpse(df2011a)

df2012 <- read_fwf(paste0(d20112012, "12emptot.dat"), fwf_positions(starts, ends, col_names=vnames))
glimpse(df2012)
count(df2012, govtype)
count(df2012, st)
df2012a <- df2012 %>% mutate_at(vars(-st, -govtype, -fcode), funs(as.numeric)) %>%
  mutate(year=as.integer(2012))
glimpse(df2012a)

df1112 <- bind_rows(df2011a, df2012a) %>%
  mutate(stabbr=stcodes$stabbr[match(st, stcodes$stcen)],
         fname=fcodes$fname[match(fcode, fcodes$fcode)],
         level=factor(govtype, levels=1:3, labels=c("slg", "sg", "lg")))
df1112 %>% filter(st=="00", fcode=="000")
count(df1112, st, stabbr) # these data use Census codes, NOT FIPS codes
count(df1112, fcode, fname)
glimpse(df1112)

df1112a <- df1112 %>% select(-st, -govtype) %>%
  select(stabbr, year, level, fcode, fname, everything())
glimpse(df1112a)

df1112b <- df1112a %>% gather(vtype, value, -stabbr, -year, -level, -fcode, -fname)
glimpse(df1112b)

saveRDS(df1112b, paste0(rfiles, "emp_20112012.rds"))


#****************************************************************************************************
#                ONETIME: Download 2013+ data from AFF ####
#****************************************************************************************************

fnpre <- function(year, level){
  if(level=="slg") levnum <- 4 else
    if(level=="sg") levnum <- 2 else
      if(level=="lg") levnum <- 3
  fnpre <- paste0("EP", str_sub(year, 3, 4), "00A", levnum)
  return(fnpre)
}

getempurl <- function(year, level){
  upart1 <- "http://www2.census.gov/econ"
  upart3 <- "/EP/sector00/"
  ubase <- paste0(upart1, year, upart3)
  efn <- paste0(fnpre(year, level), ".zip")
  url <- paste0(ubase, efn)
  return(url)
}
getempurl(2015, "slg")

getemp <- function(year, level){
  download.file(getempurl(year, level), paste0(affd, fnpre(year, level), ".zip"), mode="wb")
}

for(year in 2013:2015){
  getemp(year, "slg")
  getemp(year, "sg")
  getemp(year, "lg")
}


#****************************************************************************************************
#                Read and save 2013+ data from AFF ####
#****************************************************************************************************

getf <- function(year, level){
  vtypes <- c("EMPFT", "PAYANNFT", "EMP", "PAYANN", "EMPFTE", "EMPPT", "PAYANNPT", "HRSPT")
  vtypef <- c("emp.ft", "pay.ft", "emp.tot", "pay.tot", "emp.fte", "emp.pt", "pay.pt", "hrs.pt")
  fnp <- fnpre(year, level)
  df <- read_delim(unz(paste0(d2013plus, paste0(fnp, ".zip")), paste0(fnp, ".dat")), delim="|")
  dfa <- df %>% select(-contains("_CV"), -contains("_F"), -FOOTID_GEO) %>%
    mutate(stabbr=stcodes$stabbr[match(ST, stcodes$stfips)]) %>% # note FIPS, not Census
    select(-GEOTYPE, -ST, -GEO_ID, -GEO_TTL) %>%
    rename(year=YEAR, fcode=GOVEMPFUNCT, fname=GOVEMPFUNCT_TTL) %>%
    mutate(fcode=as.character(fcode)) %>%
    gather(vtype, value, -year, -stabbr, -fcode, -fname) %>%
    mutate(vtype=factor(vtype, levels=vtypes, labels=vtypef),
           level=level) %>%
    select(stabbr, year, level, fcode, fname, vtype, value)
  return(dfa)
}
tmp <- getf(2013, "slg")
glimpse(tmp)

getyear <- function(year) {
  df <- bind_rows(getf(year, "slg"),
                  getf(year, "sg"),
                  getf(year, "lg"))
}

df <- ldply(2013:2015, getyear)
glimpse(df)
count(df, year, level)
count(df, stabbr)
count(df, fcode, fname)

saveRDS(df, paste0(rfiles, "emp_2013plus.rds"))


#****************************************************************************************************
#                Combine data for all years, conform, and save ####
#****************************************************************************************************
d1 <- readRDS(paste0(rfiles, "emp_pre2011.rds"))
d2 <- readRDS(paste0(rfiles, "emp_20112012.rds"))
d3 <- readRDS(paste0(rfiles, "emp_2013plus.rds"))

glimpse(d1) # type sb level slg sg lg; drop month but add comment; activity sb fname
glimpse(d2) # good has level fcode fname then vars across, should gather
glimpse(d3) # has level; actcode sb fcode activity sb fname

dall <- bind_rows(d1, d2, d3) %>%
  select(stabbr, year, level, fcode, fname, vtype, value)
glimpse(dall)
tmp <- dall %>% group_by(year, fcode) %>% summarise(n=n()) %>% spread(fcode, n)

# check units
tmp <- dall %>% filter(level=="slg", stabbr=="US", vtype=="emp.tot") %>% 
  arrange(stabbr, year, fname) %>%
  filter(!(year<=2010 & fname!="Total"),
         !(year %in% 2011:2012 & fcode!="000"),
         !(year>2012 & fcode!="1000"))
# good, units appear to be the same across the 3 sets of years
  
comment(dall) <- "Survey month: 1957 APR; 1962-1995 OCT; 1997-2010 MAR"
saveRDS(dall, paste0(rfiles, "empall.rds"))

#****************************************************************************************************
#                Winnow the data down to common variables and values ####
#****************************************************************************************************
dall <- readRDS(paste0(rfiles, "empall.rds"))
glimpse(dall)
dall2 <- dall %>% spread(vtype, value)

codespre2011 <- dall2 %>% filter(year<2011) %>% count(fcode, fname)
codes20112012 <- dall2 %>% filter(year %in% 2011:2012) %>% count(fcode, fname)
mastercodes <- dall2 %>% filter(year>=2013) %>% count(fcode, fname)

write_csv(codespre2011, paste0(rfiles, "codes_pre2011.csv"))
write_csv(codes20112012, paste0(rfiles, "codes_20112012.csv"))
write_csv(mastercodes, paste0(rfiles, "codes_2013plus.csv"))

# create crosswalks - standardize on the 2013+ codes
xw <- read_excel(paste0(rfiles, "fcodes_xwalk(1).xlsx"), sheet="master", skip=1) %>%
  mutate_at(vars(starts_with("fcode")), funs(as.character)) %>%
  filter(fcode.master!="bad") %>%
  mutate(fcode.20112012=str_pad(fcode.20112012, width=3, side="left", pad="0"))
glimpse(xw)

glimpse(dall2)
dall3 <- dall2 %>% mutate(fcode.pre2011=ifelse(year<=2010, fcode, NA),
                          fcode.20112012=ifelse(year %in% 2011:2012, fcode, NA),
                          fcode.2013plus=ifelse(year>=2013, fcode, NA)) %>%
  mutate(fcode=ifelse(year<=2010, 
                      xw$fcode.master[match(fcode.pre2011, xw$fcode.pre2011)],
                      NA),
         fcode=ifelse(year %in% 2011:2012, 
                      xw$fcode.master[match(fcode.20112012, xw$fcode.20112012)],
                      fcode),
         fcode=ifelse(year>=2013, 
                      xw$fcode.master[match(fcode.2013plus, xw$fcode.2013plus)],
                      fcode),
         fname=xw$fname.master[match(fcode, xw$fcode.master)])

glimpse(dall3)
count(dall3, fcode, fname)
tmp <- dall3 %>% group_by(year, fcode, fname) %>% summarise(n=n())

# look at unmatched codes
tmp <- dall3 %>% filter(year<=2010, is.na(fcode))
count(tmp, fcode.pre2011) # ok

tmp <- dall3 %>% filter(year %in% 2011:2012, is.na(fcode))
count(tmp, fcode.20112012) # ok

tmp <- dall3 %>% filter(year>=2013, is.na(fcode))
count(tmp, fcode.2013plus) # ok

# wrap it up
dall4 <- dall3 %>% select(-starts_with("fcode."))
glimpse(dall4)

# calculate missing subtotals for years before 2013
codes <- c("1101", "1102", "1121", "1122", "14811", "14812", "14821", "14822", "1483")
sub20112012 <- dall4 %>% filter(year %in% 2011:2012, fcode %in% codes) %>%
  select(-fname) %>%
  gather(variable, value, -stabbr, -year, -level, -fcode) %>%
  spread(fcode, value) %>%
  mutate(`1100`= naz(`1101`) + naz(`1102`),
         `1120`= naz(`1121`) + naz(`1122`),
         `1481` = naz(`14811`) + naz(`14812`),
         `1482` = naz(`14821`) + naz(`14822`),
         `1480` = naz(`1481`) + naz(`1482`) + naz(`1483`)) %>%
  select(stabbr, year, level, variable, `1100`, `1120`, `1481`, `1482`, `1480`) %>%
  gather(fcode, value, -c(stabbr, year, level, variable)) %>%
  spread(variable, value) %>%
  mutate(fname=xw$fname.master[match(fcode, xw$fcode.master)])

codes2 <- c("1100", "1101", "1120", "1121")
subpre2011 <- dall4 %>% filter(year<=2010, fcode %in% codes2) %>%
  select(-fname) %>%
  gather(variable, value, -stabbr, -year, -level, -fcode) %>%
  spread(fcode, value) %>%
  mutate(`1102`= naz(`1100`) - naz(`1101`),
         `1122`= naz(`1120`) - naz(`1121`)) %>%
  select(stabbr, year, level, variable, `1102`, `1122`) %>%
  gather(fcode, value, -c(stabbr, year, level, variable)) %>%
  spread(variable, value) %>%
  mutate(fname=xw$fname.master[match(fcode, xw$fcode.master)])

# DJB REVIEW THE LOGIC OF THIS AGAIN AT SOME POINT ####

dall5 <- bind_rows(dall4, sub20112012, subpre2011) %>%
  arrange(stabbr, year, level, fcode, fname)
glimpse(dall5)

slgemp <- dall5 %>% gather(vtype, value, -stabbr, -year, -level, -fcode, -fname)
glimpse(slgemp)
comment(slgemp) <- comment(dall)
comment(slgemp)
use_data(slgemp, overwrite = TRUE)




