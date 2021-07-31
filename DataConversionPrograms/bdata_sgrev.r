
# 3/12/2021
# https://willamette.edu/mba/research-impact/public-datasets/
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4479543/
# https://willamette.edu/~kpierson/TheGovernmentFinanceDatabase_AllData.zip

# also consider
# Urban SLFQS  https://state-local-finance-data.taxpolicycenter.org//pages.cfm#

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
#                constants ####
#****************************************************************************************************
# https://willamette.edu/mba/research-impact/public-datasets/index.html
# https://willamette.edu/~kpierson/TheGovernmentFinanceDatabase_AllData.zip
wdir <- r"(D:\Data\bdata_package_sourcedata\williamette\)"
sgcsv <- "StateData.csv"


urlbase <- "https://willamette.edu/~kpierson/"
fn <- "TheGovernmentFinanceDatabase_StateData.zip"
url <- paste0(urlbase, fn)


#****************************************************************************************************
#                get state government data ####
#****************************************************************************************************
# https://willamette.edu/~kpierson/TheGovernmentFinanceDatabase_StateData.zip
tdir <- tempdir()


dlzip <- paste0(tdir, fn)

download.file(url, dlzip, mode = "wb")
unzip(dlzip, list=TRUE)
unzip(dlzip, files="StateData.csv", exdir=tdir)

df <- read_csv(file.path(tdir, "StateData.csv"))
names(df)
glimpse(df)
# Rows: 2,200
# Columns: 592
names(df)[1:145]
# ID,Year4,State_Code,Type_Code,County,Name,FIPS_Code_State,FIPS_County,FIPS_Place,FIPS_Combined,FYEndDate,YearPop,SchLevCode,Population
count(df, GOVSid, FIPSid, State_Code, FIPS_Code_State, str_sub(Name, 1, 4))  # 51 includes DC but not US; Name has extraneous info

stcodes <- tibble(stabbr=c(state.abb, "DC"), stfips=c(usmap::fips(state.abb), "11"))

# Gen_Rev_Own_Sources
# Total_Revenue 18
# "General_Revenue" 20
# Gen_Rev_Own_Sources 21 through "Misc_General_Revenue" 113  "Misc_General_Rev_NEC"  122
df2 <- df %>%
  select(year=Year4, FIPS_Code_State, pop=Population, 18:143) %>%
  mutate(stabbr=stcodes$stabbr[match(FIPS_Code_State, stcodes$stfips)]) %>%
  select(-FIPS_Code_State) %>%
  select(stabbr, year, everything()) %>%
  pivot_longer(-c(stabbr, year))
ht(df2)
glimpse(df2)


xwalk <- tribble(~name, ~variable,
                 "pop", "pop",
                 "General_Revenue", "genrev",
                 "Gen_Rev_Own_Sources", "osr",
                 "Total_Taxes", "tottax",
                 "Total_Gen_Sales_Tax", "gst",
                 "Total_Select_Sales_Tax", "selsales",
                 "Individual_Income_Tax", "iit",
                 "Corp_Net_Income_Tax", "cit",
                 "Severance_Tax", "sevtax",
                 "Total_General_Charges", "chg.gen",
                 "Misc_General_Revenue", "misc.gen")
xwalk

df3 <- df2 %>%
  left_join(xwalk, by = "name")
tmp <- count(df3, name, variable)

memory()

sgrev <- df3

usethis::use_data(sgrev, overwrite = TRUE)

sgrev %>%
  filter(stabbr=="FL", year==2008, variable=="chg.gen")

sgrev %>%
  filter(stabbr=="FL", year %in% 2008:2012, variable=="osr")

sgrev %>%
  filter(stabbr=="FL", year >= 2000, variable=="osr") %>%
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point()

