



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
nasboxrd <- paste0("./data-raw/NASBOxr/")


#****************************************************************************************************
#                read and save data ####
#****************************************************************************************************
df <- read_excel(paste0(nasboxrd, "EXP_DATA 1991-2014.xls"), sheet="EXP_DATA")
df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(STATE, stcodes$stname)]) %>%
  rename(year=YEAR) %>%
  select(-STATE) %>%
  gather(variable, value, -stabbr, -year) %>%
  mutate(variable=tolower(variable))
# count(df2, stabbr, STATE)

# now create a better organization for variables and values
# first, reverse the odd NASBO choice of reversing the coding for fund totals of capital-inclusive spending
# just call it totx instead of capi; af means all funds
df3 <- df2
df3$variable[df3$variable=="gftot_capi"] <- "totx_gf"
df3$variable[df3$variable=="fftot_capi"] <- "totx_ff"
df3$variable[df3$variable=="oftot_capi"] <- "totx_of"
df3$variable[df3$variable=="bftot_capi"] <- "totx_bf"
df3$variable[df3$variable=="total_capi"] <- "totx_af"
count(df3, variable) %>% data.frame

# separate the variable into purpose and fund type, then map names to each - imposing some consistency on the nasbo data
purpdf <- read_csv(
 "corcp, Corrections capital
  corr, Corrections
  elsed, Elementary & secondary education
  envcp, Environmental capital
  hedcp, Higher education capital
  hed, Higher education
  hscap, Housing capital
  mcaid, Medicaid
  othca, Other cash assistance
  othcp, Other capital
  other, All other expenditures
  tanf, TANF
  totx, Total expenditures
  trans, Transportation
  trcap, Transportation capital",
 col_names=c("purpose", "purposef"))

ftdf <- read_csv(
 "af, all funds
  bf, bond funds
  ff, federal funds
  gf, general fund
  of, other funds",
 col_names=c("fundtype", "fundtypef"))

df4 <- df3 %>% separate(variable, c("purpose", "fundtype")) %>%
  mutate(purpose=ifelse(purpose=="hgred", "hed", purpose),
         purpose=ifelse(purpose=="otca", "othca", purpose),
         fundtype=ifelse(fundtype=="tot", "af", fundtype),
         purposef=factor(purpose, levels=purpdf$purpose, labels=purpdf$purposef),
         fundtypef=factor(fundtype, levels=ftdf$fundtype, labels=ftdf$fundtypef))

count(df4, purpose, purposef)
count(df4, fundtype, fundtypef)
count(df4, stabbr) # includes PR, does not include US

nasboxr <- df4 %>% select(stabbr, year, purpose, fundtype, value, purposef, fundtypef)
use_data(nasboxr, overwrite = TRUE)
