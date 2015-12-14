



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
# nasboxrd <- paste0("./data-raw/NASBOxr/")
nasboxrd <- paste0("D:/Dropbox/NASBO/LATEST Data/")



#****************************************************************************************************
#                read and save data ####
#****************************************************************************************************
df <- read_excel(paste0(nasboxrd, "NASBO-ExpData-1991-2015.xls"), sheet="EXP_DATA")
# there are 5 unnecessary columns at the end - verify, then delete
df[, c(1, (ncol(df)-5):ncol(df))] %>% ht
df <- df[ , 1:(ncol(df)-5)]
df[, c(1, ncol(df))]
df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(STATE, stcodes$stname)]) %>%
  rename(year=YEAR) %>%
  select(-STATE) %>%
  gather(variable, value, -stabbr, -year) %>%
  mutate(variable=tolower(variable))
# count(df2, stabbr, STATE)
count(df2, variable)

# now create a better organization for variables and values
# first, reverse the odd NASBO choice of reversing the coding for fund totals of capital-inclusive spending
# just call it totx instead of capi; af means all funds
fromto <- read_csv("from, to
gftot_capi, totx_gf
fftot_capi, totx_ff
oftot_capi, totx_of
bftot_capi, totx_bf
total_capi, totx_af
")
fromto


# separate the variable into purpose and fund type, then map names to each - imposing some consistency on the nasbo data
purpdf <- read_csv("purpose, purposef
corr, Corrections
elsed, Elementary & secondary education
env, Environmental
hied, Higher education
hous, Housing
medcaid, Medicaid
othercashassist, Other cash assistance
other, All other expenditures
tanf, TANF
totx, Total expenditures
trans, Transportation")
purpdf

ftdf <- read_csv(
  "af, all funds
  bf, bond funds
  ff, federal funds
  gf, general fund
  of, other funds",
  col_names=c("fundtype", "fundtypef"))


df3 <- df2 %>% mutate(variable=mapvalues(variable, from=fromto$from, to=fromto$to),
                      variable=str_replace(variable, "_tot", "_af"),
                      variable=str_replace(variable, "cp_", "cap_"),
                      variable=str_replace(variable, "mcaid", "medcaid"),
                      variable=str_replace(variable, "otca", "othercashassist"),
                      variable=str_replace(variable, "corcap", "corrcap"),
                      variable=str_replace(variable, "hgred", "hied"),
                      variable=str_replace(variable, "hedcap", "hiedcap"),
                      variable=str_replace(variable, "hscap", "houscap"),
                      variable=str_replace(variable, "othcap", "othercap"),
                      variable=str_replace(variable, "trcap", "transcap"),
                      xtype=ifelse(str_detect(variable, "cap_"), "capital", "total"),
                      variable=str_replace(variable, "cap_", "_")
                      ) %>%
  separate(variable, c("purpose", "fundtype")) %>%
  mutate(purposef=factor(purpose, levels=purpdf$purpose, labels=purpdf$purposef),
         fundtypef=factor(fundtype, levels=ftdf$fundtype, labels=ftdf$fundtypef))

count(df3, xtype, fundtypef, purposef) %>% data.frame

nasboxr <- df3 %>% select(stabbr, year, xtype, purpose, fundtype, value, purposef, fundtypef)
use_data(nasboxr, overwrite = TRUE)



# look at the data
nb <- nasboxr
glimpse(nb)
count(nb, purpose, purposef, fundtype) %>% spread(fundtype, n)

nb %>% filter(stabbr=="NY", year==2015, fundtype=="af", xtype=="total")
nb %>% filter(stabbr=="NY", year==2015, fundtype=="af") %>% spread(xtype, value)


nb %>% filter(year %in% c(2008, 2015), fundtype=="af", xtype=="total") %>%
  group_by(stabbr, year) %>%
  mutate(share=value / value[purpose=="totx"] * 100) %>%
  group_by(stabbr, purpose) %>%
  mutate(dshare=share - share[year==2008]) %>%
  filter(year==2015) %>%
  select(stabbr, purpose, dshare) %>%
  spread(purpose, dshare) %>%
  arrange(medcaid)


