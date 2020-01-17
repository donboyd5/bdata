
# state and county land area, 2010, from Census, plus pop and housing units


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library("scales")
library("hms") # hms, for times.
library("stringr") # stringr, for strings.
library("lubridate") # lubridate, for date/times.
library("forcats") # forcats, for factors.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata") # needed for stcodes


#****************************************************************************************************
#                Download and save data ####
#****************************************************************************************************
# state data can be found at https://www.census.gov/geo/reference/state-area.html
# but state and county needs American FactFinder

# we want table GCT-PH1 Population, Housing Units, Area, and Density: 2010 - United States -- County by State; and for Puerto Rico
# 2010 Census Summary File 1

# unfortunately, some of the numbers have been revised since SF1 and for some reason Census does not include the revisions
# it seems like revisions were to pop and housing units, not to land area, so don't trust pop and housing too much
# Get the version that has annotations (of revisions) in separate files, otherwise the csv file will have a note about
# revisions after the unrevised number, in the form (R38234) -- an example -- making the number hard to read

# if using Internet Download Manager, after telling AFF to create the download file, click ctrl-Download button

# I put the zip file into data_raw


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
fnz <- "DEC_10_SF1_GCTPH1.US05PR.zip"
fnc <- "DEC_10_SF1_GCTPH1.US05PR.csv"

df <- read_csv(unz(paste0("./data_raw/", fnz), filename=fnc))
glimpse(df)

df2 <- df %>% select(fips=`GCT_STUB.target-geo-id2`,
                     areaname=`GCT_STUB.display-label_1`,
                     pop=HD01,
                     housunits=HD02,
                     totarea=SUBHD0301,
                     waterarea=SUBHD0302,
                     landarea=SUBHD0303,
                     popden=SUBHD0401,
                     housden=SUBHD0402) %>%
  filter(row_number()!=1) %>%
  mutate_at(vars(-fips, -areaname), as.numeric) %>%
  mutate(geotype=case_when(areaname == "United States" ~ "US",
                            nchar(fips) == 2 ~ "state",
                            nchar(fips) > 2 ~ "county",
                            TRUE ~ "ERROR"),
         stabbr=stcodes$stabbr[match(str_sub(fips, 1, 2), stcodes$stfips)],
         stabbr=ifelse(geotype=="US", "US", stabbr)) %>%
  select(fips, stabbr, areaname, geotype, everything())
glimpse(df2)
count(df2, geotype)

df2 %>% filter(geotype=="state")

landarea <- df2
usethis::use_data(landarea, overwrite = TRUE)




