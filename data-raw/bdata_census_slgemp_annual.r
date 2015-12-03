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

library(btools)
# library(bdata) # is it ok to use bdata when developing bdata?
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
#                get and save data for the good years ####
#****************************************************************************************************
slgemp <- readRDS(paste0(edir, "empest_goodyears.rds"))
glimpse(slgemp)

use_data(slgemp, overwrite = TRUE)


