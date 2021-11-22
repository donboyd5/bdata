# bdata_statepop_quarterly.r
# Don Boyd
# 12/28/2017


# libraries ---------------------------------------------------------------

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
library("forecast")

# create quarterly state population data:
#   forecast state population one year ahead, then
#   interpolate to get quarters

load("./data/spop.a.rda") # use the latest version of spop.a -- annual population by state and year
ht(spop.a)


#----- Forecast a year or two ahead ---------------------------------------------------------
# General notation ETS(Error,Trend,Seasonal)
# ETS(A,N,N): Simple exponential smoothing with additive errors
# ETS(A,A,N): Holt's linear method with additive errors
# ETS(A,A,A): Additive Holt-Winters' method [additive trend and season] with additive errors
# ETS(M,A,M): Multiplicative Holt-Winters' method [additive trend, multiplicative season] with multiplicative errors
# ETS(A,Ad,N): Damped trend method with additive errors

fcpop <- function(df){
  df2 <- ts(df$value, start=min(df$year))
  df3 <- ets(df2, model="MMN") # multiplicative errors, multiplicative trend, no season
  df4 <- forecast(df3, h=2) # forecast 2 years ahead
  df4 <- data.frame("year"=min(df$year):(max(df$year)+2), "value"=c(df4$x, df4$mean))
  return(df4)
}

fcspop <- spop.a %>% filter(stabbr!="PR") %>%
  group_by(stabbr) %>%
  do(fcpop(.)) # takes about 2 minutes

ht(fcspop)
min(fcspop$year)

st <- "CA"
st <- "NY"
fcspop %>%
  filter(stabbr==st & year>=2000) %>%
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point() +
  ggtitle(st) # inspect a few states

#-----------------------------------------------------------------------------------------------



#----- Interpolate ----------------------------------------------------------------------------

fqpop <- function(year, value){
  # df has year, value for one state
  # start one year before, end one year after, but only use the Qs for year1-year2
  
  # define first and last year
  year1 <- min(year, na.rm=TRUE)
  year2 <- max(year, na.rm=TRUE)
  
  # create a data frame that starts the year before year1 and goes to the year after year2, with 4 values per year
  df1 <- as.data.frame(spline(year,
                              value, 
                              xmin=year1 - 1, 
                              xmax=year2 + 1,
                              n = 4*(length(year) + 2), method="fmm"))
  # DO NOT USE PERIODIC METHOD!!! fmm seems good; maybe natural, also?
  
  first_date <- as.Date(paste0(year1 - 1, "-01-01"))
  
  df2 <- df1 %>%
    mutate(date=seq(first_date, by="3 months", length.out=nrow(df1))) %>%
    select(date, value=y)
  
  return(df2)
}


spop.q <- fcspop %>% 
  group_by(stabbr) %>%
  summarise(fqpop(year, value), .groups="drop") %>%
  filter(year(date) >= 1900,
         year(date) <= (max(spop.a$year) + 1)) # don't go more than 1 year beyond actual data

usethis::use_data(spop.q, overwrite=TRUE)


# data checks -------------------------------------------------------------

st <- "NY"
spop.q %>% 
  filter(stabbr==st, year(date)>=1990) %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  geom_point() +
  ggtitle(st)


d1 <- spop.a %>% 
  mutate(date=as.Date(paste(year, 7, 1, sep="-")),
         type="annual")  # annual pop as of July

d2 <- spop.q %>% 
  mutate(type="quarterly") 

d3 <- bind_rows(d1, d2)

st <- "NY"
d3 %>% 
  filter(stabbr==st, year(date)>=2000) %>% 
  ggplot(aes(date, value, colour=type)) +
  geom_line() +
  geom_point() +
  ggtitle(st)

