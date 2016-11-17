# DJB 11/16/2016 I AM NO LONGER USING THIS


# I use annual and quarterly state personal income so often that it makes sense to
# keep data files always at the ready - annual and quarterly


library(apitools)
library(dplyr)
library(stringr)
library(btools)

data(stcodes)

# annual state personal income ####
df <- BEA_RgnData("TPI_SI")
df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_sub(GeoFips, 1, 2), stcodes$stfips)])
count(df2, stabbr, GeoName) %>% data.frame
glimpse(df2)

# good - we can drop other variables and save
spi.a <- df2 %>% filter(stabbr %in% stcodes$stabbr) %>%
  select(stabbr, year, value)
head(spi.a)
glimpse(spi.a)
count(spi.a, year) %>% data.frame
count(spi.a, stabbr) %>% data.frame

ctext <- paste0("BEA state personal income by state and year, $ thousands, downloaded via api on ", format(Sys.time(), '%B %d, %Y'))
comment(spi.a) <- ctext
comment(spi.a)

devtools::use_data(spi.a, overwrite=TRUE)





# quarterly state personal income ####
df <- BEA_RgnData("TPI_QI")
glimpse(df)
df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_sub(GeoFips, 1, 2), stcodes$stfips)])
count(df2, stabbr, GeoName) %>% data.frame
count(df2, date) %>% data.frame
glimpse(df2)
ht(df2)

# good - we can drop other variables and save
spi.q <- df2 %>% filter(stabbr %in% stcodes$stabbr) %>%
  select(stabbr, date, value)
head(spi.q)
tail(spi.q)
glimpse(spi.q)
count(spi.q, date) %>% data.frame
count(spi.q, stabbr) %>% data.frame

ctext <- paste0("BEA state personal income by state and quarter (date=first day of quarter), $ thousands, downloaded via api on ", format(Sys.time(), '%B %d, %Y'))
comment(spi.q) <- ctext
comment(spi.q)

devtools::use_data(spi.q, overwrite=TRUE)
