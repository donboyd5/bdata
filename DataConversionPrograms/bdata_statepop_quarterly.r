# bdata_statepop_quarterly.r
# Don Boyd
# 12/28/2017

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
qplot(year, value, data=filter(fcspop, stabbr==st & year>=2000), geom=c("point","line")) # inspect a few states

#-----------------------------------------------------------------------------------------------

#----- Interpolate ----------------------------------------------------------------------------

fqpop <- function(df){
  # start one year before, end one year after, but only use the Qs for the year in question
  df <- arrange(df, stabbr, year) # just to be safe
  y1 <- min(df$year, na.rm=TRUE)
  y2 <- max(df$year, na.rm=TRUE)
  d1 <- as.data.frame(spline(df$year, df$value, xmin=y1-1, xmax=y2+1, n=4*(length(df$year)+2), method="fmm"))
  # d <- as.data.frame(spline(df$year, df$pop, xmin=y1-1, xmax=y2+1, n=4*(length(df$year)+2), method="natural"))
  # DO NOT USE PERIODIC METHOD!!! d <- as.data.frame(spline(df$year, df$pop, xmin=y1-1, xmax=y2+1, n=4*(length(df$year)+2), method="periodic"))
  d <- cbind(d1, date=seq(btools::mdyfn(1, 1, y1-1), by="3 months", length.out=(y2 - y1 + 3) * 4)) %>%
    select(-x) %>%  # x is the year
    select(date, value=y)
  return(d)
}

# df <- filter(popst, stabbr=="NY")
# qplot(date, value, data=fqpop(filter(popst, stabbr=="LA")), geom=c("point", "line"))

spop.q <- fcspop %>% group_by(stabbr) %>%
  do(fqpop(.)) %>%
  filter(year(date)>=1900,
         year(date) <= (max(spop.a$year) + 1)) # don't go more than 1 year beyond actual data

count(spop.q, date) %>% data.frame

usethis::use_data(spop.q, overwrite=TRUE)


glimpse(spop.a)
spop.q %>% filter(stabbr=="NY", year(date)>=1990) %>% qplot(date, value, data=., geom=c("point", "line"))
d1 <- spop.a %>% mutate(date=as.Date(paste(year, 7, 1, sep="-")), type="annual")
d2 <- spop.q %>% mutate(type="quarterly")
d3 <- bind_rows(d1, d2)

st <- "NJ"
d3 %>% filter(stabbr==st, year(date)>=2000) %>% qplot(date, value, data=., colour=type, geom=c("point", "line"), main=st)

glimpse(d1)

tmp <- d3 %>% filter(stabbr==st, year(date)>=2000)

#****************************************************************************************************
#                OLD: Various checks below here ####
#****************************************************************************************************
################################################################################################
# compare annual to quarterly values
# in general, quarterly seems to be 0.1 to 0.5% lower than annual in recent years
# AK and HI look bad in recent years
df <- getdata("pop")
dfq<-getdata("popq")
str(df); str(dfq)

df$date<-mdy(7,1,df$year)
df$source<-"annual"
head(df); tail(df)

dfq$source<-"quarterly"
head(dfq); tail(dfq)

keepcols<-c("stabbr","date","source","pop")
df3<-rbind(subset(df,select=keepcols),subset(dfq,select=keepcols))
str(df3)
qplot(date,pop,data=subset(df3,stabbr=="LA" & year(date)>=2000),colour=source,geom=c("point","line"))

dfw<-dcast(df3,stabbr+date~source)
str(dfw)
dfw$pct<-(dfw$quarterly/dfw$annual-1)*100
dfw<-subset(dfw,!is.na(annual))
head(dfw); tail(dfw)

dcast(subset(dfw,year(date)>=2010),stabbr~date,value.var="pct")
dcast(subset(dfw,year(date) %in% 1950:1954),stabbr~date,value.var="pct")


################################################################################################
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

# check for annual pop reasonableness
lrows<-3
lcols<-4
lpages<-1
pdf(file="apopcheck.pdf")
xyplot(pop ~ year | stabbr,
       data = pop2,
       scales = list(relation="free", x = list(rot = 45)),
       type = "b", # first is state, 2nd is US # points for one, line for the other
       distribute.type = TRUE,
       # par.settings = simpleTheme(lty=c(1,1), lwd=c(3,2), col="blue"),
       par.settings = simpleTheme(lty=1, lwd=2, pch=19, cex=0.3, col="blue"), # cex for point size
       #lwd=1,
       as.table = TRUE,
       par.strip.text = list(abbreviate = TRUE, minlength=15),
       layout=c(lcols,lrows),
       xlab=NULL,
       main=list("Annual population",cex=.95),
       )
dev.off()


# check for reasonableness
lrows<-3
lcols<-4
lpages<-1
pdf(file="qpopcheck.pdf")
xyplot(pop ~ date | stabbr,
  data = qpop,
  scales = list(relation="free", x = list(rot = 45)),
  type = "b", # first is state, 2nd is US # points for one, line for the other
  distribute.type = TRUE,
  par.settings = simpleTheme(lty=c(1,1), lwd=c(3,2), col="blue"),
  as.table = TRUE,
  par.strip.text = list(abbreviate = TRUE, minlength=15),
  layout=c(lcols,lrows),
  xlab=NULL,
  main=list("Quarterly population",cex=.95),
  # here is a way to put various reference lines on the plot
#   panel=function(...){
#     panel.xyplot(...)
#   	panel.abline(h=0)
#   }
  )
dev.off()












# wow, looks like it handles NAs well
years<-1:7
anndat<-c(15,20,17,NA,50,45,37)
d<-as.data.frame(spline(years, anndat, xmin=years[1]-1,xmax=years[length(years)]+1, n=4*(length(years)+2), method="fmm"))
d
d<-cbind(d,c(rep(NA,4),rep(anndat,each=4),rep(NA,4)))
d
d<-melt(d,id="x")
qplot(x,value,data=d,geom=c("point","line"),colour=variable)

