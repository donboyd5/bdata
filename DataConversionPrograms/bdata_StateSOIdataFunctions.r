# bdata_StateSoIdataFunctions.r


get_soi2011 <- function() {
  fn <- paste0(soidir, "11in54cm.xlsx")
  # df <- read.xls(fn, colClasses="character")
  df <- read_excel(fn)
  head(df)
  stcodes$stabbr <- as.character(stcodes$stabbr) # note that this creates a copy of stcodes in the workspace, does not overwrite btools
  stcodes$stname <- trim(as.character(stcodes$stname))
  stnames <- as.data.frame(t(df[2, ])) %>% select(stname=V1) %>%
    filter(!is.na(stname), stname!="", stname!="Item") %>%
    mutate(stname=trim(stname),
           stabbr=ifelse(stname %in% toupper(stcodes$stname), stcodes$stabbr[match(toupper(stcodes$stname), stname)], ""),
           stabbr=ifelse(grepl("OTHER AREAS", stname), "OA", stabbr))
  stnames

  # now modify the stname row of df
  df2 <- df
  # first, put state abbreviation in each relevant column - put into row 1 so we can easily compare to names already in row 2; start in col 2 as first col is a stub
  # inserts <- rep(stnames$stname, each=10)
  inserts <- rep(stnames$stabbr, each=10)
  df2[1, 2:(length(inserts)+1)] <- inserts
  df2[1:3, ]

  # now income group - put into row 3 so easily can compare to descriptions in row 4
  inserts <- rep(incgrpdescs20102011, 53)
  df2[3, 2:(length(inserts)+1)] <- inserts
  df2[1:6, ]

  # new var names so that we can gather the data
  vnames <- c("item", paste(df2[1, 2:531], df2[3, 2:531], sep="zzz"))
  vnames
  names(df2) <- vnames
  row1 <- which(df2$item=="Number of returns")
  df2 <- df2 %>% mutate(lineno=row_number() - row1 + 1)

  dfl <- df2 %>% gather(stabbr.incgrp, value, -item, -lineno) %>%
    mutate(item=trim(item), value=as.numeric(value)) %>%
    filter(!is.na(value)) %>%
    group_by(stabbr.incgrp) %>%
    arrange(lineno) %>%
    mutate(item2=ifelse(item=="Amount", paste0(trim(gsub("Number", "", lag(item))), ".Amount"), item)) %>%
    separate(stabbr.incgrp, c("stabbr", "incgrpdesc"), sep="zzz") %>%
    mutate(incgrp=factor(incgrpdesc, levels=incgrpdescs20102011, labels=1:length(incgrpdescs20102011)),
           year=2011) %>%
    select(-item) %>%
    select(stabbr, year, incgrp, incgrpdesc, lineno, item=item2, value)
  ht(dfl)
  count(dfl, item, lineno)
  return(dfl)
}
