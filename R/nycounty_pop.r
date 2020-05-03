#' New York county population, annual.
#'
#' New York county population annual, 1970 through latest available year, from the Census Bureau. One row per state per year.
#' \cr\cr
#' I have used the following Census Bureau estimates:
#' -  intercensal (July 1) estimates prior to 2010 wherever possible (including decennial census years)
#' -  postcensal (July 1) estimates for 2010+
#' -  census (April 1) for 1970 and 1980 -- I used these because I have not found intercensal estimates.
#' population (April 1).
#' 
#' Thus, the change from 1969 to 1970 is from July to April (9 months) and the change
#' from 1970 to 1971 is from April to July (15 months).
#' \cr\cr
#' Includes all 50 states, United States (US) totals, and District of Columbia (DC) for all years (no values for AK
#' or HI before 1950). Does not include Puerto Rico (may revisit this), Virgin Islands or American Samoa.
#' 
#' All numbers are as reported by the Census Bureau - no calculated values - but I did convert data for earlier years from
#' thousands to number of people so that all values are in the same units.
#'
#' @format Data frame with 1 row per state per year
#' \describe{
#' \item{year}{Calendar year, numeric}
#' \item{uni_code}{FIPS code or other id if not a FIPS area}
#' \item{rectype}{record type}
#' \item{value}{Population, number of individuals, numeric}
#' }
#' @source https://data.ny.gov/Government-Finance/Annual-Population-Estimates-for-New-York-State-and/krt9-ym2k/data.
#' @examples
#'   nycounty_pop
"nycounty_pop"

