#' State population, annual.
#'
#' State population, annual, 1900 through 2014, from the Census Bureau. One row per state per year.
#' \cr\cr
#' Wherever possible, I have used Census Bureau intercensal estimates,
#' including for the decennial census years. The exceptions are 1970 and 1980, where I have not found the intercensal
#' estimates and so have used decennial population. The intercensal estimates are as of July 1 whereas the decennial
#' estimates are as of April 1. Thus, the change from 1969 to 1970 is from July to April (9 months) and the change
#' from 1970 to 1971 is from April to July (15 months).
#' \cr\cr
#' Includes all 50 states, United States (US) totals, and District of Columbia (DC) for all years (no values for AK
#' or HI before statehood). Includes Puerto Rico (PR) for 2000 and later. Does not include Virgin Islands or American Samoa.
#' All numbers are as reported by the Census Bureau - no calculated values.
#'
#' @source Bureau of the Census, http://www.census.gov/popest and various subdirectories and archives within.
#' http://www.census.gov/popest/ in general
#' See source code on GitHub for detailed sources.
#' @format Data frame with 1 row per state per year
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{Calendar year, numeric}
#' \item{value}{State population, thousands, numeric}
#' \item{esttype}{Estimate type - intercensal or decennial, character}
#' }
#' @examples
#'   spop.a
"spop.a"

