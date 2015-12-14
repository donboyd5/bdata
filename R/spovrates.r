#' State poverty rates, annual.
#'
#' State poverty rates, annual, 2000 through most recent, from the Census Bureau. One row per state per year.
#' \cr\cr
#'
#' \cr\cr
#' Includes all 50 states, United States (US), and District of Columbia (DC) for all years.
#'
#' @source Bureau of the Census, prepared by Lucy Dadayan.
#'   Table 21. Number of Poor and Poverty Rate, by State [XLS – 125k]
#'   http://www.census.gov/hhes/www/poverty/data/historical/hstpov21.xls
#' See source code on GitHub.
#' @format Data frame with 1 row per state per year
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{Calendar year, numeric}
#' \item{povrate}{State poverty rate as a percentage, numeric}
#' }
#' @examples
#'   spovrates
"spovrates"

