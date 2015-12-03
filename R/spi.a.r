#' State personal income, annual.
#'
#' State personal income, annual, 1929-2014. Includes 50 states, DC, and US.
#'
#' @source Bureau of Economic Analysis, via API, using library(apitools)
#' @format Data frame with 1 row per state per year.
#' \describe{
#' \item{stabbr}{State postal abbreviation, factor}
#' \item{year}{Calendar year of data, numeric}
#' \item{value}{Personal income, $ thousands, numeric}
#' }
#' @examples
#'   spi.a
"spi.a"

