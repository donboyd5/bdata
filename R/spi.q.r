#' State personal income, quarterly.
#'
#' State personal income, quarterly, 1948 through latest. Includes 50 states, DC, and US.
#'
#' @source Bureau of Economic Analysis, via API, using library(apitools)
#' @format Data frame with 1 row per state and quarter.
#' \describe{
#' \item{stabbr}{State postal abbreviation, factor}
#' \item{date}{First day of quarter, date}
#' \item{value}{Personal income, $ thousands, numeric}
#' }
#' @examples
#'   spi.q
"spi.q"

