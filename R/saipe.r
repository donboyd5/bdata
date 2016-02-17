#' State poverty rates and median household income from SAIPE program, annual.
#'
#' State poverty rates, annual, 2003 through most recent, from the Census Bureau.
#' \cr\cr
#'
#' \cr\cr
#' Includes all 50 states, United States (US), and District of Columbia (DC) for all years.
#'
#' @source Bureau of the Census.
#'   http://www.census.gov/did/www/saipe/data/statecounty/data/index.html
#'   http://www.census.gov/did/www/saipe/downloads/estmod14/est14US.xls and earlier
#' See source code on GitHub.
#' @format Data frame with 1 row per variable per state per year
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{Calendar year, integer}
#' \item{variable}{Categorical, character}
#' \item{value}{Value for rate or number or median HHI, numeric}
#' }
#' @examples
#'   saipe
"saipe"

