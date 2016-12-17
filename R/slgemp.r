#' State and local government employment.
#'
#' State and local government employment (state-local, state, local), annual, 1979-2013.
#' Includes 50 states, DC, and US.
#' Still in early stages of development.
#'
#' @source Bureau of the Census. Primarily http://www.census.gov/govs/apes/historical_data.html for
#' recent history, and http://www2.census.gov/pub/outgoing/govs/special60/ for older history.
#' @format Data frame with 1 row per state per level (State-local, state, local), per variable, per year.
#' \describe{
#' \item{stabbr}{State postal abbreviation, factor}
#' \item{year}{year of data - can be March or October of year, see Census documentation, numeric}
#' \item{level}{Level of government: slg, sg, lg, character}
#' \item{fcode}{function code, character}
#' \item{fname}{function name, character}
#' \item{vtype}{Variable type (emp.ft, emp.pt, emp.tot, emp.fte, pay.ft, pay.pt, pay.tot, hrs.pt), character}
#' \item{value}{# of employees in thousands or monthly pay in dollars, numeric}
#' }
#' @examples
#'   head(slgemp)
"slgemp"

