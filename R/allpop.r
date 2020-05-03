#' State population, annual, with more details than spop.a.
#'
#' State population, annual, 1900 through latest available year, from the Census Bureau. One row per state per year. Some years may have more than one value.
#' \cr\cr
#' See spop.a documentation. This includes the filename from which the data came, and can include multiple values
#'  for a year if, for example, the year had an intercensal estimate and a census estimate.
#' \cr\cr
#' @format Data frame with 1 row per state per year
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{Calendar year, numeric}
#' \item{value}{State population, number of individuals, numeric}
#' \item{type}{intercensal, postcensal, or census}
#' \item{fname}{name of Census file from which data were pulled}
#' }
#' @source Bureau of the Census, http://www.census.gov/popest and various subdirectories and archives within.
#' http://www.census.gov/popest/ in general. See source code on GitHub for detailed sources.
#' @examples
#'   allpop
"allpop"

