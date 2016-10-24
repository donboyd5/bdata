#' State government taxes, annual.
#'
#' State government taxes, annual.
#'
#' @source Bureau of the Census, historical database (1902-2009) and state tax files (2010-2015). See GitHub source code for details.
#' @format Data frame with 1 row per state per year per variable
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{State fiscal year, numeric}
#' \item{ic}{Census Bureau item code (NA if an aggregate value)}
#' \item{vname}{Variable name}
#' \item{variable}{Variable description}
#' \item{value}{State tax revenue, $ thousands, numeric}
#' }
#' @examples
#'   sgtax.a
"sgtax.a"

