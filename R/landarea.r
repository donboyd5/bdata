#' Census Bureau area data for the US, states, and counties.
#'
#' Census Bureau area: total, land, and water area, as well as Census 2010 SF1 population and housing.
#'
#' @source Bureau of the Census
#' @format Data frame with 1 row per area.
#' \describe{
#' \item{fips}{fips code, character}
#' \item{stabbr}{state abbreviation, character}
#' \item{areaname}{state abbreviation, character}
#' \item{geotype}{US, state, or county, character}
#' \item{pop}{Census 2010 population, unrevised, numeric}
#' \item{housunits}{Census 2010 housing units, unrevised, numeric}
#' \item{totarea}{total area, square miles, numeric}
#' \item{waterarea}{water area, square miles, numeric}
#' \item{landarea}{land area, square miles, numeric}
#' \item{popden}{population density, numeric}
#' \item{housden}{housing density, numeric}
#' }
#' @examples
#'   landarea
"landarea"


