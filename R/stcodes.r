#' State codes and names.
#'
#' State codes, state name, BEA region abbreviation, BEA region name.
#' \cr\cr
#' Purpose is to allow easy conversion from various codes to state abbreviation, name, or region.
#' Includes 50 states, United States, District of Columbia, Puerto Rico, and the Virgin Islands.
#'
#' @source Various
#' @format Data frame with 1 row per entity
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{stfips}{State 2-digit FIPS code (leading zero for codes < 10), character}
#' \item{stcen}{State 2-digit Census Bureau code (leading zero for codes < 10), character}
#' \item{stname}{State name, character}
#' \item{beargn}{My abbreviation for the BEA region for the state, character}
#' \item{beargn.name}{Official Bureau of Economic Analysis region name, character}
#' }
#' @examples
#'   stcodes
"stcodes"

