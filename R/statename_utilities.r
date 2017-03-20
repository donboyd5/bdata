

#****************************************************************************************************
#                State name and abbreviation functions ####
#****************************************************************************************************

#' @title Get state fips code from state abbreviation
#'
#' @description \code{getstfips} get state name from state fips code
#' @usage getstfips(st)
#' @param st The state abbreviation.
#' @details get state fips code from state abbreviation
#' @keywords getstfips
#' @export
#' @examples
#' getstfips("CO")
getstfips <- function(st) {return(as.character(factor(st, levels=bdata::stcodes$stabbr, labels=bdata::stcodes$stfips)))}


#' @title Get state name from state abbreviation
#'
#' @description \code{getstname} get state name from state abbreviation
#' @usage getstname(st)
#' @param st The state abbreviation, character.
#' @details get state name from state abbreviation
#' @keywords getstname
#' @export
#' @examples
#' getstname("CO")
getstname <- function(st) {return(as.character(factor(st, levels=bdata::stcodes$stabbr, labels=bdata::stcodes$stname)))}


