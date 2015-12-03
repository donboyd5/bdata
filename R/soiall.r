#' Statistics of Income data for states, annual.
#'
#' Statistics of Income data for states, annual.
#'
#' @source Internal Revenue Service, Statistics of Income branch. See GitHub source code for details.
#' https://www.irs.gov/uac/SOI-Tax-Stats-Historic-Table-2 for SOI data in general
#' @format Data frame with 1 row per state per year per income group per variable
#' \describe{
#' \item{year}{Tax year, integer}
#' \item{stabbr}{State postal abbreviation, character}
#' \item{vname}{Short variable name, character}
#' \item{incgrp}{Income group, character - CAUTION: not the same groupings in all years}
#' \item{value}{Number of items or amount (amounts are in $ thousands), numeric}
#' }
#' @examples
#'   soiall
"soiall"

