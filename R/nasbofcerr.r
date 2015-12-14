#' NASBO forecast error data, annual.
#'
#' NASBO forecast error data, annual.
#'
#' @source National Association of State Budget Officers, Fall Fiscal Survey of the States. See GitHub source code for details.
#' @format Data frame with 1 row per state per year per expenditure purpose per fund type.
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{State fiscal year for which the forecast was made, integer}
#' \item{tax}{Tax for which forecast was made: pit, cit, st, sumbig3, character}
#' \item{ce}{Current estimate - current (i.e., recent) estimate of actual collections, $ millions, numeric}
#' \item{oe}{Original estimate - original (i.e., near start of fiscal year) forecast of expected collections, $ millions, numeric}
#' \item{err}{ce minus oe, $ millions}
#' \item{pcterr}{err as percent of ce}
#' \item{abspcterr}{absolute value of pcterr}
#' }
#' @examples
#'   nasbofcerr
"nasbofcerr"

