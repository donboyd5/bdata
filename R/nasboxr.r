#' NASBO expenditure report data, annual.
#'
#' NASBO expenditure report data, annual.
#'
#' @source National Association of State Budget Officers, EXP_DATA 1991-2014.xls. See GitHub source code for details.
#' @format Data frame with 1 row per state per year per expenditure purpose per fund type
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{State fiscal year, numeric}
#' \item{purpose}{Major expenditure category short name}
#' \item{fundtype}{Fund type abbreviation}
#' \item{value}{Expenditure amount, $ millions}
#' \item{purposef}{Purpose factor (description)}
#' \item{fundtype}{Fund type factor (description)}
#' }
#' @examples
#'   nasboxr
"nasboxr"

