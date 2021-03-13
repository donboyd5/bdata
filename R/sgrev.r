#' State government revenue, annual.
#'
#' State government revenue, annual.
#'
#' @source Original source: Bureau of the Census, \emph{Annual Survey of State and Local Government Finances}
#' Obtained from \url{https://willamette.edu/mba/research-impact/public-datasets/},
#' specifically \url{https://willamette.edu/~kpierson/TheGovernmentFinanceDatabase_StateData.zip}.
#' @format Data frame with 1 row per state per year per variable
#' \describe{
#' \item{stabbr}{State postal abbreviation, character}
#' \item{year}{State fiscal year, numeric}
#' \item{name}{A long name for the variable}
#' \item{variable}{Short variable name for selected variables}
#' \item{value}{State government revenue, $ thousands, numeric}
#' }
#' @examples
#'   sgrev
"sgrev"

