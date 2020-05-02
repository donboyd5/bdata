#' Recession dates.
#'
#' Peak and trough dates for recessions.
#'
#' @format Data frame with 1 row per recession
#' \describe{
#'   \item{rec_year}{Calendar year in which recession began, numeric}
#'   \item{peak}{first day of month in which recession began, date}
#'   \item{trough}{first day of month in which recession ended, date}
#'   \item{peak_decimal}{numeric - first day of month in which recession began, numeric}
#'   \item{trough_decimal}{numeric - first day of month in which recession ended, numeric}
#'   \item{peak_quarter}{first day of quarter in which recession began, date}
#'   \item{trough_quarter}{first day of quarter in which recession ended, date}
#' }
#' @source National Bureau of Economic Research (NBER)
#'  http://www.nber.org/cycles.html
#' @examples
#'   recessions
"recessions"

