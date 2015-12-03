#' Recession dates.
#'
#' Peak and trough dates for recessions.
#'
#' @source National Bureau of Economic Research (NBER)
#'  http://www.nber.org/cycles.html
#' @format Data frame with 1 row per recession
#' \describe{
#' \item{year}{Calendar year in which recession began}
#' \item{peak}{Date - first day of month in which recession began}
#' \item{trough}{Date - first day of month in which recession ended}
#' \item{rqsd}{Date - first day of quarter in which recession began}
#' \item{rqed}{Date - first day of quarter in which recession ended}
#' }
#' @examples
#'   recessions
"recessions"

