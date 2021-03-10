#' State map label longitude and latitude points.
#'
#' Longitude (x) and latitude (y) values for points suitable for putting state abbreviations or names
#' on a map of the 50 states generated by usmap::us_map(). 
#' 
#' It is difficult to determine algorithmically precisely where these points should be -- centroids
#' or simple midpoints of a state's longitude and latitude are not always best. Good labeling points
#' have been developed in the package usmap. This data frame uses those points.
#' 
#' Having the data points in a separate data frame allows them to be used in ways beyond those built
#' into the package usmap.
#' 
#' Includes 50 states and DC. Alaska and Hawaii are in the lower left corner of the map and the labels
#' are placed there.
#'
#' @source usmap package developed by Paolo Di Lorenzo (see https://dilorenzo.pl/pkgs/usmap/). The GitHub
#' package page is https://github.com/pdil/usmap and the specific file location is 
#' https://github.com/pdil/usmap/blob/master/inst/extdata/us_states_centroids.csv.
#' At CRAN: https://cran.r-project.org/web/packages/usmap/index.html.
#' 
#' @format Data frame with 1 row per state per state.
#' \describe{
#' \item{x}{Longitude, numeric}
#' \item{y}{Latitude, numeric}
#' \item{fips}{State FIPS code, character}
#' \item{stabbr}{State postal abbreviation, character}
#' \item{stname}{State name, character}
#' }
#' @examples
#'   head(statemap_labels)
#'   
#'\dontrun{
#'   # check how the labeling looks on a U.S. map
#'   p <- ggplot(data = usmap::us_map(), aes(x = x, y = y)) +
#'     geom_polygon(aes(group=group), fill="white", color = "gray90", size = 0.1, na.rm=TRUE) +
#'     coord_equal()
#'   p
#'   
#'   p + geom_text(data = statemap_labels, aes(x, y, label = stabbr), size = 2)
#'   
#'   p + geom_text(data = statemap_labels, aes(x, y, label = stname), size = 2)
#'}
#'   
"statemap_labels"

