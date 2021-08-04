
# navigation notes ----
# alt-o, shift-alt-o
# alt-l, shift-alt-l

# alt-r

# package building ---
# http://r-pkgs.had.co.nz/  # IMPORTANT

# Install Package: 'Ctrl + Shift + B'
# Check Package: 'Ctrl + Shift + E'
# Test Package: 'Ctrl + Shift + T'


# edit the DESCRIPTION file using these commands
# library(usethis)
# This block will update the DESCRIPTION file when devtools::document() is run (or via shift-ctrl-d) ####
# usethis::use_package("plyr")
# usethis::use_package("dplyr")
# usethis::use_package("ggplot")
# usethis::use_package("lubridate")
# usethis::use_package("magrittr")
# # usethis::use_package("precis")
# usethis::use_package("scales")
# usethis::use_package("stats")
# usethis::use_package("tibble")
# usethis::use_package("zoo")

# usethis::use_package("bdata")

usethis::use_package("usmap")

# build package source ----
devtools::build(
  pkg = here::here(),
  path = NULL,
  binary = FALSE,
  vignettes = FALSE,
  manual = FALSE,
  args = NULL,
  quiet = FALSE)

# other things ----



# This block (if uncommented) will update the NAMESPACE file when devtools::document() is run (or via shift-ctrl-d) ####

#' #' Pipe operator
#' #'
#' #' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#' #'
#' #' @name %>%
#' #' @rdname pipe
#' #' @keywords internal
#' #' @export
#' #' @importFrom magrittr %>%
#' #' @usage lhs \%>\% rhs
#' NULL
