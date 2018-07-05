#' Access flow data
#'
#' This accessor extract location data from an \code{epiflows} object.
#' \code{get_locations} is a generic with a method defined for \code{epiflows}
#' objects.
#'
#'
#' @rdname get_locations
#'
#' @export
#'
#' @author Thibaut Jombart, Zhian Kamvar
#'
#' @return A \code{data.frame} with at least 1 column called \code{id},
#'   specifying the id of the location used in the `flows` data frame.
#'
get_locations <- function(x, ...) {
  UseMethod("get_locations", x)
}


#' @rdname get_locations
#' 
#' @export
#' 
#' @param x An \code{epiflows} object.

get_locations.epiflows <- function(x) {
  x$linelist
}


