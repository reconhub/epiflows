#' Access flow data
#'
#' This accessor extract location data from an `epiflows` object.
#' `get_locations` is a generic with a method defined for `epiflows`
#' objects.
#'
#'
#' @rdname get_locations
#'
#' @param x An `epiflows` object.
#' @param ... unused
#' 
#' @export
#' @md
#' @seealso [get_flows()]; [get_id()]; [get_pop_size()]; [get_vars()];
#'   [get_coordinates()]; [global_vars()]
#' @author Thibaut Jombart, Zhian Kamvar
#'
#' @return A `data.frame` with at least 1 column called `id`,
#'   specifying the id of the location used in the `flows` data frame.
#' @examples
#' data("Brazil_epiflows")
#' get_locations(Brazil_epiflows)
get_locations <- function(x, ...) {
  UseMethod("get_locations", x)
}


#' @rdname get_locations
#' 
#' @export
#' 

get_locations.epiflows <- function(x, ...) {
  x$linelist
}


