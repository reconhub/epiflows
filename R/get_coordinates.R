#' @rdname get_vars
#'
#' @export
get_coordinates <- function(x, ...) {
  UseMethod("get_coordinates", x)
}


#' @export
#' @rdname get_vars
#' @param location a character specifying a single location to return as a vector of
#'   coordinates. You cannot specify multiple locations with this parameter.
#'   Defaults to `NULL`, indicating all locations.
#'
get_coordinates.epiflows <- function(x, location = NULL, ...) {
  res <- try(get_vars(x, "coordinates", id = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    xprint <- deparse(substitute(x))
    stop(sprintf("coordinates are not set in %s", xprint))
  }
  if (is.character(location) && length(location) == 1L) {
    res <- as.numeric(drop(res[res$id == location, 2:3]))
  }
  res
}
