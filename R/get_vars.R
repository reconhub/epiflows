#' Access flow data
#'
#' This accessor extracts variables from the `locations` data frame in an
#' `epiflow` object. `get_vars` is a generic with a method defined for
#' `epiflows` objects.
#'
#' The function `get_coordinates()` is equivalent to `get_vars(x, "coordinates", id = TRUE)`
#'
#' @rdname get_vars
#' @md
#'
#' @export
#'
#' @author Thibaut Jombart, Zhian Kamvar
#'
#' @return A data frame with the variables requested

get_vars <- function(x, ...) {
  UseMethod("get_vars", x)
}


#' @rdname get_vars
#'
#' @export
#'
#' @param x An `epiflows` object.
#' @param what a valid character string specifying the variable desired. If
#'   `NULL` (default), the names of the available vars will be returned.
#' @param id a logical. If `TRUE` (default), the `id` column of the locations
#'   will be the first column of the data frame. if `FALSE`, the variable will
#'   be returned without identifiers.

get_vars.epiflows <- function(x, what = NULL, id = TRUE) {
  if (is.null(what)) {
    return(x$vars)
  }
  if (!what %in% names(x$vars)) {
    if (!what %in% names(x$linelist)) {
      available_vars <- paste(names(x$linelist)[-1], collapse = " ")
      msg <- paste("%s does not appear to be in the locations data.\n",
                   "\nThe variables present are:\n%s")
      msg <- sprintf(msg, what, available_vars)
      stop(msg)
    } else {
      what <- if (id) c("id", what) else what
      return(x$linelist[what])
    }
  }
  if (id) {
    return(x$linelist[c("id", x$vars[[what]])])
  } else {
    return(x$linelist[x$vars[[what]]])
  }
}


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
get_coordinates.epiflows <- function(x, location = NULL) {
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
