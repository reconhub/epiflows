#' Access flow data
#'
#' This accessor extract location data from an \code{epiflows}
#' object. \code{get_vars} is a generic with a method defined for
#' \code{epiflows} objects.
#'
#'
#' @rdname get_vars
#' 
#' @export
#'
#' @author Thibaut Jombart, Zhian Kamvar
#'
#' @return A data frame with one 

get_vars <- function(x, ...) {
  UseMethod("get_vars", x)
}


#' @rdname get_vars
#' 
#' @export
#' 
#' @param x An \code{epiflows} object.
#' @param y a valid character string specifying the variable desired. If
#'   \code{NULL} (default), the names of the available vars will be returned. 

get_vars.epiflows <- function(x, y = NULL) {
  if (is.null(y)) {
    return(x$vars)
  }
  x$linelist[x$vars[[y]]]
}


