#' Access flow data
#'
#' This accessor extracts variables from the `locations` data frame in an
#' `epiflow` object. `get_vars` is a generic with a method defined for
#' `epiflows` objects.
#' 
#' The 
#'
#' @rdname get_vars
#' @md
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
#' @param what a valid character string specifying the variable desired. If
#'   \code{NULL} (default), the names of the available vars will be returned. 

get_vars.epiflows <- function(x, what = NULL) {
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
    }
  }
  x$linelist[x$vars[[what]]]
}


