#' Access location metadata
#'
#' This accessor extracts variables from the `locations` data frame in an
#' `epiflow` object. `get_vars` is a generic with a method defined for
#' `epiflows` objects.
#'
#' @rdname get_vars
#' @md
#' @param x An `epiflows` object.
#'
#' @export
#'
#' @author Thibaut Jombart, Zhian Kamvar
#' @seealso [global_vars()]; [make_epiflows()]; [get_pop_size()]; [get_id()]
#' @return A data frame with the variables requested
#' @examples 
#' data("Brazil_epiflows")
#' get_vars(Brazil_epiflows) # defined global variables pointint to column names
#' get_vars(Brazil_epiflows, "duration_stay")
#' get_vars(Brazil_epiflows, "duration_stay", vector = TRUE)
get_vars <- function(x, ...) {
  UseMethod("get_vars", x)
}

#' @rdname get_vars
#'
#' @export
#'
#' @param what a valid character string specifying the variable desired. If
#'   `NULL` (default), the names of the available vars will be returned.
#' @param id a logical. If `TRUE` (default), the `id` column of the locations
#'   will be the first column of the data frame. if `FALSE`, the variable will
#'   be returned with identifiers as row names.
#' @param vector if `TRUE` the result will be coerced into a vector (or a matrix in the case of coordinates)
get_vars.epiflows <- function(x, what = NULL, id = TRUE, vector = FALSE, ...) {
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
  } else {
    what <- x$vars[[what]]
    what <- if (is.numeric(what)) names(x$linelist)[what] else what
  }
  if (id) {
    res <- x$linelist[c("id", what)]
  } else {
    res           <- x$linelist[what]
    rownames(res) <- x$linelist$id
  }
  if (vector) {
    if (id) {
      rownames(res) <- res$id
      res           <- res[, -1, drop = FALSE]
    }
    res <- as.matrix(res)
    res <- if (ncol(res) == 1) drop(res) else res
  }
  res
}
