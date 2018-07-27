#' get the number of cases per flow
#'
#' This convenience function will return a named vector containing the number of
#' cases flowing to or from a given region.
#' 
#' @param x an epiflows object
#' @param from a character vector of length one specifying the location from which the flows originate
#' @param to   a character vector of length one specifying the location to which the flows terminate
#' @param ... unused
#'
#' @details There are three possible outputs of this function:
#' 
#'  - **no options specified**: an un-named vector, equivalent to `get_flows(x)$n`
#'  - **from = X**: a named vector of cases flowing *from* **X**
#'  - **to = X**: a named vector of cases flowing *to* **X**
#' 
#' @md
#' @return a character vector
#' @export
#' @seealso [get_flows()]; 
#'   For location metadata: [get_vars()], [get_pop_size()], [get_coordinates()]
#'
#' @examples
#' data(Brazil_epiflows)
#' get_n(Brazil_epiflows, from = "Espirito Santo")
#' get_n(Brazil_epiflows, to   = "Espirito Santo")
get_n <- function(x, from = NULL, to = NULL, ...) {
  UseMethod("get_n")
}

#' @export
#' @rdname get_n
get_n.epiflows <- function(x, from = NULL, to = NULL, ...) {
  null_from <- is.null(from)
  null_to   <- is.null(to)
  long_from <- length(from) > 1
  long_to   <- length(to) > 1
  if (!null_from && !null_to) {
    stop("Only one of the 'from' or 'to' parameters can be non-NULL.")
  }
  if (long_from || long_to) {
    stop("The parameters 'from' and 'to' must contain a single item.")
  }
  if (null_from && null_to) {
    return(get_flows(x)$n)
  }
  if (!null_from) {
    res <- get_flows(x, from = from)
    what <- "to"
  } else {
    res <- get_flows(x, to = to)
    what <- "from"
  }
  setNames(res$n, res[[what]])
}