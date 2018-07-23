#' Access flow data
#'
#' This accessor extract flow data from an \code{epiflows}
#' object. \code{get_flows} is a generic with a method defined for
#' \code{epiflows} objects.
#'
#'
#' @rdname get_flows
#' 
#' @export
#'
#' @param ... unused
#' 
#' @author Zhian N. Kamvar
#'
#' @return A \code{data.frame} with 3 columns:
#'
#' \itemize{
#'
#'  \item \code{from}: origin of the flow
#'
#'  \item \code{to}: destination of the flow
#' 
#'  \item \code{n}: magnitude of the flow - can be a number of passengers per
#'  unit of time, a rate, a probability of migration
#' 
#' }

get_flows <- function(x, ...) {
  UseMethod("get_flows", x)
}


#' @rdname get_flows
#' 
#' @export
#' 
#' @param x An \code{epiflows} object.
#' @param from a character string defining which regions should be included in the flows
#' @param to a character string defining which regions should be included in the flows

get_flows.epiflows <- function(x, from = NULL, to = NULL, ...) {
  res <- x$contacts[, c("from", "to", "n"), drop = FALSE]
  null_from <- is.null(from)
  null_to   <- is.null(to)
  j   <- TRUE
  j   <- if (!null_from) j & res$from %in% from else j
  j   <- if (!null_to)   j & res$to   %in% to   else j
  res[j, , drop = FALSE]
}


