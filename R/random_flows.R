#' Access flow data
#'
#' This accessor extract random flow data from an \code{epiflows}
#' object. \code{random_flows} is a generic with a method defined for
#' \code{epiflows} objects.
#'
#'
#' @rdname random_flows
#' 
#' @export
#'
#' @author Thibaut Jombart
#'
#' @return A \code{data.frame} with 3 columns:
#'
#' \itemize{
#'
#'  \item \code{from}: origin of the flow
#'
#'  \item \code{to}: destination of the flow
#' 
#'  \item \code{flow}: magnitude of the flow - can be a number of passengers per
#'  unit of time, a rate, a probability of migration
#' 
#' }

random_flows <- function(x, ...) {
  UseMethod("random_flows", x)
}


#' @rdname random_flows
#' 
#' @export
#' 
#' @param x An \code{epiflows} object.

random_flows.epiflows <- function(x, n = nrow(x$contacts), ...) {
  if (n < 1) n <- round(nrow(x$contacts), n)
  x$contacts[sample(nrow(x$contacts), n, ...), c("from", "to", "n"), drop = FALSE]
}


