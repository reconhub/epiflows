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

get_flows <- function(x, ...) {
  UseMethod("get_flows", x)
}


#' @rdname get_flows
#' 
#' @export
#' 
#' @param x An \code{epiflows} object.

get_flows.epiflows <- function(x) {
  from <- sample(letters, 10, replace = TRUE)
  to <- sample(letters, 10, replace = TRUE)
  flow <- sample(1:100, 10, replace = TRUE)

  data.frame(from = as.character(from) ,
             to = as.character(to),
             n = flow,
             stringsAsFactors = FALSE)  
}


