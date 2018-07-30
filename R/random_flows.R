#' Generate random flows inputs
#'
#' This internal function generates random flows on a scale of 1 to 100 from a
#' pool of locations.
#'
#'
#' @author Thibaut Jombart
#'
#' @return A \code{data.frame} with 3 columns, which can serve as input to the
#'   constructor:
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
#' @noRd
#' @keywords internal


random_flows <- function(n, pool = letters) {
  if (missing(n)) {
    stop("n is missing with no default")
  }
  from <- sample(pool, size = n, replace = TRUE)
  to <- sample(pool, size = n, replace = TRUE)
  n <- sample(1:100, size = n, replace = TRUE)
  data.frame(from = as.character(from),
             to = as.character(to),
             n = n,
             stringsAsFactors = FALSE)
}


