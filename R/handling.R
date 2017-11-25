#' Subset `epiflows` objects
#'
#' Returns a subset of an epiflows object based on location code(s).
#'
#' @param ef An \code{epiflows} object.
#' @param i A character vector of one or more location codes.
#' @param ... Additional parameters (not used).
#'
#' @return An \code{epiflows} object.
#'
#' @author Pawel Piatkowski
#'
#' @examples
#' flows <- Mex_travel_2009
#' flows["MEX"]
#' 
#' @export
`[.epiflows` <- function(ef, i, ...) {
  if (!is.character(i)) {
    stop("Please specify a character vector of location codes")
  }
  locationsdata <- subset(ef$locationsdata, code %in% i)
  flows <- as.data.frame(ef$flows[i, i])
  rownames(flows) <- colnames(flows) <- i
  make_epiflows(flows, locationsdata)
}

