#' Print method from epiflows objects
#'
#' Displays a short summary of an \code{epiflows} object.
#'
#' @param x An \code{epiflows} object.
#' @param ... Additional parameters (not used).
#'
#' @examples
#' flows <- do.call(make_epiflows, Mex_travel_2009)
#' print(flows)
#'
#' @author Pawel Piatkowski
#'
#' @export
print.epiflows <- function(x, ...) {
  fields <- names(x)
  if (!"locationsdata" %in% fields) {
    stop("Not a valid `epiflows` object")
  }
  locations <- sort(x$locationsdata$code)
  cat("\nAn `epiflows` object\n")
  cat("Locations:\n  ")
  cat(paste(locations, collapse = ", "))
  cat("\nMetadata slots:\n  ")
  cat(paste(names(x$locationsdata), collapse = ", "))
  cat("\n\n")
}

