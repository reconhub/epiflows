#' Print method from epiflows objects
#'
#' Displays a short summary of an \code{epiflows} object.
#'
#' @param ef An \code{epiflows} object.
#' @param ... Additional parameters (not used).
#'
#' @examples
#' flows <- make_epiflows(Mex_travel_2009)
#' print(flows)
#'
#' @author Pawel Piatkowski
#'
#' @export
print.epiflows <- function(ef, ...) {
  fields <- names(ef)
  if (!"linelist" %in% fields) {
    stop("Not a valid `epiflows` object")
  }
  locations <- sort(ef$linelist$code)
  cat("\nAn `epiflows` object\n")
  cat("Locations:\n  ")
  cat(paste(locations, collapse = ", "))
  cat("\nMetadata slots:\n  ")
  cat(paste(names(ef$linelist), collapse = ", "))
  cat("\n\n")
}

