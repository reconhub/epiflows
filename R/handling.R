#' Subset `epiflows` objects
#'
#' Returns a subset of an epiflows object.
#'
#' @param x An \code{epiflows} object.
#' @param i An integer, logical, or character vector of one or more location.
#' @param j An integer, logical, or character vector to subset the flows data frame.
#' @param k A character vector of one or more columns to be retained in the location data.
#' @param l A character vector of one or more columns to be retained in the flows data frame. Note: if using numbers, the first column stands for the first column after "n".
#' @param ... Additional parameters passed to \code{\link{[.epicontacts}}.
#'
#' @return An \code{epiflows} object.
#' 
#' @description An epiflows object inherits the epicontacts class, so the
#'   subsetting mechanism is also inherited. The benefits is that it's
#'   extremely flexible. However, this also means that it's possible for
#'   the contacts to contain IDs that are not present in the locations 
#'   metadata and vice versa. The best way to consistently subset an
#'   epiflows object is present in the examples. 
#'
#' @author Zhian N. Kamvar
#'
#' @examples
#' data(Brazil_epiflows)
#' # You can subset, but the flows information will still be present
#' Brazil_epiflows[j = "Espirito Santo"]
#' # To help with this, use `thin` from epiflows
#' epicontacts::thin(Brazil_epiflows[j = "Espirito Santo"])
#' epicontacts::thin(Brazil_epiflows[j = c("Espirito Santo", "Rio de Jenerio")])
#' 
#' @export
`[.epiflows` <- function(x, i, j, k = TRUE, l = TRUE, ...) {
  if (missing(i)) {
    i <- TRUE
  }
  if (missing(j)) {
    j <- TRUE
  }
  # Retaining n
  l       <- if (is.numeric(l)) c(1L, l + 1L)
  dots    <- list(...)
  contact <- if ("contacts" %in% names(dots)) dots$contacts else "either"
  NextMethod(contacts = contact)
}
