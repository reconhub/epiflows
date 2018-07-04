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
  cat("\n/// Epidemiological Flows //\n")
  cat("\n  // class:", paste(class(x), collapse = ", "))
  cat("\n  //", format(nrow(x$linelist), big.mark = ","), "locations;", 
      format(nrow(x$contacts), big.mark = ","), "flows; directed")
  if (length(x$vars > 0)) {
    cat("\n  // optional variables:", paste(names(x$vars), collapse = ", "), "\n")
  }
  cat("\n  // locations\n\n")
  print(dplyr::tbl_df(x$linelist))
  cat("\n  // flows\n\n")
  print(dplyr::tbl_df(x$contacts))
  cat("\n")
}
