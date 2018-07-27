#' Print method from epiflows objects
#'
#' Displays a short summary of an \code{epiflows} object.
#'
#' @param x An \code{epiflows} object.
#' @param ... Additional parameters (not used).
#'
#' @examples
#' data("Brazil_epiflows")
#' print(Brazil_epiflows)
#'
#' @author Zhian N. Kamvar, Thibaut Jombart
#'
#' @export
print.epiflows <- function(x, ...) {
  cat("\n/// Epidemiological Flows //\n")
  cat("\n  // class:", paste(class(x), collapse = ", "))
  cat("\n  //", format(nrow(x$linelist), big.mark = ","), "locations;", 
      format(nrow(x$contacts), big.mark = ","), "flows; directed")
  if (length(x$vars) > 0) {
    cat("\n  // optional variables:", paste(names(x$vars), collapse = ", "), "\n")
  } else {
    cat("\n  // no variables set; use set_vars() to define variables in your locations metadata\n")
  }
  cat("\n  // locations\n\n")
  print(tibble::as_data_frame(x$linelist))
  cat("\n  // flows\n\n")
  print(tibble::as_data_frame(x$contacts))
  cat("\n")
}
