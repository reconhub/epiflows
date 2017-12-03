#' Get location codes
#' 
#' Returns all location codes from an epiflows object.
#' 
#' @param ef An \code{epiflows} object.
#' 
#' @return A character vector.
#' 
#' @author Pawel Piatkowski
#'
#' @examples
#' flows <- do.call(make_epiflows, Mex_travel_2009)
#' get_codes(flows)
#' 
#' @export
get_codes <- function(ef) {
  if (!"epiflows" %in% class(ef)) {
    stop("`ef` must be an object of class epiflows")
  }
  ef$locationsdata$code
}
