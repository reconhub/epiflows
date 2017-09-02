#' Get location data
#'
#' Returns data for the specified location(s).
#'
#' @param x An \code{epiflows} object
#' @param codes A character vector of location codes.
#'
#' @return A dataframe structured identically to the input linelist,
#' but limited to the specified locations.
#'
#' @examples
#' flows <- make_epiflows(Mex_travel_2009)
#' get_location_data(flows, "MEX")
#'
#' @author Pawel Piatkowski
#'
#' @export
get_location_data <- function(x, codes) {
  if (!is.vector(codes)) {
    stop("`codes` must be a vector")
  }
  subset(x$linelist, code %in% codes)
}

