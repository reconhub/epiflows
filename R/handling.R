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
#' flows <- do.call(make_epiflows, Mex_travel_2009)
#' flows["MEX"]
#' 
#' @export
`[.epiflows` <- function(ef, i, ...) {
  if (!is.character(i)) {
    stop("Please specify a character vector of location codes")
  }
  matching_codes <- i %in% get_codes(ef)
  if (!all(matching_codes)) {
    stop("Code not found: ", i[!matching_codes])
  }
  locationsdata <- get_location_data(ef, i)
  to <- get_flow_data(ef, direction = "to")[i]
  from <- get_flow_data(ef, direction = "from")[i]
  make_epiflows(
    to = to,
    from = from,
    code = ef$origin,
    locationsdata = locationsdata
  )
}
