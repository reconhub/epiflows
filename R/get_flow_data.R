#' Get flow data for a given location
#'
#' Returns a vector (if direction is "to" or "from") or a list of 2 elements
#' (if direction is "both") to and/or from the specified location.
#'
#' @param ef An \code{epiflows} object.
#' @param direction If "to" or "from", the function returns a vector
#' of flows to or from the location, respectively.
#' If set to "both" - a two-element list with flows both to and from
#' the location.
#' 
#' @examples
#' flows <- do.call(make_epiflows, Mex_travel_2009)
#' get_flow_data(flows, direction = "both")
#'
#' @author Pawel Piatkowski
#'
#' @export
get_flow_data <- function(ef, direction = "both") {
  switch(
    direction,
    "to" = ef$flows$to,
    "from" = ef$flows$from,
    "both" = ef$flows,
    stop("`direction` should be one of: to, from, both")
  )
}

