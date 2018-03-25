#' Make an `epiflows` object
#' 
#' Takes a list of location data and a matrix of flows, and creates
#' an object of class `epiflows`. An error is thrown if the input is invalid.
#' 
#' @param to A named vector of flows to the specified location.
#' A one-row data frame or a list is also allowed.
#' @param from A named vector of flows from the specified location
#' (see above).
#' @param code Character string; location code of the origin.
#' @param locationsdata A data frame of location data. Locations should be
#' uniquely identified by codes from the `code` column.
#' 
#' @return An \code{epiflows} object.
#'
#' @examples
#' # TBD
#' 
#' @author Pawel Piatkowski
#'
#' @export
make_epiflows <- function(to,
                          from,
                          code,
                          locationsdata) {
  locationsdata <- validate_line_list(locationsdata)
  flows <- validate_flow_vectors(to, from)
  code <- validate_origin(code, locationsdata)

  structure(
    list(flows = flows, locationsdata = locationsdata, origin = code),
    class = "epiflows"
  )
}

## Terminates the workflow and throws an error
## when x is NULL, NA, or an empty object (e.g., character(0)).
stop_if_invalid <- function(x) {
  object_name <- as.character(substitute(x))

  if (is.null(x)) {
    stop(object_name, " is NULL")
  }
  if (length(x) == 0) {
    stop(object_name, " is empty")
  }
  if (all(is.na(x))) {
    stop(object_name, " is NA")
  }
}

## If locationsdata is valid, returns it as a data frame.
## If not, stops the workflow.
validate_line_list <- function(locationsdata) {
  stop_if_invalid(locationsdata)

  locationsdata <- as.data.frame(locationsdata, stringsAsFactors = FALSE)
  if (!"code" %in% colnames(locationsdata)) {
    stop("`code` column is mandatory in locationsdata")
  }
  locationsdata$code <- as.character(locationsdata$code)
  locationsdata
}

## If both vectors are valid, returns a combined vector of flows.
## If not, stops the workflow.
validate_flow_vectors <- function(to, from, locationsdata) {
  to <- unlist(to)
  from <- unlist(from)
  names_to <- names(to)
  names_from <- names(from)
  if (length(to) == 0 || length(from) == 0 ||
      is.null(names_to) || is.null(names_from)) {
    stop("`make_epiflows()` requires two named vectors (or lists) of flows")
  }
  codes <- locationsdata$code
  missing_names <- setdiff(codes, union(names_to, names_from))
  if (length(missing_names) > 0) {
    stop(
      sprintf(
        "%s missing from `locationsdata`",
        paste(missing_names, collapse = ", ")
      )
    )
  }
  list(to = to, from = from)
}

validate_origin <- function(code, locationsdata) {
  if (length(code) != 1 || !is.character(code)) {
    stop("`code` should be a character string")
  }
  if (!code %in% locationsdata$code) {
    stop(sprintf("'%s' missing from `locationsdata`", code))
  }
  code
}
