#' Make an `epiflows` object
#' 
#' Takes a list of location data and a matrix of flows, and creates
#' an object of class `epiflows`. An error is thrown if the input is invalid.
#' 
#' @param flows A numeric matrix of flows between locations. Row and column
#' names must denote location codes.
#' @param to An optional named vector of flows to the specified location.
#' If `flows` is missing, an epiflows object is created from two
#' vectors and a location code. Instead of flows between all locations, the
#' flow matrix will comprise only flows to and from the specified location.
#' `to` must ba a named vector of flows to the location.
#' @param from An optional named vector of flows from the specified location
#' (see above).
#' @param code An optional location code (see above).
#' @param locationsdata A data frame of location data. Locations should be
#' uniquely identified by codes from the `code` column.
#' 
#' @return An \code{epiflows} object.
#'
#' @examples
#' input_data <- Mex_travel_2009
#' flows <- input_data[[1]]
#' 
#' # Using flow matrix
#' make_epiflows(flows, input_data[[2]])
#' 
#' # Using to/from vectors
#' to <- structure(flows[["MEX"]], names = rownames(flows))
#' from <- unlist(flows["MEX", ])
#' make_epiflows(
#'   to = to,
#'   from = from,
#'   code = "MEX",
#'   locationsdata = input_data[[2]]
#' )
#' 
#' @author Pawel Piatkowski
#'
#' @export
make_epiflows <- function(flows,
                          locationsdata,
                          to = NULL,
                          from = NULL,
                          code = NULL) {
  locationsdata <- validate_line_list(locationsdata)
  if (missing(flows)) {
    validate_flow_vectors(to, from, code)
    num_flows <- length(to)
    flows <- matrix(0, ncol = num_flows, nrow = num_flows)
    colnames(flows) <- names(from)
    rownames(flows) <- names(to)
    flows[code, ] <- from
    flows[, code] <- to
  }
  flows <- validate_flows(flows, locationsdata)

  structure(
    list(flows = flows, locationsdata = locationsdata),
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

## If flows is valid (and complies with data from locationsdata),
## returns it as a data frame; if not, stops the workflow.
validate_flows <- function(flows, locationsdata) {
  stop_if_invalid(flows)
  flows <- as.data.frame(flows)

  if (!all(sapply(flows, is.numeric))) {
    stop("flows must contain numeric values only")
  }
  if (length(diffs <- setdiff(colnames(flows), rownames(flows))) > 0) {
    stop(
      "Column and row names differ\nDifferences: ",
      paste(diffs, collapse = ", ")
    )
  }
  if (length(diffs <- setdiff(colnames(flows), locationsdata$code)) > 0) {
    stop(
      "Codes should match those from locationsdata\nDifferences: ",
      paste(diffs, collapse = ", ")
    )
  }
  flows
}

validate_flow_vectors <- function(to, from, code) {
  if (is.null(to) || is.null(from) || is.null(code)) {
    stop("`make_epiflows()` requires either a flow matrix, or two vectors and a code")
  }
  if (length(code) != 1 || !is.character(code)) {
    stop("`code` must be a single character string")
  }
  if (length(to) != length(from)) {
    stop("`to` and `from` must be of equal length")
  }
  if (!code %in% names(to) || !code %in% names(from)) {
    stop(
      sprintf("'%s' missing from flow vectors", code)
    )
  }
}
