#' Make an `epiflows` object
#' 
#' Takes a list of location data and a matrix of flows, and creates
#' an object of class `epiflows`. An error is thrown if the input is invalid.
#' 
#' @param flows A numeric matrix of flows between locations. Row and column
#' names must denote location codes.
#' @param locationsdata A data frame of location data. Locations should be uniquely
#' identified by codes from the `code` column.
#' 
#' @return An \code{epiflows} object.
#'
#' @examples
#' input_data <- Mex_travel_2009
#' make_epiflows(input_data[[1]], input_data[[2]])
#' 
#' @author Pawel Piatkowski
#'
#' @export
make_epiflows <- function(flows, locationsdata) {
  locationsdata <- validate_line_list(locationsdata)
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

