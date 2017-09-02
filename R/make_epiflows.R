#' Make an `epiflows` object
#' 
#' Takes a list of location data and a matrix of flows, and creates
#' an object of class `epiflows`. An error is thrown if the input is invalid.
#' 
#' @param linelist A data frame of location data. Locations should be uniquely
#' identified by codes from the `code` column.
#' @param flows A numeric matrix of flows between locations. Row and column
#' names must denote location codes.
#' 
#' @return An \code{epiflows} object.
#'
#' @examples
#' input_data <- Mex_travel_2009
#' make_epiflows(input_data[[2]], input_data[[1]])
#' 
#' @author Pawel Piatkowski
#'
#' @export
make_epiflows <- function(linelist, flows) {
  linelist <- validate_line_list(linelist)
  flows <- validate_flows(flows, linelist)

  structure(
    list(flows = flows, linelist = linelist),
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

## If linelist is valid, returns it as a data frame.
## If not, stops the workflow.
validate_line_list <- function(linelist) {
  stop_if_invalid(linelist)

  linelist <- as.data.frame(linelist, stringsAsFactors = FALSE)
  if (!"code" %in% colnames(linelist)) {
    stop("`code` column is mandatory in linelist")
  }
  linelist$code <- as.character(linelist$code)
  linelist
}

## If flows is valid (and complies with data from linelist),
## returns it as a data frame; if not, stops the workflow.
validate_flows <- function(flows, linelist) {
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
  if (length(diffs <- setdiff(colnames(flows), linelist$code)) > 0) {
    stop(
      "Codes should match those from linelist\nDifferences: ",
      paste(diffs, collapse = ", ")
    )
  }
  flows
}

