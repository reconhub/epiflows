#' Access flow data
#'
#' This accessor extract flow data from an `epiflows`
#' object. `get_flows` is a generic with a method defined for
#' `epiflows` objects.
#'
#'
#' @rdname get_flows
#' 
#' @export
#' @md
#' @param ... unused
#' @seealso [get_n()]; 
#'   For location metadata: [get_locations()]; [get_vars()], [get_pop_size()], [get_coordinates()]
#' 
#' @author Zhian N. Kamvar
#'
#' @return A `data.frame` with 3 columns:
#'
#'  - `from`: origin of the flow
#'  - `to`: destination of the flow
#'  - `n`: magnitude of the flow---can be a number of passengers per
#'    unit of time, a rate, a probability of migration
#' @examples 
#' data("Brazil_epiflows")
#' head(get_flows(Brazil_epiflows))
#' get_flows(Brazil_epiflows, from = "Minas Gerais")
#' get_flows(Brazil_epiflows, to = "Minas Gerais")
#' get_flows(Brazil_epiflows, from = "Italy", to = "Minas Gerais")
get_flows <- function(x, ...) {
  UseMethod("get_flows", x)
}


#' @rdname get_flows
#' 
#' @export
#' 
#' @param x An `epiflows` object.
#' @param from a character string defining which regions should be included in the flows
#' @param to a character string defining which regions should be included in the flows
#' @param all when `TRUE`, all the columns of the flows data frame will be returned. Defaults to `FALSE`, which returns "from", "to", and "n".

get_flows.epiflows <- function(x, from = NULL, to = NULL, all = FALSE, ...) {
  the_columns <- if (all) TRUE else c("from", "to", "n")
  res <- x$contacts[, the_columns, drop = FALSE]
  null_from <- is.null(from)
  null_to   <- is.null(to)
  j   <- TRUE
  j   <- if (!null_from) j & res$from %in% from else j
  j   <- if (!null_to)   j & res$to   %in% to   else j
  res[j, , drop = FALSE]
}


