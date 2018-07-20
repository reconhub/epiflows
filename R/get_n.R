#' get the number of cases per flow
#'
#' This convenience function will return a named vector containing the number of
#' cases flowing to or from a given region.
#' 
#' @param x an epiflows object
#' @param from a character vector of length one specifying the location from which the flows originate
#' @param to   a character vector of length one specifying the location to which the flows terminate
#' @param ... unused
#'
#' @details There are three possible outputs of this function:
#' 
#'  - **no options specified**: an un-named vector, equivalent to `get_flows(x)$n`
#'  - **from = X**: a named vector of cases flowing *from* **X**
#'  - **to = X**: a named vector of cases flowing *to* **X**
#' 
#' @md
#' @return a character vector
#' @export
#'
#' @examples
#' data(YF_Brazil)
#' from  <- as.data.frame.table(YF_Brazil$T_D, stringsAsFactors = FALSE)
#' to    <- as.data.frame.table(YF_Brazil$T_O, stringsAsFactors = FALSE)[c(2, 1, 3)]
#' flows <- rbind(setNames(from, c("from", "to", "n")),
#'                setNames(to, c("from", "to", "n")))
#' locations <- YF_Brazil$states
#' others    <- setdiff(flows$to, YF_Brazil$states$location_code)
#' locations <- merge(locations,
#'                    data.frame(location_code = others, stringsAsFactors = FALSE),
#'                    by = "location_code", all = TRUE)
#' ef <- make_epiflows(flows, locations, pop_size = "location_population")
#' get_n(ef, from = "Espirito Santo")
#' get_n(ef, to   = "Espirito Santo")
get_n <- function(x, from = NULL, to = NULL, ...) {
  UseMethod("get_n")
}

#' @export
#' @rdname get_n
get_n.epiflows <- function(x, from = NULL, to = NULL, ...) {
  null_from <- is.null(from)
  null_to   <- is.null(to)
  long_from <- length(from) > 1
  long_to   <- length(to) > 1
  if (!null_from && !null_to) {
    stop("Only one of the 'from' or 'to' parameters can be non-NULL.")
  }
  if (long_from || long_to) {
    stop("The parameters 'from' and 'to' must contain a single item.")
  }
  if (null_from && null_to) {
    return(get_flows(x)$n)
  }
  if (!null_from) {
    res <- get_flows(x, from = from)
    what <- "to"
  } else {
    res <- get_flows(x, to = to)
    what <- "from"
  }
  setNames(res$n, res[[what]])
}