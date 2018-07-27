#' Get population size for each entry in locations
#' 
#' @param x an epiflows object
#' @return a named vector of population sizes
#' @export
#'
#' @examples
#' data("Brazil_epiflows")
#' get_pop_size(Brazil_epiflows)
get_pop_size <- function(x) {
  UseMethod("get_pop_size")
}

#' @rdname get_vars
#' @export
get_pop_size.epiflows <- function(x) {
  return(get_vars(x, "pop_size", id = FALSE, vector = TRUE))
}