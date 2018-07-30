#' Get population size for each entry in locations
#' 
#' @param x an epiflows object
#' @return a named vector of population sizes
#' @export
#' @md
#' @seealso [get_vars()], [global_vars()]
#'
#' @examples
#' data("Brazil_epiflows")
#' get_pop_size(Brazil_epiflows)
get_pop_size <- function(x) {
  UseMethod("get_pop_size")
}

#' @rdname get_pop_size
#' @export
get_pop_size.epiflows <- function(x) {
  get_vars(x, "pop_size", id = FALSE, vector = TRUE)
}