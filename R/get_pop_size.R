#' @return a named vector of population sizes
#' @rdname get_vars
#' @export
#'
#' @examples
#' 
get_pop_size <- function(x) {
  UseMethod("get_pop_size")
}

#' @rdname get_vars
#' @export
get_pop_size.epiflows <- function(x) {
  return(get_vars(x, "pop_size", id = FALSE, vector = TRUE))
}