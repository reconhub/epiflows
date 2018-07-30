#' Access population identifiers in epiflows objects
#'
#' @param x an epiflows object
#' @param ... arguments passed on to [epicontacts::get_id()]
#' 
#' @description this will return the unique population ID for your epiflows object.
#' @return a character vector of population IDs
#' @export
#' @md
#' @author Zhian N. Kamvar
#' @seealso [epicontacts::get_id()]; [get_vars()]; [get_pop_size()]; [global_vars()]
#' @examples
#' data("Brazil_epiflows")
#' get_id(Brazil_epiflows)
#' 
#' @importFrom epicontacts get_id
get_id <- function(x, ...) {
  epicontacts::get_id(x, ...)
}