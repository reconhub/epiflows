#' @rdname get_vars
#' @param ... For `set_vars()`, any number of variables defined in
#'   [global_vars()] that can be used for mapping or modelling. This is unused
#'   in `get_vars()`
#' @export
#' @md
set_vars <- function(x, ...) {
  UseMethod("set_vars")
}

#' @rdname get_vars
#' @param name the name of the variable in [global_vars()] to assign
#' @param value the name of the column in the locations data
#' @export
#' @md
"set_vars<-" <- function(x, name, value) {
  UseMethod("set_vars<-")
}

#' @rdname get_vars
#' @export
set_vars.epiflows <- function(x, ...) {
  dots <- valid_dots(list(...))
  for (dot in names(dots)) {
    # This is necesarry so that NULL values can remove the element
    x$vars[[dot]] <- dots[[dot]]
  }
  x$vars <- if (length(x$vars) > 0) x$vars else list()
  x
}

#' @rdname get_vars
#' @export
"set_vars<-.epiflows" <- function(x, name, value) {
  if (missing(name)) {
    if (is.null(value)) {
      x$vars <- list()
      return(x)
    }
    the_call <- c(list(x), as.list(value))
  } else {
    value <- list(value)
    names(value) <- name
    the_call <- c(list(x), value)
  }
  do.call("set_vars.epiflows", the_call)
}