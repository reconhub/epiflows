.onLoad <- function(...) {
  op <- options()
  if (!"epiflows.vars" %in% names(op)) {
    invisible(global_vars(reset = TRUE))
  } else {
    invisible(global_vars(set = TRUE))
  }
}

.onAttach <- function(...) {
  msg <- sprintf("epiflows is loaded with the following global variables in `global_vars()`:\n%s",
                 paste(global_vars(), collapse = ", ")
                 )
  packageStartupMessage(msg)
}