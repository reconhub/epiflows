.onLoad <- function(...) {
  epiflows.vars <- c("coords", 
                     "pop_size", 
                     "duration_stay"
  )
  op <- options()
  if (!"epiflows.vars" %in% names(op)) {
    options(epiflows.vars = epiflows.vars)
  }
}