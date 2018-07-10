.onLoad <- function(...) {
  epiflows.vars <- c("coordinates", 
                     "pop_size", 
                     "duration_stay",
                     "first_date",
                     "last_date",
                     "num_cases_time_window"
  )
  op <- options()
  if (!"epiflows.vars" %in% names(op)) {
    options(epiflows.vars = epiflows.vars)
  }
}