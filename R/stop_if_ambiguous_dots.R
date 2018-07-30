stop_if_ambiguous_dots <- function(dots, fun) {
  funnames <- names(formals(fun = fun))
  dnames   <- names(dots)
  res      <- charmatch(dnames, funnames)
  nas      <- is.na(res)
  partials <- res == 0
  any_nas      <- any(nas)
  any_partials <- any(partials, na.rm = TRUE)
  msg <- "\n"
  if (any_nas) {
    msg <- sprintf("%s  Unmatched arguments: %s\n", 
                   msg,
                   paste(dnames[nas], collapse = ", ")
                   )
  } 
  if (any_partials) {
    partials <- ifelse(is.na(partials), FALSE, partials)
    msg <- sprintf("%s  Matched multiple arguments: %s",
                   msg,
                   paste(dnames[partials], collapse = ", ")
                   )
  }
  if (any_nas || any_partials) {
    msg <- sprintf("\nAmbiguous or unknown arguments to the function %s() were found:\n%s",
                   fun,
                   msg
                   )
    stop(msg, call. = FALSE)
  }
  dots
}