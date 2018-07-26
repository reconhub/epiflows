check_estimate_args <- function(env, ..., fun = "estimate_risk_spread.default") {
  dots  <- tryCatch(stop_if_ambiguous_dots(list(...), fun = "estimate_risk_spread.default"),
                    error = function(e) e)
  the_args <- names(formals(fun))
  the_args <- the_args[the_args != "..."]
  res <- vector(mode = "character", length = length(the_args))
  for (arg in the_args) {
    the_try <- tryCatch(validate_arg(arg, env), error = function(e) e)
    if (inherits(the_try, "simpleError")) {
      res[arg] <- the_try$message
    }
  }
  res <- res[res != ""]
  res <- if (inherits(dots, "simpleError")) c(res, dots$message) else res
  if (length(res) > 0) {
    msg <- sprintf("\nThe function estimate_risk_spread() needs all of the parameters to run.\n\n%s",
                   paste(res, collapse = "\n"))
    stop(msg, call. = FALSE)
  }
  invisible(NULL)
}