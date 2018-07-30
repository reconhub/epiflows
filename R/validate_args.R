validate_arg <- function(arg, env) {
  x <- get(arg, envir = env)
  if (length(x) == 0) {
    msg <- sprintf("'%s' is missing. Please supply a %s value.",
                   arg,
                   class(x))
    stop(msg, call. = FALSE)
  }
  if (is.function(x)) {
    bod <- as.character(body(x))
    if (length(bod) == 1 && bod == "{") {
      msg <- sprintf("the function %s must take an integer and return that many simulated values",
                     arg)
      stop(msg, call. = FALSE)
    }
  }
  return(x)
}