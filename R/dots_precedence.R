dots_precedence <- function(name, value, env) {
  if (length(env$dots) == 0) {
    return(setNames(list(value), name))
  }
  the_match <- pmatch(names(env$dots), name)
  the_match <- !is.na(the_match)
  if (sum(the_match) == 0) {
    res <- value
  } else {
    the_match <- names(env$dots)[the_match]
    # Set the value to what's in the dots
    res       <- env$dots[[the_match]]
    # Remove this from the dot list
    env$dots[[the_match]] <- NULL
  }
  setNames(list(res), name)
}