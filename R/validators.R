#' Validate dots passed to `epiflows.data.frame`
#'
#' This validates the names of the variables the user wants to include in
#' subsequent analyses by checking against a hard-coded list.
#'
#' @param dots a named list
#'
#' @return a named list
#' @noRd
valid_dots <- function(dots) {
  if (length(dots) == 0) return(dots)
  # These names can be expanded
  out <- dots[names(dots) %in% getOption("epiflows.vars")]
  #
  # TODO: Throw error if there are any unnamed arguments
  #
  if (length(out) < length(dots)) {
    diffnames <- paste(setdiff(names(dots), names(out)), collapse = "', '")
    msg <- paste0("Unknown variables were found: '", diffnames, "'\n\n",
                 "Please inspect them to make sure they are correct.",
                 "If they are correct, please add them using:\n\n",
                 "\tglobal_vars('", diffnames, "', set = TRUE)\n\n",
                 "Type ?global_vars for details")
    stop(msg, call. = FALSE)
  }
  out
}

## Terminates the workflow and throws an error
## when x is NULL, NA, or an empty object (e.g., character(0)).
stop_if_invalid_column <- function(x, linelist) {
  res <- linelist[[x]]
  if (is.null(res))     stop("column '", x, "' is NULL")
  if (length(res) == 0) stop("column '", x, "' is empty")
  if (all(is.na(res)))  stop("column '", x, "' is NA")
}

valid_flows <- function(flows) {
  # No duplicate flows can be present
  ids    <- flows[c("from", "to")]
  dupids <- duplicated(ids)
  dupflw <- duplicated(flows)
  if (sum(duplicated(ids)) > 0L || sum(duplicated(flows)) > 0L) {
    # The flows between identical IDS are different
    if (!identical(dupids, dupflw)) {
      msg <- paste("Duplicated IDs found in the data with different flows.",
                   "Please de-duplicate your data before proceeding.")
      stop(msg, call. = FALSE)
    } else {
      warning("Pruning duplicate entries in the flows.", call. = FALSE)
      return(flows[!dupids, , drop = FALSE])
    }
  }
  flows
}
