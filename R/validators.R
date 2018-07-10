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
  # These names can be expanded
  out <- dots[names(dots) %in% getOption("epiflows.vars")]
  #
  # TODO: Throw error if there are any unnamed arguments
  #
  if (length(out) < length(dots)) {
    diffnames <- paste(setdiff(names(dots), names(out)), collapse = ",")
    warning(paste("Ignoring the following variables:", diffnames))
  }
  out
}

## Terminates the workflow and throws an error
## when x is NULL, NA, or an empty object (e.g., character(0)).
stop_if_invalid <- function(x) {
  object_name <- as.character(substitute(x))

  if (is.null(x)) {
    stop(object_name, " is NULL")
  }
  if (length(x) == 0) {
    stop(object_name, " is empty")
  }
  if (all(is.na(x))) {
    stop(object_name, " is NA")
  }
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
