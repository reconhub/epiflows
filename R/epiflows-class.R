#' Read linelist and contact data
#'
#' This function reads data stored as data.frame containing linelist (case
#' information, where each row corresponds to a unique patient), and contacts
#' between patients. Common identifiers should be used in the two data sources
#' for matching to be achieved.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param linelist a \link{data.frame} with at least one column providing unique
#'     patient identifiers
#'
#' @param contacts a \link{data.frame} that needs at least two columns
#'     indicating patients between which cases take place; these need not be
#'     referenced in the linelist
#'
#' @param id an index or name indicating which column in \code{linelist}
#'     contains unique identifiers; default is first column in \code{linelist}
#'     data frame
#'
#' @param from an index or name indicating which column in \code{contacts}
#'     contains the first region
#'
#' @param to an index or name indicating which column in \code{contacts}
#'     contains the second region
#'
#' @param n an index or name indicating which column in \code{contacts}
#'     contains the number of cases flowing between the "from" region and the
#'     "to" region
#' @param ... named character or integers 
#'
#' @return An \code{epicontacts} object in list format with three elements:
#'
#' \itemize{
#' \item \code{linelist}: data.frame of cases with first column 'id'
#' containing character vector of unique identifiers
#'
#' \item \code{contacts}: data.frame of contacts with first two columns named
#' 'from' and 'to' indicating unique pairs of contact between individuals
#'
#' \item \code{directed}: indicator as to whether or not the contacts are to be
#' considered directed or not
#' }
#'
#' @details
#'
#' An \code{epicontacts} object can be created from two components:
#' \itemize{
#' \item a linelist provided as a \code{data.frame} where columns are
#' different variables describing cases, and where each row is a different case.
#' and a contact list.
#'
#' \item a contact list provided as a \code{data.frame} where each row contains
#' unique pairs of contacts with unique features of contact in columns. The line
#' list and contact list should share an identification scheme for individuals.
#' }
#'
#' @references
#'     \url{http://foodborne.unl.edu/public/role/epidemiologist/lineLists.html}
#'
#' @examples
#' data(YF_Brazil)
#' from     <- as.data.frame.table(YF_Brazil$T_D)
#' to       <- as.data.frame.table(YF_Brazil$T_O)[c(2,1,3)]
#' contacts <- rbind(setNames(from, c("from", "to", "n")), 
#'                   setNames(to, c("from", "to", "n")))
#' linelist <- YF_Brazil$states
#' others   <- setdiff(contacts$to, YF_Brazil$states$location_code)
#' linelist <- merge(linelist, 
#'                   data.frame(location_code = others),
#'                   by = "location_code", all = TRUE)
#' ef <- epiflows(linelist, contacts, pop_size = "num_cases_time_window")
#' ef
#' # Access variable information
#' ef$linelist[ef$vars$pop_size]
#' @importFrom epicontacts make_epicontacts
#' 
epiflows <- function(...) {
  UseMethod("epiflows")
}

epiflows.data.frame <- function(locations, flows, id = 1L, 
                                from = 1L, to = 2L, n = 3L, ...){
  out <- epicontacts::make_epicontacts(linelist = locations, 
                                       contacts = flows, 
                                       id       = id,
                                       from     = from, 
                                       to       = to,
                                       directed = TRUE)
  # TODO: This variable needs to be validated before having the name changed.
  names(out$contacts)[n] <- "n"
  dots <- valid_dots(list(...))
  for (i in names(dots)) {
    # Numeric indices need to be re-matched to the new output
    if (is.numeric(dots[[i]])) {
      dots[[i]] <- new_column_positions(dots[[i]], 
                                        old_names = names(locations), 
                                        new_names = names(out$linelist))
    }
    stop_if_invalid(out$linelist[[dots[[i]]]])
  }
  out$vars    <- dots
  class(out)  <- c("epiflows", class(out))
  out
}

epiflows.integer <- function(from, to, focus, locations, ...) {
  if (is.null(names(from)) || is.null(names(to))) {
  # Check to make sure from and to are named
    msg <- paste("The vectors `from` and `to` must be named to",
                 "create an epiflows object.")
    stop(msg)
  }
  if (length(focus) != 1L || !is.character(focus)) {
    stop("The argument `focus` must be a single character string.")
  }
  # Check to make sure focus is in the names of from and to
  if (!focus %in% names(from) && !focus %in% names(to)) {
    stop("`focus` must be present in both the `from` and `to` vectors.")
  }
  # TODO: Validate the locations list.
  # 
  # PUT SOME CODE IN ME! (╯°□°）╯︵ ┻━┻
  # 
  # Create a data frame with from and to, repeating the focus as necessary
  flows <- data.frame(from = c(rep(focus, length(to)), names(from)),
                      to   = c(names(to), rep(focus, length(from))),
                      n    = c(to, from),
                      stringsAsFactors = FALSE)
  # Use the data frame to pass to epiflows.data.frame
  epiflows.data.frame(locations, flows, ...)
}

epiflows.numeric <- epiflows.integer

new_column_positions <- function(i, old_names, new_names) {
  match(old_names[i], old_names, nomatch = 0)
}

# This function will filter out any invalid options for dots
valid_dots <- function(dots) {
  # These names can be expanded
  out <- dots[names(dots) %in% 
              c("coords", 
                "pop_size", 
                "duration_stay")
              ]
  if (length(out) < length(dots)) {
    diffnames <- paste(setdiff(names(dots), names(out)), collapse = ",")
    warning(paste("Ignoring the following variables:", diffnames))
  }
  out
}