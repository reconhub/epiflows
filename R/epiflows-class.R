#' Create an epiflows object
#'
#' This function reads data stored as data.frame containing linelist (case
#' information, where each row corresponds to a unique patient), and contacts
#' between patients. Common identifiers should be used in the two data sources
#' for matching to be achieved.
#'
#' @export
#' @md
#'
#' @author Zhian Kamvar, Thibaut Jombart
#'
#' @return An `epiflows` object in list format with four elements:
#'
#'  - **locations** (accessible via [get_locations()]): a data frame of 
#'    locations with first column 'id' containing character vector of unique 
#'    identifiers.
#'
#'  - **flows** (accessible via [get_flows()]): data.frame of flows with first 
#'    two columns named 'from' and 'to' indicating directed flows between two 
#'    locations, and a third column named 'n', specifying the number of cases in
#'    each flow. 
#'  - **vars** (accessible via [get_vars()]). This contains a named list of 
#'    available variables that can be used in further plotting and/or modelling.
#'    Available variables are:
#'    
#'     - coords
#'     - pop_size
#'     - duration_of_stay
#'
#' @details
#'
#' The `epiflows` object can be constructed using simply a list of locations with
#' optional metadata (similar to a linelist) and a list of flows that describes
#' the number of cases flowing from one location to another. Optional metadata
#' such as coordinates and duration of stay can be included in the linelist for
#' use in [estimate_risk_spread()] or [map_epiflows()]. 
#' 
#' \subsection{Developer note: object structure}{
#'   Because a flow of cases from one location to another can be thought of as a 
#'   contact with a wider scope, the `epiflows` object inherits from the `epicontacts` object, constructed
#'   via [epicontacts::make_epicontacts()]. This means that all the methods for
#'   subsetting an object of class `epicontacts` also applies to `epiflows`,
#'   including the use of the function [epicontacts::thin()]. One caveat is that,
#'   internally, the names of the elements within the object do not match the
#'   terminology used in *epiflows*. 
#' }
#' 
#' @importFrom epicontacts make_epicontacts
#' 
epiflows <- function(...) {
  UseMethod("epiflows")
}

#' @param locations a data frame where each row represents a location. This can
#'   have any number of columns specifying useful metadata about the location,
#'   but it must contain at least one column specifying the location ID used in
#'   the `flows` data frame (as specified by the `id` argument, below).
#' @param flows a data frame where each row represents a flow from one location
#'   to the next. This must have at least three columns: 
#'   - Where the flow started (as specified in `from`, below) 
#'   - Where the flow ended (as specified in `to`, below) 
#'   - How many cases were involved (as specified in `n`, below)
#' @param id The column to use for the identifier in the `locations` data frame.
#'   This defaults to the first column.
#' @param from the column in the `flows` data frame indicating where the flow
#'   started. This can be an integer or character. Default is the first column.
#' @param to the column in the `flows` data frame indicating where the flow
#'   ended. This can be an integer or a character. Default is the second column.
#' @param n the column in the `flows` data frame indicating how many cases were
#'   contained in the flow. This can be an integer or character. Default is the
#'   third column.
#' @param ... Any number of
#'
#' @md
#' @rdname epiflows
#' @export
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
#' get_vars(ef, "pop_size")
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

#' @param focus a character vector specifying the focal location for integer 
#'   input. This is necessary for integer input to make clear what "to" and 
#'   "from" are relative to.
#'
#' @rdname epiflows
#' @export
#'
#' @examples
#' data(Mex_travel_2009)
#' flows <- Mex_travel_2009[[1]]
#' to    <- setNames(flows[["MEX"]], rownames(flows))
#' from  <- unlist(flows["MEX", , drop = TRUE])
#' ef <- epiflows(from, to, focus = "MEX", locations = Mex_travel_2009[[2]])
#' ef
epiflows.integer <- function(from, to, focus, locations, ...) {
  
  #
  # TODO: change from -> inflow
  #       change to   -> outflow
  #
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

#' @rdname epiflows
#' @export
epiflows.numeric <- epiflows.integer


new_column_positions <- function(i, old_names, new_names) {
  match(old_names[i], old_names, nomatch = 0)
}

