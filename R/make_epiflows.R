#' Create an epiflows object
#'
#' An epiflows object contains a pair of data frames that provide information
#' about locations and flows between locations. 
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
#'    each
#'
#'  - **vars** (accessible via [get_vars()]). This contains a named list of
#'    available variables that can be used in further plotting and/or modelling.
#'    Default variables are found in [global_vars()]:
#'    
#'     - `coordinates`: two columns specifying the lon and lat coordinates
#'     - `pop_size`: population size of each location
#'     - `duration_stay`: the average duration of stay for each location
#'     - `first_date`: the date of first recorded case
#'     - `last_date`: the date of the last recorded case
#'     - `num_cases`: the number of cases between the first and last date
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
#'
#'   Because a flow of cases from one location to another can be thought of as a
#'   contact with a wider scope, the `epiflows` object inherits from the
#'   `epicontacts` object, constructed via [epicontacts::make_epicontacts()].
#'   This means that all the methods for subsetting an object of class
#'   `epicontacts` also applies to `epiflows`, including the use of the function
#'   [epicontacts::thin()]. One caveat is that, internally, the names of the
#'   elements within the object do not match the terminology used in *epiflows*.
#'
#' }
#'
#' @importFrom epicontacts make_epicontacts
#'
make_epiflows <- function(...) {
  UseMethod("make_epiflows")
}

#' @param flows a data frame where each row represents a flow from one location
#'   to the next. This must have at least three columns:
#'   - Where the flow started (as specified in `from`, below)
#'   - Where the flow ended (as specified in `to`, below)
#'   - How many cases were involved (as specified in `n`, below)
#'
#' @param locations a data frame where each row represents a location. This can
#'   have any number of columns specifying useful metadata about the location,
#'   but it must contain at least one column specifying the location ID used in
#'   the `flows` data frame (as specified by the `id` argument, below).
#'
#' @param id The column to use for the identifier in the `locations` data frame.
#'   This defaults to the first column.
#'
#' @param from the column in the `flows` data frame indicating where the flow
#'   started. This can be an integer or character. Default is the first column.
#'   
#' @param to the column in the `flows` data frame indicating where the flow 
#'   terminated. This can be an integer or character. Default is the second column. 
#'
#' @param n the column in the `flows` data frame indicating how many cases were
#'   contained in the flow. This can be an integer or character. Default is the
#'   third column.
#'
#' @param ... Any number of varaibles that can be used for mapping or modelling.
#'   See [global_vars()] and [get_vars()] for details.
#'
#' @md
#'
#' @rdname make_epiflows
#' @seealso [global_vars()] for definitions of global variables, 
#'   [estimate_risk_spread()] for modelling, [plot.epiflows()] for plotting,
#'   [add_coordinates()] for adding coordinates, [get_vars()] for accession of
#'   metadata, [get_locations()] to access the locations data frame,
#'   [get_flows()] to access the flows data frame. 
#' @export
#'
#' @examples
#' ## Load data
#' data(YF_flows)
#' data(YF_locations)
#' YF_flows
#' YF_locations
#' ## Use both to create the epiflows object.
#' ef <- make_epiflows(flows         = YF_flows, 
#'                     locations     = YF_locations, 
#'                     pop_size      = "location_population",
#'                     duration_stay = "length_of_stay",
#'                     num_cases     = "num_cases_time_window",
#'                     first_date    = "first_date_cases",
#'                     last_date     = "last_date_cases"
#'                    )
#' ef
#' # Access variable information
#' get_pop_size(ef)
#' get_vars(ef, "duration_stay")
#' get_vars(ef, "num_cases")
make_epiflows.data.frame <- function(flows, locations = NULL,
                                     from = 1L, to = 2L, n = 3L,
                                     id = 1L, ...){
  if (is.null(locations)) {
    message("Locations data frame not provided. Creating one from the flow data.")
    ids       <- as.character(unique(unlist(flows[c(from, to)])))
    locations <- data.frame(id = ids, stringsAsFactors = FALSE)
  }
  out <- epicontacts::make_epicontacts(linelist = locations,
                                       contacts = flows,
                                       id       = id,
                                       from     = from,
                                       to       = to,
                                       directed = TRUE)
  if (names(flows)[n] != "n") {
    newn <- new_column_positions(n, names(flows), names(out$contacts))
    oldn <- "n" %in% names(out$contacts)
    if (oldn) {
      msg <- paste("A varaible named `n` exists in the flows data frame.",
                   "This will be renamed to n.1")
      warning(msg)
      names(out$contacts)[names(out$contacts) == "n"] <- "n.1"
    }
    names(out$contacts)[newn] <- "n"
    first_columns <- c(1:2L, newn)
    other_columns <- setdiff(seq_len(ncol(out$contacts)), first_columns)
    out$contacts  <- out$contacts[, c(first_columns, other_columns)]
  }
  dots         <- valid_dots(list(...))
  out$contacts <- valid_flows(out$contacts)
  # The vars need to be checked to make sure they are:
  #  - aligned with the re-arranged data
  #  - actually valid when matched against the linelist
  for (i in names(dots)) {
    if (is.numeric(dots[[i]])) {
      dots[[i]] <- new_column_positions(dots[[i]],
                                        old_names = names(locations),
                                        new_names = names(out$linelist))
    }
    for (j in dots[[i]]) {
      stop_if_invalid_column(j, out$linelist)
    }
  }
  out$vars    <- dots
  class(out)  <- c("epiflows", class(out))
  out
}

#' @param inflow a **named** integer or numeric vector specifying the number of
#' cases flowing into a location specified by `focus`.
#' @param outflow a **named** integer or numeric vector specifying the number of
#' cases flowing from a location specified by `focus`.
#' @param focus a character vector specifying the focal location for integer
#'   input. This is necessary for integer input to make clear what "outflow" and
#'   "inflow" are relative to.
#'
#' @rdname make_epiflows
#' @md
#' @export
#'
#' @examples
#' data(YF_Brazil)
#' (inflows   <- YF_Brazil$T_O["Espirito Santo", , drop = TRUE])
#' (outflows  <- YF_Brazil$T_D["Espirito Santo", , drop = TRUE])
#' (locations <- subset(YF_Brazil$states, location_code == "Espirito Santo", drop = FALSE))
#' los <- data.frame(location_code    = names(YF_Brazil$length_of_stay), 
#'                   length_of_stay   = YF_Brazil$length_of_stay,
#'                   stringsAsFactors = FALSE
#'                  )
#' locations <- merge(x   = locations, 
#'                    y   = los, 
#'                    by  = "location_code", 
#'                    all = TRUE)
#' ef <- make_epiflows(inflow = inflows, 
#'                     outflow = outflows, 
#'                     focus = "Espirito Santo", 
#'                     locations = locations,
#'                     pop_size = "location_population",
#'                     duration_stay = "length_of_stay",
#'                     num_cases = "num_cases_time_window",
#'                     first_date = "first_date_cases",
#'                     last_date = "last_date_cases"
#'                    )
#' ef
make_epiflows.integer <- function(inflow, outflow, focus, locations, id = 1L, ...) {
  if (is.null(names(inflow)) || is.null(names(outflow))) {
  # Check to make sure inflow and outflow are named
    msg <- paste("The vectors `inflow` and `outflow` must be named to",
                 "create an epiflows object.")
    stop(msg)
  }
  if (length(focus) != 1L || !is.character(focus)) {
    stop("The argument `focus` must be a single character string.")
  }
  # Create a data frame with inflow and outflow, repeating the focus as necessary
  flows <- data.frame(inflow  = c(rep(focus, length(outflow)), names(inflow)),
                      outflow = c(names(outflow), rep(focus, length(inflow))),
                      n       = c(outflow, inflow),
                      stringsAsFactors = FALSE)
  # Use the data frame to pass to epiflows.data.frame
  make_epiflows.data.frame(flows, locations, ...)
}

#' @rdname make_epiflows
#' @export
make_epiflows.numeric <- make_epiflows.integer


#' Reposition the columns displaced by make_epicontacts
#'
#' epicontacts will reposition columns in the linelist and contacts list so that
#' important features appear first. Because of this, user-specified numeric
#' columns will need to change.
#'
#' This also helps me because I don't have to remember which order the arguments
#' of match need.
#'
#' @param i an vector of integers specifying column names.
#' @param old_names Names of the original data frame
#' @param new_names Names in the modified data frame
#'
#' @return a vector of integers
#' @noRd
#' @keywords internal
#'
#' let <- letters[10:1]
#' bc1 <- 2:3
#' bc2 <- new_column_positions(bc1, letters, let)
#' letters[bc1]
#' letters[bc2]
new_column_positions <- function(i, old_names, new_names) {
  match(old_names[i], new_names, nomatch = 0)
}
