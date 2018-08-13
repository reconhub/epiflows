#' Epiflow Global Variables
#' 
#' The metadata in **locations** such as population size, duration of stay in a 
#' given location, date of first and last cases, etc. can be useful in estimating
#' the risk of spread, but not everyone will code their data with identical column
#' names. To facilitate their use in the function `estimate_risk_spread()`, the
#' epiflows object stores a dictionary of variables in a place called `$vars`. 
#' We can tell epiflows what variables are important when we create the object. 
#'
#' The default varaibles are:
#' 
#'  - `coordinates`: two columns specifying the lon and lat coordinates
#'  - `pop_size`: population size of each location
#'  - `duration_stay`: the average duration of stay for each location
#'  - `first_date`: the date of first recorded case
#'  - `last_date`: the date of the last recorded case
#'  - `num_cases`: the number of cases between the first and last date
#'
#' @param ... quoted varaibles to add to the default variables
#' @param set when `TRUE`, the variables provided in `...` will be added to the
#'   global variables. Defaults to `FALSE`
#' @param reset when `TRUE`, the global variables are reset to the default 
#'   variables listed above. Defaults to `FALSE`
#'
#' @md
#' @aliases epiflows.vars
#' @export
#' @seealso [make_epiflows()], [get_locations()], [get_vars()], [set_vars()], [get_coordinates()]
#' @examples 
#' 
#' # see the default varaibles
#' global_vars()
#' 
#' # Equivalent
#' getOption("epiflows.vars")
#' 
#' # create an object, specifying these variables
#' data("YF_locations")
#' data("YF_flows")
#' ef <- make_epiflows(flows         = YF_flows, 
#'                     locations     = YF_locations, 
#'                     pop_size      = "location_population",
#'                     duration_stay = "length_of_stay",
#'                     num_cases     = "num_cases_time_window",
#'                     first_date    = "first_date_cases",
#'                     last_date     = "last_date_cases"
#'                    )
#' ef
#' 
#' \donttest{
#' # You will receive an error if a variable is specified incorrectly
#' YF_locations$random_variable <- runif(nrow(YF_locations))
#' try({
#'   ef <- make_epiflows(flows         = YF_flows, 
#'                       locations     = YF_locations, 
#'                       Pop_size      = "location_population",
#'                       duration_stay = "length_of_stay",
#'                       num_cases     = "num_cases_time_window",
#'                       first_date    = "first_date_cases",
#'                       last_date     = "last_date_cases",
#'                       random        = "random_variable"
#'                      )
#'    })
#' }
#' 
#' # If you create a new method and need other varaibles, or just want a shorter
#' # representation, they can be added to your options:
#' 
#' global_vars("random", set = TRUE)
#' 
#' YF_locations$random_variable <- runif(nrow(YF_locations))
#' ef <- make_epiflows(flows         = YF_flows, 
#'                     locations     = YF_locations, 
#'                     pop_size      = "location_population",
#'                     duration_stay = "length_of_stay",
#'                     num_cases     = "num_cases_time_window",
#'                     first_date    = "first_date_cases",
#'                     last_date     = "last_date_cases",
#'                     random        = "random_variable"
#'                    )
#' 
#' # You can also reset the variables
#' global_vars(reset = TRUE)
global_vars <- function(..., set = FALSE, reset = FALSE) {
  epiflows.vars <- c("coordinates",   # xy coordinates
                     "pop_size",      # population size
                     "duration_stay", # duration of stay in each 'to' location
                     "first_date",    # date of first recorded case
                     "last_date",     # date of last recorded case
                     "num_cases"      # number of cases within time interval
  )
  res <- unique(c(getOption("epiflows.vars"), c(...), epiflows.vars))
  if (set)   options(epiflows.vars = res)
  if (reset) options(epiflows.vars = epiflows.vars)
  getOption("epiflows.vars")
}