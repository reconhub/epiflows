#' Yellow Fever Data from Brazil; 2016-12 to 2017-05
#' 
#' This data set contains flows to and from five states in Brazil formatted in
#' a list with the following items:
#' 
#'  - `$states`: a data frame containing metadata for five Brazilian States:
#'    Espirito Santo, Minas Gerais, Rio de Janeiro, Sao Paulo, and Southeast
#'    Brazil
#'      - `$location_code` : names of the states
#'      - `$location_population` : population size for each state
#'      - `$num_cases_time_window` : number of cases recorded between 2016-12 and 2017-05
#'      - `$first_date_cases` : date of first disease case in the given location in ISO 8601 format
#'      - `$last_date_cases` : date of last disease case in the given location in ISO 8601 format
#'  - `$T_D` A matrix containing the number of travellers from the infectious
#'    location visiting other locations
#'  - `$T_O` A matrix containing the number of travellers visiting the infectious location
#'  - `$length_of_stay` A named vector containing the average length of stay in
#'    days of travellers from other locations visiting the infectious locations.
#' 
#' @md
#' @usage data("Brazil_epiflows")
#'   data("YF_coordinates")
#'   data("YF_locations")
#'   data("YF_flows")
#'   data("YF_Brazil")
#' @aliases Brazil_epiflows YF_flows YF_locations YF_coordinates
#' @seealso [make_epiflows()] for transformation to an epiflows object
#'   [estimate_risk_spread()]
#' @references Dorigatti I, Hamlet A, Aguas R, Cattarino L, Cori A, Donnelly CA,
#' Garske T, Imai N, Ferguson NM. International risk of yellow fever spread from
#' the ongoing outbreak in Brazil, December 2016 to May 2017. Euro Surveill.
#' 2017;22(28):pii=30572. DOI: [10.2807/1560-7917.ES.2017.22.28.30572](http://dx.doi.org/10.2807/1560-7917.ES.2017.22.28.30572)
#' @examples
#' # This is an example of an epiflows object
#' data("Brazil_epiflows")
#' Brazil_epiflows
#' 
#' # The above data was constructed from a data frame containing flows and 
#' # one containing location metadata
#' data("YF_flows")
#' data("YF_locations")
#' ef <- make_epiflows(flows         = YF_flows, 
#'                     locations     = YF_locations, 
#'                     pop_size      = "location_population",
#'                     duration_stay = "length_of_stay",
#'                     num_cases     = "num_cases_time_window",
#'                     first_date    = "first_date_cases",
#'                     last_date     = "last_date_cases"
#'                    )
#' 
#' # Both of the above data frames were constructed like so:
#' 
#' data("YF_Brazil")
#' 
#' # Create the flows data frame
#' from  <- as.data.frame.table(YF_Brazil$T_D, stringsAsFactors = FALSE)
#' to    <- as.data.frame.table(t(YF_Brazil$T_O), stringsAsFactors = FALSE)
#' flows <- rbind(from, to)
#' colnames(flows) <- c("from", "to", "n")
#' 
#' ## Create the locations data frame
#' los <- data.frame(location_code    = names(YF_Brazil$length_of_stay), 
#'                   length_of_stay   = YF_Brazil$length_of_stay,
#'                   stringsAsFactors = FALSE
#'                  )
#' locations <- merge(x   = YF_Brazil$states, 
#'                    y   = los, 
#'                    by  = "location_code", 
#'                    all = TRUE)
#' 
#' ## Use both to create the epiflows object.
#' ef <- make_epiflows(flows, 
#'                     locations, 
#'                     pop_size = "location_population",
#'                     duration_stay = "length_of_stay",
#'                     num_cases = "num_cases_time_window",
#'                     first_date = "first_date_cases",
#'                     last_date = "last_date_cases"
#' )
"YF_Brazil"

