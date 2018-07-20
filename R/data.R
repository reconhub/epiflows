#' Travel data to and from Mexico in 2009
#'
#' Travel data to and from Mexico in 2009
#'
#' @format A list of 2 elements
#' \describe{
#'   \item{1}{An unsymmetrical matrix (data frame object) of numbers of flows
#'    between locations. Rows denote locations of origin, columns destinations.}
#'   \item{2}{Country metadata (data frame object): code, name and population.}
#' }
"Mex_travel_2009"

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
#' @seealso [make_epiflows()] for transformation to an epiflows object
#'   [estimate_risk_spread()]
#' @references Dorigatti I, Hamlet A, Aguas R, Cattarino L, Cori A, Donnelly CA,
#' Garske T, Imai N, Ferguson NM. International risk of yellow fever spread from
#' the ongoing outbreak in Brazil, December 2016 to May 2017. Euro Surveill.
#' 2017;22(28):pii=30572. DOI: [10.2807/1560-7917.ES.2017.22.28.30572](http://dx.doi.org/10.2807/1560-7917.ES.2017.22.28.30572)
"YF_Brazil"