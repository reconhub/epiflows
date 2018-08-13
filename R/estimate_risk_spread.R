#' Travel-related disease cases spreaded to other locations from an infectious
#' location
#'
#' Calculates the mean and 95% confidence interval of the estimated number of
#' disease cases that could potentially seed a disease outbreak in the locations
#' they are travelling to, comprising exportations (infected residents of the infectious
#' location travelling abroad during the incubation or infectious period), and
#' importations (international tourists infected by the disease during
#' their stay in the infectious location and returning to their home location).
#' The mean and 95% confidence intervals are obtained by numerically sampling
#' `n_sim` times from the incubation and infectious period distributions.
#' If parameter `return_all_simulations` is set to `TRUE`, the function returns all simulations
#' for each location.
#'
#' @param location_code a character string denoting the infectious location code
#'
#' @param location_population population of the infectious location
#'
#' @param num_cases_time_window cumulative number of cases in infectious
#'   location in time window
#'
#' @param first_date_cases string with the date of the first disease case in
#'   infectious location ("YYYY-MM-DD")
#'
#' @param last_date_cases string with the date of the last disease case in
#'   infectious location ("YYYY-MM-DD")
#'
#' @param num_travellers_to_other_locations number of travellers from the
#'   infectious location visiting other locations (T_D)
#'
#' @param num_travellers_from_other_locations number of travellers from other
#'   locations visiting the infectious location (T_O)
#'
#' @param avg_length_stay_days average length of stay in days of travellers from
#'   other locations visiting the infectious location. This can be a common
#'   number for all locations or a vector with different numbers for each
#'   location
#'
#' @param r_incubation a function with a single argument `n`
#'   generating 'n' random incubation periods
#'
#' @param r_infectious a function with a single argument `n`
#'   generating 'n' random durations of infectious period
#'
#' @param n_sim number of simulations from the incubation and
#'   infectious distributions
#'
#' @param return_all_simulations logical value indicating whether the returned object is a data frame with all simulations
#' (`return_all_simulations = TRUE`) or a data frame with the mean and lower and upper limits of a 95% confidence interval of
#' the number of cases spread to each location (`return_all_simulations = FALSE`)
#' 
#' @param ... Arguments passed onto the default method.
#'
#' @return if `return_all_simulations` is `TRUE`, data frame with all simulations. If `return_all_simulations` is `FALSE`,
#' data frame with the mean and lower and upper limits of a 95% confidence interval of the number 
#' of cases spread to each location
#' 
#' @references Dorigatti I, Hamlet A, Aguas R, Cattarino L, Cori A, Donnelly CA,
#' Garske T, Imai N, Ferguson NM. International risk of yellow fever spread from
#' the ongoing outbreak in Brazil, December 2016 to May 2017. Euro Surveill.
#' 2017;22(28):pii=30572. DOI: [10.2807/1560-7917.ES.2017.22.28.30572](http://dx.doi.org/10.2807/1560-7917.ES.2017.22.28.30572)
#' @author Paula Moraga, Zhian Kamvar (epiflows class implementation)
#' @md
#' @export
#' @seealso Construction of epiflows object: [make_epiflows()]\cr
#'   Default variables used in the epiflows implementation: [global_vars()]\cr
#'   Access metadata from the epiflows object: [get_vars()]
#' @examples
#' ## Using an epiflows object --------------------------------
#' 
#' data("YF_flows")
#' data("YF_locations")
#' ef <- make_epiflows(flows         = YF_flows, 
#'                     locations     = YF_locations, 
#'                     pop_size      = "location_population",
#'                     duration_stay = "length_of_stay",
#'                     num_cases     = "num_cases_time_window",
#'                     first_date    = "first_date_cases",
#'                     last_date     = "last_date_cases"
#' )
#' ## functions generating incubation and infectious periods
#' incubation <- function(n) {
#'   rlnorm(n, 1.46, 0.35)
#' }
#'
#' infectious <- function(n) {
#'   rnorm(n, 4.5, 1.5/1.96)
#' }
#' 
#' res <- estimate_risk_spread(ef, 
#'                             location_code          = "Espirito Santo",
#'                             r_incubation           = incubation,
#'                             r_infectious           = infectious,
#'                             n_sim                  = 1e5,
#'                             return_all_simulations = TRUE)
#' boxplot(res, las = 3)
#'
#' ## Using other data --------------------------------------------------
#' data(YF_Brazil)
#' indstate <- 1 # "Espirito Santo" (indstate = 1), 
#'               # "Minas Gerais" (indstate = 2), 
#'               # "Southeast Brazil" (indstate = 5)
#'
#' res <- estimate_risk_spread(
#'   location_code = YF_Brazil$states$location_code[indstate], 
#'   location_population = YF_Brazil$states$location_population[indstate], 
#'   num_cases_time_window = YF_Brazil$states$num_cases_time_window[indstate], 
#'   first_date_cases = YF_Brazil$states$first_date_cases[indstate], 
#'   last_date_cases = YF_Brazil$states$last_date_cases[indstate],
#'   num_travellers_to_other_locations = YF_Brazil$T_D[indstate,],
#'   num_travellers_from_other_locations = YF_Brazil$T_O[indstate,],
#'   avg_length_stay_days = YF_Brazil$length_of_stay,
#'   r_incubation = incubation,
#'   r_infectious = infectious,
#'   n_sim = 100000,
#'   return_all_simulations = FALSE
#' )
#' head(res)
#'
estimate_risk_spread <- function(...) {
  UseMethod("estimate_risk_spread")
}

#' @export
#' @rdname estimate_risk_spread
estimate_risk_spread.default <- function(location_code = character(0),
                                         location_population = numeric(0),
                                         num_cases_time_window = numeric(0),
                                         first_date_cases = character(0),
                                         last_date_cases = character(0),
                                         num_travellers_to_other_locations = numeric(0),
                                         num_travellers_from_other_locations = numeric(0),
                                         avg_length_stay_days = numeric(0),
                                         r_incubation = function(n) { },
                                         r_infectious = function(n) { },
                                         n_sim = 1000,
                                         return_all_simulations = FALSE, 
                                         ...) {
  check_estimate_args(environment(), ...)
  if (n_sim < 1000) {
    warning("It is recommended the number of simulations is at least 1000.")
  }
  ## time_window_days is the number of days between the first and last disease
  ## case in infectious location
  time_window_days <- as.vector(
    as.Date(last_date_cases) - as.Date(first_date_cases)
  )

  ## population
  pop_S <- location_population

  ## Number of countries
  num_countries <- length(num_travellers_to_other_locations)


  ## Construct a a vector with the average length of stay for each
  ## of the countries in case length of avg_length_stay_days is different
    ## than the number of countries

  if (length(avg_length_stay_days) != num_countries) {
    if (length(avg_length_stay_days) == 1) {
      avg_length_stay_days <- rep(avg_length_stay_days, num_countries)
    } else {
      stop("avg_length_stay_days must have length equal to 1 or to the number of locations")
    }
  }

  # Rename variables
  C_SW <- num_cases_time_window
  # Cumulative number of YF cases in state S (origin) in time window W
  #C_hat_SW <- 10 * C_SW
  C_hat_SW <- C_SW

  W   <- time_window_days
  T_D <- num_travellers_to_other_locations
  T_O <- num_travellers_from_other_locations
  L_O <- avg_length_stay_days


  ## Incubation and infectious periods

  T_E <- r_incubation(n_sim)
  T_I <- r_infectious(n_sim)


  ## Exportations

  # Per capita probability that a resident from the infectious country travelled
  # to the other countries during time window W
  #p_D <- (T_D * W / 365) / pop_S

  p_D <- T_D / pop_S


  # Probability p_i that a disease case was incubated or was infectious in time
  # window W (each element of the vector corresponds to a simulation)
  p_i <- (T_E + T_I) / W
  p_i[p_i > 1] <- 1

  # Number of residents of infectious country infected by the virus and
  # travelling abroad during their incubation or infectious period in time
  # window W (each row corresponds to a simulation, each column corresponds
  # to a country)
  exportations <- t(sapply(p_i, function(e) { C_hat_SW * p_D * e}))

  message("Exportations done")

  ## Importations

  # Per capita risk of infection of travellers visiting infectious country
  # during their stay
  lambda_S <- (C_hat_SW * L_O) / (pop_S * W)

  # vector of probabilities of returning to the home country while incubating
  # or infectious (each row corresponds to a simulation, each column corresponds
  # to a country)
  p_l <- t(mapply(function(e, i) { (e + i) / L_O }, T_E, T_I))
  p_l[p_l > 1] <- 1

  # Number of international tourists infected by the virus during their stay
  # in infectious country and returning to the home country before the end of
  # the infectious period
  # multiply each row of p_l * T_O * lambda_S
  # (each row corresponds to a simulation, each column corresponds to a country)
  importations <- t(apply(p_l, 1, function(i) { T_O * lambda_S * i }))

  message("Importations done")

  ## Total
  total <- exportations + importations
  meancases <- colMeans(total, na.rm = TRUE)
  quant <- t(apply(total, 2, stats::quantile, c(.025, .975), na.rm = TRUE))

  out <- data.frame(mean_cases = meancases,
                    lower_limit_95CI = quant[, 1],
                    upper_limit_95CI = quant[, 2])

  if(return_all_simulations){
    return(total)
  }else{
    return(out)
  }
}

#' @export
#' @rdname estimate_risk_spread
#' @param x an epiflows object
#' @importFrom stats na.omit
estimate_risk_spread.epiflows <- function(x, 
                                          location_code = character(0), 
                                          r_incubation  = function(n) { }, 
                                          r_infectious  = function(n) { }, 
                                          n_sim = 1000, 
                                          return_all_simulations = FALSE, 
                                          ...) {
  
  check_estimate_args(environment(), ..., fun = "estimate_risk_spread.epiflows")
  xi <- epicontacts::thin(x[, j = location_code])
  # Scalars ---------------------------------------------------------
  pop_size   <- get_vars(xi, "pop_size", vector = TRUE)[location_code]
  num_cases  <- get_vars(xi, "num_cases", vector = TRUE)[location_code]
  first_date <- get_vars(xi, "first_date", vector = TRUE)[location_code]
  last_date  <- get_vars(xi, "last_date", vector = TRUE)[location_code]
  
  # vectors from a given location -----------------------------------
  n_to       <- get_n(xi, from = location_code)
  n_from     <- get_n(xi, to   = location_code)
  anti_loc   <- get_id(xi) != location_code
  duration   <- get_vars(xi, "duration_stay", vector = TRUE)[anti_loc]
  
  # Environment to store dots ---------------------------------------
  denv      <- new.env()
  denv$dots <- stop_if_ambiguous_dots(list(...), "estimate_risk_spread.default")
  
  # Construct the arguments -----------------------------------------
  # If an argument was specified in the dots, it should be given 
  # precedence since the user had to manually put that in there.
  args <- c(
    dots_precedence("location_code", location_code, denv),
    dots_precedence("r_incubation", r_incubation, denv),
    dots_precedence("r_infectious", r_infectious, denv),
    dots_precedence("n_sim", n_sim, denv),
    dots_precedence("location_population", pop_size, denv),
    dots_precedence("num_cases_time_window", num_cases, denv),
    dots_precedence("first_date_cases", first_date, denv),
    dots_precedence("last_date_cases", last_date, denv),
    dots_precedence("num_travellers_to_other_locations", n_to, denv),
    dots_precedence("num_travellers_from_other_locations", n_from, denv),
    dots_precedence("avg_length_stay_days", duration, denv),
    dots_precedence("return_all_simulations", return_all_simulations, denv),
    denv$dots
  )
  do.call("estimate_risk_spread.default", args)
}
