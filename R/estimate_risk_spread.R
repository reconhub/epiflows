#' Travel-related disease cases spreaded to other locations from an infectious location
#'
#' Calculates the mean and 95% confidence interval of the estimated number of
#' disease cases that could potentially seed a disease outbreak in the locations
#' they are travelling to, comprising exportations (infected residents of the infectious
#' location travelling abroad during the incubation or infectious period), and
#' importations (international tourists infected by the disease during
#' their stay in the infectious location and returning to their home location).
#' The mean and 95% confidence intervals are obtained by numerically sampling
#' num_simulations times from the incubation and infectious period distributions.
#'
#' @param ef an \code{epiflows} object. It contains the number of travellers
#' to and from other locations
#' @param ef_time character string denoting the time window of the population flows in the epiflows object.
#' Options are "time_window", "annual", "quarterly"
#' @param location_code a character string denoting the location code
#' @param time_window_days number of days between the first and last
#' disease case in infectious location
#' @param num_infec_cases_in_time_window cumulative number of cases
#' in infectious location in time window
#' @param avg_length_stay_days average length of stay in days of travellers from
#' other locations visiting the infectious location. This can be a common number
#' for all locations or a vector with different numbers for each location
#' @param distribution_incubation random generation distribution of the incubation period
#' @param params_incubation vector with the parameters of the incubation distribution
#' @param distribution_infectious random generation distribution of the infectious period
#' @param params_infectious vector with the parameters of the infectious distribution
#' @param num_simulations number of simulations from the incubation and infectious distributions
#'
#' @return data.frame with the mean and 95% confidence interval of the number
#' of cases spread to each country
#' 
#' @details
#' parameters \code{distribution_incubation} and \code{distribution_infectious} denote the random generation distributions of the
#' incubation and infectious periods distributions, respectively.
#' Thes can be specified with the name of an R function or with a function defined by the user with parameters
#' \code{num_simulations}, \code{parameter1}, \code{parameter2}, etc.
#' Examples: \code{rnorm(n, mean, sd)}, \code{rlnorm(n, meanlog, sdlog)}, \code{rgamma(n, shape, rate)}, \code{rweibull(n, shape, scale)}, \code{rexp(n, rate)}
#' 
#' @examples 
#' ef <- do.call(make_epiflows, Mex_travel_2009)
#' num_countries <- nrow(ef$locationsdata)
#' avg_length_stay_days <- rpois(num_countries, 30)
#' res <- estimate_risk_spread(
#'   ef = ef,
#'   ef_time = "time_window",
#'   location_code = "MEX",
#'   time_window_days = 365,
#'   num_infec_cases_in_time_window = 1000,
#'   avg_length_stay_days = avg_length_stay_days,
#'   distribution_incubation = rlnorm,
#'   params_incubation = c(4.6, sqrt(2.7)),
#'   distribution_infectious = rnorm,
#'   params_infectious = c(4.5, sqrt(0.6)),
#'   num_simulations = 1000
#' )
#' head(res)
#' 
#' @author Paula Moraga
#' 
#' @export
#' 
estimate_risk_spread <- function(ef,
                                 ef_time = "time_window",
                                 location_code,
                                 time_window_days,
                                 num_infec_cases_in_time_window,
                                 avg_length_stay_days,
                                 distribution_incubation,
                                 params_incubation,
                                 distribution_infectious,
                                 params_infectious,
                                 num_simulations = 1000) {
  
  if(!(ef_time %in% c("time_window", "annual", "quarterly"))){
    stop("ef_time must be 'time_window', 'annual', or 'quarterly'.")
  }
  
  if(num_simulations < 1000){
    message("It is recommended the number of simulations is at least 1000.")
  }
  


  # Get flow and location data from ef
  # number_travellers_to_other_countries and number_travellers_from_other_countries
  # refer to the number of travellers in the time window
  number_travellers_to_other_countries <- get_flow_data(ef, direction = "from")
  number_travellers_from_other_countries <- get_flow_data(ef, direction = "to")
  pop_country <- get_location_data(ef, location_code)$population
  
  # Number of countries
  num_countries <- length(number_travellers_to_other_countries)
  
  # Construct a a vector with the average length of stay for each
  # of the countries in case length of avg_length_stay_days is different
  # than the number of countries
  if (length(avg_length_stay_days) != num_countries) {
    if (length(avg_length_stay_days) == 1) {
      avg_length_stay_days <- rep(avg_length_stay_days, num_countries)
    } else {
      stop("avg_length_stay_days must have length equal to 1 or to the number of countries")
    }
  }
  
  # Rename variables
  C_SW <- num_infec_cases_in_time_window
  # Cumulative number of YF cases in state S (origin) in time window W
  #C_hat_SW <- 10 * C_SW
  C_hat_SW <- C_SW
  
  W <- time_window_days
  T_D <- number_travellers_to_other_countries
  T_O<- number_travellers_from_other_countries
  L_O <- avg_length_stay_days
  
  if(ef_time == "annual"){
    T_D <- T_D*W/365
    T_O <- T_O*W/365
  }else{
    if(ef_time == "quarterly"){
      T_D <- T_D*W*4/365
      T_O <- T_O*W*4/365
    }}
               
  
  
  ## Incubation and infectious periods
  
  # T_E: incubation period was log-normally distributed mean = 4.6, var = 2.7 days
  # T_I: infectious period was normally distributed mean = 4.5, var = 0.6 days
  # T_E <- rlnorm(num_simulations, mean = mean_incubation, sd = sqrt(var_incubation))
  # T_I <- rnorm(num_simulations, mean = mean_infectious, sd = sqrt(var_infectious))

  T_E <- do.call(match.fun(distribution_incubation), as.list(c(num_simulations, params_incubation)))
  T_I <- do.call(match.fun(distribution_infectious), as.list(c(num_simulations, params_infectious)))

  
  ## Exportations
  
  # Per capita probability that a resident from the infectious country travelled
  # to the other countries during time window W
  #p_D <- (T_D * W / 365) / pop_country
  
  p_D <- T_D / pop_country

  
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
  lambda_S <- (C_hat_SW * L_O) / (pop_country * W)
  
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
  
  ## INI Total
  total <- exportations + importations
  meancases <- colMeans(total, na.rm = TRUE)
  quant <- t(apply(total, 2, stats::quantile, c(.025, .975), na.rm = TRUE))

  data.frame(mean_cases = meancases, lower_limit_95CI = quant[, 1], upper_limit_95CI = quant[, 2])
}

