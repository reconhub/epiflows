#' Travel-related disease cases spreaded to other countries from an infectious country
#'
#' Calculates the mean and 95% confidence interval of the estimated number of
#' disease cases that could potentially seed a disease outbreak in the countries
#' they are travelling to, comprising infected residents of the infectious
#' country travelling abroad during the incubation or infectious period
#' (exportations) and international tourists infected by the disease during
#' their stay in the infectious country and returning to their home country
#' (importations).
#' The mean and 95% confidence intervals are obtained by numerically sampling
#' 10,000 times the incubation and infectious period distributions.
#'
#' @param ef an \code{epiflows} object
#' @param code a character string denoting location (country) code
#' @param time_window_days number of days between the first and last confirmed
#' disease cases in infectious country
#' @param num_infec_cases_in_time_window cumulative number of confirmed cases
#' reported in infectious country in time window W
#' @param avg_length_stay_days average length of stay in days of travellers from
#' other countries visiting the infectious country.
#' This can be an number that is common for all countries or a vector with
#' different numbers for each country.
#' @param mean_incubation mean of the incubation lognormal distribution
#' @param var_incubation variance of the incubation lognormal distribution
#' @param mean_infectious mean of the infectious normal distribution
#' @param var_infectious variance of the infectious normal distribution
#'
#' @return data.frame with the mean and 95% confidence interval of the number
#' of cases spread to each country
#' 
#' @examples 
#' flows <- do.call(make_epiflows, Mex_travel_2009)
#' num_countries <- nrow(flows$locationsdata)
#' avg_length_stay_days <- rpois(num_countries, 30)
#' fn_number_cases_spread(
#'   flows,
#'   "MEX",
#'   time_window_days = 365,
#'   num_infec_cases_in_time_window = 1000,
#'   avg_length_stay_days = avg_length_stay_days,
#'   mean_incubation = 4.6,
#'   var_incubation = 2.7,
#'   mean_infectious = 4.5,
#'   var_infectious = 0.6
#' )
#' 
#' @author Paula Moraga
#' 
#' @export
fn_number_cases_spread <- function(ef,
                                   code,
                                   time_window_days,
                                   num_infec_cases_in_time_window,
                                   avg_length_stay_days,
                                   mean_incubation,
                                   var_incubation,
                                   mean_infectious,
                                   var_infectious) {

  # Get flow and location data from ef
  annual_travellers_to_other_countries <- get_flow_data(ef, code, direction = "from")
  annual_travellers_from_other_countries <- get_flow_data(ef, code, direction = "to")
  pop_country <- get_location_data(ef, code)$population
  
  # Number of countries
  num_countries <- length(annual_travellers_to_other_countries)
  
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
  W <- time_window_days
  T_D <- annual_travellers_to_other_countries
  T_O<- annual_travellers_from_other_countries
  L_O <- avg_length_stay_days
  
  #TODO: Change this?
  # Cumulative number of YF cases in state S (origin) in time window W
  C_hat_SW <- 10 * C_SW
  
  ## Incubation and infectious periods
  
  # T_E: incubation period was log-normally distributed mean = 4.6, var = 2.7 days
  # T_I: infectious period was normally distributed mean = 4.5, var = 0.6 days
  num_sim <- 10
  T_E <- rlnorm(num_sim, mean = mean_incubation, sd = sqrt(var_incubation))
  T_I <- rnorm(num_sim, mean = mean_infectious, sd = sqrt(var_infectious))
  
  ## Exportations
  
  # Per capita probability that a resident from the infectious country travelled
  # to the other countries during time window W
  p_D <- (T_D * W / 365) / pop_country
  
  # Probability p_i that a disease case was incubated or was infectious in time
  # window W (each element of the vector corresponds to a simulation)
  p_i <- (T_E + T_I) / W
  
  # Number of residents of infectious country infected by the virus and
  # travelling abroad during their incubation or infectious period in time
  # window W (each row corresponds to a simulation, each column corresponds
  # to a country)
  exportations <- t(sapply(p_i, function(e) { C_hat_SW * p_D * e}))
  
  message("Exportations done")
  
  ## Importations
  
  # Per capita risk of infection of travellers visiting infectious country
  # during their stay
  lambda_S <- (C_SW * L_O) / (pop_country * W)
  
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
  quant <- t(apply(total, 2, quantile, c(.025, .975), na.rm = TRUE))

  data.frame(mean_cases = meancases, lwr = quant[, 1], upr = quant[, 2])
}

