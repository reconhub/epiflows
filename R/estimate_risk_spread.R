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
#' to and from other locations in the time window
#' @param location_code a character string denoting the infectious location code
#' @param first_date_cases string with the date of the first disease case in infectious location ("YYYY-MM-DD")
#' @param last_date_cases string with the date of the last disease case in infectious location ("YYYY-MM-DD")
#' @param num_infec_cases_in_time_window cumulative number of cases in infectious location in time window
#' @param avg_length_stay_days average length of stay in days of travellers from
#' other locations visiting the infectious location. This can be a common number
#' for all locations or a vector with different numbers for each location
#' @param distribution_incubation random generation distribution of the incubation period
#' @param params_incubation vector with the parameters of the incubation distribution
#' @param distribution_infectious random generation distribution of the infectious period
#' @param params_infectious vector with the parameters of the infectious distribution
#' @param num_simulations number of simulations from the incubation and infectious distributions
#'
#' @return data.frame with the mean and lower and upper limits of a 95% confidence interval of the number
#' of cases spread to each country
#' 
#' @details
#' parameters \code{distribution_incubation} and \code{distribution_infectious} denote the random generation distributions of the
#' incubation and infectious periods distributions, respectively.
#' These can be specified with the name of an R function or with a function defined by the user with parameters
#' \code{num_simulations}, \code{parameter1}, \code{parameter2}, etc.
#' Examples: \code{rnorm(n, mean, sd)}, \code{rlnorm(n, meanlog, sdlog)}, \code{rgamma(n, shape, rate)}, \code{rweibull(n, shape, scale)}, \code{rexp(n, rate)}
#' 
#' @examples 
#' data(YF_Brazil)
#' indstate <- 1 # "Espirito Santo" (indstate = 1), "Minas Gerais" (indstate = 2), "Southeast Brazil" (indstate = 5)
#'
#' res <- estimate_risk_spread(
#'   ef = NULL,
#'   location_code = YF_states$location_code[indstate],
#'   first_date_cases = YF_states$first_date_cases[indstate],
#'   last_date_cases = YF_states$last_date_cases[indstate],
#'   num_infec_cases_in_time_window = YF_states$num_infec_cases_in_time_window[indstate],
#'   avg_length_stay_days = length_of_stay,
#'   distribution_incubation = rlnorm,
#'   params_incubation = c(1.46, 0.35),
#'   distribution_infectious = rnorm,
#'   params_infectious = c(4.5, 1.5/1.96),
#'   num_simulations = 100000,
#'   number_travellers_to_other_countries = T_D[indstate,],
#'   number_travellers_from_other_countries = T_O[indstate,],
#'   pop_S = YF_states$population[indstate]
#' )
#' head(res)
#' 
#' @author Paula Moraga
#' 
#' @export
#' 
estimate_risk_spread <- function(ef,
                                 location_code,
                                 first_date_cases,
                                 last_date_cases,
                                 num_infec_cases_in_time_window,
                                 avg_length_stay_days,
                                 distribution_incubation,
                                 params_incubation,
                                 distribution_infectious,
                                 params_infectious,
                                 num_simulations = 1000,
                                 number_travellers_to_other_countries,
                                 number_travellers_from_other_countries,
                                 pop_S) {
  
  if(num_simulations < 1000){
    message("It is recommended the number of simulations is at least 1000.")
  }
  
  
  # time_window_days is the number of days between the first and last disease case in infectious location
  time_window_days <- as.vector(as.Date(last_date_cases) - as.Date(first_date_cases))

  # When make_epiflows() is fixed, delete this and the 3 last arguments of estimate_risk_spread()
  if(!is.null(ef)){
  # Get flow and location data from ef
  # number_travellers_to_other_countries and number_travellers_from_other_countries
  # refer to the number of travellers in the time window
  number_travellers_to_other_countries <- get_flow_data(ef, location_code, direction = "from")
  number_travellers_from_other_countries <- get_flow_data(ef, location_code, direction = "to")
  pop_S <- get_location_data(ef, location_code)$population
  }

  
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

  return(data.frame(mean_cases = meancases, lower_limit_95CI = quant[, 1], upper_limit_95CI = quant[, 2]))
  
}

