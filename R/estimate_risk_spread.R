#' Travel-related disease cases spreaded to other locations from an infectious
#' location
#'
#' Calculates the mean and 95% confidence interval of the estimated number of
#' disease cases that could potentially seed a disease outbreak in the locations
#' they are travelling to, comprising exportations (infected residents of the
#' infectious location travelling abroad during the incubation or infectious
#' period), and importations (international tourists infected by the disease
#' during their stay in the infectious location and returning to their home
#' location). The mean and 95% confidence intervals are obtained by numerically
#' sampling \code{num_simulations} times from the incubation and infectious
#' period distributions.
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
#' @param r_incubation a function with a single argument \code{n}
#'   generating 'n' random incubation periods
#'
#' @param r_infectious a function with a single argument \code{n}
#'   generating 'n' random durations of infectious period
#'
#' @param n_sim number of simulations from the incubation and
#'   infectious distributions
#'
#' @return a \code{data.frame} with the mean and lower and upper limits of a 95%
#'   confidence interval of the number of cases spread to each location
#'
#' @examples
#'
#' ## load data
#' data(YF_Brazil)
#' indstate <- 1 # "Espirito Santo" (indstate = 1), "Minas Gerais" (indstate = 2), "Southeast Brazil" (indstate = 5)
#'
#'
#' ## functions generating incubation and infectious periods
#' incubation <- function(n) {
#'   rlnorm(n, 1.46, 0.35)
#' }
#'
#' infectious <- function(n) {
#'   rnorm(n, 4.5, 1.5/1.96)
#' }
#'
#'
#' res <- estimate_risk_spread(location_code =
#'   YF_Brazil$states$location_code[indstate], location_population =
#'   YF_Brazil$states$location_population[indstate], num_cases_time_window =
#'   YF_Brazil$states$num_cases_time_window[indstate], first_date_cases =
#'   YF_Brazil$states$first_date_cases[indstate], last_date_cases =
#'   YF_Brazil$states$last_date_cases[indstate],
#'   num_travellers_to_other_locations = YF_Brazil$T_D[indstate,],
#'   num_travellers_from_other_locations = YF_Brazil$T_O[indstate,],
#'   avg_length_stay_days = YF_Brazil$length_of_stay,
#'   r_incubation = incubation,
#'   r_infectious = infectious,
#'   n_sim = 100000 )
#'
#' head(res)
#'
#' @author Paula Moraga
#'
#' @export
#'
estimate_risk_spread <- function(location_code,
                                 location_population,
                                 num_cases_time_window,
                                 first_date_cases,
                                 last_date_cases,
                                 num_travellers_to_other_locations,
                                 num_travellers_from_other_locations,
                                 avg_length_stay_days,
                                 r_incubation,
                                 r_infectious,
                                 n_sim = 1000) {


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

  W <- time_window_days
  T_D <- num_travellers_to_other_locations
  T_O<- num_travellers_from_other_locations
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
  return(out)

}

