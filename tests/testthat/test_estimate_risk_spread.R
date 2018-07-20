context("Test estimate_risk_spread()")

test_that("Correct value is returned", {
  data(YF_Brazil)
  from  <- as.data.frame.table(YF_Brazil$T_D, stringsAsFactors = FALSE)
  to    <- as.data.frame.table(YF_Brazil$T_O, stringsAsFactors = FALSE)[c(2, 1, 3)]
  flows <- rbind(setNames(from, c("from", "to", "n")),
                 setNames(to, c("from", "to", "n")))
  locations <- YF_Brazil$states
  others    <- setdiff(flows$to, YF_Brazil$states$location_code)
  los       <- data.frame(location_code = names(YF_Brazil$length_of_stay), 
                          length_of_stay = YF_Brazil$length_of_stay)
  locations <- merge(locations,
                     data.frame(location_code = others, stringsAsFactors = FALSE),
                     by = "location_code", all = TRUE)
  locations <- merge(locations, los, by = "location_code", all = TRUE)
  ef <- make_epiflows(flows, 
                      locations, 
                      pop_size = "location_population",
                      duration_stay = "length_of_stay")
  ef.es <- epicontacts::thin(ef[, j = "Espirito Santo", contacts = "either"])
  
  
  # num_countries <- length(to)
  codes <- epicontacts::get_id(ef.es)
  set.seed(9000)
  outcome <- estimate_risk_spread(
    ## These arguments specified by user
    location_code = "Espirito Santo",
    r_incubation = function(n) rlnorm(n, 1.46, 0.35),
    r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
    n_sim = 1e3,
    ## These arguments will be done inside the function
    location_population = get_vars(ef.es, "pop_size", id = FALSE)["Espirito Santo", ],
    num_cases_time_window = get_vars(ef.es, "num_cases_time_window", id = FALSE)["Espirito Santo", ],
    first_date_cases = get_vars(ef.es, "first_date_cases", id = FALSE)["Espirito Santo", ],
    last_date_cases = get_vars(ef.es, "last_date_cases", id = FALSE)["Espirito Santo", ],
    num_travellers_to_other_locations = get_n(ef.es, from = "Espirito Santo"),
    num_travellers_from_other_locations = get_n(ef.es, to   = "Espirito Santo"),
    avg_length_stay_days = na.omit(get_vars(ef.es, "duration_stay", id = FALSE, vector = TRUE))
  )
  expect_true(all(rownames(outcome) == codes[-1]))
})
