context("Test estimate_risk_spread()")
data("Brazil_epiflows")
ef.es <- epicontacts::thin(Brazil_epiflows[, j = "Espirito Santo"])

test_that("Correct value is returned", {
  # num_countries <- length(to)
  codes <- get_id(ef.es)
  set.seed(9000)
  outcome <- estimate_risk_spread(
    ## These arguments specified by user
    location_code = "Espirito Santo",
    r_incubation = function(n) rlnorm(n, 1.46, 0.35),
    r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
    n_sim = 1e3,
    ## These arguments will be done inside the function
    location_population = get_pop_size(ef.es)["Espirito Santo"],
    num_cases_time_window = get_vars(ef.es, "num_cases_time_window", id = FALSE)["Espirito Santo", ],
    first_date_cases = get_vars(ef.es, "first_date_cases", id = FALSE)["Espirito Santo", ],
    last_date_cases = get_vars(ef.es, "last_date_cases", id = FALSE)["Espirito Santo", ],
    num_travellers_to_other_locations = get_n(ef.es, from = "Espirito Santo"),
    num_travellers_from_other_locations = get_n(ef.es, to   = "Espirito Santo"),
    avg_length_stay_days = na.omit(get_vars(ef.es, "duration_stay", id = FALSE, vector = TRUE))
  )
  expect_true(all(rownames(outcome) %in% codes[-1]))
})

test_that("a matrix of simulations is returned if requested", {
  expect_warning({
    outcome <- estimate_risk_spread(
      ## These arguments specified by user
      location_code = "Espirito Santo",
      r_incubation = function(n) rlnorm(n, 1.46, 0.35),
      r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
      n_sim = 99,
      ## These arguments will be done inside the function
      location_population = get_pop_size(ef.es)["Espirito Santo"],
      num_cases_time_window = get_vars(ef.es, "num_cases_time_window", id = FALSE)["Espirito Santo", ],
      first_date_cases = get_vars(ef.es, "first_date_cases", id = FALSE)["Espirito Santo", ],
      last_date_cases = get_vars(ef.es, "last_date_cases", id = FALSE)["Espirito Santo", ],
      num_travellers_to_other_locations = get_n(ef.es, from = "Espirito Santo"),
      num_travellers_from_other_locations = get_n(ef.es, to   = "Espirito Santo"),
      avg_length_stay_days = na.omit(get_vars(ef.es, "duration_stay", id = FALSE, vector = TRUE)),
      return_all_simulations = TRUE
    )},
    regexp = "number of simulations"
  )
  expect_is(outcome, "matrix")
  expect_equal(dim(outcome), c(99, 10))
})
