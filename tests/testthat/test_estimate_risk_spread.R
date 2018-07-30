context("Test estimate_risk_spread()")
data("Brazil_epiflows")
ef.es <- epicontacts::thin(Brazil_epiflows[, j = "Espirito Santo"])
codes <- get_id(ef.es)

test_that("an error will be thrown if all arguments are missing", {
  expect_error(estimate_risk_spread(),
               "'location_code' is missing. Please supply a character value.")
})

test_that("an error will be thrown if arguments are misspelled", {
  expect_error({
    outcome <- estimate_risk_spread(
      ## These arguments specified by user
      loocation_code = "Espirito Santo",
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
  },
  regexp = "'location_code' is missing. Please supply a character value.\n.+Unmatched arguments: loocation_code")
})

test_that("avg_length_stay_days can take a single value as input", {
  expect_warning({
    set.seed(9000)
    res1 <- estimate_risk_spread(Brazil_epiflows, 
                                location_code = "Espirito Santo",
                                r_incubation = function(n) rlnorm(n, 1.46, 0.35),
                                r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
                                n_sim = 99,
                                avg_length_stay_days = rep(2, 10)
    )
  }, 
  regexp = "number of simulations"
  )
  expect_warning({
    set.seed(9000)
    res2 <- estimate_risk_spread(Brazil_epiflows, 
                                location_code = "Espirito Santo",
                                r_incubation = function(n) rlnorm(n, 1.46, 0.35),
                                r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
                                n_sim = 99,
                                avg_length_stay_days = 2
    )
  }, 
  regexp = "number of simulations"
  )
  
  
  
  expect_identical(res1, res2)
})

test_that("avg_length_stay_days will bork if given an incorrect length", {
  expect_error({
    set.seed(9000)
    suppressWarnings({
      res2 <- estimate_risk_spread(Brazil_epiflows, 
                                   location_code = "Espirito Santo",
                                   r_incubation = function(n) rlnorm(n, 1.46, 0.35),
                                   r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
                                   n_sim = 99,
                                   avg_length_stay_days = c(2, 2)
      )
    })
  }, 
  regexp = "avg_length_stay_days must have length equal to 1 or to the number of locations"
  )
})






test_that("Correct value is returned", {
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


test_that("estimate_risk_spread works on epiflow objects", {
  expect_warning({
    set.seed(9000)
    res <- estimate_risk_spread(Brazil_epiflows, 
                                location_code = "Espirito Santo",
                                r_incubation = function(n) rlnorm(n, 1.46, 0.35),
                                r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
                                n_sim = 99
                                )
    }, 
    regexp = "number of simulations"
  )
  expect_true(all(rownames(res) %in% codes[-1]))
})

test_that("estimate_risk_spread arguments can be overridden", {
  expect_warning({
    set.seed(9000)
    res1 <- estimate_risk_spread(Brazil_epiflows, 
                                location_code = "Espirito Santo",
                                r_incubation = function(n) rlnorm(n, 1.46, 0.35),
                                r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
                                n_sim = 9
    )
  }, 
  regexp = "number of simulations"
  )
  expect_warning({
    set.seed(9000)
    res2 <- estimate_risk_spread(Brazil_epiflows, 
                                 location_code = "Espirito Santo",
                                 r_incubation = function(n) rlnorm(n, 1.46, 0.35),
                                 r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
                                 n_sim = 9,
                                 num_cases_time_window = 2.6e5
    )
  }, 
  regexp = "number of simulations"
  )
  expect_true(all(res2 > res1))
})


test_that("errors are thrown for ambiguities or wrong arguments", {
  expect_error({
    set.seed(9000)
    res2 <- estimate_risk_spread(Brazil_epiflows, 
                                 location_code = "Espirito Santo",
                                 r_incubation = function(n) rlnorm(n, 1.46, 0.35),
                                 r_infectious = function(n) rnorm(n, 4.5, 1.5/1.96),
                                 n_sim = 9,
                                 num = 2.6e5,
                                 grind = 1,
                                 core = TRUE
    )
  },
  regexp = "Unmatched arguments: grind, core\n  Matched multiple arguments: num")
})



