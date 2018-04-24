context("Test estimate_risk_spread()")

test_that("Correct value is returned", {
  flows <- Mex_travel_2009[[1]]
  to <- structure(flows[["MEX"]], names = rownames(flows))
  from <- unlist(flows["MEX", ])
  ef <- make_epiflows(
    to = to,
    from = from,
    code = "MEX",
    locationsdata = Mex_travel_2009[[2]]
  )
  codes <- get_codes(ef)
  num_countries <- length(to)
  outcome <- estimate_risk_spread(
    ef,
    location_code = "MEX",
    time_window_days = 365,
    num_infec_cases_in_time_window = 1000,
    avg_length_stay_days = rpois(num_countries, 30),
    distribution_incubation = rlnorm,
    params_incubation = c(4.6, sqrt(2.7)),
    distribution_infectious = rnorm,
    params_infectious = c(4.5, sqrt(0.6))
  )
  expect_true(all(rownames(outcome) == codes))
})
