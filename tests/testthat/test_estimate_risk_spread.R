context("Test estimate_risk_spread()")

test_that("Correct value is returned", {
  ef <- do.call(make_epiflows, Mex_travel_2009)
  codes <- get_codes(ef)
  outcome <- estimate_risk_spread(
    ef,
    location_code = "MEX",
    time_window_days = 365,
    num_infec_cases_in_time_window = 1000,
    avg_length_stay_days = rep(30, length(codes)),
    distribution_incubation = rlnorm,
    params_incubation = c(4.6, sqrt(2.7)),
    distribution_infectious = rnorm,
    params_infectious = c(4.5, sqrt(0.6))
  )
  expect_true(all(rownames(outcome) == codes))
})
