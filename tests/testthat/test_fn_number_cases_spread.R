context("Test fn_number_cases_spread()")

test_that("Correct value is returned", {
  ef <- do.call(make_epiflows, Mex_travel_2009)
  codes <- get_codes(ef)
  outcome <- fn_number_cases_spread(
    ef,
    code = "MEX",
    time_window_days = 365,
    num_infec_cases_in_time_window = 1000,
    avg_length_stay_days = rep(30, length(codes)),
    mean_incubation = 4.6,
    var_incubation = 2.7,
    mean_infectious = 4.5,
    var_infectious = 0.6
  )
  expect_true(all(rownames(outcome) == codes))
})
