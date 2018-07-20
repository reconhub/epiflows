context("Test make_epiflows() constructor")

test_that("make_epiflows() creates correct object", {
  data(Mex_travel_2009)
  flows   <- Mex_travel_2009[[1]]
  outflow <- setNames(flows[["MEX"]], rownames(flows))
  inflow  <- unlist(flows["MEX", , drop = TRUE])
  expect_warning(ef <- make_epiflows(inflow, outflow, focus = "MEX", locations = Mex_travel_2009[[2]]),
                 "pruning")
  # Test if a valid epiflows object has been created
  expect_s3_class(ef, "epiflows")
  expect_s3_class(ef, "epicontacts")
  expect_named(ef, c("linelist", "contacts", "directed", "vars"))
  # Test location data format
  expect_named(get_flows(ef), c("from", "to", "n"))
  expect_named(get_locations(ef), c("id", "country", "population"))
})

test_that("make_epiflows() throws an error for incorrect input", {
  # make_epiflows() shouldn't accept incomplete input
  expect_error(make_epiflows(Mex_travel_2009[[1]], NULL))
  expect_error(make_epiflows(NULL, Mex_travel_2009[[2]]))
  # It should fail when user provides arguments in the wrong order
  expect_error(make_epiflows(Mex_travel_2009[[2]], Mex_travel_2009[[1]]))
})
