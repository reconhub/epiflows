context("Test make_epiflows() constructor")
data("YF_locations")
data("YF_flows")
data("Brazil_epiflows")
data("Mex_travel_2009")

test_that("make_epiflows() creates correct object", {
  flows   <- Mex_travel_2009[[1]]
  outflow <- setNames(flows[["MEX"]], rownames(flows))
  inflow  <- unlist(flows["MEX", , drop = TRUE])
  expect_warning(ef <- make_epiflows(inflow, outflow, focus = "MEX", locations = Mex_travel_2009[[2]]),
                 "Pruning")
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

test_that("make_epiflows.data.frame() works as expected", {
  ef <- make_epiflows(flows         = YF_flows, 
                      locations     = YF_locations, 
                      pop_size      = "location_population",
                      duration_stay = "length_of_stay",
                      num_cases     = "num_cases_time_window",
                      first_date    = "first_date_cases",
                      last_date     = "last_date_cases"
  )
  expect_identical(ef, Brazil_epiflows)
})

test_that("make_epiflows() will bork if there are extra varaibles", {
  expect_error({
    make_epiflows(flows         = YF_flows, 
                  locations     = YF_locations, 
                  poop_size     = "location_population",
                  duration_stay = "length_of_stay",
                  num_cases     = "num_cases_time_window",
                  first_date    = "first_date_cases",
                  last_date     = "last_date_cases",
                  grind         = "core"
    )
  },
  regex = "Unknown variables were found: poop_size, grind")
})



test_that("make_epiflows() can work with numeric data", {
  ef <- make_epiflows(flows         = YF_flows, 
                      locations     = YF_locations, 
                      pop_size      = 2,
                      duration_stay = 6,
                      num_cases     = 3,
                      first_date    = "first_date_cases",
                      last_date     = "last_date_cases"
  )
  expect_identical(get_pop_size(ef), get_pop_size(Brazil_epiflows))
  expect_identical(get_n(ef), get_n(Brazil_epiflows))
  expect_identical(get_vars(ef, "duration_stay"), get_vars(Brazil_epiflows, "duration_stay"))
  expect_identical(get_vars(ef, "first_date"), get_vars(Brazil_epiflows, "first_date"))
  expect_identical(get_vars(ef, "last_date"), get_vars(Brazil_epiflows, "last_date"))
})


