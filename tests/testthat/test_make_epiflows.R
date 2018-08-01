context("Test make_epiflows() constructor")
data("YF_locations")
data("YF_flows")
data("Brazil_epiflows")

data("YF_Brazil")
inflow <- YF_Brazil$T_O["Espirito Santo", , drop = TRUE]
outflow <- YF_Brazil$T_D["Espirito Santo", , drop = TRUE]
locations <- subset(YF_Brazil$states, location_code == "Espirito Santo", drop = FALSE)
los <- data.frame(location_code    = names(YF_Brazil$length_of_stay), 
	          length_of_stay   = YF_Brazil$length_of_stay,
	          stringsAsFactors = FALSE
	         )
locations <- merge(x   = locations, 
		   y   = los, 
		   by  = "location_code", 
		   all = TRUE)


test_that("make_epiflows() creates correct object", {
  ef <- make_epiflows(inflow, outflow, focus = "Espirito Santo", locations = locations)
  set_vars(ef) <- list(pop_size = "location_population", 
                       num_cases = "num_cases_time_window", 
                       first_date = "first_date_cases", 
                       last_date = "last_date_cases",
                       duration_stay = "length_of_stay" 
                      )
  # Test if a valid epiflows object has been created
  expect_s3_class(ef, "epiflows")
  expect_s3_class(ef, "epicontacts")
  expect_named(ef, c("linelist", "contacts", "directed", "vars"))
  # Test location data format
  expect_named(get_flows(ef), c("from", "to", "n"))
  expect_named(get_locations(ef), c("id", unlist(get_vars(ef), use.names = FALSE)))
})

test_that("make_epiflows() will give a warning for duplicated entries", {
  infl <- c(inflow, "Espirito Santo" = 69)
  oufl <- c(outflow, "Espirito Santo" = 69)
  oufl_err <- c(outflow, "Espirito Santo" = 6911)
  expect_warning(make_epiflows(infl, oufl, focus = "Espirito Santo", locations = locations), "Pruning")
  expect_error(make_epiflows(infl, oufl_err, focus = "Espirito Santo", locations = locations), 
               "Duplicated IDs found in the data with different flows. Please de-duplicate your data before proceeding.")
})

test_that("make_epiflows.integer will bork if vectors are not named", {
  expect_error(make_epiflows(unname(inflow), outflow, focus = "Espirito Santo", locations = locations),
               "The vectors `inflow` and `outflow` must be named to create an epiflows object.")
})

test_that("make_epiflows.integer will bork if focus is not correct", {
  expect_error(make_epiflows(inflow, outflow, focus = 1, locations = locations),
               "The argument `focus` must be a single character string.")
  expect_error(make_epiflows(inflow, outflow, focus = c("TEX", "Espirito Santo"), locations = locations),
               "The argument `focus` must be a single character string.")
})



test_that("make_epiflows() throws an error for incorrect input", {
  # make_epiflows() shouldn't accept incomplete input
  expect_message(make_epiflows(locations, NULL), "Locations data frame not provided. Creating one from the flow data.")
  expect_error(make_epiflows(NULL, locations))
  # It should fail when user provides arguments in the wrong order
  expect_error(make_epiflows(YF_locations, YF_flows))
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

test_that("make_epiflows.data.frame() can handle 'n' correctly", {
  # If there is an extra variable called "n", it should be correctly moved to
  # the third position and all the handlers should be able to do their jobs.
  flo <- YF_flows
  names(flo)[3] <- "flows"
  flo$n <- "n"
  flo$rando <- runif(nrow(flo))
  flo <- flo[c(1, 2, 4, 3, 5)]
  expect_warning({
    ef <- make_epiflows(flows         = flo, 
                        locations     = YF_locations, 
                        n             = 4,
                        pop_size      = "location_population",
                        duration_stay = "length_of_stay",
                        num_cases     = "num_cases_time_window",
                        first_date    = "first_date_cases",
                        last_date     = "last_date_cases"
    )
  }, regex = "A varaible named `n` exists in the flows data frame. This will be renamed to n.1")
  expect_identical(get_flows(ef), get_flows(Brazil_epiflows))
  expect_identical(names(get_flows(ef)), c("from", "to", "n"))
  expect_identical(names(get_flows(ef[], all = TRUE)), c("from", "to", "n", "n.1", "rando"))
  expect_identical(names(get_flows(ef[l = 2], all = TRUE)), c("from", "to", "n", "rando"))
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
  regex = "Unknown variables were found: 'poop_size', 'grind'")
})

test_that("make_epiflows() will bork if the user tries to specify extra vars that don't exist in their data", {
  expect_error({
    make_epiflows(flows         = YF_flows, 
                  locations     = YF_locations, 
                  pop_size      = "location_population",
                  duration_stay = "length_of_stay",
                  num_cases     = "num_cases_time_window",
                  first_date    = "first_date_cases",
                  last_date     = "last_date_cases",
                  coordinates   = "core"
    )
  },
  regex = "column 'core' is NULL")
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


