context("Test make_epiflows() constructor")

test_that("make_epiflows() creates correct object", {
  flows <- Mex_travel_2009[[1]]
  to <- structure(flows[["MEX"]], names = rownames(flows))
  from <- unlist(flows["MEX", ])
  ef <- make_epiflows(
    to = to,
    from = from,
    code = "MEX",
    locationsdata = Mex_travel_2009[[2]]
  )
  # Test if a valid epiflows object has been created
  expect_s3_class(ef, "epiflows")
  expect_named(ef, c("flows", "locationsdata", "origin"))
  # Test location data format
  expect_named(ef$locationsdata, c("code", "country", "population"))
  expect_is(ef$locationsdata$population, "integer")
  # Test if location codes match
  expect_length(ef$flows$to, 41)
  expect_equal(names(ef$flows$to), names(ef$flows$from))
  expect_equal(ef$locationsdata$code, names(ef$flows$to))
})

test_that("make_epiflows() throws an error for incorrect input", {
  # make_epiflows() shouldn't accept incomplete input
  expect_error(make_epiflows(Mex_travel_2009[[1]], NULL))
  expect_error(make_epiflows(NULL, Mex_travel_2009[[2]]))
  # It should fail when user provides arguments in the wrong order
  expect_error(make_epiflows(Mex_travel_2009[[2]], Mex_travel_2009[[1]]))
})
