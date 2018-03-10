context("Test make_epiflows() constructor")

test_that("make_epiflows() creates correct object", {
  ## Using input data with a flow matrix
  ef <- do.call(make_epiflows, Mex_travel_2009)
  # Test if a valid epiflows object has been created
  expect_s3_class(ef, "epiflows")
  expect_named(ef, c("flows", "locationsdata"))
  # Test location data format
  expect_named(ef$locationsdata, c("code", "country", "population"))
  expect_is(ef$locationsdata$population, "integer")
  # Test if location codes match
  expect_length(colnames(ef$flows), 41)
  expect_equal(rownames(ef$flows), colnames(ef$flows))
  expect_equal(ef$locationsdata$code, rownames(ef$flows))
  
  ## Using input data with vectors
  flows <- Mex_travel_2009[[1]]
  to <- structure(flows[["MEX"]], names = rownames(flows))
  from <- unlist(flows["MEX", ])
  ef2 <- make_epiflows(
    to = to,
    from = from,
    code = "MEX",
    locationsdata = Mex_travel_2009[[2]]
  )
  # Compare with `ef`
  expect_equal(ef2$flows, ef$flows)
  expect_equal(ef2$locationsdata, ef$locationsdata)
})

test_that("make_epiflows() throws an error for incorrect input", {
  # make_epiflows() shouldn't accept incomplete input
  expect_error(make_epiflows(Mex_travel_2009[[1]], NULL))
  expect_error(make_epiflows(NULL, Mex_travel_2009[[2]]))
  # It should fail when user provides arguments in the wrong order
  expect_error(make_epiflows(Mex_travel_2009[[2]], Mex_travel_2009[[1]]))
})
