context("Test get_location_data()")

ef <- do.call(make_epiflows, Mex_travel_2009)

test_that("Correct location data are returned", {
  codes <- c("BEL", "NLD", "LUX")
  locations <- get_location_data(ef, codes)
  expect_named(locations, c("code", "country", "population"))
  expect_equal(locations$code, codes)
})

test_that("Error is thrown for incorrect input", {
  expect_error(get_location_data(ef, "I_DONT_EXIST"))
})
