context("Test get_location_data()")

flows <- Mex_travel_2009[[1]]
to <- structure(flows[["MEX"]], names = rownames(flows))
from <- unlist(flows["MEX", ])
ef <- make_epiflows(
  to = to,
  from = from,
  code = "MEX",
  locationsdata = Mex_travel_2009[[2]]
)

test_that("Correct location data are returned", {
  codes <- c("BEL", "NLD", "LUX")
  locations <- get_location_data(ef, codes)
  expect_named(locations, c("code", "country", "population"))
  expect_equal(locations$code, codes)
})

test_that("Error is thrown for incorrect input", {
  expect_error(get_location_data(ef, "I_DONT_EXIST"))
})
