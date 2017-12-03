context("Test get_codes()")

ef <- do.call(make_epiflows, Mex_travel_2009)

test_that("get_codes() returns correct codes", {
  codes <- get_codes(ef)
  expect_length(codes, 41)
  expect_equal(codes[1], "ARG")
  expect_equal(codes[41], "VEN")
})

test_that("get_codes() fails for invalid input", {
  expect_error(get_codes(ef$locationsdata))
})
