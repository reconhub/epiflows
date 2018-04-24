context("Test get_codes()")

flows <- Mex_travel_2009[[1]]
to <- structure(flows[["MEX"]], names = rownames(flows))
from <- unlist(flows["MEX", ])
ef <- make_epiflows(
  to = to,
  from = from,
  code = "MEX",
  locationsdata = Mex_travel_2009[[2]]
)

test_that("get_codes() returns correct codes", {
  codes <- get_codes(ef)
  expect_length(codes, 41)
  expect_equal(codes[1], "ARG")
  expect_equal(codes[41], "VEN")
})

test_that("get_codes() fails for invalid input", {
  expect_error(get_codes(ef$locationsdata))
})
