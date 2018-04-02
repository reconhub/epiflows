context("Test subsetting")

flows <- Mex_travel_2009[[1]]
to <- structure(flows[["MEX"]], names = rownames(flows))
from <- unlist(flows["MEX", ])
ef <- make_epiflows(
  to = to,
  from = from,
  code = "MEX",
  locationsdata = Mex_travel_2009[[2]]
)

test_that("Subset of an epiflows object is returned", {
  code_subset <- c("MEX", "JPN")
  ef_MEX_JPN <- ef[code_subset]
  # Test if location codes match
  expect_named(ef_MEX_JPN$flows$to, code_subset)
  expect_named(ef_MEX_JPN$flows$from, code_subset)
  expect_equal(ef_MEX_JPN$locationsdata$code, code_subset)
})

test_that("Subsetting fails for invalid types", {
  expect_error(ef[1])
  expect_error(ef[TRUE])
})

test_that("Subsetting fails for incorrect codes", {
  expect_error(ef["I_DONT_EXIST"])
})
