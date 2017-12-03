context("Test subsetting")

ef <- do.call(make_epiflows, Mex_travel_2009)

test_that("Subset of an epiflows object is returned", {
  code_subset <- c("MEX", "JPN")
  ef_MEX_JPN <- ef[code_subset]
  # Test if location codes match
  expect_named(ef_MEX_JPN$flows, code_subset)
  expect_equal(rownames(ef_MEX_JPN$flows), colnames(ef_MEX_JPN$flows))
  expect_equal(ef_MEX_JPN$locationsdata$code, rownames(ef_MEX_JPN$flows))
})

test_that("Subsetting fails for invalid types", {
  expect_error(ef[1])
  expect_error(ef[TRUE])
})

test_that("Subsetting fails for incorrect codes", {
  expect_error(ef["I_DONT_EXIST"])
})
