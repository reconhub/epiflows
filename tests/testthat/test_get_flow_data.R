context("Test get_flow_data()")

test_that("Correct flow data are returned", {
  ef <- do.call(make_epiflows, Mex_travel_2009)
  # Flows to Mexico
  flows_to <- get_flow_data(ef, "MEX", direction = "to")
  expect_equal(sum(flows_to), 36746233)
  expect_length(flows_to, 41)
  expect_equal(names(flows_to), get_codes(ef))
  # Flows from Mexico
  flows_from <- get_flow_data(ef, "MEX", direction = "from")
  expect_equal(sum(flows_from), 36581641)
  expect_length(flows_from, 41)
  expect_equal(names(flows_from), get_codes(ef))
  # Flows in both directions
  flows_both <- get_flow_data(ef, "MEX", direction = "both")
  expect_equal(flows_both$to, flows_to)
  expect_equal(flows_both$from, flows_from)
})

test_that("Error is thrown for incorrect input", {
  expect_error(get_flow_data(ef, "I_DONT_EXIST"))
  expect_error(get_flow_data(ef, "MEX", direction = "I don't know"))
})
