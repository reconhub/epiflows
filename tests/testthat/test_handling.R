context("Test subsetting")

data("Brazil_epiflows")

test_that("Subset of an epiflows object is returned", {
  Brazil_pops <- get_id(Brazil_epiflows)[1:5]
  code_subset <- Brazil_pops[c(1, 3)]
  ef_ES_SP    <- Brazil_epiflows[j = code_subset]
  tef_ES_SP   <- epicontacts::thin(Brazil_epiflows[j = code_subset])
  ids         <- get_id(ef_ES_SP)
  expect_identical(ids, get_id(Brazil_epiflows))
  expect_failure(expect_identical(ids, get_id(tef_ES_SP)))
  expect_false(all(Brazil_pops %in% get_flows(ef_ES_SP, from = ids)$from))
  expect_false(all(Brazil_pops %in% get_flows(tef_ES_SP, from = ids)$from))
})

test_that("subsetting with nothing returns an identical object", {
  expect_identical(Brazil_epiflows, Brazil_epiflows[])
})

test_that("print method works", {
  expect_output(print(epicontacts::thin(Brazil_epiflows[j = "Minas Gerais"])),
                "11 locations")
})