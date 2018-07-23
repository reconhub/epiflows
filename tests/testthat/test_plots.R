context("Visualization tests")

data("Brazil_epiflows")

test_that("invalid plots don't work", {
  expect_error(plot(Brazil_epiflows, type = "beer"), 
               '\'arg\' should be one of “map”, “network”, “grid”')
})
test_that("grid plot method works", {
  skip_if_not_installed("vdiffr")
  bgrid <- plot(Brazil_epiflows, type = "grid")
  vdiffr::expect_doppelganger(title = "grid plot", fig = bgrid)
})