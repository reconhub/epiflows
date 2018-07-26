context("Export Tests")
data("Brazil_epiflows")
data("YF_coordinates")

test_that("as.SpatialLinesDataFrame exports as that class", {
  ef <- add_coordinates(Brazil_epiflows, YF_coordinates[-1])
  sldf <- as.SpatialLinesDataFrame(ef)
  fl   <- get_flows(ef)
  expect_error(as.SpatialLinesDataFrame(Brazil_epiflows), "Brazil_epiflows does not have coordinates set")
  expect_is(sldf, "SpatialLinesDataFrame")
  expect_identical(row.names(sldf), paste(fl$from, fl$to, sep = ":"))
})

