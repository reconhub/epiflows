context("Visualization tests")

data("Brazil_epiflows")
data("YF_coordinates")
bef <- add_coordinates(Brazil_epiflows, coordinates = YF_coordinates[-1])



test_that("invalid plots don't work", {
  expect_error(plot(Brazil_epiflows, type = "beer"))
})

test_that("default plotting will give us a network if no coordinates are available", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_message(net <- plot(Brazil_epiflows))
  vdiffr::expect_doppelganger(title = "full net", fig = net)
})



test_that("grid plot method works", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  bgrid <- plot(Brazil_epiflows, type = "grid")
  bgrid_nocolor <- plot(Brazil_epiflows, type = "grid", color_by = "none")
  vdiffr::expect_doppelganger(title = "grid plot", fig = bgrid)
  vdiffr::expect_doppelganger(title = "grid plot no color", fig = bgrid_nocolor)
})


test_that("vis plots work", {
  skip_on_cran()
  skip_if_not_installed("curl")
  has_internet <- !is.null(curl::nslookup("r-project.org", error = FALSE))
  skip_if(!has_internet, "No internet connection")
  data("YF_coordinates")
  mg2sp <- bef[j = c("Minas Gerais", "Spain")]
  bef_map <- plot(mg2sp, center = "Minas Gerais")
  bef_map_thin <- plot(epicontacts::thin(mg2sp), center = "Minas Gerais")
  vdiffr::expect_doppelganger(title = "map plot", fig = bef_map)
  vdiffr::expect_doppelganger(title = "map plot 2", fig = bef_map_thin)
})

test_that("maps bork if the input doesn't make sense", {
  expect_error(map_epiflows(bef, center = c("house", "mouse")),
               "center must be a single character string to use for ID lookup or a set of coordinates")
})



test_that("network plots work", {
  skip_on_cran()
  mg <- bef[j = "Minas Gerais"]
  mg_net      <- plot(mg, type = "network")
  mg_net_thin <- plot(mg, type = "network")
  vdiffr::expect_doppelganger(title = "network plot", fig = mg_net)
  vdiffr::expect_doppelganger(title = "network plot 2", fig = mg_net_thin)
})