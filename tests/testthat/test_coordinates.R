context("Coordinate tests")

test_that("coordinates can be added over the internet", {
  skip_on_cran()
  skip_on_cran()
  skip_if_not_installed("curl")
  has_internet <- !is.null(curl::nslookup("r-project.org", error = FALSE))
  skip_if(!has_internet, "No internet connection")
  data("Brazil_epiflows")
  b1 <- epicontacts::thin(Brazil_epiflows[j = c("Minas Gerais", "Italy"), contacts = "both"])
  data("YF_coordinates")
  expect_error(add_coordinates(b1, YF_coordinates), "two columns")
  expect_error(add_coordinates(YF_coordinates), "YF_coordinates must be an object of class epiflows")
  expect_error(get_coordinates(b1), "coordinates are not set in b1")
  suppressWarnings(b2  <- add_coordinates(b1))
  expect_error(add_coordinates(b2), "Use `overwrite = TRUE`")
  coo                  <- get_coordinates(b2)
  for (id in get_id(b2)) {
    if (!is.na(get_coordinates(b2, location = id)[1])) {
      expect_equal(coo[coo$id == id, , drop = FALSE]$lon, 
                   YF_coordinates[YF_coordinates$id == id, , drop = FALSE]$lon, 
                   label = sprintf("coordinate id %s", id))
    }
  }
  # confirming this works
  b3 <- add_coordinates(b2, 
                        YF_coordinates[YF_coordinates$id %in% c("Minas Gerais", "Italy"), -1],
                        overwrite = TRUE)
  b4 <- add_coordinates(b2, 
                        YF_coordinates[c(YF_coordinates$id == "Minas Gerais", NA), -1],
                        overwrite = TRUE)
  suppressWarnings(b4 <- add_coordinates(b4, overwrite = TRUE))
})