context("Coordinate tests")
data("Brazil_epiflows")
data("YF_coordinates")

test_that("coordinates can be added, period", {
  b <- add_coordinates(Brazil_epiflows, YF_coordinates[-1L])
  expect_identical(get_coordinates(b), YF_coordinates)
})

test_that("coordinates won't be replaced if they already exist and none are missing", {
  b <- add_coordinates(Brazil_epiflows, YF_coordinates[-1L])
  c <- add_coordinates(b, overwrite = TRUE)
  expect_identical(get_coordinates(b), get_coordinates(c))
  expect_identical(get_coordinates(b), YF_coordinates)
})





test_that("coordinates can be added over the internet", {
  skip_on_cran()
  skip_on_cran()
  skip_if_not_installed("curl")
  has_internet <- !is.null(curl::nslookup("r-project.org", error = FALSE))
  skip_if(!has_internet, "No internet connection")
  b1 <- epicontacts::thin(Brazil_epiflows[j = c("Minas Gerais", "Italy"), contacts = "both"])
  expect_error(add_coordinates(b1, YF_coordinates), "two columns")
  expect_error(add_coordinates(YF_coordinates), "YF_coordinates must be an object of class epiflows")
  expect_null(get_coordinates(b1))
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

test_that("add_coordinates expects two character strings", {
  expect_error(add_coordinates(Brazil_epiflows, coordinates = c(1, 2)),
               "`coordinates` should contain exactly two character strings")
  expect_error(add_coordinates(Brazil_epiflows, coordinates = "lat"),
               "`coordinates` should contain exactly two character strings")
  expect_error(add_coordinates(Brazil_epiflows, coordinates = c("lat", "lon", "zee")),
               "`coordinates` should contain exactly two character strings")
})

test_that("add_coordinates will bork if the loc_column is not valid", {
  expect_error(add_coordinates(Brazil_epiflows, loc_column = "ED"),
               "`ED` is not a valid column name")
})



