context("Global variable tests")

gv <- global_vars()

test_that("global_vars() defaults make sense", {
  expect_identical(gv, c("coordinates", "pop_size", "duration_stay", "first_date", "last_date", 
                         "num_cases"))
})

test_that("global vars can be added", {
  bees <- global_vars("bees", set = TRUE)
  expect_identical(global_vars(), c(gv, "bees"))
  expect_identical(global_vars(), bees)
})

test_that("global vars are not duplicated", {
  bees <- global_vars(c(gv, "bees"), set = TRUE)
  expect_identical(global_vars(), c(gv, "bees"))
  expect_identical(global_vars(), bees)
})

test_that("global vars adds the global environment variable", {
  options(epiflows.vars = "yahoo")
  expect_equal(global_vars(), "yahoo")
  expect_equal(global_vars(set = TRUE), c("yahoo", gv))
})

test_that("global_vars can be reset", {
  res <- global_vars(reset = TRUE)
  expect_identical(global_vars(), gv)
  expect_identical(global_vars(), res)
})

context("Variable accessor tests")

data("Brazil_epiflows")
bef <- make_epiflows(flows = get_flows(Brazil_epiflows),
                     locations = get_locations(Brazil_epiflows)
                     )

test_that("no vars registered prints instructions", {
  expect_output(print(bef), "no variables set; use set_vars\\(\\) to define variables in your locations metadata")
})

test_that("get_vars returns the vars list with no extra args", {
  expect_identical(get_vars(Brazil_epiflows), Brazil_epiflows$vars)
})

test_that("get_vars returns an empty list when none are set", {
  expect_identical(get_vars(bef), list())
})

test_that("set_vars can set a single variable", {
  bef1 <- bef
  bef2 <- set_vars(bef, pop_size = "location_population")
  set_vars(bef, "pop_size") <- "location_population"
  set_vars(bef1)            <- c(pop_size = "location_population")
  expect_identical(bef, bef2)
  expect_identical(bef1, bef2)
  expect_identical(get_vars(bef), list(pop_size = "location_population"))
})

test_that("set_vars can set several variables", {
  bef3 <- set_vars(bef,
                   pop_size = "location_population", 
                   duration_stay = "length_of_stay", 
                   num_cases = "num_cases_time_window", 
                   first_date = "first_date_cases", 
                   last_date = "last_date_cases")
  set_vars(bef) <- get_vars(Brazil_epiflows)
  expect_identical(bef, bef3)
  expect_identical(get_vars(bef), get_vars(Brazil_epiflows))
})


test_that("set_vars with NULL will remove the variables", {
  set_vars(bef) <- get_vars(Brazil_epiflows)
  expect_identical(get_vars(bef), get_vars(Brazil_epiflows))
  set_vars(bef) <- NULL
  expect_identical(get_vars(bef), list())
})

context("get N")

test_that("get_n borks if both from and to are specified", {
  expect_error(get_n(bef, from = "Italy", to = "Italy"),
               "Only one of the 'from' or 'to' parameters can be non-NULL.")
})

test_that("get_n borks if there are multiple items in from or to", {
  expect_error(get_n(bef, from = c("Italy", "Franc")), 
               "The parameters 'from' and 'to' must contain a single item.")
})


