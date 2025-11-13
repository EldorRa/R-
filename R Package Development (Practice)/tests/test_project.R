library(testthat)
library(project)

test_that("Multiserver returns a tibble", {
  set.seed(123)
  arrivals <- cumsum(rexp(5, 1/60))
  service_times <- rexp(5, 1/150) + 20
  out <- Multiserver(arrivals, service_times, 1)
  expect_s3_class(out, "tbl_df")
})

test_that("Multiserver handles input errors", {
  expect_error(Multiserver(c(1,2), c(-1,3), 1), "positive")
  expect_error(Multiserver(c(1,2), c(3), 1), "same length")
})

