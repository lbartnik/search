context("images")

test_that("unwrap array", {
  exp <- array(c(4, 0,
                 4, 3,
                 4, 1,
                 4, 1,
                 4, 2,
                 4, 4),
               c(2, 6))
  inp <- array(as.double(1:4), c(2, 2))
  expect_equal(unwrap_array(inp), exp)
})

test_that("unwrap array", {
  exp <- array(c(5, 8, 0,
                 5, 4, 7,
                 5, 1, 1,
                 5, 1, 1,
                 5, 2, 3,
                 5, 5, 6),
               c(3, 6))
  inp <- array(as.double(1:9), c(3, 3))
  expect_equal(unwrap_array(inp), exp)
})

test_that("custom missing", {
  exp <- array(c(4, 1000,
                 4, 3,
                 4, 1,
                 4, 1,
                 4, 2,
                 4, 4),
               c(2, 6))
  inp <- array(as.double(1:4), c(2, 2))
  expect_equal(unwrap_array(inp, missing = 1000), exp)
})

test_that("cdf area", {
  expect_equal(cdf_area(0, 0), 0)
  expect_equal(cdf_area(1, 0), 1)
  expect_equal(cdf_area(1, 1), 0)

  expect_equal(cdf_area(1:2, 0), 1.5)
  expect_equal(cdf_area(c(1, 3), 2), 1)
  expect_equal(cdf_area(c(1, 3), c(2, 4)), 1)
})

test_that("cdf vs ecdf", {
  # two samples of 10 points each
  x <- exp(-(1:10/10))
  y <- 3*sin(1:10/10)

  # ecdf-based computation
  X <- sort(c(x, y))
  X <- c(min(X), X)
  exp <- sum(abs(ecdf(x)(X)-ecdf(y)(X)) * c(diff(X), 10))

  expect_equal(cdf_area(x, y), exp)
})
