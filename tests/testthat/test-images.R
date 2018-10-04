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
  test_that("unwrap array", {
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
})
