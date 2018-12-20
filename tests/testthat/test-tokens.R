context("tokens")

test_that("basic edit dist", {
  expect_equal(edit_dist_impl("a", "a"), 0)
  expect_equal(edit_dist_impl("a", "b"), 1)
  expect_equal(edit_dist_impl("a", c("a", "b")), 1)
  expect_equal(edit_dist_impl(c("a", "c"), c("a", "b")), 1)
  expect_equal(edit_dist_impl(c("b", "c"), c("a", "b")), 2)
})

test_that("tokenize", {
  x <- tokenize("x <- 1")
  expect_length(x, 3)
  expect_s3_class(x, 'tokens')
  expect_equal(as.character(x), c("x", "<-", "1"))
})

test_that("edit dist on tokens", {
  x <- edit_dist(tokenize("x <- 1"), tokenize("y <- 1"))
  expect_equal(x, 1)
})

