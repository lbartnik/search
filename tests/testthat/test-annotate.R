context("annotate")

test_that("basic edit dist", {
  expect_ed <- function (a, b, d) {
    expect_equal(edit_dist(as_tokens(a),as_tokens(b)), d)
  }

  expect_ed("a", "a", 0)
  expect_ed("a", "b", 1)
  expect_ed("a", c("a", "b"), 1)
  expect_ed(c("a", "c"), c("a", "b"), 1)
  expect_ed(c("b", "c"), c("a", "b"), 2)
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

test_that("expressions can be matched", {
  match_expressions(sample_expressions(), iris_model(), 3)
})

test_that("expressions can be annotated", {
  a <- match_expressions(sample_expressions(), iris_model(), 3)

  expect_true(is.list(a))
  expect_length(a, 8)
  expect_equal(map_int(a, length), c(0, 1, 1, 1, 1, 1, 1, 1))
  lapply(a[2:8], function (ants) {
    expect_equal(first(ants)$dist, 0)
  })
})

test_that("file can be annotated", {
  expr <- annotate_file(sample_script(), iris_model())
  expect_s3_class(expr, 'expression')
  expect_s3_class(expr, 'annotated')

  artref <- attr(expr, 'artref')
  expect_true(is.list(artref))

  # sanity check; should be exactly as the value in the test above
  expect_equal(map_int(artref, length), c(0, 1, 1, 1, 1, 1, 1, 1))
})
