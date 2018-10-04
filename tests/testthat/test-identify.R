context("identify")

test_that("can identify object", {
  r <- sample_repository()
  x <- identify_object(iris, r)

  expect_length(x, 1)
  expect_equal(artifact_data(first(x)), iris)
})

test_that("can identify RDS", {
  r <- sample_repository()
  p <- tempfile(fileext = '.rds')
  saveRDS(iris, p)

  x <- identify_file(p, r)

  expect_length(x, 1)
  expect_equal(artifact_data(first(x)), iris)
})

test_that("can identify RData", {
  r <- sample_repository()
  p <- tempfile(fileext = '.RData')
  save(iris, file = p)

  x <- identify_file(p, r)

  expect_length(x, 1)
  expect_equal(artifact_data(first(x)), iris)
})

