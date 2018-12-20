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

test_that("can identify plot", {
  p <- imager::load.image('roc-linux.png')
  a <- identify_plot(p, iris_model())

  expect_true(is_artifact(a))
  expect_equal(toString(a$id), '0f1105f2')
})

test_that("can identify expression", {
  x <- identify_expression("virginica$predict <- predict(m, virginica)", iris_model())
  expect_length(x, 3)
  expect_s3_class(x, 'container')

  x <- first(x)
  expect_equal(toString(x$id), "3b9c6d54")
  expect_equal(x$dist, 0)
})
