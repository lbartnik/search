context("distmatrix")

test_that("dump repo plots", {
  images <- dump_repository(iris_models())

  expect_length(images, 2)
  expect_named(images, c('0f1105f2', '909a1c6d'), ignore.order = TRUE)
  expect_true(all(map_lgl(images, is.cimg)))
})
