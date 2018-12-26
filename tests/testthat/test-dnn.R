context("dnn")

test_that("make_heatmap", {
  hm <- make_heatmap(as.factor(c("a","a","b","b")), as.factor(c("c","d","d","d")))

  expect_true(is.matrix(hm))
  expect_equal(nrow(hm), 2)
  expect_equal(ncol(hm), 2)
  expect_equal(as.numeric(hm), c(1, 0, 1, 2))
  expect_equal(rownames(hm), c("a", "b"))
  expect_equal(colnames(hm), c("c", "d"))
})

