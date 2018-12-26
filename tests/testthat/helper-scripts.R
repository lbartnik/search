sample_script <- function() {
  path <- system.file("scripts/iris-model.R", package = "repository")
  expect_true(file.exists(path))
  path
}

sample_expressions <- function() {
  parse(sample_script(), keep.source = TRUE)
}
