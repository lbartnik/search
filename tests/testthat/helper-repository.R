sample_repository <- function() {
  stopifnot(requireNamespace("repository", quietly = TRUE))
  repository::iris_models()
}
