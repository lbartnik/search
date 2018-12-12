stop_if_no_imager <- function () {
  if (!try_load("imager")) {
    abort("This API depends on the 'imager' package but it cannot be loaded. Install the package and try again.")
  }
}
