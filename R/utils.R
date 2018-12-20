stop_if_no_imager <- function () {
  if (!try_load("imager")) {
    abort("This API depends on the 'imager' package but it cannot be loaded. Install the package and try again.")
  }
}

stop_if_no_sourcetools <- function () {
  if (!try_load("sourcetools")) {
    abort("This API depends on the 'sourcetools' package but it cannot be loaded. Install the package and try again.")
  }
}
