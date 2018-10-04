#' @importFrom imager is.cimg grayscale squeeze
#' @export
unwrap_image <- function (x, dAlpha = 1, dR = 1, missing = 1) {
  stopifnot(is.cimg(x))
  a <- as.array(squeeze(grayscale(x)))
  o <- unwrap_array(a, dAlpha, dR, missing = missing)
  # rotate 90 deg counter-clockwise
  o <- t(apply(o, 2, rev))
  as.cimg(o)
}


unwrap_array <- function (x, dAlpha = 1, dR = 1, rMax = NULL, missing = 0) {
  stopifnot(is.array(x), is.numeric(x))
  rMax <- ceiling(sqrt(sum((dim(x)/2)**2)))
  .Call("C_unwrap_array", x, as.numeric(dAlpha), as.numeric(rMax), as.numeric(dR), as.numeric(missing))
}
