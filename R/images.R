#' Unwrap an image.
#'
#' Unwrapping means transforming image from polar coordinates into
#' carthsian coordinates. First, the center of the polar coordinate
#' system is placed at the center of the image rectangle. Then, pixel
#' values are read for each angle `alpha` from `0` to `2*Pi`, with a
#' step of `dAlpha`, and distance `r` from `0` to `diag/2`, with a step
#' of `dR`. Each sun "ray" becomes a column of the output image.
#'
#' Because maximum distance `diag/2` defines a circle extending beyond
#' the input image `x`, pixel values outside of `x` are assumed to be
#' equal to `missing`.
#'
#' @param x Input image.
#' @param dAlpha Step for the angle `alpha`.
#' @param dR Step for the distance `r`.
#' @param missing The value for pixels outside of `x`.
#'
#' @export
unwrap_image <- function (x, dAlpha = 1, dR = 1, missing = 1) {
  stopifnot(imager::is.cimg(x))
  stop_if_no_imager()

  a <- as.array(imager::squeeze(imager::grayscale(x)))
  o <- unwrap_array(a, dAlpha, dR, missing = missing)
  # rotate 90 deg counter-clockwise
  o <- t(apply(o, 2, rev))
  imager::as.cimg(o)
}

unwrap_array <- function (x, dAlpha = 1, dR = 1, rMax = NULL, missing = 0) {
  if (is.null(rMax)) rMax <- ceiling(sqrt(sum((dim(x)/2)**2)))
  unwrap_array_impl(x, as.numeric(dAlpha), as.numeric(rMax), as.numeric(dR), as.numeric(missing))
}


#' Compute distance between two images.
#'
#' Important: images are compared column-wise, thus they must have equal
#' numbers of columns. Distance is computed in two phases: preparation
#' followed by computing the proper distance.
#'
#' During the preparation, each image is first turned into scale of
#' grays with [imager::grayscale]. Then, gradient of color changes is
#' computed for each image as `imgradient(i, 'xy')` (see [imager::imgradient]).
#' Only gradients larger than `quantile(gradient, cutoff)` are kept.
#'
#' In the second phase, images are matched column-wise, creating pairs
#' of columns. For each column the distances from the bottom of the image
#' (bottom row) to each non-zero gradient value is calculated. These
#' values are then treated as results of sampling a random variable.
#' The empirical cumulative distribution function (_ecdf_) is obtained
#' for each column and the area between matching _ecdf_s is calculated.
#' This is similar to how the Kolmogorov-Smirnov test is performed.
#'
#' Finally, the total area for all pairs of columns is returned as the
#' distance between images `a` and `b`.
#'
#' For optimal results images should be always _unwrapped_; see
#' [unwrap_image].
#'
#' @param a First image; see [imager::cimg].
#' @param b Second image.
#' @param cutoff The cut-off quantile for gradient values.
#' @return Cumulative area between _ecdf_s for all pairs of columns.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   exp <- load.image('roc-linux.png')
#'   img <- load.image('test.png')
#'   image_dist(unwrap_image(test), unwrap_image(exp))
#' }
image_dist <- function (a, b, cutoff = .5) {
  stopifnot(imager::is.cimg(a), imager::is.cimg(b))
  stop_if_no_imager()

  as_grayscale <- function (img) {
    if (identical(last(dim(img)), 1L)) return(img)
    imager::squeeze(imager::grayscale(img))
  }

  to_distances <- function (x) {
    # only edges are interesting
    x <- as_grayscale(x) %>% imager::imgradient("xy") %>% imager::enorm() %>% imager::cannyEdges() %>% as.array
    # account for varying number of rows (distance in polar coordinates)
    apply(x, 1, function (c) which(c)/max(c))
  }

  diffs <- Map(cdf_area, to_distances(a), to_distances(b))
  sum(unlist(diffs))
}
