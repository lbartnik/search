#' Computing image distance matrix.
#'
#' @name distmatrix
#' @rdname distmatrix
NULL


#' @param ... objects to extract images from.
#'
#' @importFrom rlang is_character
#' @export
#' @rdname distmatrix
#'
#' @examples
#' \dontrun{
#'   imgs <- bind_images('path.to.png', repository(filesystem('path/to/repository')))
#'   dist <- compute_matrix(imgs)
#'   path <- map_chr(imgs, create_miniature)
#'   rownames(dist) <- colnames(dist) <- sprintf("![](%s)", path)
#' }
bind_images <- function (...) {
  images <- lapply(list(...), function (item) {
    if (is_repository(item)) {
      return(dump_repository(item))
    }
    if (is_character(item) && all(file.exists(item))) {
      imgs <- lapply(item, function (file) imager::as.cimg(read_png(file)))
      names(imgs) <- basename(item)
      return(imgs)
    }
    abort(glue("do not know how to handle {item}"))
  })
  unlist(images, recursive = FALSE)
}


#' @param images a list of [imager::cimg] objects.
#' @export
#' @rdname distmatrix
#'
#' @importFrom utils combn
compute_matrix <- function (images) {
  unwrapped <- lapply(images, function (img) unwrap_image(img, 0.01, 1))
  dists <- combn(unwrapped, 2, function(pair) image_dist(first(pair), second(pair), .95))

  distm <- matrix(NA_real_, length(unwrapped), length(unwrapped))

  hide <- apply(rbind(combn(seq(length(unwrapped)), 2), dists), 2, function (x) {
    distm[x[1], x[2]] <<- round(x[3], 2)
  })

  diag(distm) <- 0

  rownames(distm) <- colnames(distm) <- names(images)
  distm
}


dump_repository <- function (repo) {
  stop_if_no_imager()

  arts <- as_artifacts(repo) %>%
    filter('plot' %in% class) %>%
    read_artifacts

  cimgs <- lapply(arts, function (a) imager::as.cimg(to_png(a)))
  names(cimgs) <- map_chr(arts, function(x)toString(nth(x, 'id')))
  cimgs
}


#' @importFrom tools file_path_sans_ext
as.cimg.png <- function (raw_png) {
  stop_if_no_imager()

  if (length(dim(raw_png)) == 3) {
    dim(raw_png) <- c(dim(raw_png)[1:2], 1, dim(raw_png)[3])
  } else {
    dim(raw_png) <- c(dim(raw_png), 1, 1)
  }
  imager::cimg(raw_png) %>% imager::mirror("x") %>% imager::imrotate(-90)
}


#' @importFrom rlang is_character
#' @importFrom jsonlite base64_dec
to_png <- function (x) {
  if (is_artifact(x)) x <- artifact_data(x)$png
  if (!is_character(x)) abort('only artifacts and character vectors are supported')

  read_png(base64_dec(x))
}


#' @importFrom png readPNG
read_png <- function (x) {
  structure(readPNG(x), class = 'png')
}


#' @param x a [imager::cimg] object.
#' @param unique_id an unique string used to create a file name for `x`.
#'
#' @export
#' @rdname distmatrix
create_miniature <- function (x, unique_id) {
  stopifnot(imager::is.cimg(x))

  tmp_path <- file.path(tempdir(), paste0(unique_id, '.png'))
  imager::save.image(imager::resize(x, 50, 50), tmp_path)

  tmp_path
}
