#' Computing image distance matrix.
#'
#' @name distmatrix
#' @rdname distmatrix
NULL


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
      imgs <- lapply(item, function (file) as.cimg(read_png(file)))
      names(imgs) <- basename(item)
      return(imgs)
    }
    abort(glue("do not know how to handle {item}"))
  })
  unlist(images, recursive = FALSE)
}


#' @export
#'
#' @rdname distmatrix
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


#' @importFrom imager as.cimg
dump_repository <- function (repo) {
  arts <- as_artifacts(repo) %>%
    filter('plot' %in% class) %>%
    read_artifacts

  cimgs <- lapply(arts, function (a) as.cimg(to_png(a)))
  names(cimgs) <- shorten(map_chr(arts, `[[`, 'id'))
  cimgs
}


#' @importFrom imager save.image mirror imrotate cimg resize
#' @importFrom tools file_path_sans_ext
as.cimg.png <- function (raw_png) {
  if (length(dim(raw_png)) == 3) {
    dim(raw_png) <- c(dim(raw_png)[1:2], 1, dim(raw_png)[3])
  } else {
    dim(raw_png) <- c(dim(raw_png), 1, 1)
  }
  cimg(raw_png) %>% mirror("x") %>% imrotate(-90)
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


#' @export
#' @importFrom imager is.cimg resize save.image
#'
#' @rdname distmatrix
create_miniature <- function (x, unique_id) {
  stopifnot(is.cimg(x))

  tmp_path <- file.path(tempdir(), paste0(unique_id, '.png'))
  save.image(resize(x, 50, 50), tmp_path)

  tmp_path
}
