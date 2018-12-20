#' Identify artifacts by a variety of means.
#'
#' @param repo a [repository::repository] object.
#' @return Each function returns a `list` of artifacts (see
#'         [repository::new_artifact]) wrapped in a [utilities::as_container].
#'
#' @name identify-artifacts
#' @rdname identify_artifacts
NULL

#' @description `identify_file` finds artifacts matching the contents of
#' a given file: plot, data set (`csv`) or an R object (`rds` or `RData`).
#'
#' @param path file path.
#'
#' @export
#' @rdname identify_artifacts
identify_file <- function (path, repo) {
  stopifnot(file.exists(path))

  file <- new_file(path)
  identify_file_impl(file, repo)
}


#' @description `identify_object` finds artifacts matching a given R object.
#' @param obj any R object.
#'
#' @export
#' @importFrom rlang UQ
#' @rdname identify_artifacts
identify_object <- function (obj, repo) {
  id <- storage::compute_id(obj)
  q  <- as_artifacts(repo) %>% filter(id == UQ(id))

  ans <- q %>% summarise(n = n()) %>% first

  if (!ans) {
    warn("cannot match object in repository")
    return(NULL)
  }

  read_artifacts(q)
}


#' @description `identify_expression` finds artifacts created with an
#' expression matching the one passed as argument. Each artifact is
#' assigned an additional attribute, `dist`, which holds the edit
#' distance to the expression `expr`.
#'
#' @param expr expression to match against; as returned by [base::bquote] or
#'        [base::deparse].
#' @param n return that many closest matches given the edit distance (see
#'        [edit_dist]).
#'
#' @export
#' @rdname identify_artifacts
#' @importFrom utils head
identify_expression <- function (expr, repo, n = 3) {
  expr <- tokenize(expr)

  artf <- as_artifacts(repo) %>% read_artifacts
  dist <- map_dbl(artf, function (a) edit_dist(expr, tokenize(a$expression)))
  fnd  <- head(order(dist, decreasing = FALSE), n)

  artf <- lapply(fnd, function (i) {
    a <- artf[[i]]
    a$dist <- dist[[i]]
    a
  })

  # TODO here maybe something smart? like, the first n artifacts that are
  #      also distinct given the distribution of dinstances?

  as_container(artf)
}



#' @importFrom tools file_ext
new_file <- function (path) {
  ext <- tolower(file_ext(path))

  method <- "identify_file_impl"
  exts <- substr(ls(pattern = method, envir = asNamespace("search")), nchar(method)+2, 0xBEEF)
  exts <- setdiff(exts, c("default", "")) # remove the main method and the default dispatch

  if (ext %nin% exts) {
    abort(glue("extension '{ext}' is not supported"))
  }

  structure(list(path = path, extension = ext), class = ext)
}


identify_file_impl <- function (x, repo, ...) UseMethod("identify_file_impl")

identify_file_impl.default <- function (x, repo, ...) {
  abort(glue("cannot call identify_file_impl for class {first(class(x))}"))
}

identify_file_impl.rds <- function (x, repo, ...) {
  ans <- tryCatch(readRDS(x$path), error = function (e)e)
  if (is_error(ans)) {
    abort(glue("readRDS failed; {x$path} is not an RDS file"))
  }
  identify_object(ans, repo)
}

identify_file_impl.rdata <- function (x, repo, ...) {
  ans <- tryCatch(local({ load(x$path); lapply(ls(), get) }), error = function (e)e)
  if (is_error(ans)) {
    abort(glue("load() failed; {x$path} is not a RData file"))
  }

  # TODO what to do with possible multiple matches?
  ans <- unlist(lapply(ans, identify_object, repo = repo), recursive = FALSE)
  as_container(ans)
}

identify_file_impl.jpg <- identify_file_impl.png <- function (x, repo, ...) {
  stop_if_no_imager()
  as_container(list(identify_plot(imager::load.image(x$path), repo)))
}

#' @importFrom png readPNG
#' @importFrom jsonlite base64_dec
#' @importFrom grDevices png dev.off
identify_plot <- function (img, repo) {
  stop_if_no_imager()

  stopifnot(imager::is.cimg(img))
  stopifnot(is_repository(repo))

  h <- imager::height(img)
  w <- imager::width(img)

  dir_path <- file.path(tempdir(), paste0(w, 'x', h))
  stopifnot(dir.exists(dir_path) || dir.create(dir_path, showWarnings = FALSE, recursive = TRUE))

  arts <- as_artifacts(repo) %>% filter('plot' %in% class) %>% read_artifacts

  known <- arts %>% lapply(function (a) {
    path <- file.path(dir_path, paste0(toString(a$id), '.png'))
    if (!file.exists(path)) {
      png(path, w, h)
      replot(a)
      dev.off()
    }
    imager::load.image(path) %>% unwrap_image(0.01, 1)
  })

  new <- unwrap_image(img, 0.01, 1)
  dists <- map_dbl(known, function (known) image_dist(known, new))
  i <- which.min(dists)

  nth(arts, i)
}
