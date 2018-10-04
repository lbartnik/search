#' @importFrom rlang UQ
#' @export
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


#' @export
identify_file <- function (path, repo) {
  stopifnot(file.exists(path))

  file <- dispatch_file(path)
  identify_file_impl(file, repo)

  # TODO check file type
  #  1. if R data, try matching R objects
  #  2. if a plot, try matching a plot
  #  3. if csv, try matching data frames
  #  4. if none, probably fail
}


#' @importFrom tools file_ext
dispatch_file <- function (path) {
  ext <- tolower(file_ext(path))

  method <- "identify_file_impl"
  exts <- substr(ls(pattern = method, envir = asNamespace("reflection")), nchar(method)+2, 0xBEEF)
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
  structure(ans, class = 'container')
}
