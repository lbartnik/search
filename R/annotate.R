#' Calculate edit distance between R expressions.
#'
#' @description Calculates edit distance at the level of R tokens. See
#' [sourcetools::tokenize_string] for more information about the
#' tokenizer.
#'
#' @name edit-dist
#' @rdname edit_dist
#'
#' @examples
#' \dontrun{
#' edit_dist(tokenize(bquote(x <- 1)), tokenize(bquote(y <- 1)))
#' }
NULL

#' @param x R [base::expression] or R expression string (see [base::deparse]).
#'
#' @importFrom rlang is_symbolic
#' @rdname edit_dist
tokenize <- function(x) {
  stop_if_no_sourcetools()

  if (is.expression(x)) {
    x <- lapply(x, I)
    if (length(x) > 1) abort("Cannot handle more than one expression at a time.")
    x <- first(x)
  }

  if (is_symbolic(x)) x <- paste(deparse(x), collapse = '\n')

  if (!is.character(x)) {
    abort(glue("Cannot tokenize object of class {first(class(x))}. Pass R expression or a character string."))
  }

  tokens <- sourcetools::tokenize_string(x)

  i <- tokens$type == 'invalid'
  if (any(i)) {
    abort(glue("Found invalid tokens: ", paste(tokens$value[i], collapse = ' ')))
  }

  nws <- (tokens$type != "whitespace")
  as_tokens(tokens$value[nws])
}

as_tokens <- function (x) structure(x, class = 'tokens')

is_tokens <- function (x) inherits(x, 'tokens')


annotate_file <- function (path, repo, n = 3) {
  exprs <- parse(file = path, keep.source = TRUE)
  artfs <- match_expressions(exprs, repo, n = n)

  structure(exprs, class = c("annotated", class(exprs)), artref = artfs)
}

#' @export
print.annotated <- function(x, ...) {
  Map(function (expr, artref) {
    # print matches
    a_ids <- lapply(artref, function(a) toString(a$id))
    if (length(a_ids)) {
      ccat0(grey = "# Matching artifacts: ", green = paste(a_ids, collapse = " "), '\n')
    } else {
      ccat(grey = "# No matching artifacts\n")
    }

    # print expression
    print(expr)
  }, expr = x, artref = attr(x, 'artref'))

  invisible(x)
}


match_expressions <- function (exprs, repo, n) {
  # TODO here maybe something smart? like, the first n artifacts that are
  #      also distinct given the distribution of dinstances?

  etkn <- lapply(exprs, tokenize)
  artf <- as_artifacts(repo) %>% read_artifacts
  atkn <- lapply(artf, function (a) tokenize(a$expression))

  # calculate distance between all pairs
  # TODO if dist is zero, this could break the loop for that column/row
  dist <- matrix(NA_integer_, length(atkn), length(etkn))
  imap(atkn, function (at, ai) {
    imap(etkn, function (et, ei) {
      dist[ai, ei] <<- edit_dist(at, et)
    })
  })

  # store matches here
  ans <- replicate(length(exprs), list())

  # TODO make sure there are no double zeros in any column/row; if there are, they all need to be
  #      handled
  # keep track of artifacts that are already assigned and
  # expressions that met the maximum number of assignments
  while (any(!is.infinite(dist))) {
    d <- min(dist)
    i <- as.list(which(dist == d, arr.ind = TRUE)[1,])

    art <- append(nth(artf, i$row), c(dist = d))
    ans[[i$col]] <- append(ans[[i$col]], list(art))

    # remove artifact from possible assignments
    dist[i$row,] <- Inf
    # if exact match or n assignments complete, remove expression as well
    if ((d == 0) || (length(ans[i$col]) >= n)) {
      dist[,i$col] <- Inf
    }
  }

  ans
}
