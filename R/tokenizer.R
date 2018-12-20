#' Calculate edit distance between R expressions.
#'
#' @description Calculates edit distance at the level of R tokens. See
#' [sourcetools::tokenize_string] for details on tokenizer.
#'
#' @name edit-dist
#' @rdname edit_dist
NULL

#' @param x R [base::expression] or R expression string (see [base::deparse]).
#'
#' @name edit-dist
#' @rdname edit_dist
#'
#' @importFrom rlang is_symbolic
tokenize <- function(x) {
  stop_if_no_sourcetools()
  if (is_symbolic(x)) x <- deparse(x)
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

#' @param a tokenized R expression; output of `tokenize()`.
#' @param b tokenized R expression; output of `tokenize()`.
#'
#' @name edit-dist
#' @rdname edit_dist
#'
#' @examples
#' \dontrun{
#' edit_dist(tokenize(bquote(x <- 1)), tokenize(bquote(y <- 1)))
#' }
edit_dist <- function (a, b) {
  stopifnot(is_tokens(a), is_tokens(b))
  edit_dist_impl(a, b)
}
