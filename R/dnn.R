generate_data <- function () {
  ds <- pick_ds()
  xy <- pick_cols(ds)
  pl <- new_plot(xy[,1], xy[,2])
}


pick_ds <- function (minrow = 15, mincol = 6) {
  while (42) {
    nm  <- sample(as.data.frame(data()$results, stringsAsFactors = FALSE)$Item, 1)
    ans <- get(strsplit(nm, " ", fixed = TRUE)[[1]][[1]])
    if (is.data.frame(ans) && nrow(ans) >= minrow && ncol(ans) >= mincol) return(ans)
  }
}

pick_cols <- function (data) {
  data[, sample.int(ncol(data), 2)]
}

is_categorical <- function (x) is.character(x) || is.factor(x)


new_plot <- function(data) {
  stopifnot(is.data.frame(data), ncol(data) == 2)

  nms <- names(data)
  c1 <- is_categorical(data[,1])
  c2 <- is_categorical(data[,2])

  # x cat, y cat
  if (c1 && c2) {
    data[,1] <- as.factor(data[,1])
    data[,2] <- as.factor(data[,2])

    return(structure(list(data = data, x = nms[1], y = nms[2]), class = 'heatmap'))
  }

  # x cat, y cont
  # x cont, y cat
  if (xor(c1, c2)) {
    i <- if (c1) 1 else 2
    return(structure(list(data = data, x = nms[i], y = nms[3-i]), class = 'boxplot'))
  }

  # x cont, y cont
  return(structure(list(data = data, x = nms[1], y = nms[2]), class = 'scatterplot'))
}

do_plot <- function (x, ggplot = FALSE, ...) UseMethod("do_plot")

#' @importFrom stats heatmap
do_plot.heatmap <- function (x, ggplot = FALSE, ...) {
  hm <- make_heatmap(x$data[[x$x]], x$data[[x$y]])
  heatmap(hm)
}


# https://stackoverflow.com/questions/23600122/counting-existing-permutations-in-r
#' @importFrom stats aggregate
make_heatmap <- function (f1, f2) {
  stopifnot(is.factor(f1), is.factor(f2))
  m <- matrix(0, nlevels(f1), nlevels(f2))

  a <- aggregate(seq_along(f1), list(x = as.integer(f1), y = as.integer(f2)), FUN = length)
  Map(function (i1, i2, x) {
    m[i1,i2] <<- x
  }, i1 = a$x, i2 = a$y, x = a$x)

  rownames(m) <- as.character(levels(f1))
  colnames(m) <- as.character(levels(f2))
  m
}


