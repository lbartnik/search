library(jsonlite)
library(repository)
library(utilities)
library(imager)
library(glue)
devtools::load_all(".")

if (FALSE) {
  aid <- '0f1105f2e5992669196384b0a66536ef7dfc4111'
  as_artifacts(sample_repository()) %>% filter(id == aid) %>% read_artifacts %>% first
}


if (FALSE) {
  a <- array(as.double(1:9), c(3, 3))
  a
  reflection:::unwrap_array(a)

  o <- unwrap_image(load.image("tests/testthat/roc.png"), 0.01, 1)

  r <- sample_repository()
  p1 <- unwrap_image(load_plot('0f1105f2e5992669196384b0a66536ef7dfc4111')$image, 0.01, 1)
  p2 <- unwrap_image(load_plot('909a1c6d8e6fc025fe34f8ed33f908889a6a108d')$image, 0.01, 1)

  o <- crop.bbox(o, bbox(o < .95))
  p1 <- crop.bbox(p1, bbox(p1 < .95))
  p2 <- crop.bbox(p2, bbox(p2 < .95))

  w <- max(unlist(lapply(list(o, p1, p2), function (p) dim(p)[1])))
  h <- max(unlist(lapply(list(o, p1, p2), function (p) dim(p)[2])))

  imgs <- lapply(list(o, p1, p2), function(p) resize(p, w, h))

  arrays <- lapply(imgs, function (x) isoblur(x, sigma = 5) %>% as.array %>% as.numeric)
  cor(arrays[[1]], arrays[[2]])
  cor(arrays[[1]], arrays[[3]])
}

load_array <- function (path) {
  a <- as.array(squeeze(grayscale(load.image(path))))
  unwrap_array(a, 0.01, 1, missing = 1)
}

if (FALSE) {
  r <- sample_repository()

  o <- load_array("tests/testthat/roc.png")
  p1 <- load_array('work/p1.png')
  p2 <- load_array('work/p2.png')

  cor(as.numeric(o), as.numeric(p1))
}


if (FALSE) {
  r <- sample_repository()

  roc_plot <- load.image("tests/testthat/roc.png")
  plot_1 <- load_plot('0f1105f2e5992669196384b0a66536ef7dfc4111')$image
  plot_2 <- load_plot('909a1c6d8e6fc025fe34f8ed33f908889a6a108d')$image

  # 0.156828
  compare_plots(roc_plot, plot_1)
  # 0.1568029
  compare_plots(roc_plot, plot_2)

  display(roc_plot)
  display(plot_1)
  display(plot_2)
}

# dim(x)[1], dim(x)[2]
load_plot <- function (art_id, svg = FALSE, width = NULL, height = NULL) {
  d <- as_artifacts(r) %>% filter(id == art_id) %>% read_artifacts %>% first %>% artifact_data
  p <- tempfile(fileext = '.png')

  if (svg) {
    rsvg::rsvg_png(base64_dec(d$svg), p, width, height)
  } else {
    f <- file(p, 'wb')
    writeBin(base64_dec(d$png), f)
    close(f)
  }

  list(image = load.image(p), path = p)
}

compare_plots <- function (p1, p2) {
  width <- max(dim(p1)[1], dim(p2)[1])
  height <- max(dim(p1)[2], dim(p2)[2])

  p1 <- resize(p1, width, height)
  p2 <- resize(p2, width, height)

  sqrt(mean(as.numeric(as.array(p1) - as.array(p2))**2))
}


as_polar <- function (im) {
  width <- dim(im)[1]
  height <- dim(im)[2]

  R <- as.integer(max(width, height)/2)
  polar <- imfill(360, R, val = 0)

  im <- grayscale(im)
  for (alpha in seq(1, 360)) {
    cos_alpha <- cos(alpha/180*pi)
    sin_alpha <- sin(alpha/180*pi)
    for (r in seq(R)) {
      x <- as.integer(width/2 + cos_alpha * r)
      y <- as.integer(height/2 + sin_alpha * r)
      if (x > 0 && x <= width && y > 0 && y <= height) {
        #cat(glue("{alpha},{r} <- {x},{y}"), '\n')
        at(polar, alpha, r) <- at(im, x, y)
      }
    }
    cat(glue("{alpha}\n"), '\n')
  }

  polar
}

if (FALSE) {
  roc_plot <- load.image("tests/testthat/roc.png")
  plot_1 <- load_plot('0f1105f2e5992669196384b0a66536ef7dfc4111')$image
  plot_2 <- load_plot('909a1c6d8e6fc025fe34f8ed33f908889a6a108d')$image

  roc_plot <- grayscale(roc_plot)
  plot_1 <- grayscale(plot_1)
  plot_2 <- grayscale(plot_2)

  edge_roc <- cannyEdges(roc_plot)
  edge_plot1 <- cannyEdges(plot_1)
  edge_plot2 <- cannyEdges(plot_2)

  lev_roc <- roc_plot < .95
  lev_plot_1 <- plot_1 < .95
  lev_plot_2 <- plot_2 < .95
  #  plot(roc_plot)
#  highlight(edge_roc)

  bound_roc <- boundary(edge_roc)
  bound_plot1 <- boundary(edge_plot1)
  bound_plot2 <- boundary(edge_plot2)

  polar_roc <- as_polar(bound_roc)
  polar_plot1 <- as_polar(bound_plot1)
  polar_plot2 <- as_polar(bound_plot2)

  lev_polar_roc <- as_polar(lev_roc)
  lev_polar_plot1 <- as_polar(lev_plot_1)
  lev_polar_plot2 <- as_polar(lev_plot_2)

  display(polar_roc)
  display(polar_plot1)
  display(polar_plot2)

  a <- as.array(squeeze(polar_roc))
  p1 <- as.array(squeeze(polar_plot1))
  p2 <- as.array(squeeze(polar_plot2))

  compare_cor(a, p1)
  compare_cor(a, p2)

  compare_cor(as.array(squeeze(lev_polar_roc)), as.array(squeeze(lev_polar_plot1)))
  compare_cor(as.array(squeeze(lev_polar_roc)), as.array(squeeze(lev_polar_plot2)))
}

compare_cor <- function (a, b) {
  vec <- lapply(seq(360), function (alpha) {
    x <- which(a[alpha,] > 0)/(dim(a)[2])
    y <- which(b[alpha,] > 0)/(dim(b)[2])
    if (length(x) < length(y)) {
      x <- c(x, rep(NA, length(y) - length(x)))
    }
    else if (length(x) > length(y)) {
      y <- c(y, rep(NA, length(x) - length(y)))
    }
    list(x = x, y = y)
  })

  x <- unlist(lapply(vec, `[[`, i = 'x'))
  y <- unlist(lapply(vec, `[[`, i = 'y'))

  list(
    cor(x, y, use = "pairwise.complete.obs"),
    cor(x, y, use = "pairwise.complete.obs", method = "spearman")
  )
}

