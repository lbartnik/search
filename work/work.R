library(jsonlite)
library(repository)
library(utilities)
library(imager)
library(glue)
library(ggplot2)
library(tibble)
devtools::load_all(".")

if (FALSE) {
  r <- sample_repository()
  a <- as_artifacts(r) %>% filter(id == '0f1105f2e5992669196384b0a66536ef7dfc4111') %>% read_artifacts %>% first
  d <- artifact_data(a)
}

if (FALSE) {
  x <- unwrap_image(load.image("tests/testthat/roc.png"), 0.01, 1)

  r <- sample_repository()
  a <- unwrap_image(load_plot(r, '0f1105f2e5992669196384b0a66536ef7dfc4111')$image, 0.01, 1)
  b <- unwrap_image(load_plot(r, '909a1c6d8e6fc025fe34f8ed33f908889a6a108d')$image, 0.01, 1)

  s <- london_meters()
  c <- unwrap_image(load_plot(s, '14e3598b1c58f1f48b74aab35d6c39183568286f')$image, 0.01, 1)
  d <- unwrap_image(load_plot(s, '539d7b916fd845319be242640085b59dbcf52506')$image, 0.01, 1)
  e <- unwrap_image(load_plot(s, '70f3c0a43d894b3cd0d678ef2acbb96f9a5c679e')$image, 0.01, 1)
  f <- unwrap_image(load_plot(s, 'ba5bb30555c5fba63655cb73b5d08bd074997550')$image, 0.01, 1)
  g <- unwrap_image(load_plot(s, 'd0fe8d94550be6fdbe370b88cef5bb7f1f9c4676')$image, 0.01, 1)
  h <- unwrap_image(load_plot(s, 'fb79a00868474d6371201271065cd30877e65d03')$image, 0.01, 1)

  imgs <- list(x, a, b, c, d, e, f, g, h)
  dists <- combn(imgs, 2, function(pair) compare_images(first(pair), second(pair)))
  distm <- matrix(NA, length(imgs), length(imgs))
  apply(rbind(combn(seq(length(imgs)), 2), dists), 2, function (x) {
    distm[x[1], x[2]] <<- x[3]
  })


  compare_imgs(x, a)
  compare_imgs(x, b)
  compare_imgs(x, c)

  as_dist_vec(x, c) %>% as.tibble %>% ggplot(aes(x = a, y = b)) +
    geom_count(aes(alpha = ..prop..))

  edge_diff(x, a)
  edge_diff(x, b)
  edge_diff(x, c)
  edge_diff(x, d)
  edge_diff(x, e)
  edge_diff(x, f)
  edge_diff(x, g)
  edge_diff(x, h)

  edge_diff(d, f)
  edge_diff(d, g)
  edge_diff(c, e)
}

if (FALSE) {
  s <- london_meters()
  as_artifacts(s) %>% filter('plot' %in% class) %>% read_artifacts %>% lapply(function (a) {
    file.copy(load_plot(s, a$id)$path, file.path('work', paste0(storage::shorten(a$id), '.png')))
  })
}


compare_imgs <- function (a, b) {
  dv <- as_dist_vec(a, b)
  cor(dv$a, dv$b, use = 'complete.obs', method = 'spearman')
}

as_dist_vec <- function (a, b) {
  matched <- mapply(a = to_distances(a), b = to_distances(b), FUN = function (a, b) {
    dl <- length(a) - length(b)
    if (dl < 0) {
      a <- c(a, rep(0, -dl))
    } else if (dl > 0) {
      b <- c(b, rep(0, dl))
    }
    list(a = a, b = b)
  }, SIMPLIFY = FALSE)
  a <- unlist(lapply(matched, `[[`, i = 'a'))
  b <- unlist(lapply(matched, `[[`, i = 'b'))
  list(a = a, b = b)
}

to_distances <- function (img, threshold = .95) {
  img <- crop.bbox(img, bbox(img < threshold))
  a <- as.array(img)
  apply(a, 1, function (c) {
    which(c < .95) / length(c)
  })
}


load_plot <- function (repo, art_id, svg = FALSE, width = NULL, height = NULL) {
  d <- as_artifacts(repo) %>% filter(id == art_id) %>% read_artifacts %>% first %>% artifact_data
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



ecdf_diff <- function (a, b) {
  diffs <- mapply(a = to_distances(a), b = to_distances(b), FUN = function (a, b) {
    x <- c(0, seq(min(a, b), max(a, b), length.out = 5))
    dx <- c(diff(x), 1 - max(a, b))
    sum(abs(ecdf(a)(x) - ecdf(b)(x)) * dx)
  }, SIMPLIFY = FALSE)
  sum(unlist(diffs))
}


edge_diff <- function (a, b) {
  to_distances <- function (x) {
    x <- suppressWarnings(imgradient(x, "xy") %>% enorm %>% grayscale %>% squeeze %>% as.array)
    apply(x > quantile(as.numeric(x), .5), 1, function (c) which(c)/length(c))
  }
  diffs <- mapply(a = to_distances(a), b = to_distances(b), FUN = function (a, b) {
    x <- c(0, seq(min(a, b), max(a, b), length.out = length(a)+length(b)))
    dx <- c(diff(x), 1 - max(a, b))
    sum(abs(ecdf(a)(x) - ecdf(b)(x)) * dx)
  }, SIMPLIFY = FALSE)
  sum(unlist(diffs))
}
