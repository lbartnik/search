x <- "iris %>% mutate(is_virgninica = Species == 'virginica') %>% group_by(species) %>% summarize(n = n())"

y <- "iris %>% mutate(is_virgninica = Species == 'virginica') %>% group_by(Sepal.Width) %>% summarize(n = n())"

edit_dist(tokenize(x), tokenize(y))


edit_dist(tokenize("x <- 1"), tokenize("y <- 1"))

edit_dist_impl("a", "b")

