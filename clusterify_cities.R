source("clustering.R")
source("load_cities.R")

clusterify_cities <- function(radius,
                              per_attractor,
                              metric = dist_cities,
                              post_clusterify = TRUE) {
  clusterify(cities, radius, per_attractor, city_primes, metric, post_clusterify)
}

clusterify_and_plot_cities <- function(radius,
                                       per_attractor,
                                       post_clusterify = TRUE,
                                       metric = dist_cities,
                                       directory = "plots/",
                                       id = 1,
                                       width = 1200,
                                       height = 800,
                                       on_screen = FALSE) {
  clusterify_and_plot(cities,
                      radius,
                      per_attractor,
                      city_primes,
                      post_clusterify = post_clusterify,
                      metric = metric,
                      directory = directory,
                      id = id,
                      width = width,
                      height = height,
                      on_screen = on_screen)
}

tune_clusterify_cities <- function(radius = seq(250, 500, 25), 
                                   per_attractor = seq(7, 11, 0.1),
                                   tries = 5,
                                   metric = dist_cities,
                                   post_clusterify = TRUE,
                                   plot = TRUE,
                                   directory = 'plots/',
                                   width = 1200,
                                   height = 800) {
  tune_clusterify(cities,
                  radius, 
                  per_attractor,
                  city_primes,
                  tries = 5,
                  metric = metric,
                  post_clusterify = post_clusterify,
                  plot = plot,
                  directory = directory,
                  width = width,
                  height = height)
}
