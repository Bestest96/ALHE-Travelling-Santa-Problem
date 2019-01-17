if (!require("sfsmisc")) {
  print("Sfsmisc not found! Installing.")
  install.packages("sfsmisc")
  if (!require("sfsmisc")) {
    stop("Cannot install ")
  }
}

DEBUG <- FALSE

cities <- read.csv("data/cities.csv", header = T)
cities <- cities[,2:3]
city_primes <- primes(dim(cities)[1] - 1)

dist_cities <- function(x1, y1, x2, y2) {
  dist <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
  return (dist)
}

pathLength <- function(order, c = cities, cp = city_primes) {
  x1 <- order[1:(length(order) - 1)]
  x2 <- order[2:length(order)]
  paths <- dist_cities(c[x1 + 1, 'X'],
                       c[x1 + 1, 'Y'],
                       c[x2 + 1, 'X'],
                       c[x2 + 1, 'Y'])
  inds.to.penalize <- x1[!(x1 %in% cp)]
  inds.to.penalize <- inds.to.penalize[inds.to.penalize %% 10 == 0]
  paths[inds.to.penalize] <- 1.1 * paths[inds.to.penalize]
  len <- sum(paths)
  return (list("length" = len, "paths" = paths))
}

print.info <- function(params, logger = NULL) {
  write(paste(params, collapse = '\t'), stdout(), sep = '\t')
  if (!is.null(logger))
    write(paste(params, collapse = '\t'), logger, sep = '\t', append = T)
}

print.debug <- function(...) if(DEBUG) cat(...)
