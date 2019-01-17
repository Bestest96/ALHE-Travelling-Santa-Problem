order <- integer(dim(cities)[1] + 1)

checked_neighbours <- length(dim(cities)[1])
path_length <- 0
paths <- double(length(order) - 1)

for (i in 1:(length(order) - 2)) {
  to_check <- setdiff(1:(dim(cities)[1] - 1), order)
  if (length(to_check) > checked_neighbours)
    neighbours <- sample(to_check, checked_neighbours)
  else
    neighbours <- to_check
  min_dist <- Inf
  min_neigh <- NA
  x1 <- cities[order[i] + 1, 'X']
  y1 <- cities[order[i] + 1, 'Y']
  x2 <- cities[neighbours + 1, 'X']
  y2 <- cities[neighbours + 1, 'Y']
  dists <- dist_cities(x1, y1, x2, y2)
  if (i %% 10 == 0) {
    check_dists <- ifelse(neighbours %in% city_primes, dists, 1.1 * dists)
    dists <- check_dists
  }
  else
    check_dists <- ifelse(neighbours %in% city_primes, 1.1 * dists, dists)
  min_indx <- which.min(check_dists)
  min_dist <- dists[min_indx]
  min_neigh <- neighbours[min_indx]
  order[i + 1] <- min_neigh
  path_length <- path_length + min_dist
  paths[i] <- min_dist
  print.debug("\r", i, ",", path_length)
}
last_len <- dist_cities(cities[order[i + 1] + 1, 'X'], cities[order[i + 1] + 1, 'Y'], cities[1, 'X'], cities[1, 'Y'])
paths[i + 1] <- last_len
path_length <- path_length + last_len
