source("load_cities.R")

NEIGH_TO_CHECK <- 10

find.city <- function(x, y) {
  for (i in 1:dim(cities)[1]) {
    if (isTRUE(all.equal(cities[i,][[1]], x)) && isTRUE(all.equal(cities[i,][[2]], y)))
      return (i - 1)
  }
  return (NULL)
}

NN <- function(clusters, c.order, dt) {
  order <- integer(length(clusters) + 1)
  order[1] <- 0
  order[length(order)] <- 0
  counter <- 2
  start.city <- 0
  end.city <- find.city(dt[c.order[1], c.order[2],][[1]], dt[c.order[1], c.order[2],][[2]]) # p1.x p1.y 
  print(end.city)
  i <- 1
  while (counter < length(order)) {
    to_check <- setdiff(which(clusters == c.order[i]) - 1, c(order, start.city, end.city))
    if (length(to_check) > NEIGH_TO_CHECK) {
      to_check <- sample(to_check, NEIGH_TO_CHECK)
    }
    if (length(to_check) == 0) {
      order[counter] <- end.city
      start.city <- find.city(dt[c.order[i], c.order[i + 1],][[3]], dt[c.order[i], c.order[i + 1],][[4]])
      order[counter + 1] <- start.city
      end.city <- find.city(dt[c.order[i + 1], c.order[i + 2],][[1]], dt[c.order[i + 1], c.order[i + 2],][[2]])
      i <- i + 1
      counter <- counter + 2
      print(end.city)
      next
    }
    # [, , , , ,]
    min_dist <- Inf
    min_neigh <- NA
    x1 <- cities[order[counter - 1] + 1, 'X']
    y1 <- cities[order[counter - 1] + 1, 'Y']
    x2 <- cities[to_check + 1, 'X']
    y2 <- cities[to_check + 1, 'Y']
    dists <- dist_cities(x1, y1, x2, y2)
    if ((counter - 1) %% 10 == 0) {
      check_dists <- ifelse(to_check %in% city_primes, dists, 1.1 * dists)
      dists <- check_dists
    }
    else
      check_dists <- ifelse(to_check %in% city_primes, 1.1 * dists, dists)
    min_indx <- which.min(check_dists)
    min_dist <- dists[min_indx]
    min_neigh <- to_check[min_indx]
    order[counter] <- min_neigh
    counter <- counter + 1
    cat("\r", counter)
  }
  
  return (order)
}