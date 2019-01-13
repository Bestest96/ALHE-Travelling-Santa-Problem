source("load_cities.R")

NEIGH_TO_CHECK <- 10000000

find.city <- function(x, y, eps = 1e-30) {
  toRet <- intersect(which(abs(cities[,][[1]] - x) <= eps), which(abs(cities[,][[2]] - y) <= eps)) - 1
  if (length(toRet) == 1)
    return (toRet)
  else
    return (NULL)
}

NN <- function(clusters, c.order, dt) {
  order <- integer(length(clusters) + 1)
  order[1] <- 0
  order[length(order)] <- 0
  counter <- 2
  pair <- 1
  change.counter <- 1
  start.city <- 0
  end.city <- find.city(dt[c.order[1], c.order[2], 1,][[1]], dt[c.order[1], c.order[2], 1,][[2]]) # p1.x p1.y 
  if (start.city == end.city) {
    pair <- (change.counter) %% 2 + 1
    change.counter <- change.counter + 1
    end.city <- find.city(dt[c.order[1], c.order[2], pair,][[1]], dt[c.order[1], c.order[2], pair,][[2]])
  }
  print(start.city)
  print(end.city)
  i <- 1
  to_check <- setdiff(which(clusters == c.order[1]) - 1, c(start.city, end.city))
  while (counter < length(order)) {
    if (length(to_check) > NEIGH_TO_CHECK) {
      to_check <- sample(to_check, NEIGH_TO_CHECK)
    }
    if (length(to_check) == 0) {
      order[counter] <- end.city
      start.city <- find.city(dt[c.order[i], c.order[i + 1], pair,][[3]], dt[c.order[i], c.order[i + 1], pair,][[4]])
      end.city <- find.city(dt[c.order[i + 1], c.order[i + 2], pair,][[1]], dt[c.order[i + 1], c.order[i + 2], pair,][[2]])
      if (is.null(end.city))
        end.city = 0
      if (start.city == end.city) {
        pair <- (change.counter) %% 2 + 1
        change.counter <- change.counter + 1
        end.city <- find.city(dt[c.order[i + 1], c.order[i + 2], pair,][[1]], dt[c.order[i + 1], c.order[i + 2], pair,][[2]])
        if (is.null(end.city))
          end.city = 0
      }
      i <- i + 1
      order[counter + 1] <- start.city
      counter <- counter + 2
      to_check <- setdiff(which(clusters == c.order[i]) - 1, c(start.city, end.city))
      print(start.city)
      print(end.city)
      print(corder[i])
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
    to_check <- setdiff(to_check, min_neigh)
    counter <- counter + 1
    cat("\r", counter)
  }
  
  return (order)
}