source("load_cities.R")

NEIGH_TO_CHECK <- 10000000

find.city <- function(x, y, eps = 1e-30) {
  toRet <- intersect(which(abs(cities[,][[1]] - x) <= eps), which(abs(cities[,][[2]] - y) <= eps)) - 1
  if (length(toRet) == 1)
    return (toRet)
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
  cat ("\n")
  return (order)
}

SA <- function (clusters, c.order, dt, temp = 100, t.eps = 0.99999, iterations = 10000) {
  best.order <<- NN(clusters, c.order, dt)
  best.corder <<- c.order
  best.length <<- pathLength(best.order, cities, city_primes)$length
  cur.length <- best.length
  cur.order <- best.order
  for (i in 1:iterations) {
    neighs <- sample(2:(length(c.order) - 1), 2)
    new.corder <- c.order
    tmp <- new.corder[neighs[1]]
    new.corder[neighs[1]] <- new.corder[neighs[2]]
    new.corder[neighs[2]] <- tmp
    new.order <- NN(clusters, new.corder, dt)
    path <- pathLength(new.order, cities, city_primes)
    if (path$length < cur.length || runif(1, min = 0, max = 1) < exp(-abs(path$length - cur.length) /  temp)) {
      cur.length <- path$length
      c.order <- new.corder
      cur.order <- new.order
      if (path$length < best.length) {
        best.length <<- path$length
        best.corder <<- new.corder
        best.order <<- new.order
      }
    }
    temp <- temp * t.eps
    print.info(c(path$length, cur.length, best.length, temp), logger = "logs/SA.log")
  }
  return (list(order = best.order, corder = best.corder, length = best.length))
}

create_initial_corder <- function(clusters, dt) {
  c.order <- prepare_corder(clusters)
  c.order <- cluster_NN(c.order, clusters, dt)
  
  return (c.order)
}

prepare_corder <- function(clusters) {
  city0_cluster <- clusters[1]
  dists <- dist_cities(cities[1,1], cities[1,2], cities[,1], cities[,2])
  closest_city_idx <- intersect(order(dists), which(clusters != city0_cluster))[1]
  closest_city_cluster <- clusters[closest_city_idx]
  c.order <- 1:max(clusters)
  c.order[1] <- city0_cluster
  c.order[city0_cluster] <- 1
  c.order[max(clusters)] <- closest_city_cluster
  c.order[closest_city_cluster] <- max(clusters)
  
  return (c.order)
}

cluster_NN <- function(initial_corder, clusters, dt) {
  c.order <- c()
  c.order[1] <- initial_corder[1]
  c.order[length(initial_corder)] <- initial_corder[length(initial_corder)]
  counter <- 2
  to_check <- initial_corder[2:(length(initial_corder)-1)]
  while(counter <= length(initial_corder)-1) {
    if(length(to_check) == 1) {
      c.order[counter] <- to_check
      break
    }
    pairs <- dt[c.order[counter-1],to_check,1,]
    dists <- dist_cities(pairs[,1], pairs[,2], pairs[,3], pairs[,4])
    closest_cluster <- to_check[order(dists)[1]]
    c.order[counter] <- closest_cluster
    to_check <- setdiff(to_check, closest_cluster)
    counter <- counter + 1
  }
  
  return (c.order)
}