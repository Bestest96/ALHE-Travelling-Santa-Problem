if (!require("sfsmisc")) {
  print("Sfsmisc not found! Installing.")
  install.packages("sfsmisc")
  if (!require("sfsmisc")) {
    stop("Cannot install ")
  }
}

source("colors.R")

printf <- function(...) invisible(print(sprintf(...)))

dist_cities <- function(x1, y1, x2, y2) {
  dist <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
  
  return (dist)
}

manhattan_metric <- function(x1, y1, x2, y2) {
  dist <- abs(x2 - x1) + abs(y2 - y1)
  
  return (dist)
}

max_metric <- function(x1, y1, x2, y2) {
  xs <- abs(x2 - x1)
  ys <- abs(y2 - y1)
  dist <- cbind(xs, ys)
  dist <- apply(dist, 1, function(x) max(x))
  
  return (dist)
}

clusterify <- function(X, radius, metric = dist_cities) {
  n_cluster <- 1
  clusters <- vector(length=nrow(X))
  remaining_idxs <- 1:nrow(X)
  n <- 0
  while(length(remaining_idxs) > 0) {
    s <- sample(length(remaining_idxs), 1)
    idx <- remaining_idxs[s]
    core <- X[idx,]
    dists <- metric(cities[remaining_idxs,1], 
                    cities[remaining_idxs,2], 
                    core[[1]], 
                    core[[2]])
    if(length(which(dists <= radius)) == 1) {
      clusters[s] <- 0 # noise
      n <- n + 1
    }
    else {
      clusters[remaining_idxs][dists <= radius] <- n_cluster 
      n_cluster <- n_cluster + 1
    }
    remaining_idxs <- remaining_idxs[dists > radius]
  }
  
  return (clusters)
}

"
Removes n furthest points from a cluster.
"
trim_cluster <- function(n, 
                         group, 
                         points, 
                         dists, 
                         inverted = FALSE, 
                         return_dists = FALSE){
  if(n > length(points)) stop("n can't be greater than length of points.")
  sorted_by_dist_idxs <- order(dists, decreasing = TRUE)
  to_remove <- c()
  for(idx in sorted_by_dist_idxs) {
    if(n == 0) break
    point <- points[idx]
    if((!inverted && point %in% group) || (inverted && !(point %in% group))){
      n <- n - 1
      to_remove <- c(to_remove, idx)
    }
  }
  
  if(return_dists){
    if(is.null(to_remove)) return (list(points, dists))
    return (list(points[-to_remove], dists[-to_remove]))
  }
  if(is.null(to_remove)) return (points)
  return (points[-to_remove])
}

"
Clusterifies points from X.

Args:
  X: A list of points. 
  radius: Base radius of a cluster.
  metric: Metric to calculate distance between points.
  attractors: A list of indices of points from X
  on which clusters would be concentrated.
  per_attractor: Number of normal points per one attractor.

Returns:
  A list of length equal number of points containing
  integers which indicate cluster membership of points.
"
clusterify_around_attractors <- function(X, 
                                         radius,
                                         attractors,
                                         per_attractor,
                                         metric = dist_cities) {
  n_cluster <- 1
  clusters <- vector(length=nrow(X))
  remaining_idxs <- 1:nrow(X)
  remaining_attractors <- attractors
  total_attractors <- length(attractors)
  not.changed <- 0
  not.changed.max <- 100
  while(length(remaining_attractors) > 0 && not.changed < not.changed.max) {
    old.length <- length(remaining_attractors)
    s <- sample(length(remaining_attractors), 1)
    idx <- remaining_attractors[s]
    core <- X[idx,]
    dists <- metric(cities[remaining_idxs,1], 
                    cities[remaining_idxs,2], 
                    core[[1]], 
                    core[[2]])
    
    neighbours_idxs <- remaining_idxs[dists <= radius]
    n_attractors <- length(intersect(neighbours_idxs, attractors))
    neighbourhood_size <- length(neighbours_idxs)
    n_normal_points <- neighbourhood_size - n_attractors
    neighbours_dists <- dists[dists <= radius]
    
    if(n_normal_points - per_attractor * n_attractors >= 0) {
      # removing normal points
      inverted <- TRUE
      n_remove <- n_normal_points - as.integer(per_attractor * n_attractors)
    }
    else {
      # removing attractors
      inverted <- FALSE
      n_remove <- n_attractors - as.integer(n_normal_points / per_attractor)
    }
    neighbours_idxs <- trim_cluster(n_remove,
                                    attractors,
                                    neighbours_idxs,
                                    neighbours_dists,
                                    inverted = inverted)
    
    clusters[neighbours_idxs] <- n_cluster
    n_cluster <- n_cluster +  1
    
    cluster_attractors <- intersect(neighbours_idxs, attractors)
    remaining_attractors <- setdiff(remaining_attractors, cluster_attractors)
    n_attractors <- length(cluster_attractors)
    neighbourhood_size <- length(neighbours_idxs)
    n_normal_points <- neighbourhood_size - n_attractors
    remaining_idxs <- setdiff(remaining_idxs, neighbours_idxs)
    printf("total_attractors = %d, attractors = %d, normal points = %d", 
           length(remaining_attractors),
           n_attractors, 
           n_normal_points)
    if (length(remaining_attractors) - old.length == 0)
      not.changed <- not.changed + 1
    else
      not.changed <- 0
  }
  if (not.changed == 0)
    remaining_idxs <- union(remaining_idxs, remaining_attractors)
  clusters[remaining_idxs] <- 0
  
  print(length(remaining_idxs))
  
  return (list(clusters, length(remaining_idxs)))
}

cities <- read.csv("cities.csv", header = T)
cities <- cities[,2:3]
city_primes <- primes(dim(cities)[1] - 1)

clusterify_cities <- function(radius = seq(250, 550, 25), 
                              per_attractor = seq(7, 11, 0.1), 
                              tries = 5,
                              plot = TRUE, 
                              metric = dist_cities) {
  for (r in radius) {
    for (p in p.a) {
      for (i in 1:tries) {
        rv <- clusterify_around_attractors(cities, 
                                           r, 
                                           city_primes, 
                                           p,
                                           metric = metric)
        clusters <- rv[[1]]
        remaining_idxs <- rv[[2]]
        if(plot){
          png(sprintf("plots/%d_%.1f_%d_%d.png", r, p, i, remaining_idxs), 
              width = 1200, 
              height = 800)
          plot(cities)
          count <- 1
          for (i in unique(clusters)) {
            if(i == 0) {
              points(cities[clusters==0,], pch = 3, col = "black") 
              next
            }
            points(cities[clusters==i,], pch = 3, col = colors[count])
            count <- count + 1
          }
          dev.off()
        }  
      }
    }
  }
}
