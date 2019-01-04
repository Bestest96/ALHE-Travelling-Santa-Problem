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

cities <- read.csv("cities.csv", header = T)
cities <- cities[,2:3]

clusters <- clusterify(cities, 600, max_metric)

plot(cities, col=clusters)
points(cities[clusters==0,], pch = 3, col = "grey")
