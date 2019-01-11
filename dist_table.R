source("convex_hull.R")
source("clusterify_cities.R")

min_dist_pair <- function(points1, points2) {
  n <- dim(points1)[1]
  m <- dim(points2)[1]
  x <- rep(1:n, m)
  y <- rep(1:m, rep(n, m))
  idx <- which.min(rowSums(((points1[x,] - points2[y,])^2)))
  min_i <- ((idx - 1) %% n) + 1
  min_j <- floor((idx - 1) / n) + 1

  return (c(min_i, min_j))
}

calculate_convex_hulls <- function(points, clusters) {
  x <- list()
  for(i in 1:max(clusters)) {
    x[[i]] <- calculate_convex_hull(points[clusters == i,])
  }
  
  return (x)
}

create_cluster_dist_table <- function(points, clusters) {
  convex_hulls <- calculate_convex_hulls(points, clusters)
  
  n_clusters <- max(clusters)
  rv <- array(rep(NaN, n_clusters*n_clusters*4), c(n_clusters, n_clusters, 4))
  for(i in 1:n_clusters) {
    for(j in i:n_clusters) {
      if(i == j) next
      cluster_i_ch <- convex_hulls[[i]]
      cluster_j_ch <- convex_hulls[[j]]
      x <- min_dist_pair(cluster_i_ch, cluster_j_ch)
      p1 <- cluster_i_ch[x[1],]
      p2 <- cluster_j_ch[x[2],]
      rv[i,j,] <- c(p1,p2)
    }
    print(i/n_clusters * 100)
  }

  return (rv)
}
