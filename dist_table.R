source("convex_hull.R")

min_dist_pair <- function(points1, points2) {
  stopifnot(dim(points1)[1] > 1, dim(points2)[1] > 1)
  
  n <- dim(points1)[1]
  m <- dim(points2)[1]
  x <- rep(1:n, m)
  y <- rep(1:m, rep(n, m))
  idxs <- order(rowSums(((points1[x,] - points2[y,])^2)))
  idx1 <- idxs[1]
  i1 <- ((idx1 - 1) %% n) + 1
  j1 <- floor((idx1 - 1) / n) + 1
  for(idx in idxs[-1]) {
    i2 <- ((idx - 1) %% n) + 1
    if(i2 == i1) next
    j2 <- floor((idx - 1) / n) + 1
    break
  }
  
  return (c(i1, j1, i2, j2))
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
  rv <- array(rep(NaN, n_clusters*n_clusters*4*2), c(n_clusters, n_clusters, 2, 4))
  for(i in 1:n_clusters) {
    for(j in 1:n_clusters) {
      if(i == j) next
      cluster_i_ch <- convex_hulls[[i]]
      cluster_j_ch <- convex_hulls[[j]]
      x <- min_dist_pair(cluster_i_ch, cluster_j_ch)
      p1 <- cluster_i_ch[x[1],]
      p2 <- cluster_j_ch[x[2],]
      p1_2 <- cluster_i_ch[x[3],]
      p2_2 <- cluster_j_ch[x[4],]
      rv[i,j,1,] <- c(p1,p2)
      rv[i,j,2,] <- c(p1_2,p2_2)
    }
    print.debug(i/n_clusters * 100)
  }
  
  return (rv)
}
