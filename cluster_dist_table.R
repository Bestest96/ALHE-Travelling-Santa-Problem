source("convex_hull.R")

min_dist_pair <- function(points1, points2) {
  min_dist <- sum((points1[1,] - points2[1,])^2)
  min_i <- 1
  min_j <- 1
  for(i in 2:dim(points1)[1]) {
    for(j in 1:dim(points2)[1]) {
      dist <- sum((points1[i,] - points2[j,])^2)
        if(dist < min_dist) {
          min_dist <- dist
          min_i <- i
          min_j <- j
        }
    }
  }
  
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

test_cluster_dist_table <- function(points, clusters, x) {
  n_clusters <- dim(x)[1]
  
  plot(points, col = "white")
  for (i in 1:n_clusters) {
    points(points[clusters==i,], pch = 19, col = colors[i])
  }
  
  for(i in 1:n_clusters) {
    for(j in 1:n_clusters) {
      segments(x[i,j,1], x[i,j,2], x[i,j,3], x[i,j,4], col = "red", lwd = 1)
    }
  }
}
