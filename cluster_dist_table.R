source("convex_hull.R")

min_dist_pair <- function(points1, points2) {
  n <- dim(points1)[1]
  m <- dim(points2)[1]
  x <- rep(1:n, m)
  y <- rep(1:m, rep(n, m))
  idx <- which.min(rowSums(((points1[x,] - points2[y,])^2)))
  min_i <- ((idx - 1) %% n) + 1
  min_j <- floor((idx - 1) / n) + 1
  
  return (c(min_i, min_j))
  
  # min_dist <- sum((points1[1,] - points2[1,])^2)
  # min_i <- 1
  # min_j <- 1
  # for(i in 1:dim(points1)[1]) {
  #   for(j in 1:dim(points2)[1]) {
  #     dist <- sum((points1[i,] - points2[j,])^2)
  #       if(dist < min_dist) {
  #         min_dist <- dist
  #         min_i <- i
  #         min_j <- j
  #       }
  #   }
  # }
  
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

test_cluster_dist_table <- function(clusters, x, to_plot, id = 1, points = cities) {
  n_clusters <- dim(x)[1]
  
  name <- paste("cluster_dist_table_", id, ".png", sep = "")
  png(name, width = 1200, height = 800)
  plot(points, col = "white")
  for (i in 1:n_clusters) {
    points(points[clusters==i,], pch = 19, col = colors[i])
  }
  for(i in to_plot) {
    for(j in to_plot) {
      segments(x[i,j,1], x[i,j,2], x[i,j,3], x[i,j,4], col = "red", lwd = 1)
    }
  }
  dev.off()
  
  name <- paste("cluster_dist_table_", id, "_ref.png", sep = "")
  png(name, width = 1200, height = 800)
  plot(points, col = "white")
  for (i in 1:n_clusters) {
    points(points[clusters==i,], pch = 19, col = colors[i])
  }
  for(i in to_plot) {
    cluster_i <- points[clusters==i,]
    for(j in to_plot) {
      print(j)
      cluster_j <- points[clusters==j,]
      min_pair <- min_dist_pair(cluster_i, cluster_j)
      x1 <- points[min_pair[1]]
      x2 <- points[min_pair[2]]
      segments(x1[1], x1[2], x2[1], x2[2], col = "blue", lwd = 1)
    }
  }
  dev.off()
}
