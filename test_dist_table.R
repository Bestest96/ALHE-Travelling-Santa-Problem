source("load_cities.R")
source("dist_table.R")

.clusters <- NULL
.dt <- NULL

test_dist_table_with_brute <- function(n = 4) {
  if(is.null(.clusters)) .clusters <<- clusterify_cities(300, 9.3)[[3]]
  if(is.null(.dt)) .dt <<- create_cluster_dist_table(cities, .clusters)
  n_clusters <- max(.clusters)
  to_plot <- sample(1:n_clusters, n)
  
  plot(cities, col = "white")
  for (i in to_plot) {
    points(cities[.clusters==i,], pch = 19, col = colors[i])
  }
  n_step <- (length(to_plot) * (length(to_plot) - 1))/2
  step <- 0
  for(i in to_plot) {
    cluster_i <- cities[.clusters==i,]
    for(j in to_plot) {
      if(i >= j) next

      segments(.dt[i,j,1,1], .dt[i,j,1,2], .dt[i,j,1,3], .dt[i,j,1,4], col = "orange", lwd = 1)
      
      cluster_j <- cities[.clusters==j,]
      min_pair <- min_dist_pair(cluster_i, cluster_j)
      x1 <- cluster_i[min_pair[1],]
      x2 <- cluster_j[min_pair[2],]
      segments(x1[[1]], x1[[2]], x2[[1]], x2[[2]], col = "blue", lwd = 1)
      
      step <- step + 1
      print.debug(100*step/n_step)
    }
  }
  
  convex_hulls <- calculate_convex_hulls(cities, .clusters)
  for(i in to_plot) {
    points(convex_hulls[[i]], col = "yellow")
  }
}

test_dist_table_pair <- function(n = 4) {
  if(is.null(.clusters)) .clusters <<- clusterify_cities(300, 9.3)[[3]]
  if(is.null(.dt)) .dt <<- create_cluster_dist_table(cities, .clusters)
  n_clusters <- max(.clusters)
  to_plot <- sample(1:n_clusters, n)
  
  plot(cities, col = "white")
  for (i in to_plot) {
    points(cities[.clusters==i,], pch = 19, col = colors[i])
  }
  n_step <- (length(to_plot) * (length(to_plot) - 1))/2
  step <- 0
  for(i in to_plot) {
    cluster_i <- cities[.clusters==i,]
    for(j in to_plot) {
      if(i >= j) next
      
      segments(.dt[i,j,1,1], .dt[i,j,1,2], .dt[i,j,1,3], .dt[i,j,1,4], col = "orange", lwd = 1)
      segments(.dt[i,j,2,1], .dt[i,j,2,2], .dt[i,j,2,3], .dt[i,j,2,4], col = "red", lwd = 1)
      
      step <- step + 1
      print.debug(100*step/n_step)
    }
  }
  
  convex_hulls <- calculate_convex_hulls(cities, .clusters)
  for(i in to_plot) {
    points(convex_hulls[[i]], col = "yellow")
  }
}