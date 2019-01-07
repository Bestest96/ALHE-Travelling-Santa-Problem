source("clusterify_cities.R")
source("convex_hull.R")

.clusters <- NULL

test_convex_hull <- function(clusters = .clusters, 
                             id = "random", 
                             points = cities) {
  if(is.null(clusters)) {
    .clusters <<- clusterify_cities(300, 9.3)[[3]]
    clusters <- .clusters
  }
  if(id == "random") id <- sample(1:max(clusters), 1)
  
  cluster_points <- points[clusters == id,]
  convex_hull <- calculate_convex_hull(cluster_points)
  
  plot(cluster_points, t = "p", col = "black")
  points(convex_hull, pch = 19, col = "red")
}
