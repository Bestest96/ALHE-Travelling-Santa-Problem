if (!require("sfsmisc")) {
  print("Sfsmisc not found! Installing.")
  install.packages("sfsmisc")
  if (!require("sfsmisc")) {
    stop("Cannot install ")
  }
}

source("colors.R")
source("load_cities.R")

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

"
Clusterifies set of points from X. 
It pulls points from X as long as there is
a point not processed yet. New clusters are
made around those points including all points
not processed yet and lying not further than 'radius'.

Args:
  X: Points to cluster.
  radius: Radius of creating clusters.
  metric: Metric to use.

Returns:
  Vector of length equal lenght of X. Each point
  has assigned one corresponding integer in this vector
  denoting to which cluster the point belongs. 0 is reserved
  for points which are isolated and don't form clusters 
  with any points. Values i>=1 are assigned to all points
  belonging to the same cluster i.
"
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
Trims the cluster by n furthest points.

Args:
  n: Number of points to remove.
  group: A group of the points. See: inverted.
  points: Defines the cluster.
  dists: Distances from some specific point
    to all the points from the cluster in the
    same order as they appear in 'points'.
  inverted: If false, the function trims n
    furthest points from 'group' which are 
    present in the cluster. Otherwise, it trims
    n furthest points NOT belonging to 'group'.
  return_dists: If true, the function returns also
    the second value which is 'dist' only those
    points which are still in the cluster after trimming.

Returns:
  A vector without n furthest points. Or, if 'return_dists'
  is true, it returns also the second value: 'dists' without
  removed points.
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

clusterify_noise <- function(X, clusters, attractors, metric = dist_cities) {
  new_clusters <- clusters
  X_attractors <- X[attractors,]
  X_clusterified_attractors <- X_attractors[X_attractors != 0,]
  clusters_attractors <- clusters[attractors]
  clusters_clusterified_attractors <- clusters_attractors[X_attractors != 0]
  for(i in 1:length(clusters)) {
    if(clusters[i] != 0) next
    point <- X[i,]
    dists <- metric(point[[1]], point[[2]], 
                    X_clusterified_attractors[,1], X_clusterified_attractors[,2])
    min_idx <- order(dists)[[1]]
    new_clusters[i] <- clusters_clusterified_attractors[[min_idx]]
  }
  
  return (new_clusters)
}

"
Clusterifies points from X.

Args:
  X: A list of points. 
  radius: Base radius of a cluster.
  attractors: A list of indices of points from X,
  on which clusters would be concentrated.
  per_attractor: Number of normal points per one attractor.
  metric: Metric to calculate distance between points.
  post_clusterify: If true, after main clustering phase,
    noise points will be assigned to created clusters.

Returns:
  A list of length equal number of points containing
  integers which indicate cluster membership of points.
"
clusterify_around_attractors <- function(X, 
                                         radius,
                                         attractors,
                                         per_attractor,
                                         metric = dist_cities,
                                         post_clusterify = TRUE) {
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
  
  post_clusters <- NULL
  if(post_clusterify) {
    post_clusters <- clusterify_noise(X, clusters, attractors, metric = metric)
  }
  
  print(length(remaining_idxs))
  
  return (list(clusters, length(remaining_idxs), post_clusters))
}

plot_clusters <- function(X, 
                          y, 
                          params, 
                          width = 1200, 
                          height = 800, 
                          directory = 'plots/') {
  parsed_params <- paste(params, collapse = '_')
  name <- paste(directory, "/", parsed_params, ".png", sep = "")
  png(name, width = width, height = height)
  
  plot(X)
  for (i in unique(y)) {
    if(i == 0) points(X[y==0,], pch = 3, col = "black") 
    points(X[y==i,], pch = 19, col = colors[i])
  }
  dev.off()
}

"
Clusterifies cities using 'clusterifies_around_attractors',
in short CAA.

Args:
  radius: radius parameter to use in CAA. 
  per_attractor: per_attractor parameter to use in CAA.
  plot: If true, plot is made. Plots name follows 
  the naming rule:
    [radius]_[per_attractor]_[image_id]_[noise].png
  metric: metric parameter to use in CAA.
  directory: Directory where a plot will be saved.
  id: Id of a plot.
  width: Width of plot.
  height: Height of plot.

Returns:
  Result from CAA.
"
clusterify_cities <- function(radius,
                              per_attractor,
                              post_clusterify = TRUE,
                              plot = TRUE,
                              metric = dist_cities,
                              directory = "plots/",
                              id = 1,
                              width = 1200,
                              height = 800) {
  rv <- clusterify_around_attractors(cities, 
                                     radius, 
                                     city_primes, 
                                     per_attractor,
                                     metric = metric,
                                     post_clusterify = post_clusterify)
  clusters <- rv[[1]]
  remaining_idxs <- rv[[2]]
  post_clusters <- rv[[3]]
  per_attractor_str <- sprintf("%.1f", per_attractor)
  plot_clusters(cities, 
                clusters, 
                c(radius, per_attractor_str, id, remaining_idxs, "noise"),
                directory = directory)
  if(post_clusterify) {
    plot_clusters(cities,
                  post_clusters,
                  c(radius, per_attractor_str, id, remaining_idxs, "no_noise"),
                  directory = directory)
  }
  
  return (list(clusters, remaining_idxs, post_clusters))
}

"
Launches clusterify_cities for many sets of parameters.

Args:
  radius: Sequence of radius values to use in 
    clusterify_cities.
  per_attractor: Sequence of per_attractor values
    to use in clusterify_cities.
  tries: Number of tries per a set of parameters.
  plot: If true, a plot is creating for each set of
    parameters. Else, the function accumulates all
    results from clusterify_cities in one vector.
  metric: Metric to use in clusterify_cities.

Returns:
  If plot is true, it returns nothing. If not,
  it returns a vector of results from cluserify_cities
  for all sets of parameters.
"
tune_clusterify_cities <- function(radius = seq(250, 550, 25), 
                                   per_attractor = seq(7, 11, 0.1), 
                                   tries = 5,
                                   plot = TRUE, 
                                   metric = dist_cities,
                                   post_clusterify = TRUE) {
  if(!plot) {
    rvs = vector()
    count <- 1
  }
  for (r in radius) {
    for (p in per_attractor) {
      for (i in 1:tries) {
        rv = clusterify_cities(r, p, plot = plot, id = i, post_clusterify = post_clusterify)
        if(!plot) {
          rvs[[count]] <- rv
          count <- count + 1
        }
      }
    }
  }
  
  if(!plot) return (rvs)
}
