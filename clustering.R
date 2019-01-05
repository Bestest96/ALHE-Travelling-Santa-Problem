if (!require("sfsmisc")) {
  print("Sfsmisc not found! Installing.")
  install.packages("sfsmisc")
  if (!require("sfsmisc")) {
    stop("Cannot install ")
  }
}

colors <- c("#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
            "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
            "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
            "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
            "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
            "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
            "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
            "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
            
            "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
            "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
            "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
            "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
            "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C",
            "#83AB58", "#001C1E", "#D1F7CE", "#004B28", "#C8D0F6", "#A3A489", "#806C66", "#222800",
            "#BF5650", "#E83000", "#66796D", "#DA007C", "#FF1A59", "#8ADBB4", "#1E0200", "#5B4E51",
            "#C895C5", "#320033", "#FF6832", "#66E1D3", "#CFCDAC", "#D0AC94", "#7ED379", "#012C58",
            
            "#7A7BFF", "#D68E01", "#353339", "#78AFA1", "#FEB2C6", "#75797C", "#837393", "#943A4D",
            "#B5F4FF", "#D2DCD5", "#9556BD", "#6A714A", "#001325", "#02525F", "#0AA3F7", "#E98176",
            "#DBD5DD", "#5EBCD1", "#3D4F44", "#7E6405", "#02684E", "#962B75", "#8D8546", "#9695C5",
            "#E773CE", "#D86A78", "#3E89BE", "#CA834E", "#518A87", "#5B113C", "#55813B", "#E704C4",
            "#00005F", "#A97399", "#4B8160", "#59738A", "#FF5DA7", "#F7C9BF", "#643127", "#513A01",
            "#6B94AA", "#51A058", "#A45B02", "#1D1702", "#E20027", "#E7AB63", "#4C6001", "#9C6966",
            "#64547B", "#97979E", "#006A66", "#391406", "#F4D749", "#0045D2", "#006C31", "#DDB6D0",
            "#7C6571", "#9FB2A4", "#00D891", "#15A08A", "#BC65E9", "#FFFFFE", "#C6DC99", "#203B3C",
            
            "#671190", "#6B3A64", "#F5E1FF", "#FFA0F2", "#CCAA35", "#374527", "#8BB400", "#797868",
            "#C6005A", "#3B000A", "#C86240", "#29607C", "#402334", "#7D5A44", "#CCB87C", "#B88183",
            "#AA5199", "#B5D6C3", "#A38469", "#9F94F0", "#A74571", "#B894A6", "#71BB8C", "#00B433",
            "#789EC9", "#6D80BA", "#953F00", "#5EFF03", "#E4FFFC", "#1BE177", "#BCB1E5", "#76912F",
            "#003109", "#0060CD", "#D20096", "#895563", "#29201D", "#5B3213", "#A76F42", "#89412E",
            "#1A3A2A", "#494B5A", "#A88C85", "#F4ABAA", "#A3F3AB", "#00C6C8", "#EA8B66", "#958A9F",
            "#BDC9D2", "#9FA064", "#BE4700", "#658188", "#83A485", "#453C23", "#47675D", "#3A3F00",
            "#061203", "#DFFB71", "#868E7E", "#98D058", "#6C8F7D", "#D7BFC2", "#3C3E6E", "#D83D66",
            
            "#2F5D9B", "#6C5E46", "#D25B88", "#5B656C", "#00B57F", "#545C46", "#866097", "#365D25",
            "#252F99", "#00CCFF", "#674E60", "#FC009C", "#92896B")

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
    rv <- list(points[-to_remove], dists[-to_remove])
    return (rv)
  }
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
      #mod <- n_normal_points %% per_attractor
      #n_remove <- n_attractors - (n_normal_points - mod) / per_attractor
      # remving normal points
      # rv <- trim_cluster(mod,
      #                    attractors,
      #                    neighbours_idxs,
      #                    neighbours_dists,
      #                    return_dists = TRUE,
      #                    inverted = TRUE)
      # neighbours_idxs <- rv[[1]]
      # neighbours_dists <- rv[[2]]
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
  
  return (clusters)
}

cities <- read.csv("cities.csv", header = T)
cities <- cities[,2:3]
city_primes <- primes(dim(cities)[1] - 1)

clusterify_cities <- function(radius, 
                              per_attractor, 
                              plot = TRUE, 
                              metric = dist_cities) {
  radius <- seq(250, 550, 25)
  p.a <- seq(7, 11, 0.1)
  tries <- 5
  
  for (r in radius) {
    for (p in p.a) {
      for (i in 1:tries) {
        clusters <<- clusterify_around_attractors(cities, 
                                                  r, 
                                                  city_primes, 
                                                  p,
                                                  metric = metric)
        if(plot){
          png(sprintf("plots/%d_%.1f_%d.png", r, p, i), width = 1200, height = 800)
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
