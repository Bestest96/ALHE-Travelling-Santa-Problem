if (!require("dequer")) {
  print("dequer not found! Installing.")
  install.packages("dequer")
  if (!require("dequer")) {
    stop("Cannot install ")
  }
}

x_axis_angle <- function(vs) {
  p1s.x <- vs[,1]
  p1s.y <- vs[,2]
  p2s.x <- vs[,3]
  p2s.y <- vs[,4]
  x <- p2s.x - p1s.x
  y <- p2s.y - p1s.y
  atan2(y, x)
}

squared_modulus <- function(vs) {
  p1s.x <- vs[,1]
  p1s.y <- vs[,2]
  p2s.x <- vs[,3]
  p2s.y <- vs[,4]
  x <- p2s.x - p1s.x
  y <- p2s.y - p1s.y
  x*x + y*y
}

angle_sort <- function(points, O) {
  size <- dim(points)[1]
  vs <- c(rep(O[1], size), rep(O[2], size))
  vs <- c(vs, points[,1], points[,2])
  vs <- matrix(vs, nrow=size)
  dists <- squared_modulus(vs)
  dist_idxs <- order(dists)
  vs_by_dist <- vs[dist_idxs,]
  angles <- x_axis_angle(vs_by_dist)
  angles_idxs <- order(angles)
  idxs <- dist_idxs[angles_idxs]
  
  return (idxs)
}

find_min_point_idx <- function(points) {
  return (order(points[,2], points[,1])[1])
}

peek_first <- function(s) {
  rv <- pop(s)
  push(s, rv)
  
  return (rv)
}

peek_second <- function(s) {
  v1 <- pop(s)
  v2 <- pop(s)
  push(s, v2)
  push(s, v1)
  
  return (v2)
}

counterclockwise_check <- function(p1, p2, p3) {
  return ((p2[2] - p1[2]) * (p3[1] - p2[1]) - (p2[1] - p1[1]) * (p3[2] - p2[2]))
}

calculate_convex_hull <- function(points) {
  min_point_idx <- find_min_point_idx(points)
  min_point <- points[min_point_idx,]
  points <- points[-min_point_idx,]
  
  O <- c(min_point[[1]], min_point[[2]])
  points <- points[angle_sort(points, O),]

  convex_hull <- stack()
  push(convex_hull, O)
  push(convex_hull, c(points[1,1], points[1,2]))
  push(convex_hull, c(points[2,1], points[2,2]))

  for(i in 3:dim(points)[1]) {
    point <- c(points[i,1], points[i,2])
    while(counterclockwise_check(peek_second(convex_hull),
                                 peek_first(convex_hull),
                                 point) > 0) {
      pop(convex_hull)
    }

    push(convex_hull, point)
  }

  rv <- unlist(as.list(convex_hull))
  rv <- matrix(rv, ncol = 2, byrow = TRUE)

  return (rv)
}

calculate_convex_hull_2 <- function(points) {
  min_point_idx <- find_min_point_idx(points)
  min_point <- points[min_point_idx,]
  points <- points[-min_point_idx,]
  
  O <- c(min_point[[1]], min_point[[2]])
  points <- points[angle_sort(points, O),]
  
  convex_hull <- stack()
  push(convex_hull, O)
  push(convex_hull, c(points[1,1], points[1,2]))
  push(convex_hull, c(points[2,1], points[2,2]))
  
  for(i in 3:dim(points)[1]) {
    point <- c(points[i,1], points[i,2])
    while(counterclockwise_check(peek_second(convex_hull),
                                 peek_first(convex_hull),
                                 point) > 0) {
      pop(convex_hull)
    }
    
    push(convex_hull, point)
  }
  
  rv <- unlist(as.list(convex_hull))
  rv <- matrix(rv, ncol = 2, byrow = TRUE)
  
  return (rv)
}
