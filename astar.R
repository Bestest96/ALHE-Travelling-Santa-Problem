if (!require("datastructures")) {
  print("datastructures not found! Installing.")
  install.packages("datastructures")
  if (!require("datastructures")) {
    stop("Cannot install ")
  }
}

dist_by_id <- function(V, x) {
  dist_cities(cities[V,1], cities[V,2], cities[x,1], cities[x,2])
}

# ITERATE FROM 1
astar <- function(start, end, V) {
  heap <- binomial_heap("numeric")
  h_value <- sum(dist_by_id(V, end))
  insert(heap, h_value, list(0, h_value, c(start)))
  V_size <- length(V)
  while(size(heap) > 0) {
    x <- pop(heap)[[1]]
    curr_cost <- x[[1]]
    curr_h_value <- x[[2]]
    curr_path <- x[[3]]
    curr_size <- length(curr_path)
    curr_v <- curr_path[[curr_size]]
    printf("Visiting: %d", curr_v)
    if(curr_size == V_size) return (list(curr_cost, curr_path))
    for(neighbour in setdiff(V, curr_path)) {
      if(neighbour == end && curr_size != V_size - 1) next
      new_cost <- curr_cost + dist_by_id(curr_v, neighbour)
      new_h <- if (neighbour == end) 0.0 else curr_h_value
      # new_h <- curr_h_value - dist_by_id(curr_v, end)
      new_path <- c(curr_path, neighbour)
      insert(heap, new_cost + new_h, list(new_cost, new_h, new_path))
    }
  }
}

x <- astar(1, 25, 1:25)
path <- x[[2]]

plot(cities[path,1], cities[path,2], xlim=c(0,5000), ylim=c(0,5000))
text(cities[path,1], cities[path,2], path)
for(i in 1:(length(path)-1)) {
  segments(cities[path[i],1], cities[path[i],2], cities[path[i+1],1], cities[path[i+1],2])
}
