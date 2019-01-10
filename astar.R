if (!require("datastructures")) {
  print("datastructures not found! Installing.")
  install.packages("datastructures")
  if (!require("datastructures")) {
    stop("Cannot install ")
  }
}

dist_by_id <- function(V, x) {
  dist_cities(cities[V], x)
}

# ITERATE FROM 1
astar <- function(start, end, V) {
  n_vertices <- dim(V)[1]
  heap <- binomial_heap("integer")
  h_value <- sum(dist_by_id(V, end))
  insert(heap, h_value, list(0, h_value, c(start)))
  V_size <- length(V)
  while(size(h) > 0) {
    x <- pop(heap)
    curr_cost <- x[[1]]
    curr_path <- x[[3]]
    curr_h_value <- x[[2]]
    curr_v <- curr_path[[length(curr_path)]]
    curr_size <- length(curr_path)
    if(curr_size == V_size) return (curr_cost, curr_path)
    for(neighbour in setdiff(curr_path, V)) {
      if(neighbour == end && curr_size != V_size - 1) next
      new_cost <- curr_cost + dist_by_id(curr_v, neighbour)
      new_h <- curr_h_value - dist_by_id(curr_v, end)
      new_path <- c(curr_path, neighbour)
      insert(heap, new_h, list(new_cost, new_h, new_path))
    }
  }
}
