source("NN.R")
source("load_cities.R")

cluster_random_walk <- function(clusters, first_cluster, dt, iterations = 200) {
  best_length <<- Inf
  best_corder <<- NULL
  best_order <<- NULL
  counter <- 1
  while(counter <= iterations) {
    new_corder <- c(first_cluster, setdiff(sample(max(clusters)), first_cluster))
    new_order <- NN(clusters, new_corder, dt)
    new_length <- pathLength(new_order, cities, city_primes)$length
    if(new_length < best_length) {
      best_length <<- new_length
      best_corder <<- new_corder
      best_order <<- new_order
    }
    print.info(c(new_length, best_length))
    counter <- counter + 1
  }
  
  return (list(order = best_order, corder = best_corder, length = best_length))
}

random_walk <- function(order) {
  path_obj <- pathLength(order)
  best_order <<- order
  best_length <<- path_obj$length
  cur_order <<- best_order
  cur_paths <<- path_obj$paths
  cur_length <<- best_length
  to_sample <- 2:(length(order)-1)
  i <- 1
  while(T) {
    to_change <- sample(to_sample, 2)
    path_obj <- pathLength(cur_order, toChange = to_change, old_length = cur_length, old_paths = cur_paths)
    cur_order[to_change] <- cur_order[rev(to_change)]
    cur_paths <- path_obj$paths
    cur_length <- path_obj$length
    if(cur_length < best_length) {
      best_length <<- new_length
      best_order <<- new_order
      
      print.info(c("Iteration =", i, "best length = ", best_length))
    }
    
    cat("\r", i, cur_length)
    i <- i + 1
  }
}
