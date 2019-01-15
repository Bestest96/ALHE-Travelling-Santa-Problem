source("NN.R")
source("load_cities.R")

random_walk <- function(clusters, first_cluster, dt, iterations = 200) {
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