dist_cities <- function(x1, y1, x2, y2) {
  dist <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
  if (dist < 0 || is.complex(dist))
    stop("Wrong dist!")
  return (dist)
}

pathLength <- function(ord, cities, toChange = NULL, old_length = NULL) {
  len <- 0.0
  if (is.null(toChange)) { 
    for (i in 1:(length(ord) - 1)) {
      len = len +  dist_cities(cities[ord[i + 1], 'x'], cities[ord[i + 1], 'y'], 
                               cities[ord[i], 'x'], cities[ord[i], 'y'])  
    }
  }
  else {
    if (is.null(old_length))
      stop("No old length.")
    i <- 1
    len <- old_length
    while (i <= length(toChange)) {
      first_city <- cities[ord[toChange[i]],]
      second_city <- cities[ord[toChange[i] + 1],]
      prev_city <- cities[ord[toChange[i] - 1],]
      next_city <- cities[ord[toChange[i] + 1 + 1],]
      if (toChange[i] == 1) {
        len <- len - dist_cities(next_city[,'x'], next_city[,'y'],
                                 second_city[,'x'], second_city[,'y'])
        len <- len + dist_cities(next_city[,'x'], next_city[,'y'],
                                 first_city[,'x'], first_city[,'y'])
      }
      else if (toChange[i] == 149999) {
        len <- len - dist_cities(first_city[,'x'], first_city[,'y'],
                                 prev_city[,'x'], prev_city[,'y'])
        len <- len + dist_cities(second_city[,'x'], second_city[,'y'],
                                 prev_city[,'x'], prev_city[,'y'])
      }
      else {
        len <- len - dist_cities(first_city[,'x'], first_city[,'y'],
                                 prev_city[,'x'], prev_city[,'y'])
        len <- len - dist_cities(next_city[,'x'], next_city[,'y'],
                                 second_city[,'x'], second_city[,'y'])
        len <- len + dist_cities(second_city[,'x'], second_city[,'y'],
                                 prev_city[,'x'], prev_city[,'y'])
        len <- len + dist_cities(next_city[,'x'], next_city[,'y'],
                                 first_city[,'x'], first_city[,'y'])
      }
      tmp <- ord[toChange[i]]
      ord[toChange[i]] <- ord[toChange[i + 1]]
      ord[toChange[i + 1]] <- tmp
      i <- i + 1
    }
  }
  if (len < 0) {
    write.table("Error, len less than 0!", "log.txt", append = T, quote = F, row.names = F, col.names = F)
    write.table(toChange, "log.txt", append = T, quote = F, row.names = F, col.names = F)
    write.table(len, "log.txt", append = T, quote = F, row.names = F, col.names = F)
    stop("Error, len less than 0!")
  }
  return (len)
}

p.a <- function(q_y, q_x, temp) {
  p.a <- exp(-abs(q_y - q_x) / temp)
  return (p.a)
}

cities <- read.csv("santa_cities.csv", header = TRUE)

starting_points <- 10

start <- sample(1:150000, starting_points)

paths <- matrix(nrow = 150000, ncol = starting_points)

paths[1, ] <- start

path_lengths <- double(starting_points)

checked_neighbours <- 10

for (j in 1:starting_points) {
  for (i in 2:150000) {
    neighbours <- sample(setdiff(1:150000, paths[, j]), checked_neighbours, replace = T)
    min_dist <- Inf
    min_neigh <- NA
    for (n in neighbours) {
      neigh_dist <- dist_cities(cities[paths[i - 1, j], 'x'],
                                cities[paths[i - 1, j], 'y'],
                                cities[n, 'x'],
                                cities[n, 'y'])
      if (neigh_dist < min_dist) {
        min_neigh <- n
        min_dist <- neigh_dist
      }
    }
    paths[i, j] <- min_neigh
    path_lengths[j] <- path_lengths[j] + min_dist
    cat("\r", j, ",", i, ",", path_lengths[j])
  }
}