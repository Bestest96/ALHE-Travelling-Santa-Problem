if (!require("sfsmisc")) {
  print("Sfsmisc not found! Installing.")
  install.packages("sfsmisc")
  if (!require("sfsmisc")) {
    stop("Cannot install ")
  }
}

dist_cities <- function(x1, y1, x2, y2) {
  dist <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
  return (dist)
}

pathLength <- function(ord, cities, city_primes, toChange = NULL, old_length = NULL) {
  len <- 0.0
  if (is.null(toChange)) { 
    for (i in 1:(length(ord) - 1)) {
      dist <- dist_cities(cities[ord[i + 1] + 1, 'X'], cities[ord[i + 1] + 1, 'Y'], 
                          cities[ord[i] + 1, 'X'], cities[ord[i] + 1, 'Y'])
      if (i %% 10 == 0 && !(ord[i] %in% city_primes)) {
        dist <- 1.1 * dist
      }
      len <- len + dist
    }
  }
  else {
    if (is.null(old_length))
      stop("No old length.")
    else if (length(toChange) %% 2 != 0)
      stop("No even length of toChange.")
    i <- 1
    len <- old_length
    while (i <= length(toChange)) {
      c1 <- cities[ord[toChange[i]] + 1,]
      p1 <- cities[ord[(toChange[i] - 1)] + 1,]
      n1 <- cities[ord[(toChange[i] + 1)] + 1,]
      c2 <- cities[ord[toChange[i + 1]] + 1,]
      p2 <- cities[ord[(toChange[i + 1] - 1)] + 1,]
      n2 <- cities[ord[(toChange[i + 1] + 1)] + 1,]
      
      p1.c1 <- dist_cities(c1[,'X'], c1[,'Y'], p1[,'X'], p1[,'Y'])
      p2.c2 <- dist_cities(c2[,'X'], c2[,'Y'], p2[,'X'], p2[,'Y'])
      c1.n1 <- dist_cities(n1[,'X'], n1[,'Y'], c1[,'X'], c1[,'Y'])
      c2.n2 <- dist_cities(n2[,'X'], n2[,'Y'], c2[,'X'], c2[,'Y'])
      
      p1.c2 <- dist_cities(c2[,'X'], c2[,'Y'], p1[,'X'], p1[,'Y'])
      p2.c1 <- dist_cities(c1[,'X'], c1[,'Y'], p2[,'X'], p2[,'Y'])
      c2.n1 <- dist_cities(n1[,'X'], n1[,'Y'], c2[,'X'], c2[,'Y'])
      c1.n2 <- dist_cities(n2[,'X'], n2[,'Y'], c1[,'X'], c1[,'Y'])
      
      if (toChange[i] %% 10 == 0) {
        if (!(ord[toChange[i]] %in% city_primes))
          c1.n1 <- 1.1 * c1.n1
        if (!(ord[toChange[i + 1]] %in% city_primes))
          c2.n1 <- 1.1 * c2.n1
      }
      else if (((toChange[i] - 1)) %% 10 == 0) {
        if (!(ord[((toChange[i] - 1))] %in% city_primes)) {
          p1.c1 <- 1.1 * p1.c1
          p1.c2 <- 1.1 * p1.c2
        }
      }
      
      if (toChange[i + 1] %% 10 == 0) {
        if (!(ord[toChange[i + 1]] %in% city_primes))
          c2.n2 <- 1.1 * c2.n2
        if (!(ord[toChange[i]] %in% city_primes))
          c1.n2 <- 1.1 * c1.n2
      }
      else if (((toChange[i + 1] - 1)) %% 10 == 0) {
        if (!(ord[((toChange[i + 1] - 1))] %in% city_primes)) {
          p2.c2 <- 1.1 * p2.c2
          p2.c1 <- 1.1 * p2.c1
        }
      }
      len <- len - p1.c1 - p2.c2 - c1.n1 - c2.n2 + p1.c2 + p2.c1 + c2.n1 + c1.n2
      tmp <- ord[toChange[i]]
      ord[toChange[i]] <- ord[toChange[i + 1]]
      ord[toChange[i + 1]] <- tmp
      i <- i + 2
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

cities <- read.csv("cities.csv", header = T)

city_primes <- primes(dim(cities)[1] - 1)

order <- integer(dim(cities)[1] + 1)

# order[dim(cities)[1] + 1] <- 0
# order[1:dim(cities)[1]] <- cities[,"CityId"]

checked_neighbours <- length(cities[,"CityId"])
path_length <- 0
paths <- double(length(order) - 1)

for (i in 1:(length(order) - 2)) {
  to_check <- setdiff(1:max(cities[,"CityId"]), order)
  if (length(to_check) > checked_neighbours)
    neighbours <- sample(to_check, checked_neighbours)
  else
    neighbours <- to_check
  min_dist <- Inf
  min_neigh <- NA
  x1 <- cities[order[i] + 1, 'X']
  y1 <- cities[order[i] + 1, 'Y']
  x2 <- cities[neighbours + 1, 'X']
  y2 <- cities[neighbours + 1, 'Y']
  dists <- dist_cities(x1, y1, x2, y2)
  if (i %% 10 == 0) {
    check_dists <- ifelse(neighbours %in% city_primes, dists, 1.1 * dists)
    dists <- check_dists
  }
  else
    check_dists <- ifelse(neighbours %in% city_primes, 1.1 * dists, dists)
  min_indx <- which.min(check_dists)
  min_dist <- dists[min_indx]
  min_neigh <- neighbours[min_indx]
  order[i + 1] <- min_neigh
  path_length <- path_length + min_dist
  paths[i] <- min_dist
  cat("\r", i, ",", path_length)
}
last_len <- dist_cities(cities[order[i + 1] + 1, 'X'], cities[order[i + 1] + 1, 'Y'], cities[1, 'X'], cities[1, 'Y'])
paths[i + 1] <- last_len
path_length <- path_length + last_len

# to check numerical errors
# changes <- sample(100:120000, 90000)
# newLen1000 <- pathLength(order, cities, city_primes, changes, pathLength(order, cities, city_primes))
# i <- 1
# while (i <= length(changes)) {
#   tmp <- order[changes[i]]
#   order[changes[i]] <- order[changes[i + 1]]
#   order[changes[i + 1]] <- tmp
#   i <- i + 2
# }
# checkLen1000 <- pathLength(order, cities, city_primes)