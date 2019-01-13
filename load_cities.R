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

pathLength <- function(ord, cities, city_primes, toChange = NULL, old_length = NULL, old_paths = NULL) {
  len <- 0.0
  if (is.null(toChange)) {
    paths <- double(length(ord) - 1)
    for (i in 1:(length(ord) - 1)) {
      dist <- dist_cities(cities[ord[i + 1] + 1, 'X'], cities[ord[i + 1] + 1, 'Y'], 
                          cities[ord[i] + 1, 'X'], cities[ord[i] + 1, 'Y'])
      if (i %% 10 == 0 && !(ord[i] %in% city_primes)) {
        dist <- 1.1 * dist
      }
      len <- len + dist
      paths[i] <- dist
    }
  }
  else {
    if (is.null(old_length) || is.null(old_paths))
      stop("No old sth.")
    else if (length(toChange) %% 2 != 0)
      stop("No even length of toChange.")
    i <- 1
    len <- old_length
    paths <- old_paths
    while (i <= length(toChange)) {
      c1 <- cities[ord[toChange[i]] + 1,]
      p1 <- cities[ord[(toChange[i] - 1)] + 1,]
      n1 <- cities[ord[(toChange[i] + 1)] + 1,]
      c2 <- cities[ord[toChange[i + 1]] + 1,]
      p2 <- cities[ord[(toChange[i + 1] - 1)] + 1,]
      n2 <- cities[ord[(toChange[i + 1] + 1)] + 1,]
      
      p1.c1 <- old_paths[toChange[i] - 1]
      p2.c2 <- old_paths[toChange[i + 1] - 1]
      c1.n1 <- old_paths[toChange[i]]
      c2.n2 <- old_paths[toChange[i + 1]]
      
      p1.c2 <- dist_cities(c2[,'X'], c2[,'Y'], p1[,'X'], p1[,'Y'])
      p2.c1 <- dist_cities(c1[,'X'], c1[,'Y'], p2[,'X'], p2[,'Y'])
      c2.n1 <- dist_cities(n1[,'X'], n1[,'Y'], c2[,'X'], c2[,'Y'])
      c1.n2 <- dist_cities(n2[,'X'], n2[,'Y'], c1[,'X'], c1[,'Y'])
      
      if (toChange[i] %% 10 == 0 && !(ord[toChange[i + 1]] %in% city_primes))
        c2.n1 <- 1.1 * c2.n1
      else if (((toChange[i] - 1)) %% 10 == 0 && !(ord[((toChange[i] - 1))] %in% city_primes))
        p1.c2 <- 1.1 * p1.c2
      if (toChange[i + 1] %% 10 == 0 && !(ord[toChange[i]] %in% city_primes))
        c1.n2 <- 1.1 * c1.n2
      else if (((toChange[i + 1] - 1)) %% 10 == 0 && !(ord[((toChange[i + 1] - 1))] %in% city_primes))
        p2.c1 <- 1.1 * p2.c1
      len <- len - p1.c1 - p2.c2 - c1.n1 - c2.n2 + p1.c2 + p2.c1 + c2.n1 + c1.n2
      p1.c2 -> paths[toChange[i] - 1]
      p2.c1 -> paths[toChange[i + 1] - 1]
      c2.n1 -> paths[toChange[i]]
      c1.n2 -> paths[toChange[i + 1]]
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
  return (list("length" = len, "paths" = paths))
}

print.info <- function(params, logger = NULL) {
  write(paste(params, collapse = '\t'), stdout(), sep = '\t')
  if (!is.null(logger))
    write(paste(params, collapse = '\t'), logger, sep = '\t', append = T)
}

cities <- read.csv("cities.csv", header = T)
cities <- cities[,2:3]
city_primes <- primes(dim(cities)[1] - 1)
