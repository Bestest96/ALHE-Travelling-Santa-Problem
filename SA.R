rm(list = setdiff(ls(), "x"))
if (!exists("x")) {
  tryCatch({
    rm(list = ls())
    x <- as.integer(unlist(read.table("x.txt")))
  }, error = function(e) {
    x <<- sample(as.integer(seq(1, 150000)), 150000)
    return (x)
  }
  )
  file.remove("log.txt")
}

cities <- read.csv("santa_cities.csv", header = TRUE)

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
      ord[toChange[i]] <- ord[toChange[i] + 1]
      ord[toChange[i] + 1] <- tmp
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


iter_write <- 1000
counter <- 0
temperature <- 1000
t.change <- 0.999999
t.eps <- 1e-8

q_x <- pathLength(x, cities)

while(T) {
  swap <- unique(sample(1:149999, sample(1:100, 1), replace = T))
  q_y <- pathLength(x, cities, swap, q_x)
  p_a <- p.a(q_y, q_x, temperature)
  if (q_y < q_x || runif(1) < p_a) {
    for (s in swap) {
      tmp <- x[s]
      x[s] <- x[s + 1]
      x[s + 1] <- tmp
    }
    q_x <- q_y
  }
  counter <- counter + 1
  if (counter %% iter_write == 0) {
    print(q_x)
    print(temperature)
    write.table(q_x, "log.txt", append = T, quote = F, row.names = F, col.names = F)
  }
  temperature <- temperature * t.change
  if (temperature < t.eps) {
    temperature <- t.eps
  }
}

print("Ended searching.")

print(q_x)
