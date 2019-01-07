if (!require("sfsmisc")) {
  print("Sfsmisc not found! Installing.")
  install.packages("sfsmisc")
  if (!require("sfsmisc")) {
    stop("Cannot install ")
  }
}

cities <- read.csv("cities.csv", header = T)
cities <- cities[,2:3]
city_primes <- primes(dim(cities)[1] - 1)
