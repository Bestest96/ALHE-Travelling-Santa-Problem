if (!require("dbscan")) {
  print("dbscan not found! Installing.")
  install.packages("dbscan")
  if (!require("dbscan")) {
    stop("Cannot install ")
  }
}

cities <- read.csv("cities.csv", header = T)
cities <- cities[,2:3]

res <- dbscan(cities, 20)
plot(cities, col=res$cluster)
points(cities[res$cluster==0,], pch = 3, col = "grey")

results <- list()
count <- 1
minPts <- seq(3, 50, 1)
eps <- seq(1, 50, .01)
for (i in minPts) {
  for (j in eps) {
    dbscan_res <- dbscan(cities, eps = j, minPts = i)
    cluster <- dbscan_res$cluster
    noise_pts <- length(cluster[cluster == 0])
    results[[count]] <- c(noise_pts, dbscan_res)
    cat("Dbscan for", "eps =", j, "minPts =", i, "\n")
    count <- count + 1
  }
}

s_noise_pts <- list()
i <- 1
for (l in results) {
  s_noise_pts[[i]] <- l[[1]]
  i <- i + 1
}

s_noise_pts <- sort(unlist(s_noise_pts), index.return = TRUE)

for (i in s_noise_pts$ix) {
  r <- results[[i]]
  noise_pts <- r[[1]]
  cluster <- r$cluster
  eps <- r$eps
  minPts <- r$minPts
  png(sprintf("plots/%f_%f.png", eps, minPts))
  plot(cities, 
       col = cluster,
       main = sprintf("DBSCAN for eps=%f minPts=%f", eps, minPts),
       sub = sprintf("%d noise points", noise_pts))
  points(cities[cluster==0,], pch = 3, col = "grey")
  dev.off()
}

# results <- list()
# count <- 1
minPts <- seq(40, 40, 1)
eps <- seq(37, 40, .2)
for (i in minPts) {
  for (j in eps) {
    dbscan_res <- dbscan(cities, eps = j, minPts = i)
    cluster <- dbscan_res$cluster
    noise_pts <- length(cluster[cluster == 0])
    # results[[count]] <- c(noise_pts, dbscan_res)
    cat("Dbscan for", "eps =", round(j, 2), "minPts =", round(i, 0), "\n")
    # count <- count + 1
    
    png(sprintf("plots/%.2f_%.0f.png", j, i))
    plot(cities, 
         col = cluster,
         main = sprintf("DBSCAN for eps=%.2f minPts=%.0f", j, i),
         sub = sprintf("%d noise points", noise_pts))
    points(cities[cluster==0,], pch = 3, col = "grey")
    dev.off()
  }
}
