generate_population <- function(clusters, mi) {
  n_clusters <- max(clusters)
  P <- list()
  to_sample <- setdiff(1:n_clusters, clusters[1])
  for(i in 1:mi) {
    c.order <- c(clusters[1], sample(to_sample))
    length <- pathLength(NN(clusters, c.order, dt))
    P[[i]] <- list(c.order = c.order, length = length)
  }
  
  return (P)
}

basic_reproduce <- function(P, lambda) {
  scores <- sapply(P, function(x){length})
  scores <- exp(scores)
  scores <- scores / sum(scores)
  
  total <- length(P) + lambda
  R <- sample(P, total, replace = T, prob = scores)
  
  return (R)
}

threshold_reproduce <- function(P, lambda, fi) {
  P_best <- P[select_n_best(P, fi*length(P))]
  total <- length(P) + lambda
  R <- sample(P_best, replace = T)
  
  return (R)
}

tourney_reproduce <- function(P, lambda, s) {
  mi <- length(P)
  probs <- 1:mi
  probs <- sapply(probs, function(i){(mi-i+1)^s - (mi-i)^2})
  probs <- probs/(mi^s)
  P_sorted <- P[select_n_best(P, length(P))]
  R <- sample(P_sorted, prob = probs)
  
  return (R)
}

cross <- function(R, X, clusters, dt) {
  C <- list()
  counter <- 1
  organism <- 1
  while(counter <= length(R)) {
    # cross
    if(counter%%2==1 && X[(counter+1)/2]) {
      corder_1 <- R[[counter]]$c.order
      corder_2 <- R[[counter+1]]$c.order
      new_corder <- c(corder_1[1])
      used <- c()
      idxs_to_fill <- c()
      for(i in 2:length(corder_1)) {
        x1 <- corder_1[i]
        x2 <- corder_2[i]
        xs <- c()
        if(!(x1 %in% used)) xs <- c(xs, x1)
        if(!(x2 %in% used)) xs <- c(xs, x2)
        if(length(xs) == 0){
          idxs_to_fill <- c(idxs_to_fill, i)
          new_corder <- c(new_corder, NaN)
        }
        else if(length(xs) == 1) {
          used <- c(used, xs)
          new_corder <- c(new_corder, xs)
        }
        else if(length(xs) == 2) {
          to_choose <- sample(1:2, 1)
          new_corder <- c(new_corder, xs[to_choose])
          used <- c(used, xs[to_choose])
        }
      }
      to_fill <- setdiff(corder_1[-1], used)
      new_corder[idxs_to_fill] <- sample(to_fill)
      C[[organism]] <- list(c.order = new_corder, length = pathLength(NN(clusters, new_corder, dt))) 
      
      counter <- counter + 2
    }
    else {
      C[[organism]] <- R[[counter]]
      counter <- counter + 1
    }
    organism <- organism + 1
  }
  
  return (C)
}

mutate <- function(C) {
  return (C)
}

succession <- function(P, O) {
  new_P <- O
  n_elites <- max(0, length(P) - length(O))
  idxs_sorted_P <- select_n_best(P, length(P))
  counter <- 1
  while(counter <= n_elites) {
    new_P[[length(O) + counter]] <- P[[idxs_sorted_P[counter]]]
    counter <- counter + 1
  }
  
  return (new_P)
}

select_n_best <- function(P, n) {
  P_scores <- sapply(P, function(x){x$length})
  
  return (order(P_scores)[1:n])
}

evo <- function(clusters, dt, mi, lambda, pc, reproduce_method = "basic") {
  if(reproduce_method == "basic") reproduce <- basic_reproduce
  else if(reproduce_method == "threshold") reproduce <- threshold_reproduce
  else if(reproduce_method == "tourney") reproduce <- tourney_reproduce
  else stop("Invalid reproduce method.")
  
  P <- generate_population(clusters, mi)
  t <- 1
  best <<- P[[1]]
  n_steady_iterations <- 0
  while(T) {
    R <- reproduce(P, lambda)
    X <- sample(c(0,1), (mi+lambda)/2, replace = T, prob = c(1-pc, pc))
    C <- cross(R, X, clusters, dt)
    O <- mutate(C)
    P <- succession(P, O)
    
    current_best <- select_n_best(P, 1)
    if(current_best < best){
      best <<- current_best
      n_steady_iterations <- 0
    }
    else n_steady_iterations <- n_steady_iterations + 1
    print.info(c(t, n_steady_iterations, current_best$length, best$length), logger = "logs/evo.log")
    
    t <- t+1
  }
  
  return (best)
}
