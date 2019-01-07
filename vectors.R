x_axis_angle <- function(v) {
  p1 <- v[[1]]
  p2 <- v[[2]]
  x <- p2[1] - p1[1]
  y <- p2[2] - p1[1]
  atan2(y, x)
}

squared_modulus <- function(v) {
  p1 <- v[[1]]
  p2 <- v[[2]]
  x <- p2[[1]] - p1[[1]]
  y <- p2[[2]] - p1[[1]]
  x*x + y*y
}

compare_vectors <- function(v1, v2) {
  angle1 <- x_axis_angle(v1)
  angle2 <- x_axis_angle(v2)
  compare <- angle1 - angle2
  if(compare != 0) return (ifelse(compare > 0, TRUE, FALSE))
  dist1 <- squared_modulus(v1)
  dist2 <- squared_modulus(v2)
  ifelse(dist1 - dist2 > 0, TRUE, FALSE)
}

'[.Vectors' <- function(x, i) {
  class(x) <- "list"
  x <- x[i]
  class(x) <- "Vectors"
  x
}

'>.Vectors' <- function(a,b) {
  compare_vectors(list(O, a), list(O, b))
}

'==.Vectors' <- function(a, b) ifelse(a > b || b > a, FALSE, TRUE)

'<.Vectors' <- function(a, b) b > a
