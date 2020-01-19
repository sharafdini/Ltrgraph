New_Index <- function(A){
  g <- as(A, "graphNEL")
  mat_dist <- distanceMatrix(g)
  sigma <- apply(mat_dist,2,sum)
  B <- diag(sigma)-A
  W <- wiener(g)
  lambda <- svd(B)$d
  n <- length(lambda)
  sum(abs(lambda-2*W/n))
}
