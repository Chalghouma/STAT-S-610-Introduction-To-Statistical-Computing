# STAT-S 610
# LAB 3
# 2020-10-02
# https://jfukuyama.github.io/teaching/stat610/assignments/lab3.pdf

# --- functions --- #

#' @param x (numeric) vector of same length as y
#' @param y (numeric) vector of same length as y
#' @param z (numeric) vector, can be of a different length
#' @param omega (numeric) must be a scalar
#' @return (numeric) vector of the same length as z
llr <- function(x, y, z, omega) {
  fits <- sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

#' @param z (numeric) must be a scalar
#' @param x (numeric) vector of the same length as y
#' @param y (numeric) vector of the same length as x
#' @param omega (numeric) must be a scalar
#' @return (numeric) scalar
compute_f_hat <- function(z, x, y, omega) {
  WzDiagonal <- make_diagonal_weight(z, x, omega)
  X <- make_predictor_matrix(x)

  m1 = multiply_diagonal_vector_by_matrix(WzDiagonal, X)
  first = t(X) %*% m1
  m2 = WzDiagonal * y
  
  second = t(X) %*% m2
  f_hat <- c(1, z) %*% solve(t(X) %*% m1) %*% t(X) %*% m2
  return(f_hat)
}

#' @param z (numeric) must be a scalar
#' @param x (numeric) vector of arbitrary length
#' @param omega (numeric) must be a scalar
#' @return (numeric) a diagonal matrix
make_weight_matrix <- function(z, x, omega) {
  r <- abs(x - z) / omega # this is a vector of the same length as x
  w <- sapply(r, W) # this is a vector of the same length as x and r
  Wz <- diag(w) # this is a diagonal matrix with elements from w
  return(Wz)
}

make_diagonal_weight <- function(z, x, omega) {
  r <- abs(x - z) / omega # this is a vector of the same length as x
  w <- sapply(r, W) # this is a vector of the same length as x and r
  return(w)
}

multiply_diagonal_vector_by_matrix <- function(diagonal_vector, matrix) {
  r = (sweep(matrix, 1, diagonal_vector, "*"))
  return(r)
}

#' @param r (numeric) must be a scalar
#' @return (numeric) scalar
W <- function(r) {
  if (abs(r) < 1) {
    return((1 - abs(r) ** 3) ** 3)
  } else {
    return(0)
  }
}

#' @param x (numeric) vector of arbitrary length
#' @return (numeric) matrix with 2 columns and rows equal to length of x
make_predictor_matrix <- function(x) {
  n <- length(x)
  return(cbind(rep(1, n), x))
}