create_true_covmat <- function(conditions) {
  
  lambda <- conditions[1]
  rho    <- conditions[2]
  delta  <- conditions[3]
  
  Lambda <- matrix(c(
    lambda, lambda,
    lambda, delta,
    0, 0,
    0, 0,
    0, lambda,
    lambda, lambda
  ), 6)
  
  Psi <- matrix(c(1, 0.3, 0.3, 1), 2)
  
  Theta <- diag(6)
  
  Sigma <- Lambda %*% Psi %*% t(Lambda) + Theta
  
  return(Sigma)
}

create_true_covmat(conditions[[1]])
conditions
