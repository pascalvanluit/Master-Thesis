create_true_covmat <- function(conditions) {
  
  lambda <- conditions[, 1]
  rho    <- conditions[, 2]
  delta  <- conditions[, 3]
  
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

pmap(conditions, create_true_covmat)


rot.conditions <- t(apply(conditions, 2, rev))

sapply(rot.conditions, create_true_covmat, simplify = FALSE)

lapply(rot.conditions, create_true_covmat)

apply(rot.conditions, 2, create_true_covmat)
