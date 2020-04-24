create_true_covmat <- function(conditions) {
  
  lambda <- conditions["lambda"]
  rho    <- conditions["rho"]
  delta  <- conditions["delta"]

  # Preparing the Lambda matrix:
  Lambda <- matrix(c(lambda, lambda, lambda, delta, 0, 0, 0, 0, 0, lambda, lambda, lambda), 6)
  Lambda <- as.numeric(Lambda)
  Lambda <- matrix(Lambda, ncol = 2)
  
  # Preparing the Psi matrix:
  Psi <- matrix(c(1, 0.3, 0.3, 1), 2)
  
  # Preparing the Theta matrix:
  Theta <- diag(6)
  
  # Computing the variance-covariance matrix:
  Sigma <- Lambda %*% Psi %*% t(Lambda) + Theta
  
  Sigma <- as.matrix(Sigma)
  
  return(Sigma)
}
