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
  
  return(Sigma)
}

out <- purrrlyr::by_row(conditions, create_true_covmat, .collate = "list")
out %>% 
  select(.out)


create_true_covmat(conditions[1,])

apply(conditions, 2, create_true_covmat)






pmap(conditions, create_true_covmat)


rot.conditions <- t(apply(conditions, 2, rev))

sapply(rot.conditions, create_true_covmat, simplify = FALSE)

lapply(rot.conditions, create_true_covmat)

apply(rot.conditions, 2, create_true_covmat)



