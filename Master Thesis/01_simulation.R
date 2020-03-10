library(simsem)

GenerateData <- function(lambda, rho, delta, n){
  
########################################
# Specifying LY: Factor loading matrix # 
########################################
  # Loadings of observed variables on latent factors:
  loadings <- matrix(0, 6, 2)
  
    # Making space for loadings on latent factors:
    loadings[1:4, 1] <- NA
    loadings[4:6, 2] <- NA
    
    # Specifying value of loadings
    loadingValues <- matrix(0, 6, 2)
    loadingValues[1:3, 1] <- lambda
    loadingValues[4, 1]   <- delta
    loadingValues[4:6, 2] <- lambda
    
  # Specification of lambda matrix:
  LY <- bind(loadings, loadingValues)
  
# Specifying RTE: Error correlation matrix among observed variables
  
  
}