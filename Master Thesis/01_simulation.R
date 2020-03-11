library(simsem)
library(tidyverse)

# Import the conditions grid:
conditions <- read_rds(path = "Simulation study/00_conditions.rds")

########################################
##### FUNCTION FOR GENERATING DATA #####
########################################

GenerateData <- function(conditions){
  
  lambda <- conditions[1]
  rho    <- conditions[2]
  delta  <- conditions[3]
  n      <- conditions[4]

########################################
# Specifying LY: Factor loading matrix # 
########################################
  # Loadings of observed variables on latent factors:
  loadings <- matrix(0, 6, 2)
  
    # Making space for loadings on latent factors:
    loadings[1:4, 1] <- NA
    loadings[4:6, 2] <- NA
    
    # Specifying value of loadings
    loadingValues         <- matrix(0, 6, 2)
    loadingValues[1:3, 1] <- lambda
    loadingValues[4, 1]   <- delta
    loadingValues[4:6, 2] <- lambda
    
  # Specification of lambda matrix:
  LY <- bind(loadings, loadingValues)

##########################################################################
# Specifying RPS: Residual correlation matrix among endogenous variables #
##########################################################################
  # Making space for correlation between latent variables
  latent.cor <- matrix(NA, 2, 2)
  diag(latent.cor) <- 1
  RPS <- binds(latent.cor, rho)
  
#################################################################################
# Specifying RTE: Measurement error correlation matrix among observed variables #
#################################################################################
  RTE <- binds(diag(6))

#########################
# Create Model Template #
#########################
  model <- model.cfa(LY = LY, RPS = RPS, RTE = RTE)

####################
# Generate dataset #
####################

  dataset <- simsem::generate(model, n, maxDraw = 1000000) 

  return(dataset)
      
}


############################################
# Applying the function for each condition #
############################################
sim_data <- apply(conditions, 1, FUN = GenerateData)

write_rds(sim_data, path = "Simulation study/01_sim_data.rds")