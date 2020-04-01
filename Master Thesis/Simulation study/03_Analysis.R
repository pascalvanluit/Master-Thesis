                ###################################
                # Script for analyzing MSE of POI #
                ###################################
set.seed(88)
library(shapes)
                
# Combining the MSE values of each of the methods:
mses <- cbind(mse_mod_adj_mi, mse_mod_adj_mi_cv, mse_mod_adj_chisq_cv)

# Combining the distance between the estimated and true covariance matrices:
covmats <- cbind(covmats_mod_adj_mi, covmats_mod_adj_mi_cv, covmats_mod_adj_chisq_cv)
  