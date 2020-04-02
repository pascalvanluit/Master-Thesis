                ###################################
                # Script for analyzing MSE of POI #
                ###################################
set.seed(88)
library(shapes)
                
# Combining the MSE values of each of the methods:
mses <- cbind(mse_mod_adj_mi, mse_mod_adj_mi_cv, mse_mod_adj_chisq_cv)

# Finding the true covariance matrices for each condition:
true_covmats <- lapply(conditions, 1, FUN = create_true_covmat)

# Combining all the covariance matrics (true and estimated by each method):
all_covmats <- cbind(true_covmats, covmats_mod_adj_mi, covmats_mod_adj_mi_cv, covmats_mod_adj_chisq_cv)

# Finding the distance of true and estimated covariance matrices:
dist_covmats 