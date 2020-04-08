                ###################################
                # Script for analyzing MSE of POI #
                ###################################
set.seed(88)
library(shapes)
library(purrrlyr)
source("Simulation study/create_true_covmat.R")                

                
# Combining the MSE values of each of the methods:
mses <- cbind(mse_mod_adj_mi_4, mse_mod_adj_mi_10, mse_mod_adj_mi_cv, mse_mod_adj_chisq_cv)

# Finding the true covariance matrices for each condition:
true_covmats <- purrrlyr::by_row(conditions, create_true_covmat, .collate = "list")
true_covmats <- true_covmats %>% select(.out)

# Combining all the covariance matrics (true and estimated by each method):
all_covmats <- cbind(true_covmats, covmats_mod_adj_mi_4, covmats_mod_adj_mi_10, covmats_mod_adj_mi_cv, covmats_mod_adj_chisq_cv)

# Finding the distance of true and estimated covariance matrices:
dist_covmats <- matrix(dist_mod_adj_mi_4 = distcov(true_covmats, covmats_mod_adj_mi_4), dist_mod_adj_mi_10 = distcov(true_covmats, covmats_mod_adj_mi_10), dist_mod_adj_mi_cv = distcov(true_covmats, covmats_mod_adj_mi_cv), dist_mod_adj_chisq_cv = distcov(true_covmats, covmats_mod_adj_chisq_cv))