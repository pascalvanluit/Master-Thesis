                ###################################
                # Script for analyzing MSE of POI #
                ###################################
set.seed(88)

results_mod_adj_mi_4        <- read_rds("Simulation study/Results/02_results_mod_adj_mi_4.rds")
results_mod_adj_mi_10       <- read_rds("Simulation study/Results/02_results_mod_adj_mi_10.rds")
results_mod_adj_mi_cv_4     <- read_rds("Simulation study/Results/02_results_mod_adj_mi_cv_4.rds")
results_mod_adj_mi_cv_10    <- read_rds("Simulation study/Results/02_results_mod_adj_mi_cv_10.rds")
results_mod_adj_chisq_cv_4  <- read_rds("Simulation study/Results/02_results_mod_adj_chisq_cv_4.rds")
results_mod_adj_chisq_cv_10 <- read_rds("Simulation study/Results/02_results_mod_adj_chisq_cv_10.rds")


                
                
              
                
# # Obtaining the true covariance matrices:
# true_covmats <- purrrlyr::by_row(conditions, create_true_covmat, .collate = "list")
# true_covmats <- true_covmats %>% select(.out)
# true_covmats <- true_covmats[[1]]
                

                
