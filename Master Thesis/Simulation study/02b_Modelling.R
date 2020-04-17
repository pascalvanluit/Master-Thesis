source("Simulation study/poi.R")
source("Simulation study/create_true_covmat.R")
library(gmodels)
library(shapes)
library(purrrlyr)
library(rlist)

            ########################################################
            # Obtaining the estimates of the Parameter of Interest # 
            ########################################################

################
# mod_adj_mi_4 #
################

# Using a nested lapply to obtain models:
conditions_mod_adj_mi_4$models <- lapply(conditions_mod_adj_mi_4$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions_mod_adj_mi_4$fits <- lapply(conditions_mod_adj_mi_4$outputs, lapply, function(x) list.extract(x, 'fit'))


# Using a nested lapply to obtain poi estimates:
conditions_mod_adj_mi_4$pois <- lapply(conditions_mod_adj_mi_4$fits, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions_mod_adj_mi_4$mses <- lapply(conditions_mod_adj_mi_4$pois, lapply, function(x) (as.matrix(x) - conditions[,2]))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_mi_4$mean_mse <- lapply(conditions_mod_adj_mi_4$mses, function(x) mean(unlist(x)))
conditions_mod_adj_mi_4$mse_ci_lower <- lapply(conditions_mod_adj_mi_4$mses, function(x) ci(unlist(x))["CI lower"])
conditions_mod_adj_mi_4$mse_ci_upper <- lapply(conditions_mod_adj_mi_4$mses, function(x) ci(unlist(x))["CI upper"])


# # Using nested lapply to obtain estimated covariance matrices:
# conditions$covmats <- lapply(conditions$fits, lapply, fitted)
# 
# # Obtaining the true covmat for each condition:
# conditions <- purrrlyr::by_row(conditions, create_true_covmat)
# 
# # Computing the covmat distance:
# conditions$distcov <- vector("list", nrow(conditions))
# for (i in 1:nrow(conditions)) {
#   distcovlist <- lapply(conditions$covmats[[i]], function(x) distcov(x$cov, conditions$.out[[i]]))
#   conditions$distcov[i] <- list(distcovlist)
# }
# 
# # Using a nested lapply to obtain mean and CI's distcov for each condition:
# conditions$mean_distcov <- lapply(conditions$distcov, function(x) mean(unlist(x)))
# conditions$distcov_ci_lower <- lapply(conditions$distcov, function(x) ci(unlist(x))["CI lower"])
# conditions$distcov_ci_upper <- lapply(conditions$distcov, function(x) ci(unlist(x))["CI upper"])






#################
# mod_adj_mi_10 #
#################

# Using a nested lapply to obtain models:
conditions_mod_adj_mi_10$models <- lapply(conditions_mod_adj_mi_10$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions_mod_adj_mi_10$fits <- lapply(conditions_mod_adj_mi_10$outputs, lapply, function(x) list.extract(x, 'fit'))


# Using a nested lapply to obtain poi estimates:
conditions_mod_adj_mi_10$pois <- lapply(conditions_mod_adj_mi_10$fits, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions_mod_adj_mi_10$mses <- lapply(conditions_mod_adj_mi_10$pois, lapply, function(x) (as.matrix(x) - conditions[,2]))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_mi_10$mean_mse <- lapply(conditions_mod_adj_mi_10$mses, function(x) mean(unlist(x)))
conditions_mod_adj_mi_10$mse_ci_lower <- lapply(conditions_mod_adj_mi_10$mses, function(x) ci(unlist(x))["CI lower"])
conditions_mod_adj_mi_10$mse_ci_upper <- lapply(conditions_mod_adj_mi_10$mses, function(x) ci(unlist(x))["CI upper"])


###################
# mod_adj_mi_cv_4 #
###################




####################
# mod_adj_mi_cv_10 #
####################




######################
# mod_adj_chisq_cv_4 #
######################





#######################
# mod_adj_chisq_cv_10 #
#######################

