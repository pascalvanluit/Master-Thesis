source("Simulation study/poi.R")
source("Simulation study/create_true_covmat.R")
library(gmodels)
library(shapes)
library(purrrlyr)
            ########################################################
            # Obtaining the estimates of the Parameter of Interest # 
            ########################################################

################
# mod_adj_mi_4 #
################

# Using a nested lapply to obtain models:
conditions$models <- lapply(conditions$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions$fits <- lapply(conditions$outputs, lapply, function(x) list.extract(x, 'fit'))


# Using a nested lapply to obtain poi estimates:
conditions$pois <- lapply(conditions$pois, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions$mses <- lapply(conditions$mses, lapply, function(x) (as.matrix(x) - conditions[,2]))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions$mean_mse <- lapply(conditions$mses, function(x) mean(unlist(x)))
conditions$mse_ci_lower <- lapply(conditions$mses, function(x) ci(unlist(x))["CI lower"])
conditions$mse_ci_upper <- lapply(conditions$mses, function(x) ci(unlist(x))["CI upper"])


# Using nested lapply to obtain estimated covariance matrices:
conditions$covmats <- lapply(conditions$fits, lapply, fitted)

# Obtaining the true covmat for each condition:
conditions <- purrrlyr::by_row(conditions, create_true_covmat)

# Computing the covmat distance:
conditions$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
  distcovlist <- lapply(conditions$covmats[[i]], function(x) distcov(x$cov, conditionstest$.out[[i]]))
  conditions$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions$mean_distcov <- lapply(conditions$distcov, function(x) mean(unlist(x)))
conditions$distcov_ci_lower <- lapply(conditions$distcov, function(x) ci(unlist(x))["CI lower"])
conditions$distcov_ci_upper <- lapply(conditions$distcov, function(x) ci(unlist(x))["CI upper"])










# #################
# # mod_adj_mi_10 #
# #################
# 
# # Obtaining the summary of each fit:
# sums_mod_adj_mi_10 <- lapply(fits_mod_adj_mi_10, summary)
# 
# # Obtaining the parameter of interest from each fit summary:
# pois_mod_adj_mi_10 <- subset(sums_mod_adj_mi_10$PE$est, sums_mod_adj_mi_10$PE$lhs == "f1" & sums_mod_adj_mi_10$PE$rhs == "f2")
# 
# # Finding the MSE of the PoI:
# mse_mod_adj_mi_10 <- pois_mod_adj_mi_10 - conditions[, 2]
# 
# 
# #################
# # mod_adj_mi_cv #
# #################
# 
# # Obtaining the summary of each fit:
# sums_mod_adj_mi_cv <- lapply(fits_mod_adj_mi_cv, summary)
# 
# # Obtaining the parameter of interest from each fit summary:
# pois_mod_adj_mi_cv <- subset(sums_mod_adj_mi_cv$PE$est, sums_mod_adj_mi_cv$PE$lhs == "f1" & sums_mod_adj_mi_cv$PE$rhs == "f2")
# 
# # Finding the MSE of the PoI:
# mse_mod_adj_mi_cv <- pois_mod_adj_mi_cv - conditions[, 2]
# 
# 
# ####################
# # mod_adj_chisq_cv #
# ####################
# 
# # Obtaining the summary of each fit:
# sums_mod_adj_chisq_cv <- lapply(fits_mod_adj_mi_cv, summary)
# 
# # Obtaining the parameter of interest from each fit summary:
# pois_mod_adj_chisq_cv <- subset(sums_mod_adj_chisq_cv$PE$est, sums_mod_adj_chisq_cv$PE$lhs == "f1" & sums_mod_adj_mi_cv$PE$rhs == "f2")
# 
# # Finding the MSE of the PoI:
# mse_mod_adj_chisq_cv <- pois_mod_adj_chisq_cv - conditions[, 2]


###################################################
# Obtaining the covariance matrix of each dataset # 
###################################################

################
# mod_adj_mi_4 #
################
covmats_mod_adj_mi_4 <- lapply(unlist(fits_mod_adj_mi_4), fitted)


# ################
# # mod_adj_mi_10 #
# ################
# covmats_mod_adj_mi_10 <- lapply(fits_mod_adj_mi_10, fitted)
# 
# 
# #################
# # mod_adj_mi_cv #
# #################
# covmats_mod_adj_mi_cv <- lapply(fits_mod_adj_mi_cv, fitted)
# 
# 
# ####################
# # mod_adj_chisq_cv #
# ####################
# covmats_mod_adj_chisq_cv <- lapply(fits_mod_adj_chisq_cv, fitted)