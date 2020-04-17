source("Simulation study/poi.R")

library(gmodels)
library(shapes)
            ########################################################
            # Obtaining the estimates of the Parameter of Interest # 
            ########################################################

################
# mod_adj_mi_4 #
################

# Using a nested apply to obtain models:
conditions$models <- lapply(conditions$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested apply to obtain fits:
conditions$fits <- lapply(conditions$outputs, lapply, function(x) list.extract(x, 'fit'))


# Using a nested apply to obtain poi estimates:
conditions$pois <- lapply(conditions$pois, lapply, function(x) poi(x))

# Using a nested apply to obtain mse of poi estimates:
conditions$mses <- lapply(conditions$mses, lapply, function(x) (as.matrix(x) - conditions[,2]))

# Using a nested apply to obtain mean mse and CI's for each condition:
conditions$mean_mse <- lapply(conditions$mses, function(x) mean(unlist(x)))
conditions$mse_ci_lower <- lapply(conditions$mses, function(x) ci(unlist(x))["CI lower"])
conditions$mse_ci_upper <- lapply(conditions$mses, function(x) ci(unlist(x))["CI upper"])


# Using nested apply to obtain estimated covariance matrices:
conditions$covmats <- lapply(conditions$fits, lapply, fitted)

# Using nested lapply to obtain distcov from true covmat:
conditions$truecov <- purrrlyr::by_row(conditions, create_true_covmat)





# Sourcing the tibble with all the models and fits of mod_adj_mi_4:
tib_mod_adj_mi_4 <- source("Simulation study/02_tib_mod_adj_mi_4.rds")

# Separating the models:
models_mod_adj_mi_4 <- tib_mod_adj_mi_4 %>% 
  select(contains("models"))

# Separating the fits:
fits_mod_adj_mi_4 <- tib_mod_adj_mi_4 %>% 
  select(contains("fits"))

# Finding the value of the parameter of interest:
fits_mod_adj_mi_4 <- unlist(fits_mod_adj_mi_4)
pois_mod_adj_mi_4 <- lapply(fits_mod_adj_mi_4, parameterestimates)
pois_mod_adj_mi_4 <- lapply(unname(pois_mod_adj_mi_4), poi)

# Computing the MSE of the parameter of interest:
mse_mod_adj_mi_4 <- as.data.frame(c((unlist(pois_mod_adj_mi_4) - conditions[, 2])^2))



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