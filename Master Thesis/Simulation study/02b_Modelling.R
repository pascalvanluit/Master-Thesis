models_and_fits <- read_rds(path = "Simulation study/02_models_and_fits.rds")

models <- models_and_fits %>% 
  select(contains("models"))

fits <- models_and_fits %>% 
  select(contains("fits"))

            ########################################################
            # Obtaining the estimates of the Parameter of Interest # 
            ########################################################

################
# mod_adj_mi_4 #
################
pois_mod_adj_mi <- lapply(unlist(unlist(fits)), parameterestimates)

lapply(fits_mod_adj_mi_4, function(x) summary(x$fit))
parameterestimates(models_and_fits[[,2]])

## lavInspect

# Obtaining the summary of each fit:
sums_mod_adj_mi_4 <- vector("list", length(fits_mod_adj_mi_4))

for (i in 1:length(fits_mod_adj_mi_4)) {
  sums_mod_adj_mi_4[[i]] <- lavaan::summary(fits_mod_adj_mi_4[[i]]$fit)
}

# Obtaining the parameter of interest from each fit summary:
pois_mod_adj_mi_4 <- list()

for (i in 1:length(sums_mod_adj_mi_4)) {
  pois_mod_adj_mi_4[[i]] <- subset(sums_mod_adj_mi_4[[i]]$PE$est, sums_mod_adj_mi_4[[i]]$PE$lhs == "f1" & sums_mod_adj_mi_4[[i]]$PE$rhs == "f2")
}

# pois_mod_adj_mi_4 <- subset(sums_mod_adj_mi_4, sums_mod_adj_mi_4$PE$lhs == "f1" & sums_mod_adj_mi_4$PE$rhs == "f2")

# Finding the MSE of the PoI:
mse_mod_adj_mi_4 <- c(mse_rho = (unlist(pois_mod_adj_mi_4) - conditions[, 2])^2)


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