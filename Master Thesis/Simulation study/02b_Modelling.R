source("Simulation study/poi.R")
source("Simulation study/create_true_covmat.R")
library(gmodels)
library(shapes)
library(purrrlyr)
library(rlist)

# Loading the conditions rds files:
conditions_mod_no_adj          <- read_rds("Simulation study/Outputs/02_conditions_mod_no_adj.rds")
conditions_mod_adj_mi_4        <- read_rds("Simulation study/Outputs/02_conditions_mod_adj_mi_4")
conditions_mod_adj_mi_10       <- read_rds("Simulation study/Outputs/02_conditions_mod_adj_mi_10")
conditions_mod_adj_mi_cv_4     <- read_rds("Simulation study/Outputs/02_conditions_mod_adj_mi_cv_4")
conditions_mod_adj_mi_cv_10    <- read_rds("Simulation study/Outputs/02_conditions_mod_adj_mi_cv_10")
conditions_mod_adj_chisq_cv_4  <- read_rds("Simulation study/Outputs/02_conditions_mod_adj_chisq_cv_4")
conditions_mod_adj_chisq_cv_10 <- read_rds("Simulation study/Outputs/02_conditions_mod_adj_chisq_cv_10")




            ########################################################
            # Obtaining the estimates of the Parameter of Interest # 
            ########################################################

##############
# mod_no_adj #
##############

# Using a nested lapply to obtain models:
conditions_mod_no_adj$models <- lapply(conditions_mod_no_adj$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions_mod_no_adj$fits <- lapply(conditions_mod_no_adj$outputs, lapply, function(x) list.extract(x, 'fit'))

# Using a nested lapply to obtain poi estimates:
conditions_mod_no_adj$pois <- lapply(conditions_mod_no_adj$fits, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions_mod_no_adj$mses <- lapply(conditions_mod_no_adj$pois, lapply, function(x) ((as.matrix(x) - conditions[,2])^2))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_no_adj$mean_mse <- lapply(conditions_mod_no_adj$mses, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_no_adj$mse_ci_lower <- lapply(conditions_mod_no_adj$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_no_adj$mse_ci_upper <- lapply(conditions_mod_no_adj$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])

# Computing the modelled covariance matrices:
conditions_mod_no_adj$covmats <- lapply(conditions_mod_no_adj$fits, lapply, fitted)

# Computing the true covmat for each condition:
conditions_mod_no_adj <- purrrlyr::by_row(conditions_mod_no_adj, create_true_covmat)

# Computing the covmat distance:
conditions_mod_no_adj$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
    distcovlist <- lapply(conditions_mod_no_adj$covmats[[i]], function(x) distcov(x$cov, conditions_mod_no_adj$.out[[i]]))
    conditions_mod_no_adj$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions_mod_no_adj$mean_distcov <- lapply(conditions_mod_no_adj$distcov, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_no_adj$distcov_ci_lower <- lapply(conditions_mod_no_adj$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_no_adj$distcov_ci_upper <- lapply(conditions_mod_no_adj$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Obtaining the relevant results:
results_mod_no_adj <- conditions_mod_no_adj %>% select(lambda, rho, delta, n, mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper) %>% unnest(cols = c(mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper))

write_rds(results_mod_no_adj, path = "Simulation study/Results/02_results_mod_no_adj.rds")


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
conditions_mod_adj_mi_4$mses <- lapply(conditions_mod_adj_mi_4$pois, lapply, function(x) ((as.matrix(x) - conditions[,2])^2))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_mi_4$mean_mse <- lapply(conditions_mod_adj_mi_4$mses, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_4$mse_ci_lower <- lapply(conditions_mod_adj_mi_4$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_4$mse_ci_upper <- lapply(conditions_mod_adj_mi_4$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Computing the modelled covariance matrices:
conditions_mod_adj_mi_4$covmats <- lapply(conditions_mod_adj_mi_4$fits, lapply, function(x) ifelse(is.na(x), return(NA), return(fitted(x))))

# Computing the true covmat for each condition:
conditions_mod_adj_mi_4 <- purrrlyr::by_row(conditions_mod_adj_mi_4, create_true_covmat)

# Computing the covmat distance:
conditions_mod_adj_mi_4$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
  distcovlist <- lapply(conditions_mod_adj_mi_4$covmats[[i]], function(x) ifelse(is.na(x), return(NA), distcov(x$cov, conditions_mod_adj_mi_4$.out[[i]])))
  conditions_mod_adj_mi_4$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions_mod_adj_mi_4$mean_distcov <- lapply(conditions_mod_adj_mi_4$distcov, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_4$distcov_ci_lower <- lapply(conditions_mod_adj_mi_4$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_4$distcov_ci_upper <- lapply(conditions_mod_adj_mi_4$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Obtaining the relevant results:
results_mod_adj_mi_4 <- conditions_mod_adj_mi_4 %>% select(lambda, rho, delta, n, mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper) %>% unnest(cols = c(mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper))




write_rds(results_mod_adj_mi_4, path = "Simulation study/Results/02_results_mod_adj_mi_4.rds") 


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
conditions_mod_adj_mi_10$mses <- lapply(conditions_mod_adj_mi_10$pois, lapply, function(x) ((as.matrix(x) - conditions[,2])^2))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_mi_10$mean_mse <- lapply(conditions_mod_adj_mi_10$mses, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_10$mse_ci_lower <- lapply(conditions_mod_adj_mi_10$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_10$mse_ci_upper <- lapply(conditions_mod_adj_mi_10$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])





# Computing the modelled covariance matrices:
conditions_mod_adj_mi_10$covmats <- lapply(conditions_mod_adj_mi_10$fits, lapply, fitted)

# Computing the true covmat for each condition:
conditions_mod_adj_mi_10 <- purrrlyr::by_row(conditions_mod_adj_mi_10, create_true_covmat)

# Computing the covmat distance:
conditions_mod_adj_mi_10$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
  distcovlist <- lapply(conditions_mod_adj_mi_10$covmats[[i]], function(x) ifelse(is.na(x), return(NA), distcov(x$cov, conditions_mod_adj_mi_10$.out[[i]]))) 
  conditions_mod_adj_mi_10$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions_mod_adj_mi_10$mean_distcov <- lapply(conditions_mod_adj_mi_10$distcov, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_10$distcov_ci_lower <- lapply(conditions_mod_adj_mi_10$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_10$distcov_ci_upper <- lapply(conditions_mod_adj_mi_10$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Obtaining the relevant results:
results_mod_adj_mi_10 <- conditions_mod_adj_mi_10 %>% select(lambda, rho, delta, n, mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper) %>% unnest(cols = c(mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper))



write_rds(results_mod_adj_mi_10, path = "Simulation study/Results/02_results_mod_adj_mi_10.rds") 


###################
# mod_adj_mi_cv_4 #
###################

# Using a nested lapply to obtain models:
conditions_mod_adj_mi_cv_4$models <- lapply(conditions_mod_adj_mi_cv_4$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions_mod_adj_mi_cv_4$fits <- lapply(conditions_mod_adj_mi_cv_4$outputs, lapply, function(x) list.extract(x, 'fit'))

# Using a nested lapply to obtain poi estimates:
conditions_mod_adj_mi_cv_4$pois <- lapply(conditions_mod_adj_mi_cv_4$fits, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions_mod_adj_mi_cv_4$mses <- lapply(conditions_mod_adj_mi_cv_4$pois, lapply, function(x) ((as.matrix(x) - conditions[,2])^2))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_mi_cv_4$mean_mse <- lapply(conditions_mod_adj_mi_cv_4$mses, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_cv_4$mse_ci_lower <- lapply(conditions_mod_adj_mi_cv_4$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_cv_4$mse_ci_upper <- lapply(conditions_mod_adj_mi_cv_4$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Computing the modelled covariance matrices:
conditions_mod_adj_mi_cv_4$covmats <- lapply(conditions_mod_adj_mi_cv_4$fits, lapply, fitted)

# Computing the true covmat for each condition:
conditions_mod_adj_mi_cv_4 <- purrrlyr::by_row(conditions_mod_adj_mi_cv_4, create_true_covmat)



# Computing the covmat distance:
conditions_mod_adj_mi_cv_4$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
  distcovlist <- lapply(conditions_mod_adj_mi_cv_4$covmats[[i]], function(x) ifelse(is.na(x), return(NA), distcov(x$cov, conditions_mod_adj_mi_cv_4$.out[[i]]))) 
  conditions_mod_adj_mi_cv_4$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions_mod_adj_mi_cv_4$mean_distcov <- lapply(conditions_mod_adj_mi_cv_4$distcov, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_cv_4$distcov_ci_lower <- lapply(conditions_mod_adj_mi_cv_4$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_cv_4$distcov_ci_upper <- lapply(conditions_mod_adj_mi_cv_4$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Obtaining the relevant results:
results_mod_adj_mi_cv_4 <- conditions_mod_adj_mi_cv_4 %>% select(lambda, rho, delta, n, mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper) %>% unnest(cols = c(mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper))


write_rds(results_mod_adj_mi_cv_4, path = "Simulation study/Results/02_results_mod_adj_mi_cv_4.rds") 



####################
# mod_adj_mi_cv_10 #
####################

# Using a nested lapply to obtain models:
conditions_mod_adj_mi_cv_10$models <- lapply(conditions_mod_adj_mi_cv_10$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions_mod_adj_mi_cv_10$fits <- lapply(conditions_mod_adj_mi_cv_10$outputs, lapply, function(x) list.extract(x, 'fit'))

# Using a nested lapply to obtain poi estimates:
conditions_mod_adj_mi_cv_10$pois <- lapply(conditions_mod_adj_mi_cv_10$fits, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions_mod_adj_mi_cv_10$mses <- lapply(conditions_mod_adj_mi_cv_10$pois, lapply, function(x) ((as.matrix(x) - conditions[,2])^2))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_mi_cv_10$mean_mse <- lapply(conditions_mod_adj_mi_cv_10$mses, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_cv_10$mse_ci_lower <- lapply(conditions_mod_adj_mi_cv_10$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_cv_10$mse_ci_upper <- lapply(conditions_mod_adj_mi_cv_10$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Computing the modelled covariance matrices:
conditions_mod_adj_mi_cv_10$covmats <- lapply(conditions_mod_adj_mi_cv_10$fits, lapply, fitted)

# Computing the true covmat for each condition:
conditions_mod_adj_mi_cv_10 <- purrrlyr::by_row(conditions_mod_adj_mi_cv_10, create_true_covmat)


# Computing the covmat distance:
conditions_mod_adj_mi_cv_10$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
  distcovlist <- lapply(conditions_mod_adj_mi_cv_10$covmats[[i]], function(x) ifelse(is.na(x), return(NA), distcov(x$cov, conditions_mod_adj_mi_cv_10$.out[[i]]))) 
  conditions_mod_adj_mi_cv_10$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions_mod_adj_mi_cv_10$mean_distcov <- lapply(conditions_mod_adj_mi_cv_10$distcov, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_mi_cv_10$distcov_ci_lower <- lapply(conditions_mod_adj_mi_cv_10$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_mi_cv_10$distcov_ci_upper <- lapply(conditions_mod_adj_mi_cv_10$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Obtaining the relevant results:
results_mod_adj_mi_cv_10 <- conditions_mod_adj_mi_cv_10 %>% select(lambda, rho, delta, n, mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper) %>% unnest(cols = c(mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper))



write_rds(results_mod_adj_mi_cv_10, path = "Simulation study/Results/02_results_mod_adj_mi_cv_10.rds") 



######################
# mod_adj_chisq_cv_4 #
######################

# Using a nested lapply to obtain models:
conditions_mod_adj_chisq_cv_4$models <- lapply(conditions_mod_adj_chisq_cv_4$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions_mod_adj_chisq_cv_4$fits <- lapply(conditions_mod_adj_chisq_cv_4$outputs, lapply, function(x) list.extract(x, 'fit'))

# Using a nested lapply to obtain poi estimates:
conditions_mod_adj_chisq_cv_4$pois <- lapply(conditions_mod_adj_chisq_cv_4$fits, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions_mod_adj_chisq_cv_4$mses <- lapply(conditions_mod_adj_chisq_cv_4$pois, lapply, function(x) ((as.matrix(x) - conditions[,2])^2))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_chisq_cv_4$mean_mse <- lapply(conditions_mod_adj_chisq_cv_4$mses, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_chisq_cv_4$mse_ci_lower <- lapply(conditions_mod_adj_chisq_cv_4$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_chisq_cv_4$mse_ci_upper <- lapply(conditions_mod_adj_chisq_cv_4$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Computing the modelled covariance matrices:
conditions_mod_adj_chisq_cv_4$covmats <- lapply(conditions_mod_adj_chisq_cv_4$fits, lapply, fitted)

# Computing the true covmat for each condition:
conditions_mod_adj_chisq_cv_4 <- purrrlyr::by_row(conditions_mod_adj_chisq_cv_4, create_true_covmat)





# Computing the covmat distance:
conditions_mod_adj_chisq_cv_4$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
  distcovlist <- lapply(conditions_mod_adj_chisq_cv_4$covmats[[i]], function(x) ifelse(is.na(x), return(NA), distcov(x$cov, conditions_mod_adj_chisq_cv_4$.out[[i]]))) 
  conditions_mod_adj_chisq_cv_4$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions_mod_adj_chisq_cv_4$mean_distcov <- lapply(conditions_mod_adj_chisq_cv_4$distcov, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_chisq_cv_4$distcov_ci_lower <- lapply(conditions_mod_adj_chisq_cv_4$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_chisq_cv_4$distcov_ci_upper <- lapply(conditions_mod_adj_chisq_cv_4$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Obtaining the relevant results:
results_mod_adj_chisq_cv_4 <- conditions_mod_adj_chisq_cv_4 %>% select(lambda, rho, delta, n, mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper) %>% unnest(cols = c(mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper))


write_rds(results_mod_adj_chisq_cv_4, path = "Simulation study/Results/02_results_mod_adj_chisq_cv_4.rds") 


#######################
# mod_adj_chisq_cv_10 #
#######################

# Using a nested lapply to obtain models:
conditions_mod_adj_chisq_cv_10$models <- lapply(conditions_mod_adj_chisq_cv_10$outputs, lapply, function(x) list.extract(x, 'model'))

# Using a nested lapply to obtain fits:
conditions_mod_adj_chisq_cv_10$fits <- lapply(conditions_mod_adj_chisq_cv_10$outputs, lapply, function(x) list.extract(x, 'fit'))

# Using a nested lapply to obtain poi estimates:
conditions_mod_adj_chisq_cv_10$pois <- lapply(conditions_mod_adj_chisq_cv_10$fits, lapply, function(x) poi(x))

# Using a nested lapply to obtain mse of poi estimates:
conditions_mod_adj_chisq_cv_10$mses <- lapply(conditions_mod_adj_chisq_cv_10$pois, lapply, function(x) ((as.matrix(x) - conditions[,2])^2))

# Using a nested lapply to obtain mean mse and CI's for each condition:
conditions_mod_adj_chisq_cv_10$mean_mse <- lapply(conditions_mod_adj_chisq_cv_10$mses, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_chisq_cv_10$mse_ci_lower <- lapply(conditions_mod_adj_chisq_cv_10$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_chisq_cv_10$mse_ci_upper <- lapply(conditions_mod_adj_chisq_cv_10$mses, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Computing the modelled covariance matrices:
conditions_mod_adj_chisq_cv_10$covmats <- lapply(conditions_mod_adj_chisq_cv_10$fits, lapply, fitted)

# Computing the true covmat for each condition:
conditions_mod_adj_chisq_cv_10 <- purrrlyr::by_row(conditions_mod_adj_chisq_cv_10, create_true_covmat)





# Computing the covmat distance:
conditions_mod_adj_chisq_cv_10$distcov <- vector("list", nrow(conditions))
for (i in 1:nrow(conditions)) {
  distcovlist <- lapply(conditions_mod_adj_chisq_cv_10$covmats[[i]], function(x) ifelse(is.na(x), return(NA), distcov(x$cov, conditions_mod_adj_chisq_cv_10$.out[[i]])))
  conditions_mod_adj_chisq_cv_10$distcov[i] <- list(distcovlist)
}

# Using a nested lapply to obtain mean and CI's distcov for each condition:
conditions_mod_adj_chisq_cv_10$mean_distcov <- lapply(conditions_mod_adj_chisq_cv_10$distcov, function(x) mean(unlist(x), na.rm = TRUE))
conditions_mod_adj_chisq_cv_10$distcov_ci_lower <- lapply(conditions_mod_adj_chisq_cv_10$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI lower"])
conditions_mod_adj_chisq_cv_10$distcov_ci_upper <- lapply(conditions_mod_adj_chisq_cv_10$distcov, function(x) ci(unlist(x), na.rm = TRUE)["CI upper"])


# Obtaining the relevant results:
results_mod_adj_chisq_cv_10 <- conditions_mod_adj_chisq_cv_10 %>% select(lambda, rho, delta, n, mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper) %>% unnest(cols = c(mean_mse, mse_ci_lower, mse_ci_upper, mean_distcov, distcov_ci_lower, distcov_ci_upper))



write_rds(results_mod_adj_chisq_cv_10, path = "Simulation study/Results/02_results_mod_adj_chisq_cv_10.rds")
