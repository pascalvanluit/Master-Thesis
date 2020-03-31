              ####################################
              # Script for using the SEM Methods #
              ####################################
set.seed(88)
library(lavaan)

# Starting model used
model <- " f1 =~ y1 + y2 + y3
           f2 =~ y4 + y5 + y6 "

# Sourcing my functions
source("Methods/mod_adj_mi.R")
source("Methods/modindices_cv.R")
source("Methods/mod_adj_mi_cv.R")
source("Methods/mod_adj_chisq_cv.R")

                 ###############################
                 # Creating lavaan fit objects # 
                 ###############################

##############
# mod_adj_mi #
##############

# Applying mod_adj_mi to each simulated dataset:
models_mod_adj_mi <- lapply(sim_data, mod_adj_mi, baseline.model = model, min.mi = 10)

# Creating a fit object for each model:
fits_mod_adj_mi <- lapply(models_mod_adj_mi, lavaan::cfa)

# Obtaining the summary of each fit:
sums_mod_adj_mi <- lapply(fits_mod_adj_mi, summary)

# Obtaining the parameter of interest from each fit summary:
pois_mod_adj_mi <- subset(sums_mod_adj_mi$PE$est, sums_mod_adj_mi$PE$lhs == "f1" & sums_mod_adj_mi$PE$rhs == "f2")

# Finding the MSE of the PoI:
mse_mod_adj_mi <- pois_mod_adj_mi - conditions[[1, 2]]


# Comparing the estimated PoI to the true value:
true_rho <- conditions[[1,2]]
est_rho  <- subset(sum$PE$est, sum$PE$lhs == "f1" & sum$PE$rhs == "f1")
mse <- sum((true_rho - est_rho)^2)
mse



#################
# mod_adj_mi_cv #
#################

# Applying mod_adj_mi_cv to each simulated dataset:
models_mod_adj_mi_cv <- lapply(sim_data, mod_adj_mi_cv, baseline.model = model, min.mi = 10)

# Creating a fit object for each model:
fits_mod_adj_mi_cv <- lapply(models_mod_adj_mi_cv, lavaan::cfa)


####################
# mod_adj_chisq_cv #
####################

# Applying mod_adj_mi to each simulated dataset:
models_mod_adj_chisq_cv <- lapply(sim_data, mod_adj_chisq_cv, baseline.model = model, min.mi = 10)

# Creating a fit object for each model:
fits_mod_adj_chisq_cv <- lapply(models_mod_adj_chisq_cv, lavaan::cfa)
