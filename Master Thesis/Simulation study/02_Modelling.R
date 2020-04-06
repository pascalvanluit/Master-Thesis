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
source("Methods/modindices_train.R")
source("Methods/mod_adj_chisq_cv.R")

                 ###############################
                 # Creating lavaan fit objects # 
                 ###############################

################
# mod_adj_mi_4 #
################

# Applying mod_adj_mi to each simulated dataset:
models_mod_adj_mi_4 <- lapply(sim_data, mod_adj_mi, baseline.model = model, min.mi = 4)

# Creating a fit object for each model:
fits_mod_adj_mi_4 <- lapply(models_mod_adj_mi, lavaan::cfa)


#################
# mod_adj_mi_10 #
#################

# Applying mod_adj_mi to each simulated dataset:
models_mod_adj_mi_10 <- lapply(sim_data, mod_adj_mi, baseline.model = model, min.mi = 10)

# Creating a fit object for each model:
fits_mod_adj_mi_10 <- lapply(models_mod_adj_mi, lavaan::cfa)


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



                  ##############################################
                  # Obtaining MSE of the Parameter of Interest # 
                  ##############################################

################
# mod_adj_mi_4 #
################

# Obtaining the summary of each fit:
sums_mod_adj_mi_4 <- lapply(fits_mod_adj_mi_4, summary)

# Obtaining the parameter of interest from each fit summary:
pois_mod_adj_mi_4 <- subset(sums_mod_adj_mi_4$PE$est, sums_mod_adj_mi_4$PE$lhs == "f1" & sums_mod_adj_mi_4$PE$rhs == "f2")

# Finding the MSE of the PoI:
mse_mod_adj_mi_4 <- pois_mod_adj_mi_4 - conditions[[1, 2]]#############
####################
########CHECK HERE#######


#################
# mod_adj_mi_10 #
#################

# Obtaining the summary of each fit:
sums_mod_adj_mi_10 <- lapply(fits_mod_adj_mi_10, summary)

# Obtaining the parameter of interest from each fit summary:
pois_mod_adj_mi_10 <- subset(sums_mod_adj_mi_10$PE$est, sums_mod_adj_mi_10$PE$lhs == "f1" & sums_mod_adj_mi_10$PE$rhs == "f2")

# Finding the MSE of the PoI:
mse_mod_adj_mi_10 <- pois_mod_adj_mi_10 - conditions[[1, 2]]



#################
# mod_adj_mi_cv #
#################

# Obtaining the summary of each fit:
sums_mod_adj_mi_cv <- lapply(fits_mod_adj_mi_cv, summary)

# Obtaining the parameter of interest from each fit summary:
pois_mod_adj_mi_cv <- subset(sums_mod_adj_mi_cv$PE$est, sums_mod_adj_mi_cv$PE$lhs == "f1" & sums_mod_adj_mi_cv$PE$rhs == "f2")

# Finding the MSE of the PoI:
mse_mod_adj_mi_cv <- pois_mod_adj_mi_cv - conditions[[1, 2]]


####################
# mod_adj_chisq_cv #
####################

# Obtaining the summary of each fit:
sums_mod_adj_chisq_cv <- lapply(fits_mod_adj_mi_cv, summary)

# Obtaining the parameter of interest from each fit summary:
pois_mod_adj_chisq_cv <- subset(sums_mod_adj_chisq_cv$PE$est, sums_mod_adj_chisq_cv$PE$lhs == "f1" & sums_mod_adj_mi_cv$PE$rhs == "f2")

# Finding the MSE of the PoI:
mse_mod_adj_chisq_cv <- pois_mod_adj_chisq_cv - conditions[[1, 2]]


              ###################################################
              # Obtaining the covariance matrix of each dataset # 
              ###################################################

################
# mod_adj_mi_4 #
################
covmats_mod_adj_mi_4 <- lapply(fits_mod_adj_mi_4, fitted)


################
# mod_adj_mi_10 #
################
covmats_mod_adj_mi_10 <- lapply(fits_mod_adj_mi_10, fitted)


#################
# mod_adj_mi_cv #
#################
covmats_mod_adj_mi_cv <- lapply(fits_mod_adj_mi_cv, fitted)


####################
# mod_adj_chisq_cv #
####################
covmats_mod_adj_chisq_cv <- lapply(fits_mod_adj_chisq_cv, fitted)



