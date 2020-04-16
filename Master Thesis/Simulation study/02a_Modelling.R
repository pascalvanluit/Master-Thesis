              ####################################
              # Script for using the SEM Methods #
              ####################################
set.seed(88)
library(lavaan)
library(plyr)

# Starting model used
model <- " f1 =~ y1 + y2 + y3
           f2 =~ y4 + y5 + y6 "

# Sourcing my functions
source("Methods/mod_adj_mi.R")
source("Methods/modindices_cv.R")
source("Methods/mod_adj_mi_cv.R")
source("Methods/modindices_train.R")
source("Methods/mod_adj_chisq_cv.R")

                 ###################################
                 # Creating lavaan fits and models # 
                 ###################################

################
# mod_adj_mi_4 #
################

conditions$outputs <- vector("list", nrow(conditions))

for (i in nrow(conditions)) {
  
  for (j in 1:replications) {
    
    outputlist <- lapply(conditions$datasets[[i]], mod_adj_mi, baseline.model = model, min.mi = 4, optim.force.converged = TRUE)
    # mod_adj_mi(baseline.model = model, data = conditions$datasets[[i]][[j]], min.mi = 4, optim.force.converged = TRUE)
    conditions$outputs[j] <- list(outputlist)
    
  }

  # #lapply(dat[i], mod_adj_mi, baseline.model = model)
  # outputlist <- lapply(1:replications, function(x) mod_adj_mi(model, data = conditions$datasets, optim.force.converged = TRUE))
  # 
  # conditions$outputs[i] <- list(outputlist)
  
}

write_rds(conditions, path = "Simulation study/02_outputs.rds")



# Tibble to save mod_adj_mi_4 models and fits in:
tib_mod_adj_mi_4 <- tibble(.rows = nrow(out))

# Applying function to each dataset:
out_mod_adj_mi_4 <- lapply(out, mod_adj_mi, baseline.model = model, min.mi = 4,
                           optim.force.converged = TRUE)
out_mod_adj_mi_4 <- matrix(out_mod_adj_mi_4, ncol = replications)

# Obtaining all the models:
models_mod_adj_mi_4 <- lapply(out_mod_adj_mi_4, `[`, c('model'))
models_mod_adj_mi_4 <- matrix(models_mod_adj_mi_4, ncol = replications)

# Obtaining a fit object from each model:
fits_mod_adj_mi_4 <- lapply(out_mod_adj_mi_4, `[`, c('fit'))
fits_mod_adj_mi_4 <- matrix(fits_mod_adj_mi_4, ncol = replications)

tib_mod_adj_mi_4 <- tib_mod_adj_mi_4 %>% 
  mutate(models_mod_adj_mi_4 = models_mod_adj_mi_4,
         fits_mod_adj_mi_4 = fits_mod_adj_mi_4)

write_rds(tib_mod_adj_mi_4, path = "Simulation study/02_tib_mod_adj_mi_4.rds")


#################
# mod_adj_mi_10 #
#################

# Applying function to each dataset:
out_mod_adj_mi_10 <- lapply(sim_data, mod_adj_mi, baseline.model = model, min.mi = 10)

# Obtaining all the models:
models_mod_adj_mi_10 <- lapply(out_mod_adj_mi_10, `[`, c('model'))

# Obtaining a fit object from each model:
fits_mod_adj_mi_10 <- lapply(out_mod_adj_mi_10, `[`, c('fit'))


# Adding the models and fits to the big tibble:
out_df <- out_df %>% 
  mutate(models_mod_adj_mi_4 = unlist(models_mod_adj_mi_4),
         fits_mod_adj_mi_4 = unlist(fits_mod_adj_mi_4),
         models_mod_adj_mi_10 = unlist(models_mod_adj_mi_10),
         fits_mod_adj_mi_10 = unlist(fits_mod_adj_mi_10))


# #################
# # mod_adj_mi_cv #
# #################
# 
# # Applying function to each dataset:
# out_mod_adj_mi_cv <- lapply(sim_data, mod_adj_mi_cv, baseline.model = model, min.mi = 10)
# 
# # Obtaining all the models:
# models_mod_adj_mi_cv <- lapply(out_mod_adj_mi_cv, `[`, c('model'))
# 
# # Obtaining a fit object from each model:
# fits_mod_adj_mi_cv <- lapply(out_mod_adj_mi_cv, `[`, c('fit'))
# 
# 
# ####################
# # mod_adj_chisq_cv #
# ####################
# 
# # Applying mod_adj_mi to each simulated dataset:
# out_mod_adj_chisq_cv <- lapply(sim_data, mod_adj_chisq_cv, baseline.model = model, min.mi = 10)
# 
# # Obtaining all the models:
# models_mod_adj_chisq_cv <- lapply(out_mod_adj_chisq_cv, `[`, c('model'))
# 
# # Obtaining a fit object from each model:
# fits_mod_adj_chisq_cv <- lapply(out_mod_adj_chisq_cv, `[`, c('fit'))


                 


write_rds(out_df, path = "Simulation study/02_models_and_fits.rds")
