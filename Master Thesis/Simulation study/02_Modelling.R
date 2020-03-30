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
models <- vector("list", length(sim_data))

for (i in length(rev_sim_data)) {
  
  models[[i]] <- mod_adj_mi(baseline.model = model, data = rev_sim_data[[i]], min.mi = 3)
  
  print(models)

}


lapply(models, lavaan::cfa, data = rev_sim_data)

# How to extract estimate of PoI:
# Using subset()
subset(sum$PE$est, sum$PE$lhs == "f1" & sum$PE$rhs == "f2")
 # where 'sum' is the summary of a fit object.




rev_sim_data <- rev(sim_data)


mod_adj_mi(baseline.model = model, data = rev_sim_data[[1]], min.mi = 3)

lapply(rev_sim_data, mod_adj_mi, baseline.model = model, min.mi = 3)

#################
# mod_adj_mi_cv #
#################
mod_adj_mi_cv()


####################
# mod_adj_chisq_cv #
####################
mod_adj_chisq_cv()
