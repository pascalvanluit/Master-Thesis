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

# Creating 6 data frames (2(MI4, MI10) per method)
conditions_mod_adj_mi_4        <- read_rds("Simulation study/01_sim_data.rds")
conditions_mod_adj_mi_10       <- read_rds("Simulation study/01_sim_data.rds")
conditions_mod_adj_mi_cv_4     <- read_rds("Simulation study/01_sim_data.rds")
conditions_mod_adj_mi_cv_10    <- read_rds("Simulation study/01_sim_data.rds")
conditions_mod_adj_chisq_cv_4  <- read_rds("Simulation study/01_sim_data.rds")
conditions_mod_adj_chisq_cv_10 <- read_rds("Simulation study/01_sim_data.rds")



                 ###################################
                 # Creating lavaan fits and models # 
                 ###################################

################
# mod_adj_mi_4 #
################

conditions_mod_adj_mi_4$outputs <- vector("list", nrow(conditions))

# Using a nested apply to obtain outputs:
conditions_mod_adj_mi_4$outputs <- lapply(conditions$datasets, lapply, mod_adj_mi, baseline.model = model, min.mi = 4, optim.force.converged = TRUE)

write_rds(conditions_mod_adj_mi_4, path = "Simulation study/02_conditions_mod_adj_mi_4.rds")


#################
# mod_adj_mi_10 #
#################

conditions_mod_adj_mi_10$outputs <- vector("list", nrow(conditions))

# Using a nested apply to obtain outputs:
conditions_mod_adj_mi_10$outputs <- lapply(conditions$datasets, lapply, mod_adj_mi, baseline.model = model, min.mi = 10, optim.force.converged = TRUE)

write_rds(conditions_mod_adj_mi_4, path = "Simulation study/02_conditions_mod_adj_mi_4.rds")




# #################
# # mod_adj_mi_cv #
# #################




# ####################
# # mod_adj_chisq_cv #
# ####################



                 


