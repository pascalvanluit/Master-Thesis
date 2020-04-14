                  ###########################
                  # Script for making plots #
                  ###########################

                  
##############
# mod_adj_mi #
##############          

# Plots for mod_adj_mi_4                                    
plot(density(unlist(mse_mod_adj_mi_4)))
plot(density(unlist(distcov_mod_adj_mi_4)))

# Plots for mod_adj_mi_10
plot(density(unlist(mse_mod_adj_mi_4)))
plot(density(unlist(distcov_mod_adj_mi_4)))

# #################
# # mod_adj_mi_cv #
# #################
# 
# # mod_adj_mi_cv_4
# plot(density(unlist(mse_mod_adj_cv_4)))
# plot(density(unlist(distcov_mod_adj_mi_cv_4)))
# 
# # mod_adj_mi_cv_10
# plot(density(unlist(mse_mod_adj_cv_10)))
# plot(density(unlist(distcov_mod_adj_mi_cv_10)))
# 
# 
# ####################
# # mod_adj_chisq_cv #
# ####################
# 
# # mod_adj_chisq_cv_4
# plot(density(unlist(mse_mod_adj_cv_4)))
# plot(density(unlist(distcov_mod_adj_mi_cv_4)))
# 
# # mod_adj_chisq_cv_10
# plot(density(unlist(mse_mod_adj_chisq_cv_10)))
# plot(density(unlist(distcov_mod_adj_chisq_cv_10)))