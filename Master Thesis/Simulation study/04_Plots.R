library(ggplot2)
library(gridExtra)
                  ###########################
                  # Script for making plots #
                  ###########################

plot1 <- results_mod_adj_mi_4 %>% ggplot(aes(x = mean_mse)) + geom_density() + xlim(0, .17)
plot2 <- results_mod_adj_mi_10 %>% ggplot(aes(x = mean_mse)) + geom_density() + xlim(0, .17)
plot3 <- results_mod_adj_mi_cv_4 %>% ggplot(aes(x = mean_mse)) + geom_density() + xlim(0, .17)
plot4 <- results_mod_adj_mi_cv_10 %>% ggplot(aes(x = mean_mse)) + geom_density() + xlim(0, .17)

grid.arrange(plot1, plot2, plot3, plot4, ncol=1)

                  
##############
# mod_adj_mi #
##############          

# Plots for mod_adj_mi_4                                    
plot_mod_adj_mi <- ggplot(results_mod_adj_mi_4)


plot(density(unlist(results_mod_adj_mi_4$mean_mse)))
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
