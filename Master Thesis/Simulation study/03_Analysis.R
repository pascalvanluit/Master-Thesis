library(xtable)
                ###################################
                # Script for analyzing MSE of POI #
                ###################################
set.seed(88)

results_mod_no_adj          <- read_rds("Simulation study/Results/02_results_mod_no_adj.rds")
results_mod_no_adj          <- results_mod_no_adj %>% mutate(method = "mod_no_adj")

results_mod_adj_mi_4        <- read_rds("Simulation study/Results/02_results_mod_adj_mi_4.rds")
results_mod_adj_mi_4        <- results_mod_adj_mi_4 %>% mutate(method = "mod_adj_mi_4")

results_mod_adj_mi_10       <- read_rds("Simulation study/Results/02_results_mod_adj_mi_10.rds")
results_mod_adj_mi_10       <- results_mod_adj_mi_10 %>% mutate(method = "mod_adj_mi_10")

results_mod_adj_mi_cv_4     <- read_rds("Simulation study/Results/02_results_mod_adj_mi_cv_4.rds")
results_mod_adj_mi_cv_4     <- results_mod_adj_mi_cv_4 %>% mutate(method = "mod_adj_mi_cv_4")

results_mod_adj_mi_cv_10    <- read_rds("Simulation study/Results/02_results_mod_adj_mi_cv_10.rds")
results_mod_adj_mi_cv_10    <- results_mod_adj_mi_cv_10 %>% mutate(method = "mod_adj_mi_cv_10")

results_mod_adj_chisq_cv_4  <- read_rds("Simulation study/Results/02_results_mod_adj_chisq_cv_4.rds")
results_mod_adj_chisq_cv_4  <- results_mod_adj_chisq_cv_4 %>% mutate(method = "mod_adj_chisq_cv_4")

results_mod_adj_chisq_cv_10 <- read_rds("Simulation study/Results/02_results_mod_adj_chisq_cv_10.rds")
results_mod_adj_chisq_cv_10 <- results_mod_adj_chisq_cv_10 %>% mutate(method = "mod_adj_chisq_cv_10")

# Combining all results:
all_results <- bind_rows(results_mod_adj_mi_4, results_mod_adj_mi_10, results_mod_adj_mi_cv_4, results_mod_adj_mi_cv_10, results_mod_adj_chisq_cv_4, results_mod_adj_chisq_cv_10)

# Making rds file of all results:
write_rds(all_results, path = "Simulation study/Results/all_results.rds")




# Adding a method column, and a ci column:



test_results_mod_no_adj <- results_mod_no_adj %>% mutate(method = "mod_no_adj", mse_ci = mean_mse - mse_ci_lower)

test_results_mod_no_adj %>% ggplot(aes(x = n, y = mean(mean_mse))) + geom_line() + geom_point() + geom_errorbar(aes(ymin = mean(mean_mse) - mean(mse_ci), ymax = mean(mean_mse) + mean(mse_ci)))


test_plot_data <- matrix(data = c(mean_100 = 0.08429432, mean_200 = 0.08120053, mean_500 = 0.07417991, ci_100 = 0.00408948, ci_200 = 0.00383713, ci_500 = 0.003460868), nrow = 3)
colnames(test_plot_data) <- c("mean", "ci" )
rownames(test_plot_data) <- c("100", "200", "500")




all_results <- bind_rows(results_mod_adj_mi_4, results_mod_adj_mi_10, results_mod_adj_mi_cv_4, results_mod_adj_mi_cv_10, results_mod_adj_chisq_cv_4, results_mod_adj_chisq_cv_10)

# Making rds file of all results:
write_rds(all_results, path = "Simulation study/Results/all_results.rds")

results_by_n <- all_results
results_by_n %>% ggplot(mapping = aes(x = n))

# Making a plot for only mod_no_adj
# n on x-axis, mean_mse on y-axis

results_mod_no_adj %>% ggplot(aes(x = n, y = mean_mse)) + geom_line() + geom_point() + geom_errorbar()







