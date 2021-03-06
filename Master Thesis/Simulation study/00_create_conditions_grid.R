library(tidyverse)

replications <- 100
conditions_grid <- expand_grid(lambda = c(0.1, 0.3, 0.5), rho = c(0.1, 0.3, 0.5), delta = c(0.1, 0.3, 0.5), n = c(100, 200, 500))

write_rds(conditions_grid, path = "Simulation study/00_conditions.rds")