library(tidyverse)

conditions_grid <- expand_grid(lambda = c(0.1, 0.7), rho = c(0.1), delta = c(0.1, 0.7), n = c(100, 200, 500, 1000))

write_rds(conditions_grid, path = "Simulation study/00_conditions.rds")
