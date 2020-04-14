start.time <- Sys.time()

source("Simulation study/00_create_conditions_grid.R")
source("Simulation study/01_simulation.R")
source("Simulation study/02a_Modelling.R")
source("Simulation study/02b_Modelling.R")
source("Simulation study/03_Analysis.R")
source("Simulation study/04_Plots.R")

end.time <- Sys.time()
time.taken <- end.time - start.time

time.taken