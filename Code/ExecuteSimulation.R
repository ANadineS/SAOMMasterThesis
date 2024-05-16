setwd(dirname(sys.frame(1)$ofile))

library(snow)
library(parallel)

source("./SimulationRun.R")
source("./HelperFunctions.R")

second_observations <- readRDS("../Data/Output/SecondObservations.RData")
load("../Data/Input/InputSimulation.RData")

cl <- getMPIcluster(outfile = "output.txt")
clusterExport(cl, c("SafeSimulation", "SimulationRun", "IntroduceError_Random", 
                    "second_observations", "netwmatrices", "sex2", "controls", 
                    "simulation_element", "ChangeCoding"))

clusterEvalQ(cl, {library(igraph); library(RSiena)})

random_output <- parApply(cl, MARGIN = 1, X = as.matrix(simulation_element), FUN = SafeSimulation)

stopCluster(cl)

saveRDS(random_output, "../Data/Output/01_Random_Raw_Output.RData")