setwd("/home3/s3363430/Thesis/Code")

library(snow)
library(parallel)

source("./SimulationRun.R")
source("./HelperFunctions.R")

second_observations <- readRDS("../Data/Output/SecondObservations.RData")
load("../Data/Input/InputSimulation.RData")
list2env(simulation_input, globalenv())

cpu <- Sys.getenv("SLURM_CPUS_ON_NODE", 1)
hosts <- rep("localhost", cpu)

cl <- makeCluster(hosts, type = "SOCK", outfile = "outlogs.txt")

clusterExport(cl, c("SafeSimulation", "SimulationRun", "IntroduceError_Random", 
                    "second_observations", "netwmatrices", "sex2", "controls", 
                    "simulation_element", "ChangeCoding"))


clusterEvalQ(cl, {library(igraph); library(RSiena)})

random_output <- parApply(cl, as.matrix(simulation_element), 1, function(simulation_el)
  SimulationRun(IntroduceError_Random, netwmatrices[[1]], second_observations[[simulation_el[1]]], sex2, controls, simulation_el))

saveRDS(random_output, "../Data/Output/01_Random_Raw_Output.RData")

stopCluster(cl)