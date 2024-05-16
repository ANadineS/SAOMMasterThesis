#!/bin/bash
#SBATCH --job-name = random_error_1
#SBATCH --output = job-%j.log
#SBATCH --time = 15:00:00
#SBATCH --nodes = 4
#SBATCH --ntasks = 12000
#SBATCH --mem = 1000

module purge
module load R/4.3.2-gfbf-2023a

srun ${EBROOTR}/lib64/R/library/snow/RMPISNOW < ExecuteSimulation.R