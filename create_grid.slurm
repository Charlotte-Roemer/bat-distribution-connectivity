#!/bin/bash

#SBATCH --job-name=create_empty_grid
#SBATCH --mem-per-cpu=12G
#SBATCH --partition=htc
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=10



#SBATCH --time=3-00:00:00
#SBATCH --mail-user=charlotte.roemer@mnhn.fr
#SBATCH --mail-type=BEGIN,END   

Rscript data_management/Create_empty_grid_V2.R -r $1 -s $2
