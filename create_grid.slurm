#!/bin/bash

#SBATCH --job-name=create_empty_grid
#SBATCH --mem=100G
#SBATCH --partition=type_2
#SBATCH --time=3-00:00:00
#SBATCH --mail-user=thomas.severe@mnhn.fr
#SBATCH --mail-type=BEGIN,END   

module load userspace/tr17.10
module load biology
module load gcc/7.2.0        
module load lapack/3.7.1
module load blas/gcc72/3.7.1
module load jags/gcc72/4.3.0
module load proj/8.0.0     
module load geos/3.11.2
module load gdal
module load udunits
module load R/4.3.1

Rscript Create_empty_grid_V2.R -r france_met -s 500
