#!/bin/bash

#SBATCH --job-name=grow_forests_with_barbastelles
#SBATCH --mem=1200G
#SBATCH --partition=high_mem
#SBATCH --time=3-00:00:00
#SBATCH --cpus-per-task=10

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

Rscript PCIA_RF_BMRE_evaluation.R  -s "Barbar"
