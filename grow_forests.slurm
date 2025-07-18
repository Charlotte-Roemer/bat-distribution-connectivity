#!/bin/bash

#SBATCH --job-name=grow_forests
#SBATCH --mem=150G
#SBATCH --partition=type_2
#SBATCH --time=7-12:00:00
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

Rscript PCIA_RF_BMRE_evaluation.R -s "Eptser" -t "50" -d $(date +%Y-%m-%d) -r "idf" -k FALSE 
