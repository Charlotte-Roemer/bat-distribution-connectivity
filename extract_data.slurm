#!/bin/bash

#SBATCH --job-name=extract_data_bmre
#SBATCH --mem=100G
#SBATCH --partition=type_2
#SBATCH --time=15-00:00:00
#SBATCH --exclude=node-08

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

Rscript PCIA_Pipeline_GIS.R -r france_met -s 500 -m "predict" -d '2024-06-08' 
