#!/bin/bash

#SBATCH --job-name=grow_forests
#SBATCH --time=3-00:00:00
#SBATCH --partition=hpc

#SBATCH --ntasks=10
#SBATCH --mem-per-cpu=12G
#SBATCH --cpus-per-task=1
#SBATCH --mail-user=thomas.severe@mnhn.fr
#SBATCH --mail-type=END,FAIL

Rscript PCIA_RF_BMRE_evaluation.R -s $1 -t $2 -d $(date +%Y-%m-%d) -r $3 -k FALSE -p $4
