#!/bin/bash

#SBATCH --job-name=grow_forests
#SBATCH --output=%j-$1-$2-$3.out
#SBATCH --mem=20G
#SBATCH --time=3-00:00:00
#SBATCH --cpus-per-task=10
#SBATCH --mail-user=thomas.severe@mnhn.fr
#SBATCH --mail-type=END,FAIL

Rscript PCIA_RF_BMRE_evaluation.R -s $1 -t $2 -d $(date +%Y-%m-%d) -r $3 -k FALSE 
