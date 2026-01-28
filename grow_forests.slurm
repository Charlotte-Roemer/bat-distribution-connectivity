#!/bin/bash

#SBATCH --job-name=grow_forests
#SBATCH --time=05:00:00
#SBATCH --partition=htc

#SBATCH --ntasks=10
#SBATCH --mem-per-cpu=12G
#SBATCH --cpus-per-task=1
#SBATCH --mail-user=thomas.severe@mnhn.fr
#SBATCH --mail-type=END,FAIL

Rscript PCIA_RF_BMRE_evaluation.R -s $1 -t $2 -d $8 -r $3 -k FALSE -p $4 --variableselection $5 --acti $6 --data_sel $7
