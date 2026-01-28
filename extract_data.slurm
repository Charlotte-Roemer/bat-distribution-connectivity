#!/bin/bash

#SBATCH --job-name=extract_data_bmre
#SBATCH --mem=20G
#SBATCH --cpus-per-task=2
#SBATCH --partition=htc
#SBATCH --time=5-00:00:00
#SBATCH --mail-user=thomas.severe@mnhn.fr
#SBATCH --mail-type=END,FAIL

Rscript Pipeline_GIS.R -r "france_met" -m "predict" -s $1 -d $2
