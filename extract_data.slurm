#!/bin/bash

#SBATCH --job-name=extract_data_bmre
#SBATCH --mem=150G
#SBATCH --cpus-per-task=2
#SBATCH --partition=hpc
#SBATCH --time=5-00:00:00
#SBATCH --mail-user=charlotte.roemer@mnhn.fr
#SBATCH --mail-type=END,FAIL

source ~/.bashrc
conda activate ~/envs/bmre
#Rscript Pipeline_GIS.R -r $1 -m $2 -s ${3:-''}  -d ${4:-''}
Rscript Pipeline_GIS.R \
  -r "$1" \
  -m "$2" \
  ${3:+-s "$3"} \
  ${4:+-d "$4"}
