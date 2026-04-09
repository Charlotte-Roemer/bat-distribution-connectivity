#!/bin/bash

#SBATCH --job-name=grow_forests
#SBATCH --time=05:00:00
#SBATCH --partition=htc

#SBATCH --mem-per-cpu=100G
#SBATCH --cpus-per-task=1
#SBATCH --mail-user=charlotte.roemer@mnhn.fr
#SBATCH --mail-type=END,FAIL

conda init
conda activate envs/bmre
Rscript offshore_maps.R
