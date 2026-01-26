# bat-distribution-connectivity

A pipeline to extract environmental data and train and evaluate Random Forests to map bat
activity.

## Setup

Project path must be set up in variables.R under "project_path" variable name.
This path must be where this repo’s content is located on your computer.

### Environment

Those scripts are mostly relying on R libraries (and some Python), in order to
facilitate reproductibility, we recommend to install a [conda environment](https://www.anaconda.com/docs/getting-started/miniconda/install) from
the env_bmre.txt file by pasting the following lines in a linux terminal (we
dont ensure compatibility for other OS, you might have to look into the env_bmre
file and look for the correct libraries on the conda-forge channel) :

```bash
# cd bat-distribution-connectivity
conda install --name env_bmre --file env_bmre.txt
conda activate ~/envs/env_bmre # you can add this line to your .bashrc on in2p3cluster
conda activate env_bmre # if you didn’t add it to your .bashrc file
```

### Data

Data directory must be set up in variables.R file under "data_path" variable name.

#### Observation data

Observation data should be stored under {data_path}/observations.

Bat activity data comes from french museum of natural history’s citizen science
program [Vigie Chiro](https://www.vigienature.fr/fr/le-protocole-en-detail)
(protocole Point-Fixe),
please contact them in order to obtain the data.

#### Environmental data

Environmental data should be stored under {data_path}/GIS

Data description,sources and location within data_path will be detailed in the file sources_data_bmre.csv.

Modelisation is possible for three spatial areas (region name to call the script) :
- Metropolitan France (france_met)
- Europe (europe)
- Ile de France (idf)

Some data are region specific, others are common to all regions.

### Step by step routine

This repo works with three main scripts, in four steps :

- extract environmental data for bats observations locations
(PCIA_pipeline_GIS.R)
- train Random Forest / determine best parameters for each species
(PCIA_RF_BMRE_evaluation.R)
- extract environmental data on a regional grid to predict model for this
region (PCIA_pipeline_GIS.R)
- predict maps for each species (PCIA_predict_maps.R)



