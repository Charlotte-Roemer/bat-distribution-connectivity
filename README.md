# bat-distribution-connectivity

A pipeline to extract environmental data and train and evaluate Random Forests to map bat
activity.

## Setup

### Environment

Those scripts are mostly relying on R libraries (and some Python), in order to
facilitate reproductibility, we recommend to install a [conda environment](https://www.anaconda.com/docs/getting-started/miniconda/install) from
the env_bmre.txt file by pasting the following line in a terminal :

```bash
# cd bat-distribution-connectivity
conda install --name env_bmre --file env_bmre.txt
```

### Data

#### Observation data

Bat activity data comes from french museum of natural history’s citizen science
program [Vigie Chiro](https://www.vigienature.fr/fr/le-protocole-en-detail)
(protocole Point-Fixe),
please contact them in order to obtain the data.

#### Environmental data

Data description and sources will be detailed in the file sources_data_bmre.csv.

