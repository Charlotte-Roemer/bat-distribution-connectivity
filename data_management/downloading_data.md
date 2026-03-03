## Artificial Light At Night

Recently it has become mandatory to have an account to download freely the open
data from the earth observation group. It is possible to create access [there](https://eogauth-new.mines.edu/realms/eog/login-actions/registration?client_id=eogdata-new-apache&tab_id=lovsgbqu11k&client_data=eyJydSI6Imh0dHBzOi8vZW9nZGF0YS5taW5lcy5lZHUvb2F1dGgyY2FsbGJhY2siLCJydCI6ImNvZGUiLCJzdCI6InI5Q2lZMHQtZV9YcGdSbzBCVi0zSmMxeW9zZyJ9)

Once registered, data can be accessed there :

- [from 2012 to 2021](https://eogdata.mines.edu/nighttime_light/annual/v21/)
- [from 2022 to 2024](https://eogdata.mines.edu/nighttime_light/annual/v22/)

I tend to use the median masked raster, but there is also average (masked or not) minumum and maximum.

These data once unziped are quite big, it might be worth it to crop them to your
AOI before sending them to in2p3.

## MODIS VCF

S2 is needed to install modisfast, installing it with apt was easier to me :

```bash
sudo apt install r-cran-s2
```
```R
devtools::install_github("ptaconet/modisfast")
# as of today (2025-09-04 VCF is accessible only through the development version)
```
If this command asks for S2 updates, you can probably avoid it and select "none"
option.

Needs EOSDIS access

Then I made a script "download_modis" to get the rasters.
It will download them with name format "MOD44B.061.2023.europe.tif" for yearly
data. EPSG is 4326 but can be set differently

## BD ALTI (elevation)

Can be downloaded using download_alti.R script

## BD TOPAGE 2023

water data to be downloaded [there](https://www.data.gouv.fr/api/1/datasets/r/ebbcbe0f-aabe-4c19-9a46-93ad55990223)

## Corine Land Cover (landuse)

Can be downloaded [there](https://land.copernicus.eu/en/products/corine-land-cover?tab=datasets), only 2012 and 2018 are needed.


## OSO Land Cover

Should eventualy appear [there](https://catalogue.theia.data-terra.org/).

## Routes 500 (french Roads)

Can be downloaded [there](https://data.geopf.fr/telechargement/download/ROUTE500/ROUTE500_3-0__SHP_LAMB93_FXX_2021-11-03/ROUTE500_3-0__SHP_LAMB93_FXX_2021-11-03.7z).


## World clim and normals

can be downloaded [there](https://envicloud.wsl.ch/#/?bucket=https%3A%2F%2Fos.zhdk.cloud.switch.ch%2Fchelsav2%2F&prefix=%2F)

## Wind data

Can be downloaded [there](https://globalwindatlas.info/en/download/gis-files)
and then cropped to europe for limiting stockage.

