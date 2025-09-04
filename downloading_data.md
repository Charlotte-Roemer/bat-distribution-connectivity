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

