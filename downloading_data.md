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

published from 2017 to 2023, no  deprecated
need to identify tile(s) to download and use download_vcf.R script.

Needs EOSDIS access
