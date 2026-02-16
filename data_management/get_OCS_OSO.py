# %%
import geopandas
from pygeodes import Geodes
from pygeodes.utils.formatting import format_items
from pygeodes import Config
import json
import os
import re
import zipfile

geodes = Geodes()
conf = Config.from_file("config_geodes.json")

geodes.set_conf(conf)


bbox = [-13, 34, 40, 71]

items, dataframe = geodes.search_items(
    bbox=bbox, collections=["THEIA_OSO_RASTER_L3B"], get_all=True)

for item in items:
    item.download_archive()
with open('config_geodes.json', 'r') as file:
    data = json.load(file)
    dir = data['download_dir']
    files = [os.path.join(dir, f)
             for f in os.listdir(dir) if f.endswith('.zip')]

for file in files:
    with zipfile.ZipFile(file, 'r')as zipped_file:
        for info in zipped_file.infolist():
            if re.match(r'^DATA/OCS_[0-9]{4}_LZW.tif', info.filename):
                print(info.filename)
                zipped_file.extract(info, path=dir)
