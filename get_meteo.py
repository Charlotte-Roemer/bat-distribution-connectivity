import pandas as pd
import geopandas as gpd
import datetime as dt
import calendar as cal
import numpy as np
import openmeteo_requests
import requests_cache
from retry_requests import retry
import time as tps
import glob
import os
import math
import prep_data_fun as fun
import sys
import argparse
pd.set_option("mode.chained_assignment", None)

# csv_files = glob.glob("../data/observations/Data*.csv")


# filelist = []

# for file in csv_files:
# df = pd.read_csv(file, sep=';',  encoding='latin1',
# dtype={"nb_concats_nd": int,
# "confiance_validateur": str,
# "confiance_observateur": str},
# parse_dates=["Nuit"])
# filelist.append(df)

main_parser = argparse.ArgumentParser()
main_parser.add_argument("--mode",
                         help=""""Indicate if you want to get data for training
                            (based on observation data) or predicting""",
                         type=str, required=True,
                         choices=["train", "predict"])
main_parser.add_argument("--file",
                         help="""Location of csv file""",
                         required=True, type=str)
main_parser.add_argument("--date", help="""Set date for prediction (format:
                                        YYYY-mm-dd)""",
                         type=str, required=False)

main_args, _ = main_parser.parse_known_args()

# date_parser = argparse.ArgumentParser(parents=[main_parser])
# if main_args.mode == "predict":
# date_parser.add_argument("-d", "--date", help="""Set date for prediction (format:
# YYYY-mm-dd)""",
# type=str, required=True)

# date_args = date_parser.parse_args()

csv_file = main_args.file
filename_ext = os.path.basename(csv_file)
filename = os.path.splitext(filename_ext)[0]
csv_folder = os.path.dirname(csv_file)
date = main_args.date
# csv_file = "../data/dependances_fixes/loc_train.csv"
if main_args.mode == "train":
    observations = pd.read_csv(csv_file, sep=",", parse_dates=["Nuit"])
    monthly = False
else:
    observations = pd.read_csv(csv_file, sep=",")
    observations["Nuit"] = dt.datetime.strptime(
        f"{main_args.date}", '%Y-%m-%d')
    monthly = True


data = gpd.GeoDataFrame(
    observations,
    geometry=gpd.points_from_xy(observations.X,
                                observations.Y),
    crs="EPSG:4326"
)

# in no apikey
# data_weathered = fun.split_get(data, 500, m=monthly)

data_weathered = fun.get_open_weather_api_key(data, monthly=monthly)

data_tab = pd.DataFrame(data_weathered.drop(columns=['geometry'
                                                     ]))

filename_meteo = filename + "_meteo.csv"

data_tab.to_csv(os.path.join(csv_folder, filename_meteo),
                sep=",")


# # plateforme de tests

# url = "https://archive-api.open-meteo.com/v1/archive"

# df = pd.DataFrame(columns=['X', 'Y', 'Nuit'])

# df = df.append({'X': 6.15, 'Y': 44.3, 'Nuit':
# dt.datetime.strptime('2023-06-17', "%Y-%m-%d")}, ignore_index=True)

# in_gpdDF = gpd.GeoDataFrame(
# df,
# geometry=gpd.points_from_xy(df.X,
# df.Y),
# crs="EPSG:4326"
# )
