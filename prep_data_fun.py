import pandas as pd
import geopandas as gpd
import datetime as dt
import calendar as cal
import numpy as np
import openmeteo_requests
import requests_cache
from retry_requests import retry
import time as tps
import os
import time as tps
import math
from _vars import meteo_api_key

# Setup the Open-Meteo API client with cache and retry on error
cache_session = requests_cache.CachedSession('.cache', expire_after=-1)
retry_session = retry(cache_session, retries=5, backoff_factor=0.2)
openmeteo = openmeteo_requests.Client(session=retry_session)


def get_open_weather(in_gpdDF, monthly=False):
    """
    Interrogates open-meteo.com api to add meteoroligical values to a
    Geodataframe.
    It is actually set up for bat recordings. The data is retrieved for the
    night from sunset to sunrise.
    Parameters
    ----------
    in_gpdDF : GeodataFrame
        with geometry WGS84, date_nuit fields
    Returns
    ----------
    in_gpdDF : GeoDataFrame
        Updated with min, max, mean temperatures
        min, max, mean wind and total precipitations.
    """
    url = "https://archive-api.open-meteo.com/v1/archive"
    in_gpdDF = in_gpdDF.reset_index(drop=True)  # if in_gpdDF is a subset, 'i'
    # wont start at 0 in iterrows unless reindexed
    in_gpdDF['Spmean_temp'] = pd.Series(dtype='float')
    in_gpdDF['Spmin_temp'] = pd.Series(dtype='float')
    in_gpdDF['Spmax_temp'] = pd.Series(dtype='float')
    in_gpdDF['Spmean_wind'] = pd.Series(dtype='float')
    in_gpdDF['Spmin_wind'] = pd.Series(dtype='float')
    in_gpdDF['Spmax_wind'] = pd.Series(dtype='float')
    in_gpdDF['Sptotal_precipitations'] = pd.Series(dtype='float')
    if monthly:
        for j, row in in_gpdDF.iterrows():
            date = row.Nuit
            annee = date.strftime('%Y')
            mois = date.strftime('%m')
            dernier_jour = cal.monthrange(int(annee), int(mois))[1]
            date_fin = f"{annee}-{mois}-{str(dernier_jour)}"
            date_debut = f"{annee}-{mois}-01"
            params = {
                "latitude": row.geometry.y,
                "longitude": row.geometry.x,
                "start_date": date_debut,
                "end_date": date_fin,
                "hourly": ["temperature_2m", "wind_speed_10m", "wind_direction_10m",
                           "is_day", "precipitation"],
                "timezone": "Europe/Berlin"
            }
            response = openmeteo.weather_api(url, params)
            time_zone_delta = response[0].UtcOffsetSeconds()
            # in next lines variable numbers depend
            hourly = response[0].Hourly()
            # on params order
            hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()
            hourly_wind_speed_10m = hourly.Variables(1).ValuesAsNumpy()
            hourly_wind_direction_10m = hourly.Variables(2).ValuesAsNumpy()
            hourly_is_day = hourly.Variables(3).ValuesAsNumpy()
            precipitations = hourly.Variables(4).ValuesAsNumpy()
            hourly_data = {"date": pd.date_range(
                start=pd.to_datetime(
                    hourly.Time() + time_zone_delta, unit="s"),
                end=pd.to_datetime(hourly.TimeEnd() +
                                   time_zone_delta, unit="s"),
                freq=pd.Timedelta(seconds=hourly.Interval()),
                inclusive="left"
            )}
            hourly_data["temperature_2m"] = hourly_temperature_2m
            hourly_data["hourly_wind_speed_10m"] = hourly_wind_speed_10m
            hourly_data["hourly_wind_direction_10m"] = hourly_wind_direction_10m
            hourly_data["is_day"] = hourly_is_day
            hourly_data["precipitations"] = precipitations
            hourly_dataframe = pd.DataFrame(data=hourly_data)
            night_centered_dataframe = hourly_dataframe[hourly_dataframe.is_day == 0]
            night_centered_dataframe["hour"] = night_centered_dataframe.date.dt.hour
            night_centered_dataframe['date_nuit'] = np.where(night_centered_dataframe.date.dt.hour > 12,
                                                             night_centered_dataframe.date,
                                                             night_centered_dataframe.date - dt.timedelta(days=1))
            night_centered_dataframe["month"] = night_centered_dataframe.date_nuit.dt.month
            night_centered_dataframe.date_nuit = night_centered_dataframe.date_nuit.dt.date
            night_centered_dataframe = night_centered_dataframe[night_centered_dataframe.month == int(
                mois)]
            mean_temp = night_centered_dataframe.groupby(
                "date_nuit").temperature_2m.mean().median()
            max_temp = night_centered_dataframe.groupby(
                "date_nuit").temperature_2m.max().median()
            min_temp = night_centered_dataframe.groupby(
                "date_nuit").temperature_2m.min().median()
            max_wind = night_centered_dataframe.groupby(
                "date_nuit").hourly_wind_speed_10m.max().median()
            min_wind = night_centered_dataframe.groupby(
                "date_nuit").hourly_wind_speed_10m.min().median()
            mean_wind = night_centered_dataframe.groupby(
                "date_nuit").hourly_wind_speed_10m.mean().median()
            total_precipitations = night_centered_dataframe.groupby(
                "date_nuit").precipitations.sum().median()
            in_gpdDF.loc[j, ("Spmean_temp")] = mean_temp
            in_gpdDF.loc[j, ("Spmax_temp")] = max_temp
            in_gpdDF.loc[j, ("Spmin_temp")] = min_temp
            in_gpdDF.loc[j, ("Spmin_wind")] = min_wind
            in_gpdDF.loc[j, ("Spmax_wind")] = max_wind
            in_gpdDF.loc[j, ("Spmean_wind")] = mean_wind
            in_gpdDF.loc[j, ("Sptotal_precipitations")] = total_precipitations
    else:
        for j, row in in_gpdDF.iterrows():
            date = row.Nuit
            fin_nuit = date + dt.timedelta(days=1)
            date = date.strftime('%Y-%m-%d')
            fin_nuit = fin_nuit.strftime('%Y-%m-%d')
            params = {
                "latitude": row.geometry.y,
                "longitude": row.geometry.x,
                "start_date": date,
                "end_date": fin_nuit,
                "hourly": ["temperature_2m", "wind_speed_10m", "wind_direction_10m",
                           "is_day", "precipitation"],
                "timezone": "Europe/Berlin"
            }
            response = openmeteo.weather_api(url, params)
            time_zone_delta = response[0].UtcOffsetSeconds()
            # in next lines variable numbers depend
            hourly = response[0].Hourly()
            # on params order
            hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()
            hourly_wind_speed_10m = hourly.Variables(1).ValuesAsNumpy()
            hourly_wind_direction_10m = hourly.Variables(2).ValuesAsNumpy()
            hourly_is_day = hourly.Variables(3).ValuesAsNumpy()
            precipitations = hourly.Variables(4).ValuesAsNumpy()
            hourly_data = {"date": pd.date_range(
                start=pd.to_datetime(
                    hourly.Time() + time_zone_delta, unit="s"),
                end=pd.to_datetime(hourly.TimeEnd() +
                                   time_zone_delta, unit="s"),
                freq=pd.Timedelta(seconds=hourly.Interval()),
                inclusive="left"
            )}
            hourly_data["temperature_2m"] = hourly_temperature_2m
            hourly_data["hourly_wind_speed_10m"] = hourly_wind_speed_10m
            hourly_data["hourly_wind_direction_10m"] = hourly_wind_direction_10m
            hourly_data["is_day"] = hourly_is_day
            hourly_data["precipitations"] = precipitations
            hourly_dataframe = pd.DataFrame(data=hourly_data)
            night_centered_dataframe = hourly_dataframe[hourly_dataframe.is_day == 0]
            night_centered_dataframe = night_centered_dataframe[((night_centered_dataframe.date.dt.strftime('%Y-%m-%d')
                                                                  == str(date)) &
                                                                 (night_centered_dataframe.date.dt.strftime('%H').astype(int)
                                                                  > 12)) |
                                                                ((night_centered_dataframe.date.dt.strftime('%Y-%m-%d')
                                                                  == str(fin_nuit)) &
                                                                 (night_centered_dataframe.date.dt.strftime('%H').astype(int)
                                                                    < 12))]
            mean_temp = night_centered_dataframe.temperature_2m.mean()
            max_temp = night_centered_dataframe.temperature_2m.max()
            min_temp = night_centered_dataframe.temperature_2m.min()
            max_wind = night_centered_dataframe.hourly_wind_speed_10m.max()
            min_wind = night_centered_dataframe.hourly_wind_speed_10m.min()
            mean_wind = night_centered_dataframe.hourly_wind_speed_10m.mean()
            total_precipitations = night_centered_dataframe.precipitations.sum()
            in_gpdDF.loc[j, ("Spmean_temp")] = mean_temp
            in_gpdDF.loc[j, ("Spmax_temp")] = max_temp
            in_gpdDF.loc[j, ("Spmin_temp")] = min_temp
            in_gpdDF.loc[j, ("Spmin_wind")] = min_wind
            in_gpdDF.loc[j, ("Spmax_wind")] = max_wind
            in_gpdDF.loc[j, ("Spmean_wind")] = mean_wind
            in_gpdDF.loc[j, ("Sptotal_precipitations")] = total_precipitations
    return in_gpdDF


def get_open_weather_api_key(in_gpdDF, monthly=False):
    """
    Interrogates open-meteo.com api to add meteoroligical values to a
    Geodataframe.
    It is actually set up for bat recordings. The data is retrieved for the
    night from sunset to sunrise.
    Parameters
    ----------
    in_gpdDF : GeodataFrame
        with geometry WGS84, date_nuit fields
    Returns
    ----------
    in_gpdDF : GeoDataFrame
        Updated with min, max, mean temperatures
        min, max, mean wind and total precipitations.
    """
    url = "https://customer-archive-api.open-meteo.com/v1/archive"
    in_gpdDF = in_gpdDF.reset_index(drop=True)  # if in_gpdDF is a subset, 'i'
    # wont start at 0 in iterrows unless reindexed
    in_gpdDF['Spmean_temp'] = pd.Series(dtype='float')
    in_gpdDF['Spmin_temp'] = pd.Series(dtype='float')
    in_gpdDF['Spmax_temp'] = pd.Series(dtype='float')
    in_gpdDF['Spmean_wind'] = pd.Series(dtype='float')
    in_gpdDF['Spmin_wind'] = pd.Series(dtype='float')
    in_gpdDF['Spmax_wind'] = pd.Series(dtype='float')
    in_gpdDF['Sptotal_precipitations'] = pd.Series(dtype='float')
    nrow = in_gpdDF.shape[0]
    if monthly:
        for j, row in in_gpdDF.iterrows():
            print(f"Getting meteo {j + 1} / {nrow}")
            date = row.Nuit
            annee = date.strftime('%Y')
            mois = date.strftime('%m')
            dernier_jour = cal.monthrange(int(annee), int(mois))[1]
            date_fin = f"{annee}-{mois}-{str(dernier_jour)}"
            date_debut = f"{annee}-{mois}-01"
            params = {
                "latitude": row.geometry.y,
                "longitude": row.geometry.x,
                "start_date": date_debut,
                "end_date": date_fin,
                "hourly": ["temperature_2m", "wind_speed_10m", "wind_direction_10m",
                           "is_day", "precipitation"],
                "timezone": "Europe/Berlin",
                "apikey": meteo_api_key
            }
            response = openmeteo.weather_api(url, params)
            time_zone_delta = response[0].UtcOffsetSeconds()
            # in next lines variable numbers depend
            hourly = response[0].Hourly()
            # on params order
            hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()
            hourly_wind_speed_10m = hourly.Variables(1).ValuesAsNumpy()
            hourly_wind_direction_10m = hourly.Variables(2).ValuesAsNumpy()
            hourly_is_day = hourly.Variables(3).ValuesAsNumpy()
            precipitations = hourly.Variables(4).ValuesAsNumpy()
            hourly_data = {"date": pd.date_range(
                start=pd.to_datetime(
                    hourly.Time() + time_zone_delta, unit="s"),
                end=pd.to_datetime(hourly.TimeEnd() +
                                   time_zone_delta, unit="s"),
                freq=pd.Timedelta(seconds=hourly.Interval()),
                inclusive="left"
            )}
            hourly_data["temperature_2m"] = hourly_temperature_2m
            hourly_data["hourly_wind_speed_10m"] = hourly_wind_speed_10m
            hourly_data["hourly_wind_direction_10m"] = hourly_wind_direction_10m
            hourly_data["is_day"] = hourly_is_day
            hourly_data["precipitations"] = precipitations
            hourly_dataframe = pd.DataFrame(data=hourly_data)
            night_centered_dataframe = hourly_dataframe[hourly_dataframe.is_day == 0]
            night_centered_dataframe["hour"] = night_centered_dataframe.date.dt.hour
            night_centered_dataframe['date_nuit'] = np.where(night_centered_dataframe.date.dt.hour > 12,
                                                             night_centered_dataframe.date,
                                                             night_centered_dataframe.date - dt.timedelta(days=1))
            night_centered_dataframe["month"] = night_centered_dataframe.date_nuit.dt.month
            night_centered_dataframe.date_nuit = night_centered_dataframe.date_nuit.dt.date
            night_centered_dataframe = night_centered_dataframe[night_centered_dataframe.month == int(
                mois)]
            mean_temp = night_centered_dataframe.groupby(
                "date_nuit").temperature_2m.mean().median()
            max_temp = night_centered_dataframe.groupby(
                "date_nuit").temperature_2m.max().median()
            min_temp = night_centered_dataframe.groupby(
                "date_nuit").temperature_2m.min().median()
            max_wind = night_centered_dataframe.groupby(
                "date_nuit").hourly_wind_speed_10m.max().median()
            min_wind = night_centered_dataframe.groupby(
                "date_nuit").hourly_wind_speed_10m.min().median()
            mean_wind = night_centered_dataframe.groupby(
                "date_nuit").hourly_wind_speed_10m.mean().median()
            total_precipitations = night_centered_dataframe.groupby(
                "date_nuit").precipitations.sum().median()
            in_gpdDF.loc[j, ("Spmean_temp")] = mean_temp
            in_gpdDF.loc[j, ("Spmax_temp")] = max_temp
            in_gpdDF.loc[j, ("Spmin_temp")] = min_temp
            in_gpdDF.loc[j, ("Spmin_wind")] = min_wind
            in_gpdDF.loc[j, ("Spmax_wind")] = max_wind
            in_gpdDF.loc[j, ("Spmean_wind")] = mean_wind
            in_gpdDF.loc[j, ("Sptotal_precipitations")] = total_precipitations
    else:
        for j, row in in_gpdDF.iterrows():
            print(f"Getting meteo {j + 1} / {nrow}")
            date = row.Nuit
            fin_nuit = date + dt.timedelta(days=1)
            date = date.strftime('%Y-%m-%d')
            fin_nuit = fin_nuit.strftime('%Y-%m-%d')
            params = {
                "latitude": row.geometry.y,
                "longitude": row.geometry.x,
                "start_date": date,
                "end_date": fin_nuit,
                "hourly": ["temperature_2m", "wind_speed_10m", "wind_direction_10m",
                           "is_day", "precipitation"],
                "timezone": "Europe/Berlin",
                "apikey": meteo_api_key
            }
            response = openmeteo.weather_api(url, params)
            time_zone_delta = response[0].UtcOffsetSeconds()
            # in next lines variable numbers depend
            hourly = response[0].Hourly()
            # on params order
            hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()
            hourly_wind_speed_10m = hourly.Variables(1).ValuesAsNumpy()
            # hourly_wind_direction_10m = hourly.Variables(2).ValuesAsNumpy()
            hourly_is_day = hourly.Variables(3).ValuesAsNumpy()
            precipitations = hourly.Variables(4).ValuesAsNumpy()
            hourly_data = {"date": pd.date_range(
                start=pd.to_datetime(
                    hourly.Time() + time_zone_delta, unit="s"),
                end=pd.to_datetime(hourly.TimeEnd() +
                                   time_zone_delta, unit="s"),
                freq=pd.Timedelta(seconds=hourly.Interval()),
                inclusive="left"
            )}
            hourly_data["temperature_2m"] = hourly_temperature_2m
            hourly_data["hourly_wind_speed_10m"] = hourly_wind_speed_10m
            # hourly_data["hourly_wind_direction_10m"] = hourly_wind_direction_10m
            # hourly_data["is_day"] = hourly_is_day
            hourly_data["precipitations"] = precipitations
            hourly_dataframe = pd.DataFrame(data=hourly_data)
            # night_centered_dataframe = hourly_dataframe[hourly_dataframe.is_day == 0]
            night_centered_dataframe = night_centered_dataframe[((night_centered_dataframe.date.dt.strftime('%Y-%m-%d')
                                                                  == str(date)) &
                                                                 (night_centered_dataframe.date.dt.strftime('%H').astype(int)
                                                                  > 12)) |
                                                                ((night_centered_dataframe.date.dt.strftime('%Y-%m-%d')
                                                                  == str(fin_nuit)) &
                                                                 (night_centered_dataframe.date.dt.strftime('%H').astype(int)
                                                                    <= 12))]
            mean_temp = night_centered_dataframe.temperature_2m.mean()
            # max_temp = night_centered_dataframe.temperature_2m.max()
            # min_temp = night_centered_dataframe.temperature_2m.min()
            # max_wind = night_centered_dataframe.hourly_wind_speed_10m.max()
            # min_wind = night_centered_dataframe.hourly_wind_speed_10m.min()
            mean_wind = night_centered_dataframe.hourly_wind_speed_10m.mean()
            total_precipitations = night_centered_dataframe.precipitations.sum()
            in_gpdDF.loc[j, ("Spmean_temp")] = mean_temp
            # in_gpdDF.loc[j, ("Spmax_temp")] = max_temp
            # in_gpdDF.loc[j, ("Spmin_temp")] = min_temp
            # in_gpdDF.loc[j, ("Spmin_wind")] = min_wind
            # in_gpdDF.loc[j, ("Spmax_wind")] = max_wind
            in_gpdDF.loc[j, ("Spmean_wind")] = mean_wind
            in_gpdDF.loc[j, ("Sptotal_precipitations")] = total_precipitations
    return in_gpdDF


def countdown(t):
    while t:
        mins, secs = divmod(t, 60)
        timer = 'limit number of requests reached, please wait {:02d}:{:02d}'.format(
            mins, secs)
        print(timer, end="\r")
        tps.sleep(1)
        t -= 1


def split_get(data, max, m=False):
    """ 
    Splits 'data' geodataframe in blocks of 'max' length to call get_open_weather
    whilst respecting the API maximun calls (600/min).
    Parameters
    ----------
    data: GeoDataFrame 
        wgs84 with 'Nuit' date column
    max: int 
        maximum amount of API Calls/min
    Returns
    ----------
    Geodataframe
    """
    data_list = []
    nbd = math.ceil(len(data) / 10000)
    for iii in range(nbd):
        if iii > 0:
            countdown(86410)
        jetond = iii + 1
        startd = iii * 10000
        stopd = jetond * 10000
        splitd = data.iloc[startd:stopd]
        nbh = math.ceil(len(splitd) / 5000)
        for ii in range(nbh):
            if iii > 0 or ii > 0:
                countdown(3610)
            jetonh = ii + 1
            starth = ii * 5000
            stoph = jetonh * 5000
            splith = splitd.iloc[starth:stoph]
            nb = math.ceil(len(splith) / max)
            for i in range(nb):
                if iii > 0 or ii > 0 or i > 0:
                    countdown(70)
                jeton = i + 1
                start = i * max
                stop = jeton * max
                split = splith.iloc[start:stop]
                print(
                    f"getting weather...                                        ", end="\r")
                split_weathered = get_open_weather(split, monthly=m)
                data_list.append(split_weathered)
    return pd.concat(data_list)
