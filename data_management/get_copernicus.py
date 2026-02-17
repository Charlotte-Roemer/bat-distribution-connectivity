from cdsetool.credentials import Credentials
from shapely import geometry
from shapely import to_wkt
from creds import *
from cdsetool.credentials import validate_credentials
from cdsetool.query import query_features
from cdsetool.query import describe_collection
from cdsetool.download import download_features


cred = Credentials()

validate_credentials(cred)

startDate = "2023-01-01T00:00:00Z"
completionDate = "2023-12-31T23:59:59Z"

# bbox_tuple = [10.47472, 34.64027, 44.82054, 71.18082]
# bbox_tuple = [-10.47472, 34.64027, 10.47472, 71.18082]
bbox_text = "-10.47472, 34.64027, 10.47472, 71.18082"

# bbox_polygon = geometry.box(*bbox_tuple)


search_filter = {
    # "startDate": startDate,
    # "completionDate": completionDate,
    "productType": "SAR_DTE_30_615C",
    "box": bbox_text
}

features = query_features("CCM", search_filter)


dl = download_features(features,
                       # "/sps/mnhn/tsevere/bmre/data/GIS/copernicus/data/dem",
                       "/home/tsevere/tmp/copernicus/data/dem",
                       {"credentials": cred})

list(dl)
