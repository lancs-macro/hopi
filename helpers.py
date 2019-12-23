
from requests import get
from pandas.io.json import json_normalize 

def ukhp_get(release = "latest", frequency = "monthly", classification = "nuts1"):
  endpoint = "https://lancs-macro.github.io/uk-house-prices"
  query_elements = [endpoint, release, frequency, classification + ".json"]
  query = "/".join(query_elements)
  req = get(query)
  req_json = req.json()
  print(json_normalize(req_json))
  
ukhp_get()
