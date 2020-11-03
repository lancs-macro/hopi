

from pandas import pd

def ukhp_get(release = "latest", frequency = "monthly", classification = "nuts1"):
  endpoint = "https://lancs-macro.github.io/uk-house-prices"
  query_elements = [endpoint, release, frequency, classification + ".json"]
  query = "/".join(query_elements)
  print(pd.read_csv(query))
  
ukhp_get()
