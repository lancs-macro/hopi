
# uk-house-prices

## Releases

The uk-house-prices is updated on a quarterly basis.


## CSV Data

if you want to download the data in CSV format you can see the latest release in the [data directory](https://github.com/lancs-macro/uk-house-prices/tree/master/data/latest). 

# API 

You can also take advantage of the static (read-only) API on github.

## Endpoint 

`endpoint`: https://lancs-macro.github.io/uk-house-prices/

##  Parameters

GET overview/{overview}

* `ove

GET releases/{release}/{frequency}/{classification}/

* `release`: "latest" for the latest release, otherwise the data (YYYY-MM) of the release (e.g. 2019-09) 

* `frequency`: one of "annual", "quarterly" or "monthly"

* `classification`: one of "aggregate", "nuts1", "nuts2" or "nuts3"

GET archives/{archive}

* `archive`: "latest" for the latest archive

# R 

This is a simple example to use the API with R to fetch monthly data of nuts1 classification:

```r
library(jsonlite)
library(httr)

ukhp_get <- function(frequency = "monthly", classification = "nuts1", release = "latest") {
  endpoint <- "https://lancs-macro.github.io/uk-house-prices"
  query <- paste(endpoint, release, frequency, paste0(classification, ".json"), sep = "/")
  request <- GET(query)
  stop_for_status(request)
  parse_json(request, simplifyVector = TRUE)
} 
ukhp_get()
```