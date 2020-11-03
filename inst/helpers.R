library(jsonlite)
library(httr)


ukhp_get <- function(x, frequency = "monthly", release = "latest", classification = "nuts1") {
  endpoint <- "https://lancs-macro.github.io/uk-house-prices"
  query <- paste(endpoint, release, frequency, paste0(classification, ".csv"), sep = "/")
  request <- GET(query)
  stop_for_status(request)
  parse_json(request, simplifyVector = TRUE)
} 

