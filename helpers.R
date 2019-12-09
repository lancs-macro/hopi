library(jsonlite)
library(httr)

json_to_csv <- function(x, filepath = "latest/nuts1.csv") {
  df <- fromJSON(x)
  write.csv(df, file = paste0("data/", filepath))
}

# json_to_csv(x = "docs/latest/monthly/nuts1.json")
# json_to_csv(x = "docs/latest/monthly/nuts2.json", "latest/nuts2.csv")
# json_to_csv(x = "docs/latest/monthly/nuts3.json", "latest/nuts3.csv")


ukhp_get <- function(x, frequency = "monthly", classification = "nuts1", release = "latest") {
  endpoint <- "https://lancs-macro.github.io/uk-house-prices"
  query <- paste(endpoint, release, frequency, paste0(classification, ".json"), sep = "/")
  request <- GET(query)
  stop_for_status(request)
  parse_json(content(request), simplifyVector = TRUE)
} 

