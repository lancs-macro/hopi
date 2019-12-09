library(jsonlite)
library(httr)

json_to_csv <- function(x, filepath = "latest/mtcars.csv") {
  df <- fromJSON(x)
  write.csv(df, file = paste0("data/", filepath))
}

ukhp_get <- function(x, frequency = "monthly", classification = "nuts1", release = "latest") {
  endpoint <- "https://lancs-macro.github.io/uk-house-prices"
  query <- paste(endpoint, release, frequency, paste0(classification, ".json"), sep = "/")
  request <- GET(query)
  stop_for_status(request)
  parse_json(content(request), simplifyVector = TRUE)
} 

