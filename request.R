library(httr)
library(jsonlite)

endpoint <-  "https://lancs-macro.github.io/uk-house-prices"

query <- paste(endpoint, "2019-09", "monthly", "index.json", sep = "/")

GET(query) %>% 
  content() %>% 
  parse_json(simplifyVector = TRUE)

