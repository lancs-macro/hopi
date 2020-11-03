library(readr)

ukhp_get <- function(release = "2020-Q3", frequency = "monthly", classification = "nuts1") {
  endpoint <- "https://raw.githubusercontent.com/lancs-macro/hopi/master/data"
  query <- paste(endpoint, release, frequency, paste0(classification, ".csv"), sep = "/")
  read_csv(query)
} 
ukhp_get()