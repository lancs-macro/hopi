#' Get House Price Data 
#' 
#' The Housing Observatory Price Index (hopi) captures changes in the value of 
#' residential properties with the repeated sales index. Data are available at
#' national and regional levels(nuts1, nuts2, nuts3).
#' 
#' @param release The release to download the data from. The index is begin updated
#' quarterly and is subject to revisions. Defaults at last release.
#' @param frequency The frequency to download the data. Can be `annual`, `quarterly` or `monthly`.
#' @param classification The regional classification to download the data from.
#' Can be one of  `aggregate`, `nuts1`, `nuts2`, `nuts3`.
#' 
#' @return A tibble with the first column to be Date, and the rest to be house Prices.
#' 
#' @export
#' @examples 
#' \donttest{
#' hopi_get(classification = "aggregate", frequency = "quarterly")
#' }
hopi_get <- function(release = last_release(), frequency = "monthly", classification = "nuts1") {
  endpoint <- "https://raw.githubusercontent.com/lancs-macro/hopi/master/data"
  stopifnot(
    release %in% avail_releases(),
    frequency %in% c("annual", "quarterly", "monthly"),
    classification %in% c("aggregate", "nuts1", "nuts2", "nuts3")
  )
  query <- paste(endpoint, release, frequency, paste0(classification, ".csv"), sep = "/")
  date_freq <- function(frequency, x) {
    if(frequency == "annual") {
      lubridate::ymd(x, truncated = 2)
    }else if(frequency == "quarterly"){
      zoo::as.Date(zoo::as.yearqtr(x))
    }else if(frequency == "monthly"){
      zoo::as.Date(zoo::as.yearmon(x))
    }
  }
  suppressMessages({
    readr::read_csv(query) %>% 
      mutate(Date = date_freq(frequency, Date))
  })
} 
