
#'  Land registry transcaction data file
#'  
#'  
#' @export
lr_url <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv"

#' Download and process data
#' 
#' @param path the pathfile.
#' @param end_date the end date.
#' @param price_low the low threshold.
#' @param price_high the high threshold.
#' 
#' @importFrom data.table fread := setkey setnames
#' @importFrom bizdays bizseq load_quantlib_calendars is.bizday bizseq
#' @export
process_data <- function(path = lr_url, end_date = Sys.Date(), price_low = 10000, price_high = 1500000 ){ #"temp/main.csv", 
  #nuts_path = "temp/nuts123pc.csv", # try using system.file("nuts.rds", package = "hopir")
  
  if(is.null(end_date)) {
    stop("plese provide an `end_date`")
  }
  
  # Read Land registry transcation prices
  main <- fread(path, header = F, drop = c("V1", "V5", "V6", "V7", "V10", "V11", "V12", "V13", "V14", "V16"), showProgress = FALSE)
  # main <- main[, c("V1", "V5", "V6", "V7", "V10", "V11", "V12", "V13", "V14", "V16")  := NULL]
  # give names to variables
  setnames(main, c("Price", "Date", "Postcode", "PAON", "SAON", "PPCategory"))
  # Read nuts classification together with the corresponding postcodes (created using script regional-classification.R) 
  # nuts <- fread(nuts_path)
  nuts <- data.table(readRDS(system.file("nuts.rds", package = "hopi")))
  # get the dates
  dates <- sort(unique(main[, Date]))
  
  # loading UK calendar and creating binary bizday  variable --------
  load_quantlib_calendars("UnitedKingdom", from = dates[1], to = dates[length(dates)])
  # bizdates <- bizseq(dates[1], dates[length(dates)], "QuantLib/UnitedKingdom")
  main <- main[, bizday := is.bizday(Date, cal = "QuantLib/UnitedKingdom")]
  
  ## applying conditions: business days only, price min 10000, price max 1500000, PPCategory A: Standard Price Paid, Postcode is not empty
  main <- main[(bizday == T & Price > price_low & Price < price_high & PPCategory == "A" & Postcode != "")]
  
  ## filter by end_date to create full period releases
  if(end_date != Sys.Date()) {
    main <- main[Date <= end_date]
  }
  
  # Merging Land Registry Data with EC main for NUTS
  out <- merge(main, nuts, by = "Postcode")
  setkey(out, NULL) ## remove key
  out[order(Date)]
}


#'  Get the release dates for the hopi
#'  
#' @importFrom lubridate ymd %m-% ceiling_date days year quarter
#' @importFrom dplyr if_else mutate arrange
#' @export
release_dates <- function() {
  
  years <- 2020:2023
  start_months <- c(1,4,7,10)
  end_months <- c(3,6,9,12)
  
  start_ym <- expand.grid(year = years, month = start_months)
  end_ym <- expand.grid(year = years, month = end_months)
  
  start_dates <- ymd(paste(start_ym$year, start_ym$month,"1", sep = "-"))
  end_dates <- lubridate::ceiling_date(ymd(paste(end_ym$year, end_ym$month, "1", sep = "-")), "month") %m-% days(1)
  
  name_release <- paste0(year(start_dates), "-Q",quarter(start_dates))
  
  released_dirs <- list.dirs("data/", recursive = FALSE, full.names = FALSE)
  
  data.frame(
    from = start_dates,
    to = end_dates,
    name = name_release
  ) %>% 
    mutate(released = if_else(name %in% released_dirs, "X", ""))  %>% 
    arrange(from)

}



