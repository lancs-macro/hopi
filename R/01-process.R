

#'  Land registry transcaction data file
#'
#'
#' @export
lr_url <-
  "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv"

dowload_file <- function(url = lr_url, path = "temp/main.csv") {
  if (!dir_exists("temp")) {
    fs::dir_create("temp")
  }
  if (file_exists(path)) {
    stop("file already exists")
  }
  download.file(url, destfile = path, quiet = FALSE)
  return(path)
}

remove_file <- function(path) {
  if (file_exists(path)) {
    fs::file_delete(path)
  }
}


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
process_data <-
  function(path = lr_url,
           end_date = next_release_to_date(),
           price_low = 10000,
           price_high = 1500000) {
    #"temp/main.csv",
    #nuts_path = "temp/nuts123pc.csv", # try using system.file("nuts.rds", package = "hopir")
    
    if (is.null(end_date)) {
      stop("please provide an `end_date`")
    }
    # loading UK calendar and creating binary bizday  variable --------
    load_quantlib_calendars("UnitedKingdom", from = dates[1], to = dates[length(dates)])
    
    # Read Land registry transcation prices
    main <-
      fread(
        path,
        header = F,
        drop = c("V1", "V5", "V6", "V7", "V10", "V11", "V12", "V13", "V14", "V16"),
        showProgress = TRUE
      )
    # main <- main[, c("V1", "V5", "V6", "V7", "V10", "V11", "V12", "V13", "V14", "V16")  := NULL]
    # give names to variables
    setnames(main,
             c("Price", "Date", "Postcode", "PAON", "SAON", "PPCategory"))
    # Read nuts classification together with the corresponding postcodes (created using script regional-classification.R)
    # nuts <- fread(nuts_path)
    nuts <-
      data.table(readRDS(system.file("nuts_ruc.rds", package = "hopi")))
    # get the dates
    dates <- sort(unique(main[, Date]))
    
    
    # bizdates <- bizseq(dates[1], dates[length(dates)], "QuantLib/UnitedKingdom")
    main <-
      main[, bizday := is.bizday(Date, cal = "QuantLib/UnitedKingdom")]
    
    ## applying conditions: business days only, price min 10000, price max 1500000, PPCategory A: Standard Price Paid, Postcode is not empty
    main <-
      main[(bizday == T &
              Price > price_low &
              Price < price_high & PPCategory == "A" & Postcode != "")]
    
    ## filter by end_date to create full period releases
    if (as.Date(end_date) != Sys.Date()) {
      main <- main[Date <= as.Date(end_date)]
    }
    
    # Merging Land Registry Data with EC main for NUTS
    out <- merge(main, nuts, by = "Postcode")
    setkey(out, NULL) ## remove key
    out[order(Date)]
    
  }
