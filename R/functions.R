



# Download and Clean ------------------------------------------------------

#' Downloads the raw lang-registry file from amazon servers
#' 
#' The reason I am not using wget is that it has to be installed externally
#' `downlad.file` may truncate the downloaded fileif it used for files > 2G.
#' Try to use it one per session / or restart the session and use `gc()`.
#' @importFrom utils download.file
#' @export
download_lr_file <- function() {
  download.file("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv", 
                destfile = "data/raw.csv")
}

#' Sources the `data-preparation.sh` script, which cleans the lr-file for commas 
#' 
#' @export
clean_lr_file <- function() {
  shell("inst/data-preparation.sh")
}

# Clean the data ----------------------------------------------------------

#' @importFrom data.table fread := setkey setnames
#' @importFrom bizdays bizseq load_quantlib_calendars is.bizday
clean_data <- function(path = "", 
                       nuts_path = "data-raw/nuts123pc.csv",
                       price_low = 10000, price_high = 1500000) {
  
  # Read Land registry transcation prices
  main <- fread(path, header = F, drop = c("V1", "V5", "V6", "V7", "V10", "V11", "V12", "V13", "V14", "V16"))
  # main <- main[, c("V1", "V5", "V6", "V7", "V10", "V11", "V12", "V13", "V14", "V16")  := NULL]
  # give names to variables
  setnames(main, c("Price", "Date", "Postcode", "PAON", "SAON", "PPCategory"))
  # Read nuts classification together with the corresponding postcodes (created using script regional-classification.R) 
  nuts <- fread(nuts_path)
  # get the dates
  dates <- sort(unique(main[, Date]))
  
  # loading UK calendar and creating binary bizday  variable --------
  load_quantlib_calendars("UnitedKingdom", from = dates[1], to = dates[length(dates)])
  bizdates <- bizseq(dates[1], dates[length(dates)], "QuantLib/UnitedKingdom")
  main <- main[, bizday := is.bizday(Date, cal = "QuantLib/UnitedKingdom")]
  
  ## applying conditions: business days only, price min 10000, price max 1500000, PPCategory A: Standard Price Paid, Postcode is not empty
  main <- main[(bizday == T & Price > price_low & Price < price_high & PPCategory == "A" & Postcode != "")]
  
  # Merging Land Registry Data with EC main for NUTS
  main <- merge(main, nuts, by = "Postcode")
  setkey(main, NULL) ## remove key
  main
}


# Calculate the repeated-sales index --------------------------------------

# gclass = "nuts1"
# freq = "quarterly"
# period_trans = 100
# ntras_low = 1
# ntrans_high = 8
# abs_annual_ret = 0.15

#' @importFrom lubridate year
#' @importFrom purrr map compact reduce map_lgl
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols select everything
#' @importFrom tidyr gather
#' @importFrom zoo as.yearmon as.yearqtr coredata zoo
#' @importFrom Matrix sparseMatrix solve
#' @importFrom ISOweek ISOweek2date
#' @importFrom data.table data.table .GRP .SD .N key setorder setkeyv .SD shift
rsindex <- function(data, gclass = c("nuts1", "nuts2", "nuts3", "countries", "uk"), 
                    freq = c("monthly", "quarterly", "annual", "daily", "weekly"),
                    period_trans = 100, ntras_low = 1, ntrans_high = 8, abs_annual_ret = 0.15) {
  
  gclass <- match.arg(gclass)
  freq <- match.arg(freq)
  
  gareas <- unique(data[[gclass]])
  
  gnames_id <- switch(gclass, nuts1 = "nm", nuts2 = "nm2", nuts3 = "nm3", countries = "countries", uk = "uk")
  gnames <- unique(data[[gnames_id]])
  
  # Select data frequency ---------------------------------------------------
  # Maybe consider having time_diff also at the monhtly level to exclude trans that appear shorter thatn 6 months
  if (freq == "daily") {
    date_conv <- function(x) as.Date(x, format = "%Y-%m-%d")
    date_scalar_idx <- 1
    date_scalar_absret <- 365
    time_diff <- 180
  }else if (freq == "weekly") {
    date_scalar_idx <- 1
    date_scalar_absret <- 52
    time_diff <- 26
  }else if (freq == "monthly") {
    date_conv <- function(x) zoo::as.yearmon(x, format = "%Y-%m-%d")
    date_scalar_idx <- date_scalar_absret <- 12
    time_diff <- 6
  }else if (freq == "quarterly") {
    date_conv <- function(x) zoo::as.yearqtr(x, format = "%Y-%m-%d") 
    date_scalar_idx <- date_scalar_absret <- 4
    time_diff <- 2
  }else if (freq == "annual") {
    date_conv <- lubridate::year
    date_scalar_idx <- date_scalar_absret <- 1
    time_diff <- 0
  }
  
  price_level <- list()
  for (kk in 1:length(gareas)) {
    
    print(gareas[kk])
    # Filter for the region with every repetition
    ed <- data[(eval(as.name(gclass)) == gareas[kk])]
    
    ### CREATE Dates Using the Zoo function as.yearmon or as.yearqtr - as specified in the beginning of the file
    if (freq == "weekly") {
      den <- 7 # denominator only for weeks
      ed[,WeekAux := paste(ISOweek(Date),"-3",sep = "")]; # Transform all days to Wednesdays
      ed[,Period := ISOweek2date(WeekAux) ];
    }else{
      den <- 1
      ed[, Period := date_conv(Date)]
    }
    
    # Counting Transactions per time period, and if there are less than X (say 1000) transactions remove the time period
    counts <- ed[, .(rowCount = .N), by = Period]
    counts <- counts[rowCount > period_trans] # was 1000
    ed <- ed[(Period %in% counts$Period)]
    
    ############################################################################################
    # Drop columns to reduce table dimension. Keeping only: price ,date, postcode, PAON and SAON
    ed <- ed[, c("bizday", "PPCategory") := NULL]
    
    # Set key by Postcode, PAON and SAON
    setkeyv(ed, c("Postcode", "PAON", "SAON"))
    ed[, i := .GRP, by = key(ed)]
    
    # Order by date
    setorder(ed, i, Period)
    
    # Removing properties with a single transaction or with more than 8 transactions
    ed[, ntrans := .N, by = "i"] # ntrans: number of transactions per property
    ed <- ed[(ntrans > ntras_low & ntrans < ntrans_high)]
    
    ########################### Pairing Transactions ######################
    
    ed[, c("Lag_Price", "Lag_Date", "Lag_Period") := shift(.SD, 1, NA, "lag"), .SDcols = c("Price", "Date", "Period"), by = i]
    
    ### Remove lines with NA or Date1==Date2
    dd <- ed[(Price != "NA" & Period != Lag_Period)]
    
    # Creating Index For Months and calculate the time difference between transactions in months
    
    # Start Date
    start_date <- min(dd$Period)
    dd[, Time1index := date_scalar_idx * as.numeric(dd$Lag_Period - start_date)/den + 1]
    dd[, Time2index := date_scalar_idx * as.numeric(dd$Period - start_date)/den + 1]
    dd[, TimeDiff := Time2index - Time1index]
    
    # Remove Entries with Time Difference less than 180days/26weeks/6months and with Absolute Annual Returns higher than 15%
    dd <- dd[TimeDiff > time_diff]
    dd[, an_ret := (log(Price) - log(Lag_Price)) * date_scalar_absret / TimeDiff]
    dd <- dd[(abs(an_ret) <= abs_annual_ret)]
    
    # Create "Continuous" Day Indices from unique days
    ntime <- data.table(time = unique(c(dd$Time1index, dd$Time2index)))
    setorder(ntime, time) # order them
    ntime[, timeid := .GRP, by = "time"] # assign indexes
    dd <- merge(dd, ntime[, .(Time1index = time, time1id = timeid)], by = "Time1index", all.x = TRUE) # merge left join
    dd <- merge(dd, ntime[, .(Time2index = time, time2id = timeid)], by = "Time2index", all.x = TRUE) # merge left join
    
    # Estimation of House Price Index
    timediff <- dd$TimeDiff
    dd[, y := log(Price) - log(Lag_Price)]
    Ntime <- max(dd$time2id) # number of explanatory variables = number of days minus one
    N <- nrow(dd)
    i <- c(1:N, 1:N)
    j <- as.numeric(c(dd$time1id, dd$time2id))
    x <- as.numeric(c(rep(-1, N), rep(1, N)))
    
    # Catching error - in case matrix is singular
    tryCatch({
      # Sparse X matrix creation and 3 stage least squares regression
      mm <- sparseMatrix(i = i, j = j, x = x, dims = c(N, Ntime))[, -1] # create sparse matrix
      sparse.sol <- solve(crossprod(mm), crossprod(mm, dd$y)) # 8 seconds; solve (X'X)^-1X'y to obtain coefficient vector
      error <- dd$y - tcrossprod(mm, t(sparse.sol)) # a second; compute error=y-X'b
      error2 <- error * error # squared residuals from first stage regression
      beta_error <- solve(crossprod(cbind(1, timediff)), crossprod(cbind(1, timediff), error2)) # coefficients for second regression
      sq_fitted <- sqrt(tcrossprod(cbind(1, timediff), t(beta_error))) # compute square of fitted values for second regression
      dm <- dim(sq_fitted)[1]
      diagg <- sparseMatrix(i = 1:dm, j = 1:dm, x = 1 / sq_fitted, dims = c(dm, dm))
      beta_third <- solve(crossprod(mm, diagg %*% mm), crossprod(mm, diagg %*% dd$y)) # solve (X'X)^-1X'y to obtain FINAL coefficient vector
      
      ########### Getting Dates
      un_dates <- sort(unique(c(dd$Lag_Period, dd$Period)))
      
      ###### Saving Prices
      log_prices <- zoo(as.vector(beta_third), un_dates[-1])
      price_level[[kk]] <- exp(log_prices)
    }, 
    error = function(x) price_level[[kk]] <- x ,
    warning = function(x) price_level[[kk]] <- x
    )
  }
  # Catching error in case aggregation cannot happen and return price_level as list otherwise
  tryCatch({
    names_remain <- map_lgl(price_level, is.null)
    new_names <- gnames[!names_remain]
    suppressWarnings({
      x <- price_level %>% 
        compact() %>%
        map(coredata) %>% 
        reduce(cbind) %>% 
        `colnames<-`(new_names) %>% 
        as_tibble() %>% 
        bind_cols(index = index(price_level[[1]])) %>% 
        dplyr::select(index, everything())
      return(x)
    })
    }, error = function(x) return(price_level)
  )
}

