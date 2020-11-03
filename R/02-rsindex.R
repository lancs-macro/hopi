
# Calculate the repeated-sales index --------------------------------------

# data <- td
# gclass = "london_effect"
# freq = "monthly"
# period_trans = 100
# ntras_low = 1
# ntrans_high = 8
# abs_annual_ret = 0.15

#' @importFrom lubridate year
#' @importFrom purrr map map2 compact reduce map_lgl map_dbl
#' @importFrom tibble tibble as_tibble add_column
#' @importFrom dplyr bind_cols select everything
#' @importFrom tidyr gather
#' @importFrom zoo as.yearmon as.yearqtr coredata zoo
#' @importFrom Matrix sparseMatrix solve crossprod tcrossprod t
#' @importFrom ISOweek ISOweek2date
#' @importFrom progress progress_bar
#' @importFrom data.table data.table .GRP .SD .N key setorder setkeyv .SD shift
#' 
#' @export
rsindex <- function(data, gclass = c("nuts1", "nuts2", "nuts3", "countries", "uk", "london_effect"), 
                    freq = c("monthly", "quarterly", "annual", "daily", "weekly"),
                    period_trans = 100, ntras_low = 1, ntrans_high = 8, abs_annual_ret = 0.15) {
  
  gclass <- match.arg(gclass)
  freq <- match.arg(freq)
  
  gareas <- unique(data[[gclass]])
  gnames_id <- switch(gclass, nuts1 = "nm", nuts2 = "nm2", nuts3 = "nm3", countries = "countries", uk = "uk", 
                      london_effect = "london_effect")
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
  
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent) eta: :eta", width = 70, total = length(gareas))
  
  price_level <- list()
  suppressWarnings({
    for (kk in 1:length(gareas)) {
      
      pb$tick()
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
  })
  is_null <- map_lgl(price_level, is.null)
  length_diff <- length(is_null) < length(gareas)
  if(length_diff > 0) {
    pad_false <- rep(length_diff)
    is_null <- c(is_null, pad_false)
  }
  new_names <- gnames[!is_null]
  names(price_level) <- new_names
  
  out_coredata <- price_level %>% 
    compact() %>% 
    map(coredata) 
  
  out <- out_coredata %>% 
    pad_uneven_cols() %>% 
    bind_cols() %>% 
    add_column(Date = idx_max(price_level)) %>% 
    select(Date, everything()) %>% 
    dplyr::rename("England and Wales" = "United Kingdom")
  
  structure(
    out,
    geo_class = gclass,
    geo_areas = gareas,
    geo_names = gnames,
    frequency = freq,
    names_keep = gnames[!is_null],
    names_drop = gnames[is_null]
  )
}

#' @importFrom purrr map2 map reduce map_dbl
reduce_join <- function(x, y, z) {
  union_attrs <- purrr::map2(attributes(x), attributes(y), union) %>% 
    purrr::map2(attributes(z), union) %>% 
    map(~ .x[!is.na(.x)])
  out <- reduce(list(x, y, z), full_join, by = "Date")
  attributes(out) <- union_attrs
  out
}

idx_max <- function(x) {
  col_lengths <- map_dbl(x, length)
  col_num <- which.max(col_lengths)
  index(x[[col_num]])
}

pad_uneven_cols <- function(x) {
  col_lengths <- map_dbl(x, length)
  nmax <- max(col_lengths)
  npads <- nmax - col_lengths
  map2(x, npads, ~ c(.x, rep(NA, .y)))
}



